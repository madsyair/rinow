# Machine-learning regressions, the bottom-up (GDPNow-type) model and
# combinations of models.

#' Machine-learning nowcasts
#'
#' Features are the most recent `n_lags` monthly values of every indicator
#' (the U-MIDAS layout) plus the last released target growth. Tuning uses
#' cross-validation over five consecutive time blocks, never random folds.
#'
#' * `"lasso"`, `"enet"`: penalised regression with 'glmnet' (alpha 1 or 0.5).
#' * `"rf"`: random forest with 'ranger' (500 trees).
#' * `"xgb"`: gradient boosting with 'xgboost' (depth 2, learning rate 0.05,
#'   200 rounds, `min_leaf` minimum child weight).
#' * `"sglasso"`: sparse-group LASSO MIDAS with 'midasml' (Babii, Ghysels and
#'   Striaukas 2022): six lags per indicator projected on Legendre
#'   polynomials of degree two, one group per indicator.
#'
#' @param indicators Monthly indicators.
#' @param method One of `"lasso"`, `"enet"`, `"rf"`, `"xgb"`, `"sglasso"`.
#' @param n_lags Monthly lags per indicator (`"sglasso"` uses six).
#' @param min_leaf Minimum child weight of 'xgboost' trees.
#' @param name Label.
#' @return An `MLModel` object.
#' @examples
#' ml_model(c("x1", "x2"), "lasso")
#' @export
ml_model <- function(indicators, method = c("lasso", "enet", "rf", "xgb", "sglasso"), n_lags = 3, min_leaf = 5,
                     name = NULL) {
  method <- match.arg(method)
  if (is.null(name)) {
    name <- c(
      lasso = "LASSO", enet = "Elastic net", rf = "Random forest", xgb = "XGBoost",
      sglasso = "sg-LASSO"
    )[[method]]
  }
  methods::new("MLModel",
    name = name, indicators = as.character(indicators), method = method,
    n_lags = if (method == "sglasso") 6 else n_lags, min_leaf = min_leaf
  )
}

ml_features <- function(v, target, indicators, J) {
  dm <- midas_frame(v, target, indicators)
  X <- do.call(cbind, lapply(indicators, function(k) {
    L <- matrix(midas_lags(dm$x[[k]], dm$k[[k]], J), ncol = J)
    colnames(L) <- paste0(k, "_L", 0:(J - 1L))
    L
  }))
  ylag <- c(rep(NA, dm$d), utils::head(dm$y, -dm$d))
  list(X = cbind(y_lag = ylag, X), y = dm$y)
}

#' @rdname nowcast
setMethod("nowcast", "MLModel", function(model, data, target, as_of = NULL, ...) {
  pkg <- c(lasso = "glmnet", enet = "glmnet", rf = "ranger", xgb = "xgboost", sglasso = "midasml")[[model@method]]
  need_package(pkg, paste("The", model@name, "model"))
  v <- prepare_vintage(data, as_of)
  target <- check_target(v, target)
  check_names(model@indicators, indicators(v), "indicator")
  fx <- ml_features(v, target, model@indicators, model@n_lags)
  n <- nrow(fx$X)
  ok <- stats::complete.cases(fx$X[-n, , drop = FALSE]) & !is.na(fx$y[-n])
  Xl <- fx$X[-n, , drop = FALSE][ok, , drop = FALSE]
  yl <- fx$y[-n][ok]
  xb <- fx$X[n, , drop = FALSE]
  folds <- ceiling(seq_along(yl) / ceiling(length(yl) / 5))
  imp <- numeric()
  if (model@method %in% c("lasso", "enet")) {
    fit <- glmnet::cv.glmnet(Xl, yl, alpha = if (model@method == "lasso") 1 else 0.5, foldid = folds)
    point <- as.numeric(stats::predict(fit, xb, s = "lambda.min"))
    cf <- as.matrix(stats::coef(fit, s = "lambda.min"))[-1L, 1L]
    imp <- cf[cf != 0]
  } else if (model@method == "rf") {
    fit <- ranger::ranger(x = Xl, y = yl, num.trees = 500, importance = "permutation", seed = 1, num.threads = 1)
    point <- stats::predict(fit, xb, num.threads = 1)$predictions
    imp <- sort(fit$variable.importance, decreasing = TRUE)
  } else if (model@method == "xgb") {
    fit <- xgboost::xgb.train(
      params = list(
        objective = "reg:squarederror", eta = 0.05, max_depth = 2,
        min_child_weight = model@min_leaf, lambda = if (model@min_leaf > 1) 5 else 1,
        subsample = 0.8, colsample_bytree = 0.8, nthread = 1
      ),
      data = xgboost::xgb.DMatrix(Xl, label = yl), nrounds = 200, verbose = 0
    )
    point <- stats::predict(fit, xgboost::xgb.DMatrix(xb))
  } else {
    B <- midasml::lb(2, a = 0, b = 1, jmax = 6)
    proj <- function(M) do.call(cbind, lapply(model@indicators, function(k) M[, paste0(k, "_L", 0:5), drop = FALSE] %*% B))
    P <- cbind(Xl[, "y_lag"], proj(Xl))
    Pb <- cbind(xb[, "y_lag"], proj(xb))
    P <- scale(P)
    Pb <- scale(Pb, attr(P, "scaled:center"), attr(P, "scaled:scale"))
    gidx <- c(1L, rep(seq_along(model@indicators) + 1L, each = ncol(B)))
    fit <- midasml::cv.sglfit(P, yl - mean(yl), gamma = 0.5, gindex = gidx, foldid = folds)
    b <- as.numeric(fit$cv.fit$lam.min$beta)
    point <- mean(yl) + sum(Pb * b)
    grp <- c("y_lag", model@indicators)[gidx]
    imp <- tapply(abs(b), grp, sum)
    imp <- imp[imp > 0]
  }
  s <- stats::sd(yl - mean(yl)) # rough scale: ML models give no interval; report the target's dispersion
  new_result(model, v, target, point, details = list(fit = fit, importance = imp, dispersion = s))
})

# ----------------------------------------------------------- bottom-up -----

#' Bottom-up nowcast of expenditure components (GDPNow-type)
#'
#' Nowcasts expenditure components and adds up their contributions, following
#' the steps of the Atlanta Fed's GDPNow (Higgins 2014): (1) a quarterly BVAR
#' of the component growth rates gives anchor forecasts; (2) a one-factor DFM
#' on `factor_indicators` gives an activity factor; (3) factor-augmented
#' autoregressions (lags chosen by AIC) fill the months not yet released;
#' (4) bridge equations map the filled indicators to each component and are
#' combined with the BVAR anchor with a weight in \[0, 1\] chosen in sample;
#' (6) contributions (lagged share times growth) are summed. The residual
#' `rest` (target minus signed components) and the `grouped` components are
#' modelled together as one contribution, because their movements offset each
#' other (for example imports and inventories).
#'
#' The target and components must be additive levels (for example GDP at
#' constant prices).
#'
#' @param components Named character vector of signs, e.g.
#'   `c(consumption = "+", imports = "-")`.
#' @param map Named list of bridge indicators for each modelled component and
#'   `"rest"`; components without indicators use the BVAR anchor only.
#' @param grouped Components merged into `rest`.
#' @param factor_indicators Indicators of the activity factor.
#' @param start First quarter of the sample.
#' @param name Label.
#' @return A `BottomUpModel` object.
#' @examples
#' bottom_up_model(c(c1 = "+", c2 = "+"),
#'   map = list(c1 = "x1", c2 = "x2", rest = "x3"),
#'   factor_indicators = c("x1", "x2", "x3")
#' )
#' @export
bottom_up_model <- function(components, map, grouped = character(), factor_indicators,
                            start = "2002-01-01", name = "Bottom-up") {
  methods::new("BottomUpModel",
    name = name, indicators = unique(c(unlist(map), factor_indicators)),
    components = components, map = map, grouped = grouped,
    factor_indicators = as.character(factor_indicators), start = quarter_start(start)
  )
}

component_contributions <- function(q, target, components, grouped) {
  Y <- q[[target]]
  sg <- ifelse(components == "+", 1, -1)
  rest <- Y - unname(colSums(t(as.matrix(q[names(components)])) * sg))
  modelled <- setdiff(names(components), grouped)
  rest_all <- rest + if (length(grouped)) unname(colSums(t(as.matrix(q[grouped])) * sg[grouped])) else 0
  growth <- vapply(modelled, function(k) c(NA, 100 * diff(q[[k]]) / utils::head(q[[k]], -1L)), numeric(nrow(q)))
  growth <- matrix(growth, ncol = length(modelled), dimnames = list(NULL, modelled))
  share <- vapply(modelled, function(k) c(NA, sg[[k]] * utils::head(q[[k]] / Y, -1L)), numeric(nrow(q)))
  share <- matrix(share, ncol = length(modelled), dimnames = list(NULL, modelled))
  rest_c <- c(NA, 100 * diff(rest_all) / utils::head(Y, -1L))
  list(growth = growth, share = share, rest = rest_c, date = q$date)
}

factor_fill <- function(v, target, factor_indicators, fill) {
  need_package("dfms", "The bottom-up model")
  sp <- stationary_panel(v, target)
  end_m <- seq(target, by = "month", length.out = 3L)[3L]
  tm <- seq(min(sp$m$date), end_m, by = "month")
  Xf <- vapply(factor_indicators, function(k) sp$m[[k]][match(tm, sp$m$date)], numeric(length(tm)))
  first <- max(vapply(factor_indicators, function(k) min(which(!is.na(Xf[, k]))), 0))
  fit <- dfms::DFM(Xf[first:length(tm), , drop = FALSE], r = 1, p = 2, em.method = "BM", max.missing = 1, max.iter = 300)
  f <- c(rep(NA, first - 1L), fit$F_qml[, 1L])
  out <- lapply(fill, function(k) {
    x <- sp$m[[k]][match(tm, sp$m$date)]
    last <- max(which(!is.na(x)))
    if (last < length(x)) {
      best <- NULL
      for (q in 1:6) {
        d <- data.frame(y = x, f0 = f, vapply(seq_len(q), function(j) c(rep(NA, j), utils::head(x, -j)), numeric(length(x))))
        fq <- stats::lm(y ~ ., data = d[seq_len(last), ])
        if (is.null(best) || stats::AIC(fq) < stats::AIC(best$fit)) best <- list(fit = fq, q = q)
      }
      for (t in (last + 1L):length(x)) {
        nd <- data.frame(f0 = f[t], t(x[t - seq_len(best$q)]))
        names(nd) <- c("f0", paste0("X", seq_len(best$q)))
        x[t] <- stats::predict(best$fit, nd)
      }
    }
    structure(x, date = tm)
  })
  names(out) <- fill
  list(series = out, factor = f, dates = tm)
}

#' @rdname nowcast
setMethod("nowcast", "BottomUpModel", function(model, data, target, as_of = NULL, ...) {
  need_package("BVAR", "The bottom-up model")
  v <- prepare_vintage(data, as_of)
  target <- check_target(v, target)
  check_names(names(model@components), names(v@quarterly), "component")
  check_names(model@indicators, indicators(v), "indicator")
  q <- v@quarterly
  tq <- seq(max(model@start, min(q$date)), target, by = "quarter")
  q <- q[match(tq, q$date), , drop = FALSE]
  q$date <- tq
  cc <- component_contributions(q, v@target, model@components, model@grouped)
  G <- cbind(cc$growth, rest = cc$rest)
  rownames(G) <- NULL
  parts <- colnames(G)
  last <- max(which(stats::complete.cases(G)))
  h <- nrow(G) - last
  Y <- G[2:last, , drop = FALSE]
  bv <- BVAR::bvar(Y, lags = 2, n_draw = 3000, n_burn = 1000, verbose = FALSE, priors = minnesota_prior(Y))
  anchor <- stats::predict(bv, horizon = max(h, 2L), conf_bands = 0.16)$quants[2L, h, ]
  names(anchor) <- parts
  fitted_bv <- stats::fitted(bv, type = "mean")
  fill <- unique(unlist(model@map))
  ff <- if (length(fill)) factor_fill(v, target, model@factor_indicators, fill) else NULL
  reg <- lapply(fill, function(k) {
    b <- bridge_regressor(v, k, target, filled = ff$series[[k]])
    b$value[match(tq, b$date)]
  })
  names(reg) <- fill
  pred <- anchor
  info <- list()
  for (k in intersect(parts, names(model@map))) {
    ind <- model@map[[k]]
    if (!length(ind)) next
    d <- data.frame(y = G[, k], as.data.frame(reg[ind]))
    d$y_lag <- c(NA, utils::head(d$y, -1L))
    form <- stats::reformulate(c(ind, if (h == 1L) "y_lag"), "y")
    fb <- stats::lm(form, data = d[seq_len(last), ])
    bridge <- as.numeric(stats::predict(fb, d[nrow(d), ]))
    pos <- (last - nrow(fitted_bv) + 1L):last
    fb_fit <- as.numeric(stats::predict(fb, d[pos, , drop = FALSE])) # in-sample fit, aligned by position
    ok <- !is.na(fb_fit)
    grid <- seq(0, 1, by = 0.05)
    sse <- vapply(grid, function(w) sum((G[pos, k][ok] - w * fb_fit[ok] - (1 - w) * fitted_bv[ok, match(k, parts)])^2), 0)
    w <- grid[which.min(sse)]
    pred[[k]] <- w * bridge + (1 - w) * anchor[[k]]
    info[[k]] <- c(bridge = bridge, bvar = anchor[[k]], weight = w, combined = pred[[k]])
  }
  # shares of the last released quarter
  Yl <- v@quarterly[[v@target]]
  lastq <- max(which(!is.na(Yl)))
  sg <- ifelse(model@components == "+", 1, -1)
  modelled <- setdiff(names(model@components), model@grouped)
  share <- vapply(modelled, function(k) sg[[k]] * v@quarterly[[k]][lastq] / Yl[lastq], 0)
  contrib <- c(pred[modelled] * share, rest = pred[["rest"]])
  pct <- sum(contrib)
  point <- if (v@target_transform == "dlog") 100 * log1p(pct / 100) else pct
  new_result(model, v, target, point, contributions = contrib, details = list(
    components = info, anchor = anchor,
    share = share, factor = ff
  ))
})

# --------------------------------------------------------- combination -----

#' Combination of models
#'
#' Runs every member model and returns the mean or median of their nowcasts.
#' Inverse-MSE weights need past errors; use [combine_nowcasts()] on
#' evaluations for that.
#'
#' @param ... Member models, or one list of models.
#' @param method `"mean"` or `"median"`.
#' @param name Label.
#' @return A `CombinationModel` object.
#' @examples
#' combination_model(ar_model(), bridge_model("x1"))
#' @export
combination_model <- function(..., method = c("mean", "median"), name = "Combination") {
  models <- list(...)
  if (length(models) == 1L && is.list(models[[1L]]) && !methods::is(models[[1L]], "NowcastModel")) models <- models[[1L]]
  if (!length(models) || !all(vapply(models, methods::is, logical(1), "NowcastModel"))) {
    rinow_abort("`models` must be a non-empty list of NowcastModel objects.", "rinow_error_argument")
  }
  methods::new("CombinationModel",
    name = name, indicators = unique(unlist(lapply(models, methods::slot, "indicators"))),
    models = models, method = match.arg(method)
  )
}

#' @rdname nowcast
setMethod("nowcast", "CombinationModel", function(model, data, target, as_of = NULL, ...) {
  v <- prepare_vintage(data, as_of)
  target <- check_target(v, target)
  res <- lapply(model@models, function(m) nowcast(m, v, target))
  pts <- vapply(res, methods::slot, 0, "point")
  names(pts) <- vapply(model@models, methods::slot, "", "name")
  point <- if (model@method == "mean") mean(pts) else stats::median(pts)
  new_result(model, v, target, point, min(pts), max(pts),
    level = NA_real_, contributions = pts,
    details = list(members = res)
  )
})
