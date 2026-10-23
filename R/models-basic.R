# Generic nowcast() and the regression-type models: AR, bridge, MIDAS, factors.

#' Nowcast with a model
#'
#' Applies a model specification to the information set of a data object and
#' returns the nowcast of the target growth rate for the quarter `target`.
#' When `data` is final data (no `as_of`), give `as_of` to build the vintage
#' first; the model never sees observations released after `as_of`.
#'
#' @param model A [NowcastModel-class] object.
#' @param data A [NowcastData-class] object.
#' @param target Target quarter, for example `"2026Q3"` or `"2026-07-01"`.
#' @param as_of Optional date of the information set.
#' @param ... Not used.
#' @return A [NowcastResult-class] object.
#' @examples
#' sim <- simulate_nowcast_data(n_years = 15, seed = 1)
#' nowcast(bridge_model(c("x1", "x2")), sim, "2014Q3", as_of = "2014-09-15")
#' @export
setGeneric("nowcast", function(model, data, target, as_of = NULL, ...) standardGeneric("nowcast"))

prepare_vintage <- function(data, as_of) {
  if (!is.null(as_of)) {
    return(vintage(data, as_of))
  }
  if (is.na(data@as_of)) data@as_of <- max(period_end(data@monthly$date, 1L))
  data
}

new_result <- function(model, v, target, point, lower = NA_real_, upper = NA_real_, level = 0.68,
                       contributions = numeric(), details = list()) {
  methods::new("NowcastResult",
    model = model@name, target = quarter_start(target), as_of = v@as_of,
    point = as.numeric(point), lower = as.numeric(lower), upper = as.numeric(upper), level = level,
    contributions = contributions, details = details
  )
}

check_target <- function(v, target) {
  target <- quarter_start(target)
  last <- max(v@quarterly$date[!is.na(v@quarterly[[v@target]])])
  if (target <= last) {
    rinow_abort(
      sprintf("The target %s is already released in this vintage.", format_quarter(target)),
      "rinow_error_target"
    )
  }
  if (length(seq(last, target, by = "quarter")) > 3L) {
    rinow_abort("The target lies more than two quarters after the last released quarter.", "rinow_error_target")
  }
  target
}

# ---------------------------------------------------------------- AR ---------

#' Benchmark autoregression
#'
#' AR(p) model of the target growth with `p <= max_order` chosen by AIC; it
#' iterates forecasts when the previous quarter is not yet released.
#' @param max_order Largest AR order.
#' @param name Label of the model.
#' @return An `ARModel` object.
#' @examples
#' ar_model()
#' @export
ar_model <- function(max_order = 4, name = "AR") {
  check_count(max_order, "max_order")
  methods::new("ARModel", name = name, indicators = character(), max_order = max_order)
}

#' @rdname nowcast
setMethod("nowcast", "ARModel", function(model, data, target, as_of = NULL, ...) {
  v <- prepare_vintage(data, as_of)
  target <- check_target(v, target)
  g <- target_growth(v@quarterly[[v@target]], v@target_transform)
  ok <- !is.na(g)
  last <- max(v@quarterly$date[ok])
  h <- length(seq(last, target, by = "quarter")) - 1L
  fit <- stats::ar(g[ok], order.max = model@max_order, aic = TRUE, method = "ols")
  p <- stats::predict(fit, n.ahead = h)
  se <- p$se[h]
  new_result(model, v, target, p$pred[h], p$pred[h] - se, p$pred[h] + se, details = list(order = fit$order))
})

# ------------------------------------------------------------ bridge ---------

#' Bridge equation
#'
#' Regresses target growth on quarterly averages of monthly indicators (and
#' its own lag). Months not yet released are filled with AR forecasts of the
#' stationary monthly series before aggregation. When the previous quarter is
#' not released, its growth is first predicted with the same equation.
#'
#' @param indicators Monthly indicators.
#' @param target_lag Include the lagged target?
#' @param start First quarter of the estimation sample.
#' @param name Label.
#' @return A `BridgeModel` object.
#' @examples
#' bridge_model(c("x1", "x2"))
#' @export
bridge_model <- function(indicators, target_lag = TRUE, start = "2002-01-01", name = "Bridge") {
  methods::new("BridgeModel",
    name = name, indicators = as.character(indicators), target_lag = target_lag,
    start = quarter_start(start)
  )
}

bridge_frame <- function(v, target, indicators, start) {
  g <- target_growth(v@quarterly[[v@target]], v@target_transform)
  tq <- seq(min(v@quarterly$date), target, by = "quarter")
  d <- data.frame(date = tq, y = g[match(tq, v@quarterly$date)])
  d$y_lag <- c(NA, utils::head(d$y, -1L))
  for (k in indicators) {
    r <- bridge_regressor(v, k, target)
    d[[k]] <- r$value[match(tq, r$date)]
  }
  d
}

#' @rdname nowcast
setMethod("nowcast", "BridgeModel", function(model, data, target, as_of = NULL, ...) {
  v <- prepare_vintage(data, as_of)
  target <- check_target(v, target)
  check_names(model@indicators, indicators(v), "indicator")
  d <- bridge_frame(v, target, model@indicators, model@start)
  form <- stats::reformulate(c(if (model@target_lag) "y_lag", model@indicators), "y")
  # the lag of the first estimation quarter comes from before `start`
  fit <- stats::lm(form, data = d[!is.na(d$y) & d$date >= model@start, ])
  n <- nrow(d)
  gap <- which(is.na(d$y) & seq_len(n) < n & seq_len(n) > max(which(!is.na(d$y))))
  for (i in gap) {
    d$y[i] <- stats::predict(fit, d[i, ])
    d$y_lag[i + 1L] <- d$y[i]
  }
  pr <- stats::predict(fit, d[n, ], se.fit = TRUE)
  s <- sqrt(pr$se.fit^2 + summary(fit)$sigma^2)
  cf <- stats::coef(fit)
  x <- unlist(d[n, names(cf)[-1L]])
  contrib <- c(constant = unname(cf[1L]), stats::setNames(cf[-1L] * x, names(cf)[-1L]))
  new_result(model, v, target, pr$fit, pr$fit - s, pr$fit + s,
    contributions = contrib,
    details = list(fit = fit, data = d)
  )
})

# ------------------------------------------------------------- MIDAS ---------

#' MIDAS regression
#'
#' Direct mixed data sampling regression: the most recent `n_lags` monthly
#' observations available for each indicator enter the quarterly equation,
#' and the equation is re-estimated for each pattern of data availability.
#' `weights = "umidas"` gives every lag its own OLS coefficient (Foroni,
#' Marcellino and Schumacher 2015); `"nealmon"` uses normalised exponential
#' Almon weights estimated by non-linear least squares with the 'midasr'
#' package, from several starting values.
#'
#' @param indicators Monthly indicators.
#' @param n_lags Monthly lags per indicator.
#' @param weights `"umidas"` or `"nealmon"`.
#' @param name Label.
#' @return A `MidasModel` object.
#' @examples
#' midas_model(c("x1", "x2"), n_lags = 3)
#' @export
midas_model <- function(indicators, n_lags = 3, weights = c("umidas", "nealmon"), name = NULL) {
  weights <- match.arg(weights)
  check_count(n_lags, "n_lags")
  if (is.null(name)) name <- if (weights == "umidas") "U-MIDAS" else "MIDAS-Almon"
  methods::new("MidasModel", name = name, indicators = as.character(indicators), n_lags = n_lags, weights = weights)
}

midas_frame <- function(v, target, indicators, start = as.Date("2001-01-01")) {
  sp <- stationary_panel(v, target)
  tq <- seq(max(quarter_start(min(sp$m$date)), start), target, by = "quarter")
  tm <- seq(tq[1L], seq(target, by = "month", length.out = 3L)[3L], by = "month")
  y <- sp$q$growth[match(tq, sp$q$date)]
  x <- lapply(indicators, function(k) sp$m[[k]][match(tm, sp$m$date)])
  names(x) <- indicators
  k <- vapply(x, function(s) {
    o <- which(!is.na(s))
    if (length(o)) length(s) - max(o) else NA_real_
  }, 0)
  d <- length(tq) - max(which(!is.na(y)))
  list(y = y, x = x, k = k, d = d, tq = tq)
}

midas_lags <- function(x, k, J) {
  nq <- length(x) %/% 3L
  vapply(0:(J - 1L), function(j) {
    idx <- 3L * seq_len(nq) - k - j
    out <- rep(NA_real_, nq)
    ok <- idx >= 1L
    out[ok] <- x[idx[ok]]
    out
  }, numeric(nq))
}

#' @rdname nowcast
setMethod("nowcast", "MidasModel", function(model, data, target, as_of = NULL, ...) {
  v <- prepare_vintage(data, as_of)
  target <- check_target(v, target)
  check_names(model@indicators, indicators(v), "indicator")
  dm <- midas_frame(v, target, model@indicators)
  n <- length(dm$y)
  J <- model@n_lags
  ylag <- c(rep(NA, dm$d), utils::head(dm$y, -dm$d))
  if (model@weights == "umidas") {
    X <- do.call(cbind, lapply(model@indicators, function(k) {
      L <- matrix(midas_lags(dm$x[[k]], dm$k[[k]], J), ncol = J)
      colnames(L) <- paste0(k, "_L", dm$k[[k]] + 0:(J - 1L))
      L
    }))
    d <- data.frame(y = dm$y, y_lag = ylag, X)
    tr <- d[seq_len(n - 1L), ]
    tr <- tr[stats::complete.cases(tr), ]
    fit <- stats::lm(y ~ ., data = tr)
    pr <- stats::predict(fit, d[n, , drop = FALSE], se.fit = TRUE)
    s <- sqrt(pr$se.fit^2 + summary(fit)$sigma^2)
    return(new_result(model, v, target, pr$fit, pr$fit - s, pr$fit + s, details = list(fit = fit, k = dm$k, d = dm$d)))
  }
  need_package("midasr", "MIDAS with Almon weights")
  terms <- vapply(model@indicators, function(k) {
    sprintf(
      "mls(%s, %d:%d, 3, nealmon)", k,
      dm$k[[k]], dm$k[[k]] + J - 1L
    )
  }, "")
  # midas_r() looks up the lag and weight functions in the formula environment and in the data
  env <- list2env(list(mls = midasr::mls, nealmon = midasr::nealmon), parent = environment())
  form <- stats::as.formula(paste("y ~ y_lag +", paste(terms, collapse = " + ")), env = env)
  last <- max(which(!is.na(dm$y)))
  first <- max(dm$d, ceiling((J + max(dm$k)) / 3)) + 1L
  iq <- first:last
  im <- (3L * (first - 1L) + 1L):(3L * last)
  dat <- c(
    list(y = dm$y[iq], y_lag = ylag[iq]), lapply(dm$x[model@indicators], function(s) s[im]),
    # midas_r() evaluates the formula in the data, so the lag and weight functions travel with it
    list(mls = midasr::mls, nealmon = midasr::nealmon)
  )
  starts <- list(c(0.1, -0.1), c(0.5, -0.5), c(0.1, 0.1), c(0.1, 0, 0))
  fits <- lapply(starts, function(a) {
    tryCatch(
      midasr::midas_r(form, data = dat, start = stats::setNames(rep(list(a), length(model@indicators)), model@indicators)),
      error = function(e) NULL
    )
  })
  fits <- Filter(Negate(is.null), fits)
  if (!length(fits)) rinow_abort("MIDAS estimation failed from every starting value.", "rinow_error_estimation")
  fit <- fits[[which.min(vapply(fits, function(f) sum(stats::residuals(f)^2), 0))]]
  cf <- stats::coef(fit)
  point <- cf[1L] + cf["y_lag"] * ylag[n]
  for (k in model@indicators) {
    p <- cf[startsWith(names(cf), k)]
    point <- point + sum(midasr::nealmon(p, J) * midas_lags(dm$x[[k]], dm$k[[k]], J)[n, ])
  }
  s <- stats::sd(stats::residuals(fit))
  new_result(model, v, target, point, point - s, point + s, details = list(fit = fit, k = dm$k, d = dm$d))
})

# ------------------------------------------------------- static factors -----

#' Diffusion-index (static factor) model
#'
#' Principal components of the standardised indicators enter a bridge-type
#' regression (Stock and Watson 2002). With `frequency = "quarterly"`, the
#' indicators are first aggregated to quarterly growth like bridge
#' regressors, which damps monthly noise; with `"monthly"`, factors of monthly
#' growth rates are aggregated with the weights 1-2-3-2-1.
#'
#' @param indicators Monthly indicators.
#' @param n_factors Number of factors.
#' @param frequency `"quarterly"` or `"monthly"`.
#' @param start First period of the sample.
#' @param name Label.
#' @return A `FactorModel` object.
#' @examples
#' factor_model(c("x1", "x2", "x3"))
#' @export
factor_model <- function(indicators, n_factors = 1, frequency = c("quarterly", "monthly"),
                         start = "2002-01-01", name = "Factor") {
  frequency <- match.arg(frequency)
  check_count(n_factors, "n_factors")
  methods::new("FactorModel",
    name = name, indicators = as.character(indicators), n_factors = n_factors,
    frequency = frequency, start = quarter_start(start)
  )
}

#' @rdname nowcast
setMethod("nowcast", "FactorModel", function(model, data, target, as_of = NULL, ...) {
  v <- prepare_vintage(data, as_of)
  target <- check_target(v, target)
  check_names(model@indicators, indicators(v), "indicator")
  r <- model@n_factors
  tq <- seq(max(model@start, quarter_start(min(v@monthly$date))), target, by = "quarter")
  if (model@frequency == "monthly") tq <- tq[-1L]
  if (model@frequency == "quarterly") {
    X <- vapply(model@indicators, function(k) {
      b <- bridge_regressor(v, k, target)
      b$value[match(tq, b$date)]
    }, numeric(length(tq)))
    keep <- stats::complete.cases(X)
    X <- X[keep, , drop = FALSE]
    tq <- tq[keep]
    pc <- stats::prcomp(scale(X))
    Fm <- pc$x[, seq_len(r), drop = FALSE]
  } else {
    sp <- stationary_panel(v, target)
    end_m <- seq(target, by = "month", length.out = 3L)[3L]
    tm <- seq(seq(tq[1L], by = "-4 months", length.out = 2L)[2L], end_m, by = "month")
    tm <- tm[tm >= min(sp$m$date)]
    X <- vapply(model@indicators, function(k) {
      x <- sp$m[[k]][match(tm, sp$m$date)]
      o <- which(!is.na(x))
      inside <- min(o):max(o)
      if (anyNA(x[inside])) x[inside] <- stats::approx(inside[!is.na(x[inside])], x[inside][!is.na(x[inside])], inside)$y
      x[seq_len(min(o) - 1L)] <- mean(x[o])
      fill_ar(x, length(tm))
    }, numeric(length(tm)))
    pc <- stats::prcomp(scale(X))
    Fm <- vapply(seq_len(r), function(i) {
      a <- to_quarterly_mm(tm, pc$x[, i])
      a$value[match(tq, a$date)]
    }, numeric(length(tq)))
    Fm <- matrix(Fm, ncol = r)
  }
  g <- target_growth(v@quarterly[[v@target]], v@target_transform)[match(tq, v@quarterly$date)]
  for (i in seq_len(r)) {
    if (isTRUE(stats::cor(Fm[, i], g, use = "complete.obs") < 0)) {
      Fm[, i] <- -Fm[, i]
      pc$rotation[, i] <- -pc$rotation[, i]
    }
  }
  d <- data.frame(y = g, y_lag = c(NA, utils::head(g, -1L)), Fm)
  names(d)[-(1:2)] <- paste0("F", seq_len(r))
  form <- stats::reformulate(c("y_lag", paste0("F", seq_len(r))), "y")
  fit <- stats::lm(form, data = d[!is.na(d$y), ])
  n <- nrow(d)
  if (is.na(d$y_lag[n])) d$y_lag[n] <- stats::predict(fit, d[n - 1L, ])
  pr <- stats::predict(fit, d[n, ], se.fit = TRUE)
  s <- sqrt(pr$se.fit^2 + summary(fit)$sigma^2)
  new_result(model, v, target, pr$fit, pr$fit - s, pr$fit + s,
    details = list(fit = fit, loadings = pc$rotation[, seq_len(r), drop = FALSE])
  )
})
