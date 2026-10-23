# Dynamic factor model (dfms), BVAR with conditional forecasts (BVAR), and a
# Bayesian DFM with Student-t errors and stochastic volatility.

# Mixed-frequency matrix: monthly stationary indicators and the target growth
# in the third month of each quarter, up to the end of the target quarter.
mixed_matrix <- function(v, target, indicators, start) {
  sp <- stationary_panel(v, target)
  end_m <- seq(target, by = "month", length.out = 3L)[3L]
  tm <- seq(max(start, min(sp$m$date)), end_m, by = "month")
  X <- vapply(indicators, function(k) sp$m[[k]][match(tm, sp$m$date)], numeric(length(tm)))
  X <- matrix(X, ncol = length(indicators), dimnames = list(format(tm), indicators))
  y <- rep(NA_real_, length(tm))
  m3 <- as.integer(format(tm, "%m")) %% 3L == 0L
  y[m3] <- sp$q$growth[match(quarter_start(tm[m3]), sp$q$date)]
  cbind(X, .target = y)
}

#' Mixed-frequency dynamic factor model
#'
#' Estimates a dynamic factor model on monthly indicators and the quarterly
#' target with the EM algorithm of Banbura and Modugno (2014) as implemented
#' in 'dfms'. The target enters through the Mariano-Murasawa weights, the
#' Kalman smoother fills the ragged edge, and the nowcast is the model value
#' of the target in the third month of the target quarter. The interval uses
#' the smoothed state covariance.
#'
#' @param indicators Monthly indicators.
#' @param n_factors Number of factors.
#' @param n_factor_lags Lags of the factor VAR.
#' @param idio_ar1 AR(1) idiosyncratic errors?
#' @param start First month of the sample.
#' @param name Label.
#' @return A `DFMModel` object.
#' @examples
#' dfm_model(c("x1", "x2", "x3"))
#' @export
dfm_model <- function(indicators, n_factors = 1, n_factor_lags = 2, idio_ar1 = FALSE,
                      start = "2001-01-01", name = "DFM") {
  check_count(n_factors, "n_factors")
  check_count(n_factor_lags, "n_factor_lags")
  methods::new("DFMModel",
    name = name, indicators = as.character(indicators), n_factors = n_factors,
    n_factor_lags = n_factor_lags, idio_ar1 = idio_ar1, start = month_start(start)
  )
}

fit_dfm <- function(model, v, target) {
  need_package("dfms", "The dynamic factor model")
  X <- mixed_matrix(v, target, model@indicators, model@start)
  fit <- dfms::DFM(X,
    r = model@n_factors, p = model@n_factor_lags, quarterly.vars = ".target",
    em.method = "BM", max.missing = 1, max.iter = 300, idio.ar1 = model@idio_ar1
  )
  list(fit = fit, X = X)
}

#' @rdname nowcast
setMethod("nowcast", "DFMModel", function(model, data, target, as_of = NULL, ...) {
  v <- prepare_vintage(data, as_of)
  target <- check_target(v, target)
  check_names(model@indicators, indicators(v), "indicator")
  f <- fit_dfm(model, v, target)
  fit <- f$fit
  X <- f$X
  st <- attributes(fit$X_imp)$stats
  j <- ncol(X)
  t <- nrow(X)
  C <- fit$ss_full$C[j, ]
  P <- fit$ss_full$P_smooth[, , t]
  point <- st[j, "Mean"] + st[j, "SD"] * sum(C * fit$ss_full$F_smooth[t, ])
  s <- st[j, "SD"] * sqrt(drop(t(C) %*% P %*% C) + fit$ss_full$R[j, j])
  new_result(model, v, target, point, point - s, point + s, details = list(fit = fit, data = X))
})

#' News decomposition of a DFM nowcast revision
#'
#' Compares the nowcast of `target` on two dates and splits the revision into
#' the news (actual minus expected value) of every new release times its
#' weight, following Banbura and Modugno (2014) through [dfms::news()].
#'
#' @param model A `DFMModel`.
#' @param data Final data ([NowcastData-class]).
#' @param target Target quarter.
#' @param old,new Dates of the two information sets.
#' @return Data frame with one row per released series (`series`, `actual`,
#'   `forecast`, `news`, `gain`, `impact`) and attributes `old` and `new` (the
#'   two nowcasts).
#' @examples
#' \donttest{
#' if (requireNamespace("dfms", quietly = TRUE)) {
#'   sim <- simulate_nowcast_data(n_years = 15, seed = 1)
#'   dfm_news(dfm_model(c("x1", "x2", "x3")), sim, "2014Q3", "2014-08-15", "2014-09-15")
#' }
#' }
#' @export
dfm_news <- function(model, data, target, old, new) {
  target <- quarter_start(target)
  vo <- vintage(data, old)
  vn <- vintage(data, new)
  fo <- fit_dfm(model, vo, target)
  fn <- fit_dfm(model, vn, target)
  if (!identical(dim(fo$X), dim(fn$X))) rinow_abort("The two vintages give panels of different size.", "rinow_error_data")
  nw <- dfms::news(fo$fit, fn$fit, target.vars = ".target", t.fcst = nrow(fn$X))
  out <- nw$news_df[
    !is.na(nw$news_df$actual) & nw$news_df$series != ".target",
    c("series", "actual", "forecast", "news", "gain", "impact")
  ]
  rownames(out) <- NULL
  attr(out, "old") <- nw$y_old
  attr(out, "new") <- nw$y_new
  out
}

# -------------------------------------------------------------- BVAR --------

#' Bayesian VAR with conditional forecasts
#'
#' A quarterly BVAR of the target growth and the quarterly regressors of the
#' indicators (built like bridge regressors) with a hierarchical Minnesota
#' prior (Giannone, Lenza and Primiceri 2015) estimated by 'BVAR'. With
#' `conditional = TRUE` the nowcast conditions on the current-quarter values
#' of the indicators (Waggoner and Zha 1999).
#'
#' @param indicators Monthly indicators.
#' @param n_lags VAR lags.
#' @param conditional Condition on the indicators?
#' @param n_draw,n_burn MCMC draws and burn-in.
#' @param start First quarter of the sample.
#' @param name Label.
#' @return A `BVARModel` object.
#' @examples
#' bvar_model(c("x1", "x2"))
#' @export
bvar_model <- function(indicators, n_lags = 2, conditional = TRUE, n_draw = 4000, n_burn = 2000,
                       start = "2002-01-01", name = "BVAR") {
  check_count(n_lags, "n_lags")
  if (n_burn >= n_draw) rinow_abort("`n_burn` must be smaller than `n_draw`.", "rinow_error_argument")
  methods::new("BVARModel",
    name = name, indicators = as.character(indicators), n_lags = n_lags,
    conditional = conditional, n_draw = n_draw, n_burn = n_burn, start = quarter_start(start)
  )
}

minnesota_prior <- function(Y) {
  psi <- apply(Y, 2, function(x) {
    n <- length(x)
    stats::var(stats::residuals(stats::lm(x[-1L] ~ x[-n])))
  })
  BVAR::bv_priors(hyper = "auto", mn = BVAR::bv_minnesota(b = 0, psi = BVAR::bv_psi(mode = psi)))
}

#' @rdname nowcast
setMethod("nowcast", "BVARModel", function(model, data, target, as_of = NULL, ...) {
  need_package("BVAR", "The Bayesian VAR")
  v <- prepare_vintage(data, as_of)
  target <- check_target(v, target)
  check_names(model@indicators, indicators(v), "indicator")
  d <- bridge_frame(v, target, model@indicators, model@start)
  d <- d[d$date >= model@start, c("y", model@indicators)]
  last <- max(which(!is.na(d$y)))
  first <- min(which(stats::complete.cases(d[seq_len(last), ])))
  Y <- as.matrix(d[first:last, ])
  h <- nrow(d) - last
  fit <- BVAR::bvar(Y,
    lags = model@n_lags, n_draw = model@n_draw, n_burn = model@n_burn, verbose = FALSE,
    priors = minnesota_prior(Y)
  )
  hh <- max(h, 2L) # BVAR 1.0.5 needs a horizon of at least two for conditional forecasts
  if (model@conditional) {
    path <- as.matrix(d[(last + 1L):nrow(d), model@indicators, drop = FALSE])
    if (hh > h) path <- rbind(path, matrix(NA_real_, hh - h, ncol(path)))
    fc <- stats::predict(fit, horizon = hh, cond_path = path, cond_vars = model@indicators, conf_bands = 0.16)
  } else {
    fc <- stats::predict(fit, horizon = hh, conf_bands = 0.16)
  }
  q <- fc$quants[, h, 1L]
  draw <- fc$fcast[, h, 1L]
  new_result(model, v, target, q[2L], q[1L], q[3L], details = list(fit = fit, draws = draw))
})

# ------------------------------------------------------- Bayesian DFM -------

#' Bayesian DFM with Student-t errors and stochastic volatility
#'
#' A one-factor mixed-frequency DFM estimated by Gibbs sampling. Observation
#' errors are Student-t (a scale mixture of normals), the factor shocks have
#' stochastic volatility (sampled with 'stochvol'), and the factor path is
#' drawn in one block with the precision sampler of Chan and Jeliazkov (2009)
#' using sparse matrices ('Matrix'). Series are standardised robustly with
#' the median and the MAD. Use a large `nu` and `stochastic_volatility =
#' FALSE` for a Gaussian benchmark.
#'
#' @param indicators Monthly indicators.
#' @param nu Degrees of freedom of the t errors.
#' @param stochastic_volatility Logical.
#' @param n_iter,n_burn Gibbs iterations and burn-in.
#' @param reference Indicator whose loading is fixed at one (default: first).
#' @param start First month of the sample.
#' @param name Label.
#' @return A `BayesDFMModel` object.
#' @examples
#' bayes_dfm_model(c("x1", "x2", "x3"), n_iter = 500, n_burn = 200)
#' @export
bayes_dfm_model <- function(indicators, nu = 5, stochastic_volatility = TRUE, n_iter = 3000, n_burn = 1000,
                            reference = indicators[1L], start = "2001-01-01", name = "Bayesian DFM") {
  methods::new("BayesDFMModel",
    name = name, indicators = as.character(indicators), nu = nu,
    stochastic_volatility = stochastic_volatility, n_iter = n_iter, n_burn = n_burn,
    reference = reference, start = month_start(start)
  )
}

gibbs_dfm <- function(X, y, n_iter, n_burn, nu, sv, ref) {
  TT <- nrow(X)
  N <- ncol(X)
  Z <- vapply(seq_len(N), function(i) robust_scale(X[, i])$z, numeric(TT))
  Z <- matrix(Z, ncol = N)
  sy <- robust_scale(y)
  yz <- sy$z
  aw <- c(1, 2, 3, 2, 1) / 3
  qpos <- which(!is.na(yz) & seq_len(TT) >= 5L)
  obs <- !is.na(Z)
  zz <- Z
  zz[!obs] <- 0
  f <- as.numeric(scale(rowMeans(zz)))
  lam <- rep(0.5, N)
  lam[ref] <- 1
  s2 <- rep(1, N)
  mu <- 0
  lamy <- 0.3
  s2y <- 1
  phi <- c(0.5, 0)
  h <- rep(0, TT)
  W <- matrix(1, TT, N)
  wy <- rep(1, TT)
  if (sv) {
    sv_par <- list(mu = 0, phi = 0.9, sigma = 0.2, nu = Inf, rho = 0, beta = NA, latent0 = 0)
    pri <- stochvol::specify_priors(
      mu = stochvol::sv_normal(0, 10), phi = stochvol::sv_beta(20, 1.5),
      sigma2 = stochvol::sv_gamma(0.5, 5)
    )
  }
  G <- Matrix::sparseMatrix(
    i = rep(seq_along(qpos), each = 5L), j = as.vector(vapply(qpos, function(m) (m - 4L):m, integer(5))),
    x = rep(aw, length(qpos)), dims = c(length(qpos), TT)
  )
  keep <- n_iter - n_burn
  out <- list(
    lamy = numeric(keep), phi = matrix(NA_real_, keep, 2L), h = matrix(NA_real_, keep, TT),
    lam = matrix(NA_real_, keep, N), y_last = numeric(keep)
  )
  fq_w <- function(f) as.numeric(stats::filter(f, aw, sides = 1))
  for (it in seq_len(n_iter)) {
    H <- Matrix::bandSparse(TT, k = c(0L, -1L, -2L), diagonals = list(rep(1, TT), rep(-phi[1], TT - 1L), rep(-phi[2], TT - 2L)))
    K <- Matrix::crossprod(H, Matrix::Diagonal(x = exp(-h)) %*% H)
    pm <- rowSums(sweep(W * obs, 2L, lam^2 / s2, `*`))
    b <- rowSums(sweep(ifelse(obs, W * Z, 0), 2L, lam / s2, `*`))
    K <- K + Matrix::Diagonal(x = pm) + Matrix::crossprod(G, Matrix::Diagonal(x = lamy^2 * wy[qpos] / s2y) %*% G)
    b <- b + as.numeric(Matrix::crossprod(G, lamy * wy[qpos] / s2y * (yz[qpos] - mu)))
    R <- Matrix::chol(Matrix::forceSymmetric(K))
    f <- as.numeric(Matrix::solve(K, b)) + as.numeric(Matrix::solve(R, stats::rnorm(TT)))
    for (i in seq_len(N)) {
      o <- obs[, i]
      w <- W[o, i]
      xi <- Z[o, i]
      fi <- f[o]
      if (i != ref) {
        vp <- 1 / (sum(w * fi^2) / s2[i] + 1)
        lam[i] <- stats::rnorm(1L, vp * sum(w * fi * xi) / s2[i], sqrt(vp))
      }
      e <- xi - lam[i] * fi
      s2[i] <- 1 / stats::rgamma(1L, 2 + sum(o) / 2, 0.5 + sum(w * e^2) / 2)
      W[o, i] <- stats::rgamma(sum(o), (nu + 1) / 2, (nu + e^2 / s2[i]) / 2)
    }
    fq <- as.numeric(G %*% f)
    yy <- yz[qpos]
    wq <- wy[qpos]
    Xq <- cbind(1, fq)
    V <- solve(crossprod(Xq * sqrt(wq)) / s2y + diag(0.1, 2L))
    cf <- as.numeric(V %*% (crossprod(Xq, wq * yy) / s2y) + t(chol(V)) %*% stats::rnorm(2L))
    mu <- cf[1]
    lamy <- cf[2]
    ey <- yy - mu - lamy * fq
    s2y <- 1 / stats::rgamma(1L, 2 + length(yy) / 2, 0.5 + sum(wq * ey^2) / 2)
    wy[qpos] <- stats::rgamma(length(qpos), (nu + 1) / 2, (nu + ey^2 / s2y) / 2)
    Xf <- cbind(c(0, f[-TT]), c(0, 0, f[-c(TT - 1L, TT)]))
    pw <- exp(-h)
    Vf <- solve(crossprod(Xf * sqrt(pw)) + diag(1, 2L))
    prop <- as.numeric(Vf %*% crossprod(Xf, pw * f) + t(chol(Vf)) %*% stats::rnorm(2L))
    if (all(Mod(polyroot(c(1, -prop))) > 1.001)) phi <- prop
    eta <- as.numeric(f - Xf %*% phi)
    if (sv) {
      s <- stochvol::svsample_fast_cpp(eta, draws = 1, burnin = 0, priorspec = pri, startpara = sv_par, startlatent = h)
      sv_par <- list(
        mu = s$para[1, "mu"], phi = s$para[1, "phi"], sigma = s$para[1, "sigma"], nu = Inf, rho = 0,
        beta = NA, latent0 = s$latent0[1]
      )
      h <- as.numeric(s$latent[1, ])
    } else {
      h <- rep(log(1 / stats::rgamma(1L, 2 + TT / 2, 0.5 + sum(eta^2) / 2)), TT)
    }
    if (it > n_burn) {
      k <- it - n_burn
      out$lamy[k] <- lamy
      out$phi[k, ] <- phi
      out$h[k, ] <- h
      out$lam[k, ] <- lam
      shock <- stats::rt(1L, df = nu) * sqrt(s2y * (nu - 2) / nu)
      out$y_last[k] <- sy$centre + sy$scale * (mu + lamy * fq_w(f)[TT] + shock)
    }
  }
  out
}

#' @rdname nowcast
setMethod("nowcast", "BayesDFMModel", function(model, data, target, as_of = NULL, ...) {
  need_package("Matrix", "The Bayesian DFM")
  if (model@stochastic_volatility) need_package("stochvol", "Stochastic volatility")
  v <- prepare_vintage(data, as_of)
  target <- check_target(v, target)
  check_names(model@indicators, indicators(v), "indicator")
  X <- mixed_matrix(v, target, model@indicators, model@start)
  y <- X[, ".target"]
  X <- X[, model@indicators, drop = FALSE]
  fit <- gibbs_dfm(
    X, y, model@n_iter, model@n_burn, model@nu, model@stochastic_volatility,
    match(model@reference, model@indicators)
  )
  q <- stats::quantile(fit$y_last, c(0.16, 0.5, 0.84))
  fit$dates <- as.Date(rownames(X))
  new_result(model, v, target, q[[2L]], q[[1L]], q[[3L]], details = list(fit = fit, draws = fit$y_last))
})
