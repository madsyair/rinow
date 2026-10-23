# Pseudo real-time evaluation, accuracy, tests and combinations.

#' Pseudo real-time nowcast schedule
#'
#' Four nowcast dates per target quarter on day `day` of the month: `M1`,
#' `M2` and `M3` in the first, second and third month of the target quarter
#' and `B` (backcast) in the month after the quarter ends.
#'
#' @param from,to First and last target quarters.
#' @param day Day of the month.
#' @return Data frame with `target`, `position` and `as_of`.
#' @examples
#' head(nowcast_schedule("2015Q1", "2015Q4"))
#' @export
nowcast_schedule <- function(from, to, day = 15) {
  tq <- seq(quarter_start(from), quarter_start(to), by = "quarter")
  out <- do.call(rbind, lapply(tq, function(t) {
    m <- seq(t, by = "month", length.out = 4L)
    data.frame(
      target = t, position = c("M1", "M2", "M3", "B"),
      as_of = as.Date(sprintf("%s-%02d", format(m, "%Y-%m"), day))
    )
  }))
  out$position <- factor(out$position, levels = c("M1", "M2", "M3", "B"))
  out
}

#' Evaluate a model in pseudo real time
#'
#' For each row of `schedule`, builds the vintage of `data` on `as_of`, runs
#' the model and compares the nowcast with the final value of the target.
#' The random seed is fixed per row so that stochastic models are
#' reproducible. Errors in single runs give `NA` instead of stopping the
#' evaluation.
#'
#' @param model A [NowcastModel-class] object.
#' @param data Final data ([NowcastData-class]).
#' @param schedule Output of [nowcast_schedule()].
#' @param cores Number of processes (forked, Unix only). Models that use
#'   OpenMP ('ranger', 'xgboost') always run serially.
#' @param seed Base seed.
#' @return An [Evaluation-class] object.
#' @examples
#' sim <- simulate_nowcast_data(n_years = 15, seed = 1)
#' ev <- evaluate(ar_model(), sim, nowcast_schedule("2012Q1", "2014Q4"))
#' accuracy(ev)
#' @export
evaluate <- function(model, data, schedule, cores = 1L, seed = 1000L) {
  if (!methods::is(model, "NowcastModel")) rinow_abort("`model` must be a NowcastModel.", "rinow_error_argument")
  omp <- methods::is(model, "MLModel") && model@method %in% c("rf", "xgb")
  run <- function(i) {
    set.seed(seed + i)
    r <- tryCatch(nowcast(model, data, schedule$target[i], as_of = schedule$as_of[i]), error = function(e) NULL)
    if (is.null(r)) c(NA_real_, NA_real_, NA_real_) else c(r@point, r@lower, r@upper)
  }
  idx <- seq_len(nrow(schedule))
  forked <- cores > 1L && .Platform$OS.type == "unix" && !omp
  res <- if (forked) parallel::mclapply(idx, run, mc.cores = cores) else lapply(idx, run)
  res <- do.call(rbind, res)
  actual <- target_growth(data@quarterly[[data@target]], data@target_transform)[match(schedule$target, data@quarterly$date)]
  out <- data.frame(schedule,
    model = model@name, nowcast = res[, 1L], lower = res[, 2L], upper = res[, 3L],
    actual = actual
  )
  out$error <- out$nowcast - out$actual
  methods::new("Evaluation", model = model@name, results = out)
}

eval_frame <- function(...) {
  ev <- list(...)
  if (length(ev) == 1L && is.list(ev[[1L]]) && !methods::is(ev[[1L]], "Evaluation")) ev <- ev[[1L]]
  do.call(rbind, lapply(ev, function(e) if (methods::is(e, "Evaluation")) e@results else e))
}

#' Accuracy of evaluated nowcasts
#'
#' @param ... [Evaluation-class] objects (or a list of them).
#' @param exclude Target quarters to drop (for example a pandemic period).
#' @return Data frame with RMSE, MAE, bias, interval coverage and number of
#'   nowcasts by model and position.
#' @examples
#' sim <- simulate_nowcast_data(n_years = 15, seed = 1)
#' ev <- evaluate(ar_model(), sim, nowcast_schedule("2012Q1", "2014Q4"))
#' accuracy(ev)
#' @export
accuracy <- function(..., exclude = NULL) {
  d <- eval_frame(...)
  if (!is.null(exclude)) d <- d[!d$target %in% quarter_start(exclude), ]
  if (is.null(d) || !nrow(d)) {
    return(data.frame(
      model = character(), position = factor(character(), levels = c("M1", "M2", "M3", "B")),
      rmse = numeric(), mae = numeric(), bias = numeric(), coverage = numeric(), n = integer()
    ))
  }
  sp <- split(d, list(d$model, d$position), drop = TRUE)
  out <- do.call(rbind, lapply(sp, function(x) {
    e <- x$error[!is.na(x$error)]
    inside <- x$actual >= x$lower & x$actual <= x$upper
    data.frame(
      model = x$model[1L], position = x$position[1L], rmse = sqrt(mean(e^2)), mae = mean(abs(e)),
      bias = mean(e), coverage = if (all(is.na(inside))) NA_real_ else mean(inside, na.rm = TRUE),
      n = length(e)
    )
  }))
  out$position <- factor(out$position, levels = c("M1", "M2", "M3", "B"))
  rownames(out) <- NULL
  out[order(out$model, out$position), ]
}

#' Diebold-Mariano test of equal accuracy
#'
#' Compares two evaluations position by position with the small-sample
#' correction of Harvey, Leybourne and Newbold (1997) and squared-error loss.
#' Negative statistics mean that the first model is more accurate.
#'
#' @param x,y [Evaluation-class] objects.
#' @param exclude Target quarters to drop.
#' @return Data frame with `position`, `n`, `statistic` and `p_value`.
#' @examples
#' sim <- simulate_nowcast_data(n_years = 15, seed = 1)
#' s <- nowcast_schedule("2010Q1", "2014Q4")
#' dm_test(evaluate(bridge_model("x1"), sim, s), evaluate(ar_model(), sim, s))
#' @export
dm_test <- function(x, y, exclude = NULL) {
  a <- x@results
  b <- y@results
  if (!is.null(exclude)) {
    a <- a[!a$target %in% quarter_start(exclude), ]
    b <- b[!b$target %in% quarter_start(exclude), ]
  }
  do.call(rbind, lapply(levels(a$position), function(p) {
    k <- merge(a[a$position == p, c("target", "error")], b[b$position == p, c("target", "error")], by = "target")
    k <- k[stats::complete.cases(k), ]
    d <- k$error.x^2 - k$error.y^2
    n <- length(d)
    if (n < 5L || stats::var(d) == 0) {
      return(data.frame(position = p, n = n, statistic = NA_real_, p_value = NA_real_))
    }
    dbar <- mean(d)
    g0 <- mean((d - dbar)^2)
    dm <- dbar / sqrt(g0 / n)
    hln <- dm * sqrt((n + 1 - 2 + 0) / n) # horizon one: sqrt((n + 1 - 2h + h(h-1)/n) / n)
    data.frame(position = p, n = n, statistic = hln, p_value = 2 * stats::pt(-abs(hln), df = n - 1))
  }))
}

#' Combine evaluated nowcasts
#'
#' Combines the nowcasts of several evaluations made on the same schedule.
#' `"inverse_mse"` weights each model by the inverse of its mean squared
#' error over the last `window` targets whose actual value was released
#' before the nowcast date, at the same calendar position, so no future
#' information is used.
#'
#' @param ... [Evaluation-class] objects (or a list).
#' @param method `"mean"`, `"median"` or `"inverse_mse"`.
#' @param window Number of past targets for the weights.
#' @param target_lag Release lag of the target in days.
#' @param name Label of the combination.
#' @return An [Evaluation-class] object.
#' @examples
#' sim <- simulate_nowcast_data(n_years = 15, seed = 1)
#' s <- nowcast_schedule("2010Q1", "2014Q4")
#' e1 <- evaluate(ar_model(), sim, s)
#' e2 <- evaluate(bridge_model("x1"), sim, s)
#' accuracy(combine_nowcasts(e1, e2))
#' @export
combine_nowcasts <- function(..., method = c("mean", "median", "inverse_mse"), window = 12, target_lag = 45,
                             name = NULL) {
  method <- match.arg(method)
  d <- eval_frame(...)
  models <- unique(d$model)
  key <- paste(d$target, d$position)
  W <- tapply(d$nowcast, list(key, d$model), function(z) z[1L])
  E <- tapply(d$error, list(key, d$model), function(z) z[1L])
  base <- d[!duplicated(key), c("target", "position", "as_of", "actual")]
  rk <- paste(base$target, base$position)
  M <- W[rk, models, drop = FALSE]
  Er <- E[rk, models, drop = FALSE]
  if (method == "mean") nc <- rowMeans(M, na.rm = TRUE)
  if (method == "median") nc <- apply(M, 1L, stats::median, na.rm = TRUE)
  if (method == "inverse_mse") {
    released <- period_end(base$target, 3L) + target_lag
    nc <- vapply(seq_len(nrow(base)), function(i) {
      past <- which(base$position == base$position[i] & released <= base$as_of[i])
      past <- utils::tail(past[order(base$target[past])], window)
      if (length(past) < 4L) {
        return(mean(M[i, ], na.rm = TRUE))
      }
      mse <- colMeans(Er[past, , drop = FALSE]^2, na.rm = TRUE)
      w <- 1 / mse
      ok <- !is.na(M[i, ])
      sum(w[ok] * M[i, ok]) / sum(w[ok])
    }, 0)
  }
  if (is.null(name)) name <- paste0("Combination (", method, ")")
  out <- data.frame(base[, c("target", "position", "as_of")],
    model = name, nowcast = as.numeric(nc),
    lower = NA_real_, upper = NA_real_, actual = base$actual
  )
  out$error <- out$nowcast - out$actual
  rownames(out) <- NULL
  methods::new("Evaluation", model = name, results = out)
}

#' @rdname combine_nowcasts
#' @param x An [Evaluation-class] object.
#' @param row.names,optional Ignored.
#' @export
as.data.frame.Evaluation <- function(x, row.names = NULL, optional = FALSE, ...) x@results

#' @rdname nowcast
#' @param x A [NowcastResult-class] object.
#' @param row.names,optional Ignored.
#' @export
as.data.frame.NowcastResult <- function(x, row.names = NULL, optional = FALSE, ...) {
  data.frame(
    model = x@model, target = format_quarter(x@target), as_of = format(x@as_of), nowcast = x@point,
    lower = x@lower, upper = x@upper
  )
}

#' Annual growth implied by a quarterly nowcast
#'
#' Adds the nowcast of the target quarter (and of an unreleased previous
#' quarter, if given) to the released quarter-on-quarter log growth rates and
#' returns the year-on-year growth rate in per cent.
#'
#' @param data Data or vintage whose target is a `dlog` growth rate.
#' @param nowcasts Named numeric vector of q/q nowcasts; names are quarters.
#' @return Year-on-year growth (per cent) of the last quarter in `nowcasts`.
#' @examples
#' sim <- simulate_nowcast_data(n_years = 15, seed = 1)
#' v <- vintage(sim, "2014-09-15")
#' yoy_growth(v, c("2014Q3" = 0.8))
#' @export
yoy_growth <- function(data, nowcasts) {
  if (data@target_transform != "dlog") rinow_abort("Needs a `dlog` target.", "rinow_error_argument")
  g <- transform_series(data@quarterly[[data@target]], "dlog")
  names(g) <- format_quarter(data@quarterly$date)
  g <- g[!is.na(g)]
  nm <- format_quarter(quarter_start(names(nowcasts)))
  g[nm] <- nowcasts
  g <- g[order(names(g))]
  i <- which(names(g) == utils::tail(nm, 1L))
  if (i < 4L) rinow_abort("Not enough quarters for a year-on-year rate.", "rinow_error_data")
  100 * (exp(sum(g[(i - 3L):i]) / 100) - 1)
}
