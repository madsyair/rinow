# Internal helpers: conditions, argument checks, dates and series operations.

rinow_abort <- function(message, class = NULL) {
  stop(structure(
    class = c(class, "rinow_error", "error", "condition"),
    list(message = message, call = NULL)
  ))
}

rinow_warn <- function(message, class = NULL) {
  warning(structure(
    class = c(class, "rinow_warning", "warning", "condition"),
    list(message = message, call = NULL)
  ))
}

need_package <- function(pkg, what) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    rinow_abort(sprintf(
      "%s needs the '%s' package. Install it with install.packages(\"%s\").",
      what, pkg, pkg
    ), "rinow_error_dependency")
  }
  invisible(TRUE)
}

check_names <- function(x, available, what) {
  miss <- setdiff(x, available)
  if (length(miss)) {
    rinow_abort(
      sprintf("Unknown %s: %s.", what, paste(miss, collapse = ", ")),
      "rinow_error_argument"
    )
  }
  invisible(TRUE)
}

check_count <- function(x, name, min = 1) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x) || x < min || x != round(x)) {
    rinow_abort(
      sprintf("`%s` must be a whole number of at least %d.", name, min),
      "rinow_error_argument"
    )
  }
  invisible(TRUE)
}

as_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (is.character(x) && length(x) && all(grepl("^[0-9]{4}[Qq][1-4]$", x))) {
    y <- as.integer(substr(x, 1, 4))
    q <- as.integer(substr(x, 6, 6))
    return(as.Date(sprintf("%04d-%02d-01", y, 3L * q - 2L)))
  }
  out <- tryCatch(as.Date(x), error = function(e) NULL)
  if (is.null(out) || anyNA(out)) {
    rinow_abort(
      "Dates must be `Date` objects or strings such as \"2026-07-01\" or \"2026Q3\".",
      "rinow_error_argument"
    )
  }
  out
}

#' First day of the month / quarter of a date
#' @noRd
month_start <- function(x) as.Date(format(as_date(x), "%Y-%m-01"))
quarter_start <- function(x) {
  x <- as_date(x)
  m <- as.integer(format(x, "%m"))
  as.Date(sprintf("%s-%02d-01", format(x, "%Y"), 3L * ((m - 1L) %/% 3L) + 1L))
}

#' Last day of the period that starts at `x` (months = 1 or 3)
#' @noRd
period_end <- function(x, months = 1L) {
  x <- as_date(x)
  y <- as.integer(format(x, "%Y"))
  m <- as.integer(format(x, "%m")) + months
  y <- y + (m - 1L) %/% 12L
  m <- (m - 1L) %% 12L + 1L
  as.Date(sprintf("%04d-%02d-01", y, m)) - 1L
}

# Shift first-of-month dates by k months (k may be negative)
add_months <- function(x, k) {
  if (!k) {
    return(x)
  }
  m <- as.integer(format(x, "%Y")) * 12L + as.integer(format(x, "%m")) - 1L + as.integer(k)
  as.Date(sprintf("%04d-%02d-01", m %/% 12L, m %% 12L + 1L))
}

format_quarter <- function(x) {
  x <- as_date(x)
  paste0(format(x, "%Y"), "Q", (as.integer(format(x, "%m")) - 1L) %/% 3L + 1L)
}

#' Transform a series to (approximate) stationarity
#'
#' @param x Numeric vector of levels.
#' @param method `"dlog"` (100 times the first difference of logs, a growth
#'   rate in per cent), `"diff"` (first difference) or `"level"` (unchanged).
#' @return Numeric vector of the same length as `x`, with `NA` in the first
#'   position for `"dlog"` and `"diff"`.
#' @examples
#' transform_series(c(100, 101, 103), "dlog")
#' @export
transform_series <- function(x, method = c("dlog", "diff", "level")) {
  method <- match.arg(method)
  if (method == "dlog" && any(x <= 0, na.rm = TRUE)) {
    rinow_abort("`dlog` needs positive values; use `diff` instead.", "rinow_error_argument")
  }
  switch(method,
    dlog = c(NA_real_, 100 * diff(log(x))),
    diff = c(NA_real_, diff(x)),
    level = as.numeric(x)
  )
}

#' Aggregate a monthly series to quarters
#'
#' `to_quarterly()` averages the three monthly values of each quarter and
#' returns `NA` for incomplete quarters. `to_quarterly_mm()` turns monthly
#' growth rates into quarterly growth rates with the weights 1, 2, 3, 2, 1
#' (divided by 3) of Mariano and Murasawa (2003), which approximate the growth
#' of quarterly averages.
#'
#' @param date Vector of monthly dates.
#' @param x Numeric vector of the same length.
#' @return Data frame with columns `date` (first day of the quarter) and
#'   `value`.
#' @examples
#' d <- seq(as.Date("2020-01-01"), by = "month", length.out = 9)
#' to_quarterly(d, 1:9)
#' @export
to_quarterly <- function(date, x) {
  date <- as_date(date)
  if (length(date) != length(x)) rinow_abort("`date` and `x` differ in length.", "rinow_error_argument")
  q <- quarter_start(date)
  uq <- unique(q)
  val <- vapply(uq, function(u) {
    i <- which(q == u)
    if (length(i) < 3L || anyNA(x[i])) NA_real_ else mean(x[i])
  }, numeric(1))
  data.frame(date = uq, value = val)
}

#' @rdname to_quarterly
#' @export
to_quarterly_mm <- function(date, x) {
  date <- as_date(date)
  mm <- as.numeric(stats::filter(x, c(1, 2, 3, 2, 1) / 3, sides = 1))
  m3 <- as.integer(format(date, "%m")) %% 3L == 0L
  data.frame(date = quarter_start(date[m3]), value = mm[m3])
}

#' Fill the end of a series with autoregressive forecasts
#'
#' The AR order (up to `max_order`) is chosen by AIC and estimated by OLS on
#' the observed values.
#'
#' @param x Numeric vector with missing values at the end.
#' @param n Length of the returned vector.
#' @param max_order Largest AR order.
#' @return Numeric vector of length `n`.
#' @examples
#' fill_ar(c(1, 2, 1.5, 1.8, NA, NA), 6)
#' @export
fill_ar <- function(x, n = length(x), max_order = 6L) {
  obs <- which(!is.na(x))
  if (!length(obs)) rinow_abort("The series has no observations.", "rinow_error_data")
  last <- max(obs)
  if (n <= last) {
    return(as.numeric(x[seq_len(n)]))
  }
  y <- as.numeric(x[seq_len(last)])
  yy <- y[!is.na(y)]
  fit <- tryCatch(stats::ar(yy,
    order.max = min(max_order, length(yy) %/% 4L), aic = TRUE,
    method = "ols", demean = TRUE
  ), error = function(e) NULL)
  h <- n - last
  pred <- if (is.null(fit)) rep(mean(yy), h) else as.numeric(stats::predict(fit, n.ahead = h)$pred)
  c(y, pred)
}

robust_scale <- function(x) {
  centre <- stats::median(x, na.rm = TRUE)
  s <- stats::mad(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) s <- 1
  list(z = (x - centre) / s, centre = centre, scale = s)
}

# Growth target of a quarterly level series
target_growth <- function(x, method) {
  if (method == "level") as.numeric(x) else transform_series(x, method)
}

quantiles_or_na <- function(draw, probs) {
  if (is.null(draw) || !length(draw)) {
    return(c(NA_real_, NA_real_))
  }
  as.numeric(stats::quantile(draw, probs, na.rm = TRUE))
}
