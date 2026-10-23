#' Simulate data for nowcasting
#'
#' Generates a monthly activity factor (AR(2) with an optional crash), monthly
#' indicators that load on it with decreasing strength (the last one is pure
#' noise), monthly output whose quarterly sums form the target, and two
#' expenditure components. Because the data-generating process is known, the
#' data serve for teaching, testing and checking that models recover the
#' signal.
#'
#' @param n_years Number of years.
#' @param n_indicators Number of indicators (at least two).
#' @param start First month.
#' @param crash Logical; add a deep two-month contraction three quarters
#'   before the end of the sample's third quarter from the end.
#' @param seed Random seed (`NULL` keeps the current state).
#' @return A [NowcastData-class] object with target `gdp`, components `c1`
#'   and `c2`, and indicators `x1`, `x2`, ...; the factor is stored in the
#'   attribute `"factor"`.
#' @examples
#' sim <- simulate_nowcast_data(n_years = 10, seed = 42)
#' ragged_edge(sim)
#' @export
simulate_nowcast_data <- function(n_years = 20, n_indicators = 5, start = "2000-01-01", crash = TRUE, seed = NULL) {
  check_count(n_years, "n_years", 3)
  check_count(n_indicators, "n_indicators", 2)
  if (!is.null(seed)) set.seed(seed)
  TT <- 12L * n_years
  dates <- seq(month_start(start), by = "month", length.out = TT)
  e <- stats::rnorm(TT)
  if (crash) {
    k <- TT - 30L
    e[k:(k + 1L)] <- e[k:(k + 1L)] - c(6, 4)
  }
  f <- as.numeric(stats::filter(e, c(0.6, 0.1), method = "recursive"))
  lam <- c(seq(1, 0.3, length.out = n_indicators - 1L), 0)
  X <- vapply(seq_len(n_indicators), function(i) {
    g <- 0.3 + lam[i] * f + stats::rnorm(TT, sd = 1)
    100 * exp(cumsum(g / 100))
  }, numeric(TT))
  colnames(X) <- paste0("x", seq_len(n_indicators))
  gm <- 0.4 + 0.25 * f + stats::rnorm(TT, sd = 0.1)
  ym <- 1000 * exp(cumsum(gm / 100))
  q <- to_quarterly(dates, ym)
  q$value <- q$value * 3
  s1 <- 0.6 * exp(cumsum(stats::rnorm(nrow(q), sd = 0.002)))
  s2 <- 0.3 * exp(cumsum(stats::rnorm(nrow(q), sd = 0.004)))
  quarterly <- data.frame(date = q$date, gdp = q$value, c1 = q$value * s1, c2 = q$value * s2)
  lags <- stats::setNames(c(15, 30, 5, 45, 1, rep(20, max(0, n_indicators - 5)))[seq_len(n_indicators)], colnames(X))
  out <- nowcast_data(data.frame(date = dates, X), quarterly,
    target = "gdp", components = c("c1", "c2"),
    release_lag = lags, target_lag = 45,
    labels = stats::setNames(paste("Indicator", seq_len(n_indicators)), colnames(X))
  )
  attr(out, "factor") <- data.frame(date = dates, factor = f)
  out
}
