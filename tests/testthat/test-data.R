test_that("nowcast_data validates its input", {
  m <- data.frame(date = seq(as.Date("2020-01-01"), by = "month", length.out = 12), x = 1:12)
  q <- data.frame(date = seq(as.Date("2020-01-01"), by = "quarter", length.out = 4), gdp = 101:104)
  d <- nowcast_data(m, q, "gdp")
  expect_s4_class(d, "NowcastData")
  expect_equal(d@meta$transform, "dlog")
  expect_error(nowcast_data(m, q, "missing"), class = "error")
  expect_error(set_transform(d, nope = "diff"), class = "rinow_error_argument")
  expect_error(transform_series(c(1, -1), "dlog"), class = "rinow_error_argument")
})

test_that("vintages never contain data released after the information date", {
  for (as_of in c("2012-01-20", "2013-05-03", "2015-08-20")) {
    v <- vintage(sim, as_of)
    for (i in seq_len(nrow(v@meta))) {
      k <- v@meta$name[i]
      obs <- v@monthly$date[!is.na(v@monthly[[k]])]
      expect_true(all(seq(obs[length(obs)], by = "month", length.out = 2)[2] - 1 + v@meta$release_lag[i] <= as.Date(as_of)))
    }
    qd <- v@quarterly$date[!is.na(v@quarterly$gdp)]
    expect_true(max(seq(max(qd), by = "quarter", length.out = 2)[2] - 1) + v@target_lag <= as.Date(as_of))
  }
})

test_that("the ragged edge follows the release lags", {
  re <- ragged_edge(vintage(sim, "2015-08-20"))
  # x5 (1 day) has July, x3 (5 days) July, x1 (15 days) July, x2 (30 days) June, x4 (45 days) June
  expect_equal(re$last[re$series == "x5"], "2015-07")
  expect_equal(re$last[re$series == "x2"], "2015-06")
  expect_equal(re$last[re$series == "x4"], "2015-06")
  expect_equal(re$last[re$series == "gdp"], "2015Q2")
})

test_that("quarterly aggregation and Mariano-Murasawa weights agree", {
  d <- seq(as.Date("2001-01-01"), by = "month", length.out = 120)
  x <- 100 * exp(cumsum(rnorm(120, 0.3, 0.5) / 100))
  exact <- transform_series(to_quarterly(d, x)$value, "dlog")
  mm <- to_quarterly_mm(d, transform_series(x, "dlog"))$value
  expect_gt(stats::cor(exact[-(1:2)], mm[-(1:2)]), 0.99)
  expect_true(is.na(to_quarterly(d[1:4], 1:4)$value[2]))
})

test_that("fill_ar extends a series and keeps observed values", {
  x <- c(as.numeric(arima.sim(list(ar = 0.5), 60)), NA, NA)
  y <- fill_ar(x, 64)
  expect_length(y, 64)
  expect_equal(y[1:60], x[1:60])
  expect_false(anyNA(y))
})

test_that("show and plot methods run", {
  expect_output(show(sim), "NowcastData")
  expect_silent(plot(sim_v))
})

test_that("the bundled data belong to this package's classes", {
  expect_identical(attr(class(idn_nowcast), "package"), "rinow")
  expect_true(methods::validObject(idn_nowcast))
})
