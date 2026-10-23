# Objects shaped like the 'rcli' classes (rinow recognises them by their slots)
setClass("MockComposite", representation(index = "ts", cycle = "ts", components = "character"))
setClass("MockCandidates", representation(meta = "data.frame"))
setClass("MockResult", representation(candidates = "MockCandidates", composite = "ANY"))

mock_index <- function(data) {
  f <- attr(data, "factor")
  ts(100 + cumsum(f$factor) / 10, start = c(as.integer(format(f$date[1], "%Y")), 1), frequency = 12)
}

test_that("monthly ts and data frames become indicator tables", {
  x <- ts(c(100, 101, 103), start = c(2024, 11), frequency = 12)
  a <- as_indicator(x)
  expect_equal(a$date, as.Date(c("2024-11-01", "2024-12-01", "2025-01-01")))
  expect_equal(a$value, c(100, 101, 103))
  expect_equal(as_indicator(a), a)
  expect_error(as_indicator(ts(1:8, frequency = 4)), class = "rinow_error_argument")
  expect_error(as_indicator(ts(matrix(1:6, 3), frequency = 12)), class = "rinow_error_argument")
  expect_error(as_indicator(data.frame(date = as.Date("2024-01-15"), value = 1)), class = "rinow_error_argument")
  expect_error(as_indicator(list(1)), class = "rinow_error_argument")
})

test_that("add_indicator adds a series that models can use", {
  extra <- as_indicator(mock_index(sim))
  sim2 <- add_indicator(sim, extra, "activity", release_lag = 10)
  expect_true("activity" %in% indicators(sim2))
  m <- sim2@meta[sim2@meta$name == "activity", ]
  expect_equal(m$release_lag, 10)
  expect_equal(m$transform, "dlog")
  expect_equal(m$group, "indicator")
  r <- nowcast(bridge_model("activity"), sim2, sim_target, as_of = "2015-08-20")
  expect_s4_class(r, "NowcastResult")
  expect_true(is.finite(r@point))
})

test_that("vintages respect the release lag of the added indicator", {
  sim2 <- add_indicator(sim, as_indicator(mock_index(sim)), "activity", release_lag = 40)
  v <- vintage(sim2, "2015-08-20")
  last <- max(v@monthly$date[!is.na(v@monthly$activity)])
  ends <- vapply(v@monthly$date, function(d) as.numeric(period_end(d) + 40), 0)
  expect_true(all(ends[!is.na(v@monthly$activity)] <= as.numeric(as.Date("2015-08-20"))))
  expect_equal(last, as.Date("2015-06-01"))
})

test_that("'rcli'-shaped composites are recognised and flagged", {
  idx <- mock_index(sim)
  comp <- new("MockComposite", index = idx, cycle = idx - 100, components = c("a", "b"))
  res <- new("MockResult",
    candidates = new("MockCandidates", meta = data.frame(name = c("a", "b", "c"), release_lag = c(1, 2, 5))),
    composite = comp
  )
  a <- as_indicator(res, series = "cycle")
  expect_true(attr(a, "composite"))
  expect_equal(attr(a, "release_lag"), 2)
  expect_equal(a$value, as.numeric(idx - 100))
  expect_message(sim2 <- add_indicator(sim, res, "cli"), "optimistic")
  m <- sim2@meta[sim2@meta$name == "cli", ]
  expect_equal(m$release_lag, 60)
  expect_equal(m$transform, "diff")
  expect_equal(m$group, "composite")
  expect_message(sim3 <- add_indicator(sim, comp, "cli"), "optimistic")
  expect_equal(sim3@meta$release_lag[sim3@meta$name == "cli"], 30)
})

test_that("add_indicator refuses unsafe input", {
  extra <- as_indicator(mock_index(sim))
  expect_error(add_indicator(sim_v, extra, "z"), class = "rinow_error_argument")
  expect_error(add_indicator(sim, extra, "x1"), class = "rinow_error_argument")
  expect_error(add_indicator(sim, extra, "date"), class = "rinow_error_argument")
  expect_error(add_indicator(sim, extra, "z", release_lag = -1), class = "rinow_error_argument")
  neg <- extra
  neg$value <- neg$value - 200
  expect_error(add_indicator(sim, neg, "z", transform = "dlog"), class = "rinow_error_argument")
})

test_that("a longer series extends the monthly panel without gaps", {
  extra <- as_indicator(mock_index(sim))
  later <- data.frame(date = seq(max(extra$date), by = "month", length.out = 4)[-1], value = 101)
  sim2 <- add_indicator(sim, rbind(extra, later), "activity")
  d <- sim2@monthly$date
  expect_equal(length(d), length(seq(min(d), max(d), by = "month")))
  expect_true(all(is.na(sim2@monthly$x1[d > max(sim@monthly$date)])))
})
