test_that("AR, bridge, U-MIDAS and factor models return valid results", {
  ms <- list(
    ar_model(), bridge_model(c("x1", "x2")), midas_model(c("x1", "x3"), 3),
    factor_model(c("x1", "x2", "x3")), factor_model(c("x1", "x2", "x3"), frequency = "monthly")
  )
  for (m in ms) {
    r <- nowcast(m, sim_v, sim_target)
    expect_s4_class(r, "NowcastResult")
    expect_true(is.finite(r@point))
    expect_true(r@lower <= r@point && r@point <= r@upper)
    expect_equal(r@target, as.Date("2015-07-01"))
  }
})

test_that("bridge contributions add up to the nowcast", {
  r <- nowcast(bridge_model(c("x1", "x2")), sim_v, sim_target)
  expect_equal(sum(r@contributions), r@point, tolerance = 1e-8)
})

test_that("nowcast refuses released or distant targets", {
  expect_error(nowcast(ar_model(), sim_v, "2015Q2"), class = "rinow_error_target")
  expect_error(nowcast(ar_model(), sim_v, "2016Q2"), class = "rinow_error_target")
})

test_that("the vintage is applied when as_of is given", {
  a <- nowcast(bridge_model("x1"), sim, sim_target, as_of = "2015-08-20")
  b <- nowcast(bridge_model("x1"), sim_v, sim_target)
  expect_equal(a@point, b@point)
})

test_that("the bridge sample starts at `start` and keeps the lag from before it", {
  r <- nowcast(bridge_model("x1", start = "2002-01-01"), sim_v, sim_target)
  d <- r@details$data
  used <- d[!is.na(d$y) & d$date >= as.Date("2002-01-01") & d$date < as.Date("2015-07-01"), ]
  expect_equal(stats::nobs(r@details$fit), nrow(used))
  expect_false(is.na(used$y_lag[1]))
})

test_that("informative indicators beat noise in the bridge equation", {
  s <- nowcast_schedule("2010Q1", "2015Q2")
  good <- accuracy(evaluate(bridge_model("x1"), sim, s))
  noise <- accuracy(evaluate(bridge_model("x5"), sim, s))
  expect_lt(good$rmse[good$position == "B"], noise$rmse[noise$position == "B"])
})

test_that("model constructors validate their arguments", {
  expect_error(midas_model("x1", 0), class = "rinow_error_argument")
  expect_error(factor_model("x1", n_factors = 2), "More factors")
  expect_error(bvar_model("x1", n_draw = 100, n_burn = 200), class = "rinow_error_argument")
  expect_error(bayes_dfm_model("x1", n_iter = 10, n_burn = 20), "n_burn")
  expect_error(bottom_up_model(c(c1 = "*"), list(), factor_indicators = "x1"), "signs")
  expect_error(combination_model(list(1, 2)), "NowcastModel")
  expect_output(show(bridge_model("x1")), "BridgeModel")
})

test_that("combination averages its members", {
  r <- nowcast(combination_model(ar_model(), bridge_model("x1")), sim_v, sim_target)
  expect_equal(r@point, mean(r@contributions))
  expect_output(show(r), "range of members")
})
