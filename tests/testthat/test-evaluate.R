test_that("the schedule has four positions per quarter", {
  s <- nowcast_schedule("2014Q1", "2014Q4")
  expect_equal(nrow(s), 16)
  expect_equal(levels(s$position), c("M1", "M2", "M3", "B"))
  expect_equal(s$as_of[s$position == "B"][1], as.Date("2014-04-15"))
})

s <- nowcast_schedule("2011Q1", "2015Q2")
e_ar <- evaluate(ar_model(), sim, s)
e_br <- evaluate(bridge_model(c("x1", "x2")), sim, s)

test_that("evaluation compares nowcasts with final values", {
  r <- e_ar@results
  expect_equal(nrow(r), nrow(s))
  g <- transform_series(sim@quarterly$gdp, "dlog")[match(r$target, sim@quarterly$date)]
  expect_equal(r$actual, g)
  expect_equal(r$error, r$nowcast - r$actual)
  a <- accuracy(e_ar, e_br)
  expect_setequal(unique(a$model), c("AR", "Bridge"))
  expect_true(all(a$coverage >= 0 & a$coverage <= 1))
  expect_output(show(e_ar), "Evaluation")
})

test_that("excluding quarters changes the sample", {
  a <- accuracy(e_ar, exclude = c("2012Q1", "2012Q2"))
  expect_equal(a$n[1], length(unique(s$target)) - 2)
})

test_that("the Diebold-Mariano test matches forecast::dm.test", {
  d <- dm_test(e_br, e_ar)
  expect_equal(nrow(d), 4)
  skip_if_not_installed("forecast")
  a <- e_br@results[e_br@results$position == "M3", ]
  b <- e_ar@results[e_ar@results$position == "M3", ]
  ref <- forecast::dm.test(a$error, b$error, h = 1, power = 2)
  expect_equal(d$statistic[d$position == "M3"], unname(ref$statistic), tolerance = 1e-8)
  expect_equal(d$p_value[d$position == "M3"], unname(ref$p.value), tolerance = 1e-8)
})

test_that("combinations use only released information", {
  cm <- combine_nowcasts(e_ar, e_br, method = "mean")
  expect_equal(cm@results$nowcast, (e_ar@results$nowcast + e_br@results$nowcast) / 2)
  ci <- combine_nowcasts(e_ar, e_br, method = "inverse_mse", window = 8)
  expect_true(all(is.finite(ci@results$nowcast)))
  # the first nowcasts have no released errors yet: equal weights
  expect_equal(ci@results$nowcast[1], cm@results$nowcast[1])
  expect_s3_class(as.data.frame(ci), "data.frame")
})

test_that("year-on-year growth adds four quarterly log rates", {
  v <- vintage(sim, "2015-08-20")
  g <- transform_series(v@quarterly$gdp, "dlog")
  g <- g[!is.na(g)]
  expect_equal(yoy_growth(v, c("2015Q3" = 1)), 100 * (exp((sum(utils::tail(g, 3)) + 1) / 100) - 1))
})

test_that("the Indonesian data reproduce the guide's AR nowcast", {
  v <- vintage(idn_nowcast, "2026-10-23")
  expect_equal(round(nowcast(ar_model(), v, "2026Q3")@point, 2), 1.20)
})

test_that("accuracy returns an empty table when every target is excluded", {
  s <- nowcast_schedule("2012Q1", "2012Q2")
  ev <- evaluate(ar_model(), sim, s)
  a <- accuracy(ev, exclude = c("2012Q1", "2012Q2"))
  expect_s3_class(a, "data.frame")
  expect_equal(nrow(a), 0L)
})
