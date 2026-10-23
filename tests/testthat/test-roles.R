test_that("roles are stored, validated and used to select indicators", {
  expect_true(all(roles(sim) == "unknown"))
  s2 <- set_role(sim, x1 = "coincident", x2 = "leading", x3 = "lagging")
  expect_equal(unname(roles(s2)[c("x1", "x2", "x3")]), c("coincident", "leading", "lagging"))
  expect_equal(indicators(s2, role = "coincident"), "x1")
  expect_equal(indicators(s2, role = c("coincident", "leading")), c("x1", "x2"))
  expect_error(set_role(sim, x1 = "early"), class = "rinow_error_argument")
  expect_error(set_role(sim, zz = "leading"), class = "rinow_error_argument")
  expect_error(indicators(s2, role = "early"))
  expect_output(show(s2), "roles: 1 leading, 1 coincident, 1 lagging")
  bad <- s2
  bad@meta$role[1] <- "early"
  expect_error(methods::validObject(bad))
})

test_that("nowcast_data accepts roles and older objects without a role column still work", {
  m <- sim@monthly
  q <- sim@quarterly
  d <- nowcast_data(m, q, target = "gdp", roles = c(x1 = "coincident"))
  expect_equal(roles(d)[["x1"]], "coincident")
  expect_equal(roles(d)[["x2"]], "unknown")
  old <- sim
  old@meta$role <- NULL
  expect_true(methods::validObject(old))
  expect_true(all(roles(old) == "unknown"))
  expect_equal(indicators(set_role(old, x2 = "leading"), role = "leading"), "x2")
})

setClass("MockAssessment", representation(table = "data.frame"))
setClass("MockCliResult", representation(assessment = "MockAssessment"))

test_that("roles are taken from an rcli-shaped classification", {
  tab <- data.frame(
    name = c("x1", "x2", "x3", "x4", "other"),
    class = c("coincident", "leading", "rejected", "rejected", "lagging"),
    median_lead = c(0, 4, -5, 1, -3)
  )
  r1 <- roles_from_rcli(sim, tab)
  expect_equal(unname(roles(r1)[c("x1", "x2", "x3", "x4", "x5")]), c("coincident", "leading", "unknown", "unknown", "unknown"))
  expect_equal(attr(r1, "leads")[["x2"]], 4)
  r2 <- roles_from_rcli(sim, tab, use_timing = TRUE)
  expect_equal(unname(roles(r2)[c("x3", "x4")]), c("lagging", "coincident"))
  r3 <- roles_from_rcli(sim, tab, use_timing = TRUE, band = 1)
  expect_equal(roles(r3)[["x4"]], "leading")
  res <- new("MockCliResult", assessment = new("MockAssessment", table = tab))
  expect_equal(roles(roles_from_rcli(sim, res)), roles(r1))
  expect_equal(roles(roles_from_rcli(sim, res@assessment)), roles(r1))
  expect_error(roles_from_rcli(sim, list()), class = "rinow_error_argument")
  expect_error(roles_from_rcli(sim, data.frame(name = "zz", class = "leading")), class = "rinow_error_data")
  expect_error(roles_from_rcli(sim, tab, band = 0), class = "rinow_error_argument")
  expect_error(roles_from_rcli(sim@monthly, tab), class = "rinow_error_argument")
})

test_that("add_indicator records a role", {
  extra <- data.frame(date = sim@monthly$date, value = 100 + seq_len(nrow(sim@monthly)))
  s2 <- add_indicator(sim, extra, "z", role = "leading")
  expect_equal(roles(s2)[["z"]], "leading")
  expect_equal(roles(add_indicator(sim, extra, "z"))[["z"]], "unknown")
  expect_error(add_indicator(sim, extra, "z", role = "early"))
})

test_that("a shift in the bridge equals a shifted copy of the indicator", {
  x <- sim@monthly$x2
  shifted <- data.frame(date = add_months(sim@monthly$date, 3), value = x)
  s2 <- add_indicator(sim, shifted, "x2s", release_lag = sim@meta$release_lag[sim@meta$name == "x2"])
  # by 13 November every month of 2015Q3 is released for both series, the target is not
  a <- nowcast(bridge_model(c("x1", "x2"), shift = c(x2 = 3)), sim, "2015Q3", as_of = "2015-11-13")
  b <- nowcast(bridge_model(c("x1", "x2s")), s2, "2015Q3", as_of = "2015-11-13")
  expect_equal(a@point, b@point, tolerance = 1e-10)
  expect_equal(unname(a@contributions), unname(b@contributions), tolerance = 1e-10)
})

test_that("a shifted indicator uses only data released by the vintage date", {
  m <- bridge_model(c("x1", "x2"), shift = c(x2 = 2))
  a <- nowcast(m, sim, sim_target, as_of = "2015-08-20")
  future <- sim
  late <- future@monthly$date > as.Date("2015-06-01")
  future@monthly$x2[late] <- future@monthly$x2[late] * 3
  b <- nowcast(m, future, sim_target, as_of = "2015-08-20")
  expect_equal(a@point, b@point)
  expect_output(show(m), "shift: x2 = 2")
})

test_that("bridge shifts are checked", {
  expect_error(bridge_model("x1", shift = c(x2 = 1)), class = "rinow_error_argument")
  expect_error(bridge_model("x1", shift = 2), class = "rinow_error_argument")
  expect_error(bridge_model("x1", shift = c(x1 = 1.5)), class = "rinow_error_argument")
  expect_error(bridge_model("x1", shift = c(x1 = 30)), class = "rinow_error_argument")
  expect_length(bridge_model("x1")@shift, 0)
})

test_that("add_months moves dates across years", {
  d <- as.Date(c("2020-01-01", "2020-11-01"))
  expect_equal(add_months(d, 3), as.Date(c("2020-04-01", "2021-02-01")))
  expect_equal(add_months(d, -2), as.Date(c("2019-11-01", "2020-09-01")))
  expect_equal(add_months(d, 0), d)
})
