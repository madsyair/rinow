test_that("MIDAS with Almon weights works", {
  skip_if_not_installed("midasr")
  r <- nowcast(midas_model(c("x1", "x2"), 6, "nealmon"), sim_v, sim_target)
  expect_true(is.finite(r@point))
})

test_that("the mixed-frequency DFM nowcasts and decomposes news", {
  skip_if_not_installed("dfms")
  m <- dfm_model(c("x1", "x2", "x3"))
  r <- nowcast(m, sim_v, sim_target)
  expect_true(is.finite(r@point) && r@upper > r@lower)
  nw <- dfm_news(m, sim, sim_target, "2015-08-20", "2015-09-20")
  expect_true(all(c("series", "news", "gain", "impact") %in% names(nw)))
  expect_equal(nw$impact, nw$news * nw$gain, tolerance = 1e-8)
})

test_that("the BVAR nowcasts with and without conditioning", {
  skip_if_not_installed("BVAR")
  set.seed(1)
  r1 <- nowcast(bvar_model(c("x1", "x2"), n_draw = 1500, n_burn = 500), sim_v, sim_target)
  r2 <- nowcast(bvar_model(c("x1", "x2"), conditional = FALSE, n_draw = 1500, n_burn = 500), sim_v, sim_target)
  expect_true(is.finite(r1@point) && is.finite(r2@point))
  expect_length(r1@details$draws, 1000)
})

test_that("the Bayesian DFM samples the nowcast distribution", {
  skip_if_not_installed("Matrix")
  skip_if_not_installed("stochvol")
  set.seed(1)
  r <- nowcast(bayes_dfm_model(c("x1", "x2", "x3"), n_iter = 300, n_burn = 100), sim_v, sim_target)
  expect_length(r@details$draws, 200)
  expect_true(r@lower < r@point && r@point < r@upper)
  rg <- nowcast(
    bayes_dfm_model(c("x1", "x2"), nu = 1000, stochastic_volatility = FALSE, n_iter = 200, n_burn = 50),
    sim_v, sim_target
  )
  expect_true(is.finite(rg@point))
})

test_that("machine-learning models nowcast", {
  for (m in c("lasso", "enet", "rf", "xgb", "sglasso")) {
    pkg <- c(lasso = "glmnet", enet = "glmnet", rf = "ranger", xgb = "xgboost", sglasso = "midasml")[[m]]
    skip_if_not_installed(pkg)
    r <- nowcast(ml_model(c("x1", "x2", "x3"), m), sim_v, sim_target)
    expect_true(is.finite(r@point), label = m)
  }
})

test_that("the bottom-up model adds up its contributions", {
  skip_if_not_installed("BVAR")
  skip_if_not_installed("dfms")
  set.seed(1)
  m <- bottom_up_model(c(c1 = "+", c2 = "+"),
    map = list(c1 = c("x1", "x2"), c2 = "x1", rest = "x3"),
    factor_indicators = c("x1", "x2", "x3")
  )
  r <- nowcast(m, sim_v, sim_target)
  expect_equal(100 * log1p(sum(r@contributions) / 100), r@point, tolerance = 1e-10)
  w <- vapply(r@details$components, function(x) x[["weight"]], 0)
  expect_true(all(w >= 0 & w <= 1))
  # identity: shares times growth plus rest reproduce actual growth
  q <- sim@quarterly
  cc <- rinow:::component_contributions(q, "gdp", c(c1 = "+", c2 = "+"), character())
  tot <- rowSums(cc$growth * cc$share) + cc$rest
  expect_equal(tot[-1], 100 * diff(q$gdp) / head(q$gdp, -1), tolerance = 1e-10)
})

test_that("bottom-up combination weights use aligned in-sample fits", {
  skip_if_not_installed("BVAR")
  skip_if_not_installed("dfms")
  set.seed(2)
  m <- bottom_up_model(c(c1 = "+", c2 = "+"),
    map = list(c1 = c("x1", "x2"), rest = "x3"),
    factor_indicators = c("x1", "x2", "x3")
  )
  r <- nowcast(m, sim_v, sim_target)
  # x1 and x2 carry the factor, so the bridge must get positive weight for c1
  expect_gt(r@details$components$c1[["weight"]], 0)
})
