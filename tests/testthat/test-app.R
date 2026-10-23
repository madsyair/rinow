test_that("the interface builds in both languages", {
  skip_if_not_installed("shiny")
  expect_s3_class(rinow_app("id"), "shiny.appobj")
  expect_s3_class(rinow_app("en"), "shiny.appobj")
  expect_error(make_model("nope", "x1"), class = "rinow_error_argument")
})

test_that("the server loads the Indonesian example", {
  skip_if_not_installed("shiny")
  shiny::testServer(app_server("id"), {
    session$setInputs(source = "idn", load = 1)
    expect_s4_class(rv$data, "NowcastData")
    expect_equal(rv$data@target, "pdb")
  })
})

test_that("the server runs the workflow from data to evaluation", {
  skip_if_not_installed("shiny")
  shiny::testServer(app_server("en"), {
    session$setInputs(source = "sim", load = 1)
    expect_s4_class(rv$data, "NowcastData")
    session$setInputs(cal_ind = "x2", cal_lag = 10, cal_tr = "dlog", cal_apply = 1)
    expect_equal(rv$data@meta$release_lag[rv$data@meta$name == "x2"], 10)
    session$setInputs(
      as_of = as.Date("2015-08-20"), model_type = "bridge", model_ind = c("x1", "x2"),
      n_lags = 3, n_factors = 1, add_model = 1
    )
    session$setInputs(model_type = "ar", add_model = 2)
    expect_length(rv$models, 2)
    session$setInputs(add_comb = 1)
    expect_length(rv$models, 3)
    session$setInputs(target_q = "2015Q3", run_nowcast = 1)
    expect_true(all(vapply(rv$results, methods::is, logical(1), "NowcastResult")))
    session$setInputs(ev_from = "2014Q1", ev_to = "2014Q4", ev_exclude = "", run_eval = 1)
    expect_length(rv$evals, 3)
    expect_match(paste(code_text(), collapse = "\n"), "bridge_model")
  })
})

test_that("outputs wait quietly before there is anything to show", {
  skip_if_not_installed("shiny")
  shiny::testServer(app_server("en"), {
    session$setInputs(source = "sim", load = 1)
    expect_error(output$eval_plot, class = "shiny.silent.error")
    expect_error(output$acc_table, class = "shiny.silent.error")
    session$setInputs(as_of = as.Date("2015-08-20"), model_type = "ar", add_model = 1)
    session$setInputs(ev_from = "2014Q1", ev_to = "2014Q2", ev_exclude = "2014Q1, 2014Q2", run_eval = 1)
    expect_error(output$acc_table, class = "shiny.silent.error")
  })
})
