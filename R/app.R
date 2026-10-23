# Step-by-step 'shiny' interface (Indonesian and English).

app_text <- function(language = c("id", "en")) {
  language <- match.arg(language)
  id <- list(
    title = "rinow: nowcasting langkah demi langkah", step1 = "1. Data", step2 = "2. Kalender rilis",
    step3 = "3. Vintage", step4 = "4. Model", step5 = "5. Nowcast", step6 = "6. Evaluasi", step7 = "7. Laporan",
    source = "Sumber data", ex_idn = "Contoh Indonesia (FRED)", ex_sim = "Data simulasi", upload = "Unggah CSV",
    file_m = "CSV bulanan (kolom pertama tanggal)", file_q = "CSV triwulanan (kolom pertama tanggal)",
    target = "Kolom target", load = "Muat data", indicator = "Indikator", lag = "Jeda rilis (hari)",
    transform = "Transformasi", apply = "Terapkan", as_of = "Tanggal informasi (as of)",
    model_type = "Jenis model", indicators = "Indikator model", add = "Tambahkan ke daftar model",
    models = "Daftar model", tq = "Triwulan sasaran (mis. 2026Q3)", run = "Jalankan nowcast",
    from = "Dari triwulan", to = "Sampai triwulan", exclude = "Kecualikan triwulan (pisahkan koma)",
    evaluate = "Jalankan evaluasi",
    no_result = "Belum ada nowcast yang dapat dinilai untuk pilihan ini.",
    download_eval = "Unduh hasil evaluasi (CSV)", download_code = "Unduh skrip R",
    n_lags = "Lag bulanan", n_factors = "Banyak faktor", method = "Metode", clear = "Kosongkan daftar",
    no_models = "Belum ada model. Tambahkan pada langkah 4.", yoy = "Pertumbuhan y-o-y tersirat",
    loaded = "Data dimuat.", combine = "Tambahkan kombinasi rata-rata",
    role = "Peran siklus", roles = c(
      "Pendahulu" = "leading", "Sejalan" = "coincident", "Tertinggal" = "lagging", "Belum diketahui" = "unknown"
    ),
    pick_role = "Pilih indikator menurut peran", fill = "Isi indikator model",
    shift = "Geser indikator pendahulu (bulan, persamaan jembatan)"
  )
  en <- list(
    title = "rinow: step-by-step nowcasting", step1 = "1. Data", step2 = "2. Release calendar",
    step3 = "3. Vintage", step4 = "4. Models", step5 = "5. Nowcast", step6 = "6. Evaluation", step7 = "7. Report",
    source = "Data source", ex_idn = "Indonesian example (FRED)", ex_sim = "Simulated data", upload = "Upload CSV",
    file_m = "Monthly CSV (first column dates)", file_q = "Quarterly CSV (first column dates)",
    target = "Target column", load = "Load data", indicator = "Indicator", lag = "Release lag (days)",
    transform = "Transformation", apply = "Apply", as_of = "Information date (as of)",
    model_type = "Model type", indicators = "Model indicators", add = "Add to model list",
    models = "Model list", tq = "Target quarter (e.g. 2026Q3)", run = "Run nowcast",
    from = "From quarter", to = "To quarter", exclude = "Exclude quarters (comma separated)",
    evaluate = "Run evaluation",
    no_result = "No nowcasts can be assessed for this choice yet.",
    download_eval = "Download evaluation (CSV)", download_code = "Download R script",
    n_lags = "Monthly lags", n_factors = "Number of factors", method = "Method", clear = "Clear list",
    no_models = "No model yet. Add one in step 4.", yoy = "Implied year-on-year growth",
    loaded = "Data loaded.", combine = "Add mean combination",
    role = "Cyclical role", roles = c(
      "Leading" = "leading", "Coincident" = "coincident", "Lagging" = "lagging", "Unknown" = "unknown"
    ),
    pick_role = "Select indicators by role", fill = "Fill model indicators",
    shift = "Shift leading indicators (months, bridge equation)"
  )
  if (language == "id") id else en
}

model_choices <- c(
  "AR" = "ar", "Bridge" = "bridge", "U-MIDAS" = "umidas", "MIDAS-Almon" = "nealmon",
  "Factor" = "factor", "DFM" = "dfm", "BVAR" = "bvar", "Bayesian DFM (t, SV)" = "bdfm",
  "LASSO" = "lasso", "Random forest" = "rf", "XGBoost" = "xgb"
)

# Build a model from the interface inputs; returns the model and the R call as text.
make_model <- function(type, ind, n_lags = 3, n_factors = 1, method = NULL, shift = NULL) {
  q <- function(x) paste0("c(", paste0('"', x, '"', collapse = ", "), ")")
  qs <- function(x) paste0("c(", paste0(names(x), " = ", x, collapse = ", "), ")")
  switch(type,
    ar = list(model = ar_model(), code = "ar_model()"),
    bridge = if (length(shift)) {
      list(model = bridge_model(ind, shift = shift), code = sprintf("bridge_model(%s, shift = %s)", q(ind), qs(shift)))
    } else {
      list(model = bridge_model(ind), code = sprintf("bridge_model(%s)", q(ind)))
    },
    umidas = list(model = midas_model(ind, n_lags), code = sprintf("midas_model(%s, n_lags = %d)", q(ind), n_lags)),
    nealmon = list(
      model = midas_model(ind, n_lags, "nealmon"),
      code = sprintf("midas_model(%s, n_lags = %d, weights = \"nealmon\")", q(ind), n_lags)
    ),
    factor = list(model = factor_model(ind, n_factors), code = sprintf("factor_model(%s, n_factors = %d)", q(ind), n_factors)),
    dfm = list(model = dfm_model(ind, n_factors), code = sprintf("dfm_model(%s, n_factors = %d)", q(ind), n_factors)),
    bvar = list(model = bvar_model(ind), code = sprintf("bvar_model(%s)", q(ind))),
    bdfm = list(
      model = bayes_dfm_model(ind, n_iter = 1500, n_burn = 500),
      code = sprintf("bayes_dfm_model(%s, n_iter = 1500, n_burn = 500)", q(ind))
    ),
    lasso = list(model = ml_model(ind, "lasso"), code = sprintf("ml_model(%s, \"lasso\")", q(ind))),
    rf = list(model = ml_model(ind, "rf"), code = sprintf("ml_model(%s, \"rf\")", q(ind))),
    xgb = list(model = ml_model(ind, "xgb"), code = sprintf("ml_model(%s, \"xgb\")", q(ind))),
    rinow_abort("Unknown model type.", "rinow_error_argument")
  )
}

read_csv_data <- function(path) {
  d <- utils::read.csv(path, check.names = FALSE)
  names(d)[1] <- "date"
  d$date <- as_date(as.character(d$date))
  d
}

app_ui <- function(language = "id") {
  tx <- app_text(language)
  shiny::fluidPage(
    shiny::titlePanel(tx$title),
    shiny::navlistPanel(
      id = "steps", widths = c(2, 10),
      shiny::tabPanel(
        tx$step1,
        shiny::radioButtons("source", tx$source, stats::setNames(c("idn", "sim", "csv"), c(tx$ex_idn, tx$ex_sim, tx$upload))),
        shiny::conditionalPanel(
          "input.source == 'csv'",
          shiny::fileInput("file_m", tx$file_m, accept = ".csv"), shiny::fileInput("file_q", tx$file_q, accept = ".csv"),
          shiny::textInput("target_col", tx$target, "gdp")
        ),
        shiny::actionButton("load", tx$load, class = "btn-primary"),
        shiny::verbatimTextOutput("data_info")
      ),
      shiny::tabPanel(
        tx$step2,
        shiny::fluidRow(
          shiny::column(4, shiny::selectInput("cal_ind", tx$indicator, choices = NULL)),
          shiny::column(2, shiny::numericInput("cal_lag", tx$lag, 30, min = 0)),
          shiny::column(2, shiny::selectInput("cal_tr", tx$transform, c("dlog", "diff", "level"))),
          shiny::column(2, shiny::selectInput("cal_role", tx$role, tx$roles, selected = "unknown")),
          shiny::column(2, shiny::br(), shiny::actionButton("cal_apply", tx$apply))
        ),
        shiny::tableOutput("meta_table")
      ),
      shiny::tabPanel(
        tx$step3,
        shiny::dateInput("as_of", tx$as_of, value = Sys.Date()),
        shiny::plotOutput("ragged_plot", height = "360px"), shiny::tableOutput("ragged_table")
      ),
      shiny::tabPanel(
        tx$step4,
        shiny::fluidRow(
          shiny::column(4, shiny::selectInput("model_type", tx$model_type, model_choices)),
          shiny::column(4, shiny::numericInput("n_lags", tx$n_lags, 3, min = 1, max = 12)),
          shiny::column(4, shiny::numericInput("n_factors", tx$n_factors, 1, min = 1, max = 5))
        ),
        shiny::fluidRow(
          shiny::column(6, shiny::checkboxGroupInput("pick_role", tx$pick_role, tx$roles[1:3], inline = TRUE)),
          shiny::column(2, shiny::br(), shiny::actionButton("fill_role", tx$fill)),
          shiny::column(4, shiny::numericInput("shift_leading", tx$shift, 0, min = 0, max = 24))
        ),
        shiny::selectizeInput("model_ind", tx$indicators, choices = NULL, multiple = TRUE),
        shiny::actionButton("add_model", tx$add, class = "btn-primary"),
        shiny::actionButton("add_comb", tx$combine), shiny::actionButton("clear_models", tx$clear),
        shiny::h4(tx$models), shiny::verbatimTextOutput("model_list")
      ),
      shiny::tabPanel(
        tx$step5,
        shiny::textInput("target_q", tx$tq, ""), shiny::actionButton("run_nowcast", tx$run, class = "btn-primary"),
        shiny::tableOutput("nowcast_table"), shiny::plotOutput("nowcast_plot", height = "320px")
      ),
      shiny::tabPanel(
        tx$step6,
        shiny::fluidRow(
          shiny::column(3, shiny::textInput("ev_from", tx$from, "")),
          shiny::column(3, shiny::textInput("ev_to", tx$to, "")),
          shiny::column(6, shiny::textInput("ev_exclude", tx$exclude, ""))
        ),
        shiny::actionButton("run_eval", tx$evaluate, class = "btn-primary"),
        shiny::tableOutput("acc_table"), shiny::plotOutput("eval_plot", height = "320px")
      ),
      shiny::tabPanel(
        tx$step7,
        shiny::downloadButton("dl_eval", tx$download_eval), shiny::downloadButton("dl_code", tx$download_code),
        shiny::verbatimTextOutput("code")
      )
    )
  )
}

app_server <- function(language = "id") {
  tx <- app_text(language)
  function(input, output, session) {
    rv <- shiny::reactiveValues(
      data = NULL, data_code = NULL, models = list(), codes = character(),
      results = NULL, evals = list()
    )
    shiny::observeEvent(input$load, {
      if (input$source == "idn") {
        e <- new.env()
        utils::data("idn_nowcast", package = "rinow", envir = e)
        rv$data <- e$idn_nowcast
        rv$data_code <- "dat <- rinow::idn_nowcast"
      } else if (input$source == "sim") {
        rv$data <- simulate_nowcast_data(seed = 1)
        rv$data_code <- "dat <- simulate_nowcast_data(seed = 1)"
      } else {
        shiny::req(input$file_m, input$file_q)
        rv$data <- nowcast_data(read_csv_data(input$file_m$datapath), read_csv_data(input$file_q$datapath),
          target = input$target_col
        )
        rv$data_code <- sprintf(
          "dat <- nowcast_data(read.csv(\"monthly.csv\"), read.csv(\"quarterly.csv\"), target = \"%s\")",
          input$target_col
        )
      }
      d <- rv$data
      shiny::updateSelectInput(session, "cal_ind", choices = indicators(d))
      shiny::updateSelectizeInput(session, "model_ind", choices = indicators(d), selected = utils::head(indicators(d), 3))
      last_m <- max(d@monthly$date)
      shiny::updateDateInput(session, "as_of", value = period_end(last_m, 1L) + 23)
      lastq <- max(d@quarterly$date[!is.na(d@quarterly[[d@target]])])
      shiny::updateTextInput(session, "target_q", value = format_quarter(seq(lastq, by = "quarter", length.out = 2)[2]))
      shiny::updateTextInput(session, "ev_from", value = format_quarter(seq(lastq, by = "-3 years", length.out = 2)[2]))
      shiny::updateTextInput(session, "ev_to", value = format_quarter(lastq))
    })
    output$data_info <- shiny::renderPrint({
      shiny::req(rv$data)
      cat(tx$loaded, "\n")
      methods::show(rv$data)
    })
    shiny::observeEvent(input$cal_apply, {
      shiny::req(rv$data, input$cal_ind)
      rv$data <- set_release_lag(rv$data, stats::setNames(input$cal_lag, input$cal_ind))
      rv$data <- set_transform(rv$data, stats::setNames(input$cal_tr, input$cal_ind))
      role <- if (is.null(input$cal_role)) "unknown" else input$cal_role
      rv$data <- set_role(rv$data, stats::setNames(role, input$cal_ind))
      rv$data_code <- c(
        rv$data_code, sprintf("dat <- set_release_lag(dat, %s = %s)", input$cal_ind, input$cal_lag),
        sprintf("dat <- set_transform(dat, %s = \"%s\")", input$cal_ind, input$cal_tr),
        if (role != "unknown") sprintf("dat <- set_role(dat, %s = \"%s\")", input$cal_ind, role)
      )
    })
    shiny::observeEvent(input$cal_ind, {
      shiny::req(rv$data, input$cal_ind)
      i <- match(input$cal_ind, rv$data@meta$name)
      shiny::updateNumericInput(session, "cal_lag", value = rv$data@meta$release_lag[i])
      shiny::updateSelectInput(session, "cal_tr", selected = rv$data@meta$transform[i])
      shiny::updateSelectInput(session, "cal_role", selected = roles(rv$data)[[i]])
    })
    shiny::observeEvent(input$fill_role, {
      shiny::req(rv$data, length(input$pick_role) > 0)
      shiny::updateSelectizeInput(session, "model_ind", selected = indicators(rv$data, role = input$pick_role))
    })
    output$meta_table <- shiny::renderTable({
      shiny::req(rv$data)
      meta <- rv$data@meta
      meta$role <- unname(roles(rv$data))
      meta
    })
    vint <- shiny::reactive({
      shiny::req(rv$data, input$as_of)
      vintage(rv$data, input$as_of)
    })
    output$ragged_plot <- shiny::renderPlot(plot(vint()))
    output$ragged_table <- shiny::renderTable(ragged_edge(vint()))
    shiny::observeEvent(input$add_model, {
      shiny::req(rv$data)
      ind <- if (input$model_type == "ar") character() else input$model_ind
      if (input$model_type != "ar" && !length(ind)) {
        return()
      }
      lead <- intersect(ind, indicators(rv$data, role = "leading"))
      sh <- if (input$model_type == "bridge" && isTRUE(input$shift_leading > 0) && length(lead)) {
        stats::setNames(rep(as.integer(input$shift_leading), length(lead)), lead)
      }
      mm <- make_model(input$model_type, ind, as.integer(input$n_lags), as.integer(input$n_factors), shift = sh)
      rv$models <- c(rv$models, list(mm$model))
      rv$codes <- c(rv$codes, mm$code)
    })
    shiny::observeEvent(input$add_comb, {
      shiny::req(length(rv$models) >= 2)
      rv$models <- c(rv$models, list(combination_model(rv$models)))
      rv$codes <- c(rv$codes, sprintf("combination_model(list(%s))", paste(rv$codes, collapse = ", ")))
    })
    shiny::observeEvent(input$clear_models, {
      rv$models <- list()
      rv$codes <- character()
    })
    output$model_list <- shiny::renderPrint({
      if (!length(rv$models) > 0) cat(tx$no_models) else for (m in rv$models) methods::show(m)
    })
    shiny::observeEvent(input$run_nowcast, {
      shiny::req(rv$data, length(rv$models) > 0, nzchar(input$target_q))
      v <- vint()
      res <- lapply(rv$models, function(m) tryCatch(nowcast(m, v, input$target_q), error = function(e) conditionMessage(e)))
      rv$results <- res
    })
    output$nowcast_table <- shiny::renderTable(
      {
        shiny::req(rv$results)
        ok <- vapply(rv$results, methods::is, logical(1), "NowcastResult")
        tab <- do.call(rbind, lapply(rv$results[ok], as.data.frame))
        if (rv$data@target_transform == "dlog") {
          tab[[tx$yoy]] <- vapply(rv$results[ok], function(r) {
            tryCatch(yoy_growth(vint(), stats::setNames(r@point, format_quarter(r@target))), error = function(e) NA_real_)
          }, 0)
        }
        err <- rv$results[!ok]
        if (length(err)) attr(tab, "errors") <- unlist(err)
        tab
      },
      digits = 3
    )
    output$nowcast_plot <- shiny::renderPlot({
      shiny::req(rv$results)
      ok <- Filter(function(r) methods::is(r, "NowcastResult"), rv$results)
      shiny::req(length(ok) > 0)
      p <- vapply(ok, methods::slot, 0, "point")
      lo <- vapply(ok, methods::slot, 0, "lower")
      up <- vapply(ok, methods::slot, 0, "upper")
      op <- graphics::par(mar = c(4, 10, 2, 1))
      on.exit(graphics::par(op))
      graphics::plot(p, seq_along(p),
        xlim = range(c(p, lo, up), na.rm = TRUE), pch = 19, yaxt = "n", ylab = "",
        xlab = "growth (%)", col = "#1b6ca8"
      )
      graphics::axis(2, seq_along(p), vapply(ok, methods::slot, "", "model"), las = 1)
      graphics::segments(lo, seq_along(p), up, seq_along(p), col = "grey50")
    })
    shiny::observeEvent(input$run_eval, {
      shiny::req(rv$data, length(rv$models) > 0)
      sch <- nowcast_schedule(input$ev_from, input$ev_to)
      shiny::withProgress(message = tx$evaluate, value = 0, {
        rv$evals <- lapply(seq_along(rv$models), function(i) {
          shiny::incProgress(1 / length(rv$models) > 0, detail = rv$models[[i]]@name)
          evaluate(rv$models[[i]], rv$data, sch)
        })
      })
    })
    excl <- shiny::reactive({
      x <- trimws(strsplit(input$ev_exclude, ",")[[1]])
      x <- x[nzchar(x)]
      if (length(x)) quarter_start(x) else NULL
    })
    output$acc_table <- shiny::renderTable(
      {
        shiny::req(length(rv$evals) > 0)
        out <- tryCatch(accuracy(rv$evals, exclude = excl()), error = function(e) NULL)
        shiny::validate(shiny::need(!is.null(out) && nrow(out), tx$no_result))
        out
      },
      digits = 3
    )
    output$eval_plot <- shiny::renderPlot({
      shiny::req(length(rv$evals) > 0)
      ok <- any(!is.na(rv$evals[[1]]@results$nowcast))
      shiny::validate(shiny::need(ok, tx$no_result))
      plot(rv$evals[[1]])
    })
    code_text <- shiny::reactive({
      c(
        "library(rinow)", rv$data_code,
        sprintf("v <- vintage(dat, \"%s\")", format(input$as_of)),
        if (length(rv$codes)) {
          c(
            sprintf("models <- list(%s)", paste(rv$codes, collapse = ",\n  ")),
            sprintf("lapply(models, nowcast, data = v, target = \"%s\")", input$target_q),
            sprintf("s <- nowcast_schedule(\"%s\", \"%s\")", input$ev_from, input$ev_to),
            "ev <- lapply(models, evaluate, data = dat, schedule = s)",
            "accuracy(ev)"
          )
        }
      )
    })
    output$code <- shiny::renderText(paste(code_text(), collapse = "\n"))
    output$dl_code <- shiny::downloadHandler("rinow-script.R", function(file) writeLines(code_text(), file))
    output$dl_eval <- shiny::downloadHandler("rinow-evaluation.csv", function(file) {
      utils::write.csv(eval_frame(rv$evals), file, row.names = FALSE)
    })
  }
}

#' Step-by-step interface
#'
#' `run_app()` opens a 'shiny' application with seven steps: load data
#' (Indonesian example, simulated data or CSV files), edit the release
#' calendar and transformations, inspect the vintage and its ragged edge,
#' build a list of models, nowcast, evaluate in pseudo real time, and
#' download the results and an R script that reproduces them.
#' `rinow_app()` returns the application object without running it.
#'
#' @param language `"id"` (Indonesian) or `"en"` (English).
#' @param ... Passed to [shiny::runApp()].
#' @return `run_app()` is called for its side effect; `rinow_app()`
#'   returns a `shiny.appobj`.
#' @examples
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'   run_app()
#' }
#' @export
run_app <- function(language = c("id", "en"), ...) {
  shiny::runApp(rinow_app(match.arg(language)), ...)
}

#' @rdname run_app
#' @export
rinow_app <- function(language = c("id", "en")) {
  need_package("shiny", "The interface")
  language <- match.arg(language)
  shiny::shinyApp(app_ui(language), app_server(language))
}
