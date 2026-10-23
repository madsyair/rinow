# Adding monthly indicators, including composite indexes from 'rcli'.

#' Convert a monthly series to an indicator table
#'
#' `as_indicator()` turns a monthly series into a data frame with columns
#' `date` (first day of the month) and `value`, the form that
#' [add_indicator()] adds to [NowcastData-class] objects.
#'
#' Methods exist for monthly `ts` objects, for data frames whose first (or
#' `date`) column holds dates and whose other column holds values, and for
#' composite indexes built with the 'rcli' package: a `CompositeIndex`, or a
#' `CliResult` or `CciResult` whose composite is taken. 'rcli' objects are
#' recognised by their slots, so 'rinow' does not depend on 'rcli'.
#'
#' @param x A monthly `ts`, a data frame, or an 'rcli' composite index or
#'   result.
#' @param series For 'rcli' objects: `"index"` (the composite indicator with
#'   long-term average 100) or `"cycle"` (the normalised composite cycle).
#' @param ... Not used.
#' @return A data frame with columns `date` and `value`. For 'rcli' objects
#'   the attribute `"composite"` is `TRUE` and the attribute `"release_lag"`
#'   holds the largest component release lag in months when the result
#'   records it.
#' @seealso [add_indicator()]
#' @examples
#' x <- ts(c(100, 101, 103, 102), start = c(2024, 11), frequency = 12)
#' as_indicator(x)
#' @export
setGeneric("as_indicator", function(x, ...) standardGeneric("as_indicator"))

#' @rdname as_indicator
setMethod("as_indicator", "ts", function(x, ...) {
  if (NCOL(x) != 1L) rinow_abort("`x` must hold one series.", "rinow_error_argument")
  if (stats::frequency(x) != 12) {
    rinow_abort("`x` must be monthly (frequency 12); aggregate or disaggregate it first.", "rinow_error_argument")
  }
  tt <- as.numeric(stats::time(x))
  year <- floor(tt + 1e-8)
  month <- round((tt - year) * 12) + 1
  data.frame(date = as.Date(sprintf("%04d-%02d-01", as.integer(year), as.integer(month))), value = as.numeric(x))
})

#' @rdname as_indicator
setMethod("as_indicator", "data.frame", function(x, ...) {
  x <- move_date_first(as.data.frame(x))
  if (ncol(x) != 2L) rinow_abort("`x` must have a date column and one value column.", "rinow_error_argument")
  d <- as_date(x$date)
  if (any(format(d, "%d") != "01")) rinow_abort("Dates must be first days of months.", "rinow_error_argument")
  out <- data.frame(date = d, value = as.numeric(x[[2L]]))
  out <- out[order(out$date), , drop = FALSE]
  if (anyDuplicated(out$date)) rinow_abort("Dates must not repeat.", "rinow_error_argument")
  rownames(out) <- NULL
  out
})

#' @rdname as_indicator
setMethod("as_indicator", "ANY", function(x, series = c("index", "cycle"), ...) {
  lag_months <- NA_real_
  if (isS4(x) && "composite" %in% methods::slotNames(x)) {
    meta <- tryCatch(methods::slot(methods::slot(x, "candidates"), "meta"), error = function(e) NULL)
    comp <- methods::slot(x, "composite")
    if (!is.null(meta) && all(c("name", "release_lag") %in% names(meta)) && isS4(comp)) {
      keep <- meta$name %in% methods::slot(comp, "components")
      lags <- meta$release_lag[keep]
      if (length(lags) && any(is.finite(lags))) lag_months <- max(lags, na.rm = TRUE)
    }
    x <- comp
  }
  if (isS4(x) && all(c("index", "cycle", "components") %in% methods::slotNames(x))) {
    series <- match.arg(series)
    out <- as_indicator(methods::slot(x, series))
    attr(out, "composite") <- TRUE
    attr(out, "release_lag") <- lag_months
    return(out)
  }
  rinow_abort(sprintf(
    "Cannot turn an object of class '%s' into an indicator; supply a monthly ts, a data frame or an 'rcli' composite.",
    class(x)[1L]
  ), "rinow_error_argument")
})

#' Add a monthly indicator to nowcasting data
#'
#' Adds one monthly series, for example a composite leading or coincident
#' index built with the 'rcli' package, to a [NowcastData-class] object so
#' that every model can use it like the other indicators.
#'
#' A composite index from 'rcli' is computed on the final data: its trend
#' filter, component selection and weights use observations that were not
#' available at earlier dates. [vintage()] only cuts the end of the series,
#' so pseudo real-time evaluations ([evaluate()]) that use such an index are
#' optimistic. `add_indicator()` says so with a message.
#'
#' @param data A [NowcastData-class] object with final data (not a vintage).
#' @param x A series accepted by [as_indicator()].
#' @param name Name of the new indicator.
#' @param release_lag Release lag in days after the end of the reference
#'   month. By default 30 days times the largest component release lag
#'   (in months) recorded in an 'rcli' result, and otherwise 30 days.
#' @param transform `"dlog"`, `"diff"` or `"level"`. By default `"diff"`
#'   for 'rcli' composites (changes in the index) and otherwise `"dlog"` for
#'   positive series and `"diff"` for the rest.
#' @param label Label shown in tables and the interface.
#' @param group Group of the indicator; `"composite"` for 'rcli' composites
#'   and `"indicator"` otherwise.
#' @param role Cyclical role (see [set_role()]); by default `"coincident"`
#'   for a coincident index from 'rcli' (`CciResult`), `"leading"` for a
#'   leading index (`CliResult`) and `"unknown"` otherwise.
#' @param series For 'rcli' objects: `"index"` or `"cycle"`, see
#'   [as_indicator()].
#' @return The [NowcastData-class] object with the new monthly column and
#'   metadata row.
#' @seealso [as_indicator()], [set_release_lag()], [set_transform()]
#' @examples
#' sim <- simulate_nowcast_data(n_years = 10, seed = 1)
#' f <- attr(sim, "factor")
#' extra <- data.frame(date = f$date, value = 100 + cumsum(f$factor) / 10)
#' sim2 <- add_indicator(sim, extra, "activity", release_lag = 10)
#' indicators(sim2)
#' @export
add_indicator <- function(data, x, name, release_lag = NULL, transform = NULL, label = name, group = NULL,
                          series = c("index", "cycle"), role = NULL) {
  if (!methods::is(data, "NowcastData")) rinow_abort("`data` must be a NowcastData object.", "rinow_error_argument")
  if (!is.na(data@as_of)) {
    rinow_abort("Add indicators to the final data, then take vintages.", "rinow_error_argument")
  }
  if (!is.character(name) || length(name) != 1L || !nzchar(name) || name == "date") {
    rinow_abort("`name` must be one non-empty string other than \"date\".", "rinow_error_argument")
  }
  if (name %in% names(data@monthly)) {
    rinow_abort(sprintf("The data already have an indicator called '%s'.", name), "rinow_error_argument")
  }
  s <- as_indicator(x, series = match.arg(series))
  composite <- isTRUE(attr(s, "composite"))
  if (is.null(release_lag)) {
    lag_months <- attr(s, "release_lag")
    release_lag <- if (composite && length(lag_months) && is.finite(lag_months)) 30 * lag_months else 30
  }
  if (length(release_lag) != 1L || !is.finite(release_lag) || release_lag < 0) {
    rinow_abort("`release_lag` must be one non-negative number of days.", "rinow_error_argument")
  }
  if (is.null(transform)) {
    transform <- if (composite) "diff" else if (all(s$value > 0, na.rm = TRUE)) "dlog" else "diff"
  }
  transform <- match.arg(transform, c("dlog", "diff", "level"))
  if (transform == "dlog" && any(s$value <= 0, na.rm = TRUE)) {
    rinow_abort("`dlog` needs positive values; use `diff` instead.", "rinow_error_argument")
  }
  if (is.null(group)) group <- if (composite) "composite" else "indicator"
  if (is.null(role)) {
    role <- if (inherits(x, "CciResult")) "coincident" else if (inherits(x, "CliResult")) "leading" else "unknown"
  }
  role <- match.arg(role, indicator_roles)
  m <- data@monthly
  dates <- seq(min(m$date, s$date), max(m$date, s$date), by = "month")
  out <- data.frame(date = dates)
  for (k in setdiff(names(m), "date")) out[[k]] <- m[[k]][match(dates, m$date)]
  out[[name]] <- s$value[match(dates, s$date)]
  data@monthly <- out
  if (!"role" %in% names(data@meta)) data@meta$role <- "unknown"
  data@meta <- rbind(data@meta, data.frame(
    name = name, label = label, transform = transform, release_lag = release_lag, group = group,
    role = role, stringsAsFactors = FALSE
  )[names(data@meta)])
  methods::validObject(data)
  if (composite) {
    message(
      "The composite index was computed on final data (trend filter, component selection and weights), ",
      "so pseudo real-time evaluations that use '", name, "' are optimistic."
    )
  }
  data
}

#' Take indicator roles from an 'rcli' classification
#'
#' The 'rcli' package classifies candidate series as leading, coincident or
#' lagging relative to a reference cycle, at turning points and by
#' cross-correlation. `roles_from_rcli()` copies those classes into a
#' [NowcastData-class] object, so that models can be built by role (see
#' [indicators()] and `shift` in [bridge_model()]).
#'
#' `x` can be a `CliResult` or `CciResult`, a `LeadAssessment`, or a data
#' frame with columns `name` and `class` (and optionally `median_lead`).
#' Objects are recognised by their slots, so 'rinow' does not depend on
#' 'rcli'. Candidates that 'rcli' rejected get the role `"unknown"`, or, with
#' `use_timing = TRUE`, the role implied by their median lead at turning
#' points: leading when it is at least `band` months, lagging when it is at
#' most `-band` months, and coincident in between. Indicators missing from
#' the classification keep their role.
#'
#' @param data A [NowcastData-class] object.
#' @param x An 'rcli' result or assessment, or a data frame.
#' @param use_timing Classify rejected candidates by their median lead?
#' @param band Months of lead or lag that separate the roles when
#'   `use_timing = TRUE`.
#' @return The updated [NowcastData-class] object. The attribute `"leads"`
#'   holds the median leads (months) of the classified indicators when `x`
#'   has them, a convenient start for `shift` in [bridge_model()].
#' @seealso [set_role()], [roles()]
#' @examples
#' sim <- simulate_nowcast_data(seed = 1)
#' tab <- data.frame(
#'   name = c("x1", "x2", "x3"), class = c("coincident", "leading", "rejected"),
#'   median_lead = c(0, 3, -4)
#' )
#' roles(roles_from_rcli(sim, tab))
#' roles(roles_from_rcli(sim, tab, use_timing = TRUE))
#' @export
roles_from_rcli <- function(data, x, use_timing = FALSE, band = 2) {
  if (!methods::is(data, "NowcastData")) rinow_abort("`data` must be a NowcastData object.", "rinow_error_argument")
  tab <- x
  if (isS4(tab) && "assessment" %in% methods::slotNames(tab)) tab <- methods::slot(tab, "assessment")
  if (isS4(tab) && "table" %in% methods::slotNames(tab)) tab <- methods::slot(tab, "table")
  if (!is.data.frame(tab) || !all(c("name", "class") %in% names(tab))) {
    rinow_abort(
      "`x` must be an 'rcli' result or assessment, or a data frame with columns `name` and `class`.",
      "rinow_error_argument"
    )
  }
  if (length(band) != 1L || !is.finite(band) || band <= 0) {
    rinow_abort("`band` must be one positive number.", "rinow_error_argument")
  }
  tab <- tab[tab$name %in% indicators(data), , drop = FALSE]
  if (!nrow(tab)) rinow_abort("No indicator of `data` appears in the classification.", "rinow_error_data")
  cls <- as.character(tab$class)
  role <- ifelse(cls %in% c("leading", "coincident", "lagging"), cls, "unknown")
  if (use_timing && "median_lead" %in% names(tab)) {
    ml <- tab$median_lead
    by_timing <- ifelse(is.na(ml), "unknown", ifelse(ml >= band, "leading", ifelse(ml <= -band, "lagging", "coincident")))
    role <- ifelse(role == "unknown", by_timing, role)
  }
  out <- do.call(set_role, c(list(data), as.list(stats::setNames(role, tab$name))))
  if ("median_lead" %in% names(tab)) attr(out, "leads") <- stats::setNames(tab$median_lead, tab$name)
  out
}
