# Data classes: indicator panel with metadata, and data vintages.

#' Nowcasting data
#'
#' A `NowcastData` object holds a monthly indicator panel, a quarterly target
#' (and optional expenditure components) and metadata that tell every model
#' how to transform each series and when it is released. Create it with
#' [nowcast_data()].
#'
#' @slot monthly Data frame with a `date` column (first day of the month) and
#'   one numeric column per indicator, in levels.
#' @slot quarterly Data frame with a `date` column (first day of the quarter),
#'   the target and optional components, in levels.
#' @slot target Name of the target column of `quarterly`.
#' @slot target_transform How the target becomes a growth rate: `"dlog"` or
#'   `"level"` (the column already holds growth rates).
#' @slot components Names of component columns of `quarterly` (may be empty).
#' @slot meta Data frame with one row per indicator: `name`, `label`,
#'   `transform` (`"dlog"`, `"diff"` or `"level"`), `release_lag` (days after
#'   the end of the reference month) and `group`.
#' @slot target_lag Release lag of the target and components in days after
#'   the end of the quarter.
#' @slot as_of Date of the information set; `NA` for final data.
#' @seealso [nowcast_data()], [vintage()]
#' @exportClass NowcastData
setClass("NowcastData", representation(
  monthly = "data.frame", quarterly = "data.frame", target = "character",
  target_transform = "character", components = "character", meta = "data.frame",
  target_lag = "numeric", as_of = "Date"
), prototype(
  components = character(), target_transform = "dlog", target_lag = 45,
  as_of = as.Date(NA)
))

setValidity("NowcastData", function(object) {
  msg <- character()
  m <- object@monthly
  q <- object@quarterly
  if (!"date" %in% names(m) || !inherits(m$date, "Date")) msg <- c(msg, "`monthly` needs a Date column `date`.")
  if (!"date" %in% names(q) || !inherits(q$date, "Date")) msg <- c(msg, "`quarterly` needs a Date column `date`.")
  if (length(msg)) {
    return(msg)
  }
  if (any(format(m$date, "%d") != "01") || anyDuplicated(m$date) || is.unsorted(m$date)) {
    msg <- c(msg, "Monthly dates must be sorted first days of months without duplicates.")
  }
  if (any(format(q$date, "%d") != "01") || any(!format(q$date, "%m") %in% c("01", "04", "07", "10")) ||
    anyDuplicated(q$date) || is.unsorted(q$date)) {
    msg <- c(msg, "Quarterly dates must be sorted first days of quarters without duplicates.")
  }
  if (length(object@target) != 1L || !object@target %in% names(q)) msg <- c(msg, "`target` must name one quarterly column.")
  if (!object@target_transform %in% c("dlog", "level")) msg <- c(msg, "`target_transform` must be \"dlog\" or \"level\".")
  if (!all(object@components %in% names(q))) msg <- c(msg, "Every component must be a quarterly column.")
  need <- c("name", "label", "transform", "release_lag", "group")
  if (!all(need %in% names(object@meta))) {
    msg <- c(msg, paste("`meta` needs columns", paste(need, collapse = ", ")))
  } else {
    if (!setequal(object@meta$name, setdiff(names(m), "date"))) msg <- c(msg, "`meta$name` must match the monthly columns.")
    if (!all(object@meta$transform %in% c("dlog", "diff", "level"))) msg <- c(msg, "Unknown transform in `meta`.")
    bad_lag <- !is.finite(object@meta$release_lag) | object@meta$release_lag < 0
    if (any(bad_lag)) msg <- c(msg, "Release lags must be non-negative.")
  }
  if (length(msg)) msg else TRUE
})

#' Create nowcasting data
#'
#' @param monthly Data frame with a date column and monthly indicators in
#'   levels. The date column is the first column or a column named `date`.
#' @param quarterly Data frame with a date column, the target and optional
#'   components, in levels (or growth rates when `target_transform = "level"`).
#' @param target Name of the target column.
#' @param components Optional component columns for [bottom_up_model()].
#' @param transform Named character vector of transformations per indicator;
#'   indicators not named get `"dlog"` when all values are positive and
#'   `"diff"` otherwise.
#' @param release_lag Named numeric vector of release lags in days after the
#'   end of the reference month; unnamed indicators get 30 days.
#' @param target_lag Release lag of the target in days after the end of the
#'   quarter.
#' @param labels,groups Optional named character vectors of labels and groups.
#' @param target_transform `"dlog"` or `"level"`.
#' @return A [NowcastData-class] object.
#' @examples
#' sim <- simulate_nowcast_data(n_years = 12, seed = 1)
#' sim
#' @export
nowcast_data <- function(monthly, quarterly, target, components = character(), transform = NULL,
                         release_lag = NULL, target_lag = 45, labels = NULL, groups = NULL,
                         target_transform = c("dlog", "level")) {
  target_transform <- match.arg(target_transform)
  monthly <- as.data.frame(monthly)
  quarterly <- as.data.frame(quarterly)
  monthly <- move_date_first(monthly)
  quarterly <- move_date_first(quarterly)
  monthly$date <- month_start(monthly$date)
  quarterly$date <- quarter_start(quarterly$date)
  monthly <- monthly[order(monthly$date), , drop = FALSE]
  quarterly <- quarterly[order(quarterly$date), , drop = FALSE]
  ind <- setdiff(names(monthly), "date")
  if (!length(ind)) rinow_abort("`monthly` has no indicator column.", "rinow_error_data")
  tr <- vapply(ind, function(k) {
    if (!is.null(transform) && k %in% names(transform)) {
      return(unname(transform[[k]]))
    }
    if (all(monthly[[k]] > 0, na.rm = TRUE)) "dlog" else "diff"
  }, character(1))
  lag <- vapply(ind, function(k) if (!is.null(release_lag) && k %in% names(release_lag)) as.numeric(release_lag[[k]]) else 30, 0)
  meta <- data.frame(
    name = ind,
    label = if (is.null(labels)) ind else unname(ifelse(ind %in% names(labels), labels[ind], ind)),
    transform = unname(tr), release_lag = unname(lag),
    group = if (is.null(groups)) "indicator" else unname(ifelse(ind %in% names(groups), groups[ind], "indicator")),
    stringsAsFactors = FALSE
  )
  methods::new("NowcastData",
    monthly = monthly, quarterly = quarterly, target = target,
    target_transform = target_transform, components = as.character(components),
    meta = meta, target_lag = target_lag
  )
}

move_date_first <- function(d) {
  if ("date" %in% names(d)) {
    return(d[c("date", setdiff(names(d), "date"))])
  }
  names(d)[1] <- "date"
  d$date <- as_date(d$date)
  d
}

#' Indicator names of a data object
#' @param x A [NowcastData-class] object.
#' @return Character vector.
#' @examples
#' indicators(simulate_nowcast_data(seed = 1))
#' @export
indicators <- function(x) x@meta$name

#' Change the release calendar or transformations
#'
#' @param x A [NowcastData-class] object.
#' @param ... Named values, for example `exports = 15`.
#' @return The updated object.
#' @examples
#' sim <- simulate_nowcast_data(seed = 1)
#' sim <- set_release_lag(sim, x1 = 10)
#' sim <- set_transform(sim, x2 = "diff")
#' @export
set_release_lag <- function(x, ...) {
  v <- c(...)
  check_names(names(v), indicators(x), "indicator")
  x@meta$release_lag[match(names(v), x@meta$name)] <- as.numeric(v)
  methods::validObject(x)
  x
}

#' @rdname set_release_lag
#' @export
set_transform <- function(x, ...) {
  v <- c(...)
  check_names(names(v), indicators(x), "indicator")
  x@meta$transform[match(names(v), x@meta$name)] <- as.character(v)
  methods::validObject(x)
  x
}

#' Data as seen on a given date
#'
#' `vintage()` drops every observation that would not have been released by
#' `as_of` according to the release lags: a monthly value of month *m* is kept
#' when the last day of *m* plus its release lag is not later than `as_of`;
#' the same rule applies to quarters with `target_lag`. Revisions are not
#' modelled, so a vintage built from final data is a *pseudo* real-time
#' vintage.
#'
#' @param x A [NowcastData-class] object.
#' @param as_of Date of the information set.
#' @return A [NowcastData-class] object with `as_of` set.
#' @examples
#' sim <- simulate_nowcast_data(seed = 1)
#' v <- vintage(sim, "2018-08-10")
#' ragged_edge(v)
#' @export
vintage <- function(x, as_of) {
  as_of <- as_date(as_of)
  if (length(as_of) != 1L) rinow_abort("`as_of` must be one date.", "rinow_error_argument")
  m <- x@monthly
  q <- x@quarterly
  end_m <- period_end(m$date, 1L)
  for (i in seq_len(nrow(x@meta))) {
    k <- x@meta$name[i]
    m[[k]][end_m + x@meta$release_lag[i] > as_of] <- NA
  }
  end_q <- period_end(q$date, 3L)
  for (k in setdiff(names(q), "date")) q[[k]][end_q + x@target_lag > as_of] <- NA
  x@monthly <- m[m$date <= as_of, , drop = FALSE]
  x@quarterly <- q[q$date <= as_of, , drop = FALSE]
  x@as_of <- as_of
  x
}

#' Last available period of each series
#'
#' @param x A [NowcastData-class] object (usually a vintage).
#' @return Data frame with the series name, its last observed period and the
#'   number of missing periods at the end relative to the last month (or
#'   quarter) of the panel.
#' @examples
#' ragged_edge(vintage(simulate_nowcast_data(seed = 1), "2018-08-10"))
#' @export
ragged_edge <- function(x) {
  m <- x@monthly
  q <- x@quarterly
  last_m <- vapply(indicators(x), function(k) {
    i <- which(!is.na(m[[k]]))
    if (length(i)) max(i) else 0L
  }, 0L)
  out <- data.frame(
    series = indicators(x), frequency = "month",
    last = ifelse(last_m > 0, format(m$date[pmax(last_m, 1)], "%Y-%m"), NA),
    missing_at_end = nrow(m) - last_m, stringsAsFactors = FALSE
  )
  qs <- c(x@target, x@components)
  last_q <- vapply(qs, function(k) {
    i <- which(!is.na(q[[k]]))
    if (length(i)) max(i) else 0L
  }, 0L)
  rbind(out, data.frame(
    series = qs, frequency = "quarter",
    last = ifelse(last_q > 0, format_quarter(q$date[pmax(last_q, 1)]), NA),
    missing_at_end = nrow(q) - last_q, stringsAsFactors = FALSE
  ))
}

# Stationary panel of a vintage: monthly transformed indicators and the
# quarterly target growth, extended to the end of `target` with NA.
stationary_panel <- function(v, target = NULL, start = NULL) {
  m <- v@monthly
  q <- v@quarterly
  for (i in seq_len(nrow(v@meta))) {
    k <- v@meta$name[i]
    tr <- v@meta$transform[i]
    if (tr == "dlog" && any(m[[k]] <= 0, na.rm = TRUE)) tr <- "diff"
    m[[k]] <- transform_series(m[[k]], tr)
  }
  q$growth <- target_growth(q[[v@target]], v@target_transform)
  if (!is.null(target)) {
    target <- quarter_start(target)
    end_m <- seq(target, by = "month", length.out = 3L)[3L]
    if (max(m$date) < end_m) {
      add <- seq(max(m$date), end_m, by = "month")[-1L]
      m <- rbind(m, cbind(data.frame(date = add), as.data.frame(matrix(NA_real_, length(add), ncol(m) - 1L,
        dimnames = list(NULL, names(m)[-1L])
      ))))
    }
    if (max(q$date) < target) {
      add <- seq(max(q$date), target, by = "quarter")[-1L]
      extra <- as.data.frame(matrix(NA_real_, length(add), ncol(q) - 1L, dimnames = list(NULL, names(q)[-1L])))
      q <- rbind(q, cbind(data.frame(date = add), extra))
    }
    m <- m[m$date <= end_m, , drop = FALSE]
    q <- q[q$date <= target, , drop = FALSE]
  }
  if (!is.null(start)) {
    start <- as_date(start)
    m <- m[m$date >= start, , drop = FALSE]
    q <- q[q$date >= quarter_start(start), , drop = FALSE]
  }
  list(m = m, q = q)
}

# Quarterly regressor of one indicator for bridge-type models: the ragged edge
# is filled by AR forecasts of the stationary series, levels are rebuilt,
# averaged by quarter and turned into growth (dlog) or changes.
bridge_regressor <- function(v, name, target, filled = NULL) {
  tr <- v@meta$transform[v@meta$name == name]
  target <- quarter_start(target)
  end_m <- seq(target, by = "month", length.out = 3L)[3L]
  dates <- seq(min(v@monthly$date), end_m, by = "month")
  x <- v@monthly[[name]][match(dates, v@monthly$date)]
  if (tr == "dlog" && any(x <= 0, na.rm = TRUE)) tr <- "diff"
  obs <- which(!is.na(x))
  first <- min(obs)
  last <- max(obs)
  if (!is.null(filled) && last < length(dates)) {
    # filled: stationary monthly series already completed (e.g. by a factor-augmented AR)
    s <- filled[match(dates, attr(filled, "date"))]
    for (t in (last + 1L):length(dates)) {
      x[t] <- switch(tr,
        dlog = x[t - 1L] * exp(s[t] / 100),
        diff = x[t - 1L] + s[t],
        s[t]
      )
    }
  } else if (last < length(dates)) {
    h <- length(dates) - last
    if (tr == "dlog") {
      g <- 100 * diff(log(x[first:last]))
      g2 <- fill_ar(g, length(g) + h)
      x[(last + 1L):length(dates)] <- x[last] * exp(cumsum(g2[(length(g) + 1L):length(g2)]) / 100)
    } else if (tr == "diff") {
      d <- diff(x[first:last])
      d2 <- fill_ar(d, length(d) + h)
      x[(last + 1L):length(dates)] <- x[last] + cumsum(d2[(length(d) + 1L):length(d2)])
    } else {
      x[first:length(dates)] <- fill_ar(x[first:last], length(dates) - first + 1L)
    }
  }
  # interpolate interior gaps so that quarterly averages exist
  inside <- first:length(dates)
  if (anyNA(x[inside])) x[inside] <- stats::approx(inside[!is.na(x[inside])], x[inside][!is.na(x[inside])], inside, rule = 2)$y
  qq <- to_quarterly(dates, x)
  qq$value <- if (tr == "dlog") transform_series(qq$value, "dlog") else transform_series(qq$value, "diff")
  qq
}
