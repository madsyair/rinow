# show(), summary-like printing and base-graphics plots.

#' @rdname NowcastData-class
#' @param object An object of the class.
#' @export
setMethod("show", "NowcastData", function(object) {
  m <- object@monthly
  q <- object@quarterly
  cat("<NowcastData>", if (!is.na(object@as_of)) paste("vintage of", format(object@as_of)) else "final data", "\n")
  cat("  target:", object@target, sprintf(
    "(%s), %s to %s\n", object@target_transform,
    format_quarter(min(q$date)), format_quarter(max(q$date))
  ))
  if (length(object@components)) cat("  components:", paste(object@components, collapse = ", "), "\n")
  cat(sprintf(
    "  %d monthly indicators, %s to %s\n", nrow(object@meta), format(min(m$date), "%Y-%m"),
    format(max(m$date), "%Y-%m")
  ))
  r <- table(factor(roles(object), indicator_roles))
  if (any(r[c("leading", "coincident", "lagging")] > 0)) {
    cat("  roles:", paste(r[r > 0], names(r)[r > 0], collapse = ", "), "\n")
  }
  invisible(object)
})

#' @rdname NowcastModel-class
#' @param object A model.
#' @export
setMethod("show", "NowcastModel", function(object) {
  cat(sprintf("<%s> %s\n", class(object)[1L], object@name))
  if (length(object@indicators)) cat("  indicators:", paste(object@indicators, collapse = ", "), "\n")
  sl <- setdiff(methods::slotNames(object), c("name", "indicators", "models", "map"))
  for (s in sl) {
    val <- methods::slot(object, s)
    if (length(val) && !is.list(val)) {
      txt <- if (is.null(names(val))) format(val) else paste0(names(val), " = ", format(val))
      cat(sprintf("  %s: %s\n", s, paste(txt, collapse = ", ")))
    }
  }
  if (methods::is(object, "CombinationModel")) {
    cat("  members:", paste(vapply(object@models, methods::slot, "", "name"), collapse = ", "), "\n")
  }
  invisible(object)
})

#' @rdname NowcastResult-class
#' @param object A result.
#' @export
setMethod("show", "NowcastResult", function(object) {
  cat(sprintf(
    "<NowcastResult> %s, target %s, information up to %s\n", object@model, format_quarter(object@target),
    format(object@as_of)
  ))
  if (is.na(object@lower)) {
    cat(sprintf("  nowcast: %.2f\n", object@point))
  } else {
    cat(sprintf(
      "  nowcast: %.2f  [%.2f, %.2f]%s\n", object@point, object@lower, object@upper,
      if (is.na(object@level)) " (range of members)" else sprintf(" (%d%% interval)", round(100 * object@level))
    ))
  }
  if (length(object@contributions)) {
    cat("  contributions:\n")
    print(round(object@contributions, 3))
  }
  invisible(object)
})

#' @rdname Evaluation-class
#' @param object An evaluation.
#' @export
setMethod("show", "Evaluation", function(object) {
  cat(sprintf(
    "<Evaluation> %s, %d nowcasts for %d target quarters\n", object@model, nrow(object@results),
    length(unique(object@results$target))
  ))
  print(accuracy(object)[, c("position", "rmse", "mae", "coverage", "n")], row.names = FALSE, digits = 3)
  invisible(object)
})

#' Plot data, results and evaluations
#'
#' * `NowcastData`: the ragged edge of the last 12 months.
#' * `NowcastResult`: contributions (when present) or the point and interval.
#' * `Evaluation`: nowcasts and actual values over time at one position.
#'
#' @param x Object to plot.
#' @param y Not used.
#' @param position Position of the evaluation to plot.
#' @param ... Passed to the base plotting functions.
#' @return `x`, invisibly.
#' @examples
#' sim <- simulate_nowcast_data(n_years = 10, seed = 1)
#' plot(vintage(sim, "2008-08-10"))
#' @name plot-methods
#' @aliases plot,NowcastData,ANY-method plot,NowcastResult,ANY-method plot,Evaluation,ANY-method
NULL

#' @rdname plot-methods
#' @export
setMethod("plot", "NowcastData", function(x, y, ...) {
  m <- utils::tail(x@monthly, 12L)
  A <- !is.na(as.matrix(m[, indicators(x), drop = FALSE]))
  op <- graphics::par(mar = c(4, 7, 2, 1))
  on.exit(graphics::par(op))
  graphics::image(seq_len(nrow(A)), seq_len(ncol(A)), A * 1,
    col = c("grey88", "#1b6ca8"), axes = FALSE,
    xlab = "", ylab = "", main = "Data available (dark) in the last 12 months", ...
  )
  graphics::axis(1, at = seq_len(nrow(A)), labels = format(m$date, "%b\n%Y"), cex.axis = 0.7, padj = 0.5)
  graphics::axis(2, at = seq_len(ncol(A)), labels = indicators(x), las = 1, cex.axis = 0.8)
  graphics::box()
  invisible(x)
})

#' @rdname plot-methods
#' @export
setMethod("plot", "NowcastResult", function(x, y, ...) {
  if (length(x@contributions)) {
    cc <- x@contributions
    graphics::barplot(cc,
      col = ifelse(cc >= 0, "#1b6ca8", "#d1495b"), las = 2,
      main = sprintf("%s: %s nowcast %.2f", x@model, format_quarter(x@target), x@point), ...
    )
    graphics::abline(h = 0)
  } else {
    lim <- range(c(x@point, x@lower, x@upper), na.rm = TRUE) + c(-0.5, 0.5)
    graphics::plot(1, x@point,
      xlim = c(0.5, 1.5), ylim = lim, pch = 19, xaxt = "n", xlab = "", ylab = "growth (%)",
      main = sprintf("%s: %s", x@model, format_quarter(x@target)), ...
    )
    if (!is.na(x@lower)) graphics::arrows(1, x@lower, 1, x@upper, angle = 90, code = 3, length = 0.1)
  }
  invisible(x)
})

#' @rdname plot-methods
#' @export
setMethod("plot", "Evaluation", function(x, y, position = "M3", ...) {
  d <- x@results[x@results$position == position, ]
  graphics::plot(d$target, d$actual,
    type = "l", lwd = 2, xlab = "", ylab = "growth (%)",
    ylim = range(c(d$actual, d$nowcast, d$lower, d$upper), na.rm = TRUE),
    main = sprintf("%s, position %s", x@model, position), ...
  )
  if (any(!is.na(d$lower))) graphics::segments(d$target, d$lower, d$target, d$upper, col = "grey60")
  graphics::points(d$target, d$nowcast, pch = 19, col = "#1b6ca8")
  graphics::legend("bottomleft", c("actual", "nowcast"),
    lwd = c(2, NA), pch = c(NA, 19),
    col = c("black", "#1b6ca8"), bty = "n"
  )
  invisible(x)
})
