#' rinow: Nowcasting of Indicators using R
#'
#' 'rinow' stands for nowcasting of indicators using R.
#' The package organises a nowcasting system around three kinds of objects:
#' data with release calendars ([NowcastData-class], [vintage()]), model
#' specifications ([NowcastModel-class]) and results ([NowcastResult-class],
#' [Evaluation-class]). [nowcast()] applies any model to any vintage,
#' [evaluate()] repeats it over a pseudo real-time schedule, and [run_app()]
#' opens a step-by-step interface.
#'
#' @import methods
#' @keywords internal
"_PACKAGE"
