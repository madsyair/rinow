# Model specifications (strategy objects) and results.

#' Nowcasting model specifications
#'
#' Every model is an S4 object that inherits from the virtual class
#' `NowcastModel`. A model holds only its specification; [nowcast()] applies
#' it to a data vintage and [evaluate()] runs it over a pseudo real-time
#' schedule. Create models with the constructors listed under *See also*.
#'
#' @slot name Label used in tables and plots.
#' @slot indicators Monthly indicators used by the model.
#' @seealso [ar_model()], [bridge_model()], [midas_model()], [factor_model()],
#'   [dfm_model()], [bvar_model()], [bayes_dfm_model()], [ml_model()],
#'   [bottom_up_model()], [combination_model()]
#' @exportClass NowcastModel
setClass("NowcastModel", representation("VIRTUAL", name = "character", indicators = "character"))

#' @rdname NowcastModel-class
#' @slot max_order Largest autoregressive order (AIC selection).
#' @exportClass ARModel
setClass("ARModel", contains = "NowcastModel", representation(max_order = "numeric"))

#' @rdname NowcastModel-class
#' @slot target_lag Logical; include the lagged target.
#' @slot start First quarter of the estimation sample.
#' @slot shift Named whole numbers of months by which indicators are moved
#'   later (lag-shifting of leading indicators); empty when none.
#' @exportClass BridgeModel
setClass("BridgeModel",
  contains = "NowcastModel",
  representation(target_lag = "logical", start = "Date", shift = "numeric"),
  prototype(shift = numeric())
)

#' @rdname NowcastModel-class
#' @slot n_lags Number of monthly lags per indicator.
#' @slot weights `"umidas"` or `"nealmon"`.
#' @exportClass MidasModel
setClass("MidasModel", contains = "NowcastModel", representation(n_lags = "numeric", weights = "character"))

#' @rdname NowcastModel-class
#' @slot n_factors Number of factors.
#' @slot frequency `"quarterly"` (factors of quarterly aggregates) or
#'   `"monthly"` (factors of monthly growth, aggregated with weights 1-2-3-2-1).
#' @exportClass FactorModel
setClass("FactorModel",
  contains = "NowcastModel",
  representation(n_factors = "numeric", frequency = "character", start = "Date")
)

#' @rdname NowcastModel-class
#' @slot n_factor_lags Lags of the factor VAR.
#' @slot idio_ar1 Logical; AR(1) idiosyncratic errors.
#' @exportClass DFMModel
setClass("DFMModel",
  contains = "NowcastModel",
  representation(n_factors = "numeric", n_factor_lags = "numeric", idio_ar1 = "logical", start = "Date")
)

#' @rdname NowcastModel-class
#' @slot conditional Logical; condition on the current-quarter indicators.
#' @slot n_draw,n_burn MCMC draws and burn-in.
#' @exportClass BVARModel
setClass("BVARModel",
  contains = "NowcastModel",
  representation(
    n_lags = "numeric", conditional = "logical", n_draw = "numeric", n_burn = "numeric",
    start = "Date"
  )
)

#' @rdname NowcastModel-class
#' @slot nu Degrees of freedom of the Student-t errors (large values give
#'   Gaussian errors).
#' @slot stochastic_volatility Logical.
#' @slot n_iter Gibbs iterations.
#' @slot reference Indicator whose loading is fixed at one.
#' @exportClass BayesDFMModel
setClass("BayesDFMModel",
  contains = "NowcastModel",
  representation(
    nu = "numeric", stochastic_volatility = "logical", n_iter = "numeric", n_burn = "numeric",
    reference = "character", start = "Date"
  )
)

#' @rdname NowcastModel-class
#' @slot method `"lasso"`, `"enet"`, `"rf"`, `"xgb"` or `"sglasso"`.
#' @slot min_leaf Minimum observations per leaf (`"xgb"`).
#' @exportClass MLModel
setClass("MLModel",
  contains = "NowcastModel",
  representation(method = "character", n_lags = "numeric", min_leaf = "numeric")
)

#' @rdname NowcastModel-class
#' @slot components Named character vector of component signs (`"+"` or `"-"`).
#' @slot map Named list: indicators of the bridge equation of each modelled
#'   component (and of `"rest"`).
#' @slot grouped Components merged into the residual contribution `"rest"`.
#' @slot factor_indicators Indicators of the activity factor.
#' @exportClass BottomUpModel
setClass("BottomUpModel",
  contains = "NowcastModel",
  representation(
    components = "character", map = "list", grouped = "character",
    factor_indicators = "character", start = "Date"
  )
)

#' @rdname NowcastModel-class
#' @slot models List of member models.
#' @slot method `"mean"` or `"median"`.
#' @exportClass CombinationModel
setClass("CombinationModel", contains = "NowcastModel", representation(models = "list", method = "character"))

setValidity("MidasModel", function(object) {
  if (!object@weights %in% c("umidas", "nealmon")) {
    return("`weights` must be \"umidas\" or \"nealmon\".")
  }
  if (object@n_lags < 1) {
    return("`n_lags` must be positive.")
  }
  TRUE
})
setValidity("FactorModel", function(object) {
  if (!object@frequency %in% c("quarterly", "monthly")) {
    return("`frequency` must be \"quarterly\" or \"monthly\".")
  }
  if (object@n_factors > length(object@indicators)) {
    return("More factors than indicators.")
  }
  TRUE
})
setValidity("MLModel", function(object) {
  if (!object@method %in% c("lasso", "enet", "rf", "xgb", "sglasso")) {
    return("Unknown machine-learning method.")
  }
  TRUE
})
setValidity("BayesDFMModel", function(object) {
  if (object@n_burn >= object@n_iter) {
    return("`n_burn` must be smaller than `n_iter`.")
  }
  if (!object@reference %in% object@indicators) {
    return("`reference` must be one of the indicators.")
  }
  if (object@nu <= 2) {
    return("`nu` must exceed 2.")
  }
  TRUE
})
setValidity("BottomUpModel", function(object) {
  if (!all(object@components %in% c("+", "-"))) {
    return("Component signs must be \"+\" or \"-\".")
  }
  if (!all(object@grouped %in% names(object@components))) {
    return("`grouped` must name components.")
  }
  modelled <- c(setdiff(names(object@components), object@grouped), "rest")
  if (!all(names(object@map) %in% modelled)) {
    return("`map` may only name modelled components and \"rest\".")
  }
  TRUE
})
setValidity("CombinationModel", function(object) {
  if (!length(object@models) || !all(vapply(object@models, methods::is, logical(1), "NowcastModel"))) {
    return("`models` must be a non-empty list of NowcastModel objects.")
  }
  if (!object@method %in% c("mean", "median")) {
    return("`method` must be \"mean\" or \"median\".")
  }
  TRUE
})

#' Result of a nowcast
#'
#' @slot model Model name.
#' @slot target Target quarter (first day).
#' @slot as_of Date of the information set.
#' @slot point Point nowcast of the target growth rate (per cent).
#' @slot lower,upper Interval bounds (`NA` when the model has none).
#' @slot level Coverage of the interval.
#' @slot contributions Named numeric contributions to the point nowcast.
#' @slot details List of model-specific output (fitted objects, draws).
#' @exportClass NowcastResult
setClass("NowcastResult", representation(
  model = "character", target = "Date", as_of = "Date", point = "numeric", lower = "numeric",
  upper = "numeric", level = "numeric", contributions = "numeric", details = "list"
), prototype(lower = NA_real_, upper = NA_real_, level = 0.68, contributions = numeric(), details = list()))

setValidity("NowcastResult", function(object) {
  if (length(object@point) != 1L) {
    return("`point` must have length one.")
  }
  if (!is.na(object@lower) && !is.na(object@upper) && object@upper < object@lower) {
    return("`upper` is below `lower`.")
  }
  TRUE
})

#' Pseudo real-time evaluation of a model
#'
#' @slot model Model name.
#' @slot results Data frame with columns `target`, `position`, `as_of`,
#'   `model`, `nowcast`, `lower`, `upper`, `actual` and `error`.
#' @exportClass Evaluation
setClass("Evaluation", representation(model = "character", results = "data.frame"))
