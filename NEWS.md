# rinow (development version)

* New `add_indicator()` adds a monthly series to `NowcastData`, and the new
  generic `as_indicator()` turns monthly `ts` objects, data frames and
  composite indexes from the 'rcli' package (`CompositeIndex`, `CliResult`,
  `CciResult`) into indicator tables. 'rcli' objects are recognised by their
  slots, so 'rinow' gains no dependency. Composites get the `"diff"`
  transform, the group `"composite"` and a release lag taken from the
  largest component release lag; a message warns that pseudo real-time
  evaluations with a composite computed on final data are optimistic.

# rinow 0.1.0

* First release: data with release calendars and pseudo real-time vintages;
  AR, bridge, MIDAS (U-MIDAS and Almon), diffusion-index, mixed-frequency DFM
  (with news decomposition), conditional BVAR, Bayesian DFM with Student-t
  errors and stochastic volatility, machine-learning (LASSO, elastic net,
  random forest, XGBoost, sparse-group LASSO MIDAS), bottom-up (GDPNow-type)
  and combination models; pseudo real-time evaluation, accuracy,
  Diebold-Mariano tests and forecast combinations; a bilingual 'shiny'
  interface; Indonesian example data.
