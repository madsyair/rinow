# rinow: Nowcasting of Indicators using R

<!-- badges: start -->
[![R-CMD-check](https://github.com/madsyair/rinow/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/madsyair/rinow/actions/workflows/R-CMD-check.yaml)
[![License: GPL (>= 3)](https://img.shields.io/badge/license-GPL%20(%3E%3D%203)-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

`rinow` nowcasts a quarterly indicator, such as economic growth, from monthly
indicators. One object-oriented (S4) interface covers every stage of a
nowcasting system: indicator metadata and release calendars, pseudo real-time
data vintages with ragged edges, a common set of models, pseudo real-time
evaluation, forecast combination and reporting. A step-by-step 'shiny'
interface runs the same workflow without code.

## Installation

```r
# development version from GitHub
# install.packages("remotes")
remotes::install_github("madsyair/rinow")
```

The model families rely on packages listed in `Suggests` (`dfms`, `BVAR`,
`midasr`, `glmnet`, `ranger`, `xgboost`, `midasml`, `stochvol`, `Matrix`,
`shiny`). `rinow` installs without them and tells you which one to install
when a model needs it. To install everything at once:

```r
remotes::install_github("madsyair/rinow", dependencies = TRUE)
```

## Example

```r
library(rinow)

# data as they were available on 23 October 2026
v <- vintage(idn_nowcast, "2026-10-23")
ragged_edge(v)

# nowcast of 2026Q3 growth with a bridge equation and a dynamic factor model
nowcast(bridge_model(c("impor", "ikk", "ip_as")), v, "2026Q3")
nowcast(dfm_model(c("impor", "ekspor", "ikk", "saham", "ip_as")), v, "2026Q3")

# pseudo real-time evaluation against an AR benchmark
s  <- nowcast_schedule("2014Q1", "2026Q2")
ev <- list(evaluate(ar_model(), idn_nowcast, s),
           evaluate(bridge_model(c("impor", "ikk", "ip_as")), idn_nowcast, s))
accuracy(ev, exclude = c("2020Q2", "2020Q3", "2020Q4", "2021Q1"))
dm_test(ev[[2]], ev[[1]])

# step-by-step interface (Indonesian); run_app("en") for English
run_app()
```

## Models

| Family | Constructor | Reference |
|---|---|---|
| Autoregressive benchmark | `ar_model()` | |
| Bridge equations | `bridge_model()` | Baffigi, Golinelli and Parigi (2004) |
| MIDAS (U-MIDAS, exponential Almon) | `midas_model()` | Ghysels, Sinko and Valkanov (2007); Foroni, Marcellino and Schumacher (2015) |
| Diffusion indexes | `factor_model()` | Stock and Watson (2002) |
| Mixed-frequency DFM with news | `dfm_model()`, `dfm_news()` | Banbura and Modugno (2014) |
| Conditional BVAR | `bvar_model()` | Giannone, Lenza and Primiceri (2015) |
| Bayesian DFM with t errors and SV | `bayes_dfm_model()` | Antolin-Diaz, Drechsel and Petrella (2024) |
| Machine learning (LASSO, elastic net, random forest, XGBoost, sg-LASSO MIDAS) | `ml_model()` | Babii, Ghysels and Striaukas (2022) |
| Bottom-up (GDPNow-type) | `bottom_up_model()` | Higgins (2014) |
| Combinations | `combination_model()`, `combine_nowcasts()` | Timmermann (2006) |

## Extending

Every model is an S4 class that extends `NowcastModel`. A new model needs a
class and one `nowcast()` method; `evaluate()`, `accuracy()`,
`combine_nowcasts()` and the interface then work with it unchanged.

## Guide

The Indonesian guide *Pedoman Nowcasting Pertumbuhan Ekonomi dengan R*
explains every method step by step with the same data; its last chapter shows
`rinow` and its interface.

## Citation

```r
citation("rinow")
```
