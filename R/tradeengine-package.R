#' tradeengine: Backtesting Engine for Quantitative Trading
#'
#' @description
#' The tradeengine package provides a powerful and flexible backtesting framework 
#' for quantitative trading strategies in R. It is part of the TradingVerse 
#' ecosystem, which aims to provide a cohesive set of tools for the entire 
#' trading workflow.
#'
#' @section Core Features:
#' \itemize{
#'   \item Vectorized and event-driven backtesting
#'   \item Multi-asset support (equities, crypto, futures, FX)
#'   \item Realistic transaction cost modeling
#'   \item Flexible position sizing methods
#'   \item Portfolio management and rebalancing
#'   \item Walk-forward analysis capabilities
#'   \item Pipe-friendly tidyverse-style API
#' }
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{market_tbl}}: Create standardized market data
#'   \item \code{\link{add_strategy}}: Define trading strategy rules
#'   \item \code{\link{backtest}}: Run strategy backtest
#'   \item \code{\link{create_portfolio}}: Create multi-asset portfolio
#'   \item \code{\link{calc_position_size}}: Calculate position sizes
#' }
#'
#' @section TradingVerse Ecosystem:
#' tradeengine is designed to work seamlessly with other TradingVerse packages:
#' \itemize{
#'   \item \strong{tradeio}: Data acquisition and normalization
#'   \item \strong{tradefeatures}: Technical indicators and feature engineering
#'   \item \strong{tradeviz}: Trading visualization
#'   \item \strong{trademetrics}: Performance and risk analytics
#' }
#'
#' @docType package
#' @name tradeengine-package
#' @aliases tradeengine
#'
#' @examples
#' \dontrun{
#' library(tradeengine)
#' library(dplyr)
#' 
#' # Generate synthetic data
#' prices <- generate_synthetic_data(n_days = 252, start_price = 100)
#' 
#' # Add indicators
#' prices <- prices |>
#'   mutate(
#'     sma_20 = sma(close, 20),
#'     sma_50 = sma(close, 50)
#'   )
#' 
#' # Define strategy
#' strategy <- prices |>
#'   add_strategy(
#'     name = "SMA_Crossover",
#'     entry_rules = sma_20 > sma_50,
#'     exit_rules = sma_20 < sma_50,
#'     position_size = 1000
#'   )
#' 
#' # Run backtest
#' results <- backtest(strategy, initial_capital = 10000)
#' 
#' # View results
#' summary(results)
#' plot(results)
#' }
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
