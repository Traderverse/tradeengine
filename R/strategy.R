#' Add Trading Strategy to Market Data
#'
#' Adds trading strategy signals to a `market_tbl` object. This function allows 
#' you to define entry and exit rules, position sizing, and other strategy parameters.
#'
#' @param data A `market_tbl` object
#' @param name Character string naming the strategy (default: "Strategy")
#' @param entry_rules Expression or logical vector defining entry conditions
#' @param exit_rules Expression or logical vector defining exit conditions
#' @param position_size Numeric value or function for position sizing
#' @param side Character: "long", "short", or "both" (default: "long")
#' @param stop_loss Numeric value for stop loss percentage (e.g., 0.05 for 5%)
#' @param take_profit Numeric value for take profit percentage
#' @param commission Numeric value for commission rate (e.g., 0.001 for 0.1%)
#' @param slippage Numeric value for slippage rate
#'
#' @return A `market_tbl` object with added strategy columns
#' @export
#' @importFrom dplyr mutate lag
#' @importFrom rlang enquo eval_tidy
#'
#' @examples
#' \dontrun{
#' library(tradeengine)
#' 
#' # Simple moving average crossover
#' strategy <- prices |>
#'   add_strategy(
#'     name = "SMA_Cross",
#'     entry_rules = sma_fast > sma_slow,
#'     exit_rules = sma_fast < sma_slow,
#'     position_size = 1000
#'   )
#' }
add_strategy <- function(data,
                        name = "Strategy",
                        entry_rules,
                        exit_rules,
                        position_size = 1000,
                        side = "long",
                        stop_loss = NULL,
                        take_profit = NULL,
                        commission = 0.001,
                        slippage = 0.0005) {
        
        if (!is_market_tbl(data)) {
                stop("Input must be a market_tbl object")
        }
        
        # Evaluate entry and exit rules
        entry_expr <- rlang::enquo(entry_rules)
        exit_expr <- rlang::enquo(exit_rules)
        
        # Add signal columns
        result <- data |>
                mutate(
                        entry_signal = rlang::eval_tidy(entry_expr, data),
                        exit_signal = rlang::eval_tidy(exit_expr, data),
                        strategy_name = name,
                        position_size = position_size,
                        side = side,
                        commission = commission,
                        slippage = slippage
                )
        
        # Preserve market_tbl class
        if (!"market_tbl" %in% class(result)) {
                class(result) <- c("market_tbl", class(result))
        }
        
        # Add stop loss and take profit if specified
        if (!is.null(stop_loss)) {
                attr(result, "stop_loss") <- stop_loss
        }
        
        if (!is.null(take_profit)) {
                attr(result, "take_profit") <- take_profit
        }
        
        return(result)
}


#' Calculate Position Size
#'
#' Calculates position size based on various methods (fixed, percent of equity, 
#' risk-based, volatility-adjusted, etc.)
#'
#' @param method Character: "fixed", "percent_equity", "risk_parity", "volatility_target"
#' @param capital Numeric value of available capital
#' @param price Numeric value of asset price
#' @param risk_per_trade Numeric: risk per trade as fraction of capital (for risk-based methods)
#' @param volatility Numeric: asset volatility (for volatility-based methods)
#' @param target_volatility Numeric: target portfolio volatility (default: 0.10)
#' @param ... Additional parameters for custom sizing methods
#'
#' @return Numeric value representing position size (number of shares/units)
#' @export
#'
#' @examples
#' # Fixed position size
#' calc_position_size("fixed", capital = 10000, price = 100)
#'
#' # 2% of equity
#' calc_position_size("percent_equity", capital = 10000, price = 100, risk_per_trade = 0.02)
#'
#' # Volatility-targeted
#' calc_position_size("volatility_target", capital = 10000, price = 100, 
#'                    volatility = 0.20, target_volatility = 0.10)
calc_position_size <- function(method = "fixed",
                               capital,
                               price,
                               risk_per_trade = 0.02,
                               volatility = NULL,
                               target_volatility = 0.10,
                               ...) {
        
        method <- match.arg(method, c("fixed", "percent_equity", "risk_parity", "volatility_target"))
        
        position_size <- switch(
                method,
                fixed = {
                        # Fixed dollar amount
                        capital * risk_per_trade / price
                },
                percent_equity = {
                        # Percentage of total equity
                        (capital * risk_per_trade) / price
                },
                risk_parity = {
                        # Risk parity: size inversely proportional to volatility
                        if (is.null(volatility)) {
                                stop("volatility required for risk_parity method")
                        }
                        target_risk <- capital * risk_per_trade
                        target_risk / (price * volatility)
                },
                volatility_target = {
                        # Target a specific portfolio volatility
                        if (is.null(volatility)) {
                                stop("volatility required for volatility_target method")
                        }
                        (capital * target_volatility) / (price * volatility)
                }
        )
        
        # Return integer number of shares (floor to avoid over-leveraging)
        return(floor(position_size))
}


#' Create Trading Portfolio
#'
#' Creates a portfolio object for multi-asset backtesting with allocation and 
#' rebalancing rules.
#'
#' @param symbols Character vector of asset symbols
#' @param weights Numeric vector of portfolio weights (must sum to 1)
#' @param rebalance_frequency Character: "daily", "weekly", "monthly", "quarterly", "never"
#' @param rebalance_method Character: "fixed_weights", "equal_weight", "risk_parity", "minimum_variance"
#' @param initial_capital Numeric value of initial capital
#'
#' @return A portfolio object (list with class "trade_portfolio")
#' @export
#'
#' @examples
#' # Create a simple 60/40 portfolio
#' portfolio <- create_portfolio(
#'   symbols = c("SPY", "TLT"),
#'   weights = c(0.6, 0.4),
#'   rebalance_frequency = "monthly",
#'   initial_capital = 100000
#' )
create_portfolio <- function(symbols,
                            weights = NULL,
                            rebalance_frequency = "monthly",
                            rebalance_method = "fixed_weights",
                            initial_capital = 10000) {
        
        # Validate inputs
        n_assets <- length(symbols)
        
        if (is.null(weights)) {
                # Equal weight by default
                weights <- rep(1 / n_assets, n_assets)
        }
        
        if (length(weights) != n_assets) {
                stop("Length of weights must equal length of symbols")
        }
        
        if (abs(sum(weights) - 1) > 1e-6) {
                warning("Weights do not sum to 1, normalizing")
                weights <- weights / sum(weights)
        }
        
        # Create portfolio object
        portfolio <- list(
                symbols = symbols,
                weights = weights,
                rebalance_frequency = rebalance_frequency,
                rebalance_method = rebalance_method,
                initial_capital = initial_capital,
                n_assets = n_assets
        )
        
        class(portfolio) <- c("trade_portfolio", "list")
        
        return(portfolio)
}


#' Rebalance Portfolio
#'
#' Rebalances a portfolio to target weights, generating rebalancing trades.
#'
#' @param portfolio A portfolio object
#' @param current_values Numeric vector of current asset values
#' @param current_prices Numeric vector of current asset prices
#' @param method Character: rebalancing method to use
#'
#' @return A data frame with rebalancing trades
#' @export
#'
#' @examples
#' \dontrun{
#' portfolio <- create_portfolio(c("AAPL", "MSFT"), c(0.5, 0.5))
#' trades <- rebalance_portfolio(portfolio, c(6000, 4000), c(150, 300))
#' }
rebalance_portfolio <- function(portfolio,
                               current_values,
                               current_prices,
                               method = NULL) {
        
        if (!inherits(portfolio, "trade_portfolio")) {
                stop("Input must be a trade_portfolio object")
        }
        
        if (is.null(method)) {
                method <- portfolio$rebalance_method
        }
        
        total_value <- sum(current_values)
        target_values <- total_value * portfolio$weights
        
        # Calculate required trades
        trade_values <- target_values - current_values
        trade_shares <- trade_values / current_prices
        
        # Create trade data frame
        trades <- data.frame(
                symbol = portfolio$symbols,
                current_shares = current_values / current_prices,
                target_shares = target_values / current_prices,
                trade_shares = trade_shares,
                trade_value = trade_values,
                action = ifelse(trade_shares > 0, "BUY", "SELL")
        )
        
        return(trades)
}
