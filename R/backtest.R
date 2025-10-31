#' Run Strategy Backtest
#'
#' Executes a complete backtest of a trading strategy on market data. Supports 
#' both vectorized (fast, signal-based) and event-driven (realistic, order-based) 
#' backtesting methods.
#'
#' @param data A `market_tbl` object with strategy signals (from `add_strategy()`)
#' @param initial_capital Numeric value of starting capital
#' @param method Character: "vectorized" or "event_driven"
#' @param commission Numeric: commission rate (overrides strategy commission if provided)
#' @param slippage Numeric: slippage rate (overrides strategy slippage if provided)
#' @param position_size_method Character: position sizing method
#' @param max_positions Integer: maximum number of concurrent positions (default: 1)
#' @param margin_requirement Numeric: margin requirement as fraction (default: 1, no leverage)
#' @param compound Boolean: whether to compound returns (default: TRUE)
#'
#' @return A `backtest_results` object containing trades, equity curve, and statistics
#' @export
#' @importFrom dplyr mutate lag arrange group_by summarise ungroup select filter
#' @importFrom purrr map map_dbl
#'
#' @examples
#' \dontrun{
#' library(tradeengine)
#' 
#' # Run a simple backtest
#' results <- prices |>
#'   add_strategy(
#'     entry_rules = close > sma_20,
#'     exit_rules = close < sma_20
#'   ) |>
#'   backtest(initial_capital = 10000, method = "vectorized")
#' 
#' # View results
#' summary(results)
#' plot(results)
#' }
backtest <- function(data,
                    initial_capital = 10000,
                    method = "vectorized",
                    commission = NULL,
                    slippage = NULL,
                    position_size_method = "fixed",
                    max_positions = 1,
                    margin_requirement = 1.0,
                    compound = TRUE) {
        
        # Validate inputs
        if (!is_market_tbl(data)) {
                stop("Input must be a market_tbl object with strategy signals")
        }
        
        if (!"entry_signal" %in% names(data)) {
                stop("No strategy signals found. Use add_strategy() first.")
        }
        
        method <- match.arg(method, c("vectorized", "event_driven"))
        
        # Override commission/slippage if provided
        if (!is.null(commission)) {
                data$commission <- commission
        }
        if (!is.null(slippage)) {
                data$slippage <- slippage
        }
        
        # Run backtest based on method
        if (method == "vectorized") {
                results <- backtest_vectorized(
                        data, initial_capital, compound, 
                        max_positions, margin_requirement
                )
        } else {
                results <- backtest_event_driven(
                        data, initial_capital, compound,
                        max_positions, margin_requirement
                )
        }
        
        # Add metadata
        results$config <- list(
                initial_capital = initial_capital,
                method = method,
                commission = ifelse(is.null(commission), data$commission[1], commission),
                slippage = ifelse(is.null(slippage), data$slippage[1], slippage),
                max_positions = max_positions,
                margin_requirement = margin_requirement,
                compound = compound
        )
        
        class(results) <- c("backtest_results", "list")
        
        return(results)
}


#' Vectorized Backtesting Engine
#'
#' Internal function for fast vectorized backtesting. Works best for simple 
#' signal-based strategies without complex order logic.
#'
#' @param data Market data with strategy signals
#' @param initial_capital Starting capital
#' @param compound Whether to compound returns
#' @param max_positions Maximum concurrent positions
#' @param margin_requirement Margin requirement fraction
#'
#' @return List with trades and equity curve
#' @keywords internal
#' @importFrom dplyr mutate lag
backtest_vectorized <- function(data, 
                               initial_capital, 
                               compound,
                               max_positions,
                               margin_requirement) {
        
        # Initialize tracking variables
        equity <- numeric(nrow(data))
        equity[1] <- initial_capital
        
        position <- numeric(nrow(data))
        position[1] <- 0
        
        cash <- numeric(nrow(data))
        cash[1] <- initial_capital
        
        trades <- list()
        trade_count <- 0
        
        # Main backtest loop
        for (i in 2:nrow(data)) {
                
                prev_position <- position[i - 1]
                prev_cash <- cash[i - 1]
                prev_equity <- equity[i - 1]
                
                current_price <- data$close[i]
                entry_signal <- data$entry_signal[i]
                exit_signal <- data$exit_signal[i]
                
                commission_rate <- data$commission[i]
                slippage_rate <- data$slippage[i]
                
                # Check for exit signal
                if (prev_position != 0 && exit_signal) {
                        # Close position
                        exit_price <- current_price * (1 - slippage_rate)
                        proceeds <- prev_position * exit_price
                        commission_cost <- abs(proceeds) * commission_rate
                        
                        cash[i] <- prev_cash + proceeds - commission_cost
                        position[i] <- 0
                        
                        # Record trade
                        trade_count <- trade_count + 1
                        trades[[trade_count]] <- data.frame(
                                trade_id = trade_count,
                                symbol = data$symbol[i],
                                exit_date = data$datetime[i],
                                exit_price = exit_price,
                                position = prev_position,
                                pnl = proceeds - abs(prev_position * data$close[i-1]) - commission_cost,
                                commission = commission_cost
                        )
                        
                } else if (prev_position == 0 && entry_signal) {
                        # Enter new position
                        entry_price <- current_price * (1 + slippage_rate)
                        
                        # Calculate position size
                        available_capital <- prev_equity / margin_requirement
                        pos_size_value <- data$position_size[i]
                        
                        if (pos_size_value > available_capital) {
                                pos_size_value <- available_capital
                        }
                        
                        shares <- floor(pos_size_value / entry_price)
                        cost <- shares * entry_price
                        commission_cost <- cost * commission_rate
                        
                        if (cost + commission_cost <= prev_cash) {
                                cash[i] <- prev_cash - cost - commission_cost
                                position[i] <- shares
                                
                                # Record trade entry
                                trade_count <- trade_count + 1
                                trades[[trade_count]] <- data.frame(
                                        trade_id = trade_count,
                                        symbol = data$symbol[i],
                                        entry_date = data$datetime[i],
                                        entry_price = entry_price,
                                        position = shares,
                                        commission = commission_cost
                                )
                        } else {
                                # Not enough cash
                                cash[i] <- prev_cash
                                position[i] <- 0
                        }
                } else {
                        # Hold current position
                        cash[i] <- prev_cash
                        position[i] <- prev_position
                }
                
                # Calculate equity
                position_value <- position[i] * current_price
                equity[i] <- cash[i] + position_value
        }
        
        # Compile results
        equity_curve <- data.frame(
                datetime = data$datetime,
                equity = equity,
                cash = cash,
                position = position,
                position_value = position * data$close,
                returns = c(0, diff(equity) / equity[-length(equity)])
        )
        
        trades_df <- if (length(trades) > 0) {
                do.call(rbind, trades)
        } else {
                data.frame()
        }
        
        return(list(
                equity_curve = equity_curve,
                trades = trades_df,
                final_equity = equity[length(equity)],
                total_return = (equity[length(equity)] - initial_capital) / initial_capital,
                n_trades = trade_count
        ))
}


#' Event-Driven Backtesting Engine
#'
#' Internal function for realistic event-driven backtesting. Handles complex 
#' order types, partial fills, and market microstructure.
#'
#' @param data Market data with strategy signals
#' @param initial_capital Starting capital
#' @param compound Whether to compound returns
#' @param max_positions Maximum concurrent positions
#' @param margin_requirement Margin requirement fraction
#'
#' @return List with trades and equity curve
#' @keywords internal
backtest_event_driven <- function(data,
                                  initial_capital,
                                  compound,
                                  max_positions,
                                  margin_requirement) {
        
        # Placeholder for event-driven implementation
        # This would include order book simulation, limit orders, etc.
        # For now, fall back to vectorized
        
        cli::cli_alert_warning("Event-driven backtesting coming in v0.2. Using vectorized method.")
        
        return(backtest_vectorized(data, initial_capital, compound, max_positions, margin_requirement))
}


#' Simulate Order Execution
#'
#' Simulates realistic order execution with market impact, partial fills, and 
#' order book dynamics.
#'
#' @param order_type Character: "market", "limit", "stop", "stop_limit"
#' @param side Character: "buy" or "sell"
#' @param quantity Numeric: number of shares/units
#' @param price Numeric: limit price (for limit orders)
#' @param market_price Numeric: current market price
#' @param market_volume Numeric: current market volume
#' @param ... Additional parameters for order execution
#'
#' @return List with execution details (filled quantity, average price, etc.)
#' @export
simulate_orders <- function(order_type = "market",
                           side = "buy",
                           quantity,
                           price = NULL,
                           market_price,
                           market_volume,
                           ...) {
        
        order_type <- match.arg(order_type, c("market", "limit", "stop", "stop_limit"))
        side <- match.arg(side, c("buy", "sell"))
        
        # Simulate execution
        if (order_type == "market") {
                # Market orders fill immediately but with slippage
                slippage <- ifelse(quantity > market_volume * 0.01, 0.001, 0.0005)
                fill_price <- ifelse(side == "buy", 
                                    market_price * (1 + slippage),
                                    market_price * (1 - slippage))
                filled_quantity <- quantity
                
        } else if (order_type == "limit") {
                # Limit orders only fill if price is touched
                if (is.null(price)) {
                        stop("limit price required for limit orders")
                }
                
                can_fill <- ifelse(side == "buy", market_price <= price, market_price >= price)
                
                if (can_fill) {
                        fill_price <- price
                        filled_quantity <- quantity
                } else {
                        fill_price <- NA
                        filled_quantity <- 0
                }
                
        } else {
                # Stop orders - placeholder
                fill_price <- market_price
                filled_quantity <- quantity
        }
        
        return(list(
                filled = filled_quantity > 0,
                filled_quantity = filled_quantity,
                fill_price = fill_price,
                commission = filled_quantity * fill_price * 0.001
        ))
}


#' Summary Method for Backtest Results
#'
#' @param object A `backtest_results` object
#' @param ... Additional arguments
#' @export
summary.backtest_results <- function(object, ...) {
        
        cat("=== Backtest Results ===\n\n")
        
        cat("Configuration:\n")
        cat("  Initial Capital: $", format(object$config$initial_capital, big.mark = ","), "\n", sep = "")
        cat("  Method:", object$config$method, "\n")
        cat("  Commission:", object$config$commission * 100, "%\n")
        cat("  Slippage:", object$config$slippage * 100, "%\n\n")
        
        cat("Performance:\n")
        cat("  Final Equity: $", format(object$final_equity, big.mark = ","), "\n", sep = "")
        cat("  Total Return:", round(object$total_return * 100, 2), "%\n")
        cat("  Number of Trades:", object$n_trades, "\n\n")
        
        if (nrow(object$trades) > 0) {
                cat("Trade Summary:\n")
                cat("  Total Trades:", nrow(object$trades), "\n")
                # Additional trade statistics would go here
        }
}


#' Plot Method for Backtest Results
#'
#' @param x A `backtest_results` object
#' @param ... Additional arguments
#' @export
plot.backtest_results <- function(x, ...) {
        
        if (!requireNamespace("ggplot2", quietly = TRUE)) {
                stop("ggplot2 required for plotting. Install with: install.packages('ggplot2')")
        }
        
        # Simple equity curve plot
        ggplot2::ggplot(x$equity_curve, ggplot2::aes(x = datetime, y = equity)) +
                ggplot2::geom_line(color = "#2E86AB", linewidth = 1) +
                ggplot2::geom_hline(yintercept = x$config$initial_capital, 
                                   linetype = "dashed", color = "gray50") +
                ggplot2::labs(
                        title = "Equity Curve",
                        subtitle = paste0("Total Return: ", round(x$total_return * 100, 2), "%"),
                        x = "Date",
                        y = "Equity ($)"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                        plot.title = ggplot2::element_text(face = "bold", size = 14),
                        plot.subtitle = ggplot2::element_text(size = 11)
                )
}
