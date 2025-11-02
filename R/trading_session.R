#' Create Trading Session Object
#'
#' @description
#' Creates a unified trading session object that consolidates data, features,
#' strategy, backtest results, and visualizations. This provides a clean,
#' verbose workflow where you can see exactly what you have and easily
#' print/plot/dashboard everything.
#'
#' @param data Market data (market_tbl object)
#' @param name Session name (default: auto-generated)
#' @param description Session description (optional)
#' @importFrom stats sd
#'
#' @return A trading_session object
#' @export
#'
#' @examples
#' \dontrun{
#' library(tradeengine)
#' library(tradeio)
#' library(tradefeatures)
#' 
#' # Create session
#' session <- trading_session(
#'   data = fetch_yahoo("AAPL"),
#'   name = "AAPL SMA Crossover",
#'   description = "Testing 20/50 SMA crossover with RSI filter"
#' )
#' 
#' # Add features
#' session <- session %>%
#'   add_features(
#'     sma_20 = add_sma(20),
#'     sma_50 = add_sma(50),
#'     rsi = add_rsi(14)
#'   )
#' 
#' # Add strategy
#' session <- session %>%
#'   add_strategy(
#'     entry = sma_20 > sma_50 & rsi < 70,
#'     exit = sma_20 < sma_50 | rsi > 80
#'   )
#' 
#' # Run backtest
#' session <- session %>%
#'   run_backtest(initial_capital = 100000)
#' 
#' # See what you have
#' print(session)
#' summary(session)
#' 
#' # Visualize
#' plot(session)  # Equity curve
#' plot(session, type = "drawdown")
#' plot(session, type = "returns")
#' 
#' # Launch dashboard
#' dashboard(session)
#' }
trading_session <- function(data, name = NULL, description = NULL) {
  
  # Validate input
  if (!inherits(data, "market_tbl")) {
    stop("data must be a market_tbl object. Use fetch_yahoo() or similar.")
  }
  
  # Auto-generate name if not provided
  if (is.null(name)) {
    symbols <- unique(data$symbol)
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    name <- paste0("Session_", paste(symbols, collapse = "_"), "_", timestamp)
  }
  
  # Create session object
  session <- list(
    name = name,
    description = description,
    created = Sys.time(),
    data = data,
    features = list(),
    feature_names = character(0),
    strategy = NULL,
    backtest = NULL,
    stats = NULL,
    metadata = list(
      symbols = unique(data$symbol),
      date_range = range(data$datetime),
      n_observations = nrow(data)
    )
  )
  
  class(session) <- c("trading_session", "list")
  
  return(session)
}


#' Add Features to Trading Session
#'
#' @param session A trading_session object
#' @param ... Named feature specifications
#'
#' @return Updated trading_session object
#' @export
add_features.trading_session <- function(session, ...) {
  
  features <- list(...)
  
  if (length(features) == 0) {
    stop("No features specified. Use add_features(session, sma_20 = add_sma(20), ...)")
  }
  
  # Store feature specifications
  session$features <- c(session$features, features)
  session$feature_names <- names(session$features)
  
  # Apply features to data
  data <- session$data
  
  for (feat_name in names(features)) {
    feat_func <- features[[feat_name]]
    
    # Call the feature function
    if (is.function(feat_func)) {
      data <- feat_func(data)
    } else {
      stop(paste("Feature", feat_name, "must be a function"))
    }
  }
  
  # Preserve market_tbl class
  if (!"market_tbl" %in% class(data)) {
    class(data) <- c("market_tbl", class(data))
  }
  
  session$data <- data
  
  # Update metadata
  session$metadata$n_features <- length(session$feature_names)
  session$metadata$last_modified <- Sys.time()
  
  message("[OK] Added ", length(features), " feature(s): ", 
          paste(names(features), collapse = ", "))
  
  return(session)
}


#' Add Strategy to Trading Session
#'
#' @param session A trading_session object
#' @param entry Entry rules (logical expression)
#' @param exit Exit rules (logical expression)
#' @param name Strategy name (optional)
#' @param ... Additional strategy parameters
#'
#' @return Updated trading_session object
#' @export
add_strategy.trading_session <- function(session, entry, exit, name = NULL, ...) {
  
  if (is.null(name)) {
    name <- paste0("Strategy_", session$name)
  }
  
  # Store strategy specification
  session$strategy <- list(
    name = name,
    entry = deparse(substitute(entry)),
    exit = deparse(substitute(exit)),
    parameters = list(...)
  )
  
  # Apply strategy to data using tradeengine::add_strategy
  entry_expr <- substitute(entry)
  exit_expr <- substitute(exit)
  
  session$data <- tradeengine::add_strategy(
    session$data,
    name = name,
    entry_rules = eval(entry_expr, session$data),
    exit_rules = eval(exit_expr, session$data),
    ...
  )
  
  # Update metadata
  session$metadata$has_strategy <- TRUE
  session$metadata$last_modified <- Sys.time()
  
  message("[OK] Strategy '", name, "' added")
  message("  Entry: ", session$strategy$entry)
  message("  Exit:  ", session$strategy$exit)
  
  return(session)
}


#' Run Backtest on Trading Session
#'
#' @param session A trading_session object
#' @param initial_capital Initial capital (default: 100000)
#' @param ... Additional backtest parameters
#'
#' @return Updated trading_session object with backtest results
#' @export
run_backtest.trading_session <- function(session, initial_capital = 100000, ...) {
  
  if (is.null(session$strategy)) {
    stop("No strategy defined. Use add_strategy() first.")
  }
  
  message("[RUNNING] Backtest with initial capital: $", 
          format(initial_capital, big.mark = ","))
  
  # Run backtest
  results <- tradeengine::backtest(
    session$data,
    initial_capital = initial_capital,
    ...
  )
  
  session$backtest <- results
  
  # Calculate statistics
  session$stats <- calculate_session_stats(results, initial_capital)
  
  # Update metadata
  session$metadata$has_backtest <- TRUE
  session$metadata$last_modified <- Sys.time()
  
  message("[OK] Backtest complete")
  message("  Final Equity: $", format(round(results$final_equity), big.mark = ","))
  message("  Total Return: ", sprintf("%.2f%%", results$total_return * 100))
  message("  Trades:       ", results$n_trades)
  
  return(session)
}


#' Calculate Session Statistics
#' @keywords internal
calculate_session_stats <- function(results, initial_capital) {
  
  equity <- results$equity_curve$equity
  returns <- diff(log(equity))
  
  stats <- list(
    initial_capital = initial_capital,
    final_equity = results$final_equity,
    total_return = results$total_return,
    total_return_pct = results$total_return * 100,
    n_trades = results$n_trades,
    
    # Returns statistics
    avg_return = mean(returns, na.rm = TRUE),
    sd_return = sd(returns, na.rm = TRUE),
    sharpe = ifelse(sd(returns, na.rm = TRUE) > 0,
                    mean(returns, na.rm = TRUE) / sd(returns, na.rm = TRUE) * sqrt(252),
                    0),
    
    # Drawdown
    max_drawdown = min(calculate_drawdown(equity), na.rm = TRUE),
    max_drawdown_pct = min(calculate_drawdown(equity), na.rm = TRUE) * 100
  )
  
  # Trade statistics if trades exist
  if (nrow(results$trades) > 0) {
    trades <- results$trades
    stats$win_rate <- mean(trades$pnl > 0, na.rm = TRUE) * 100
    stats$avg_win <- mean(trades$pnl[trades$pnl > 0], na.rm = TRUE)
    stats$avg_loss <- mean(trades$pnl[trades$pnl < 0], na.rm = TRUE)
    stats$profit_factor <- ifelse(
      sum(trades$pnl[trades$pnl < 0], na.rm = TRUE) != 0,
      -sum(trades$pnl[trades$pnl > 0], na.rm = TRUE) / 
        sum(trades$pnl[trades$pnl < 0], na.rm = TRUE),
      Inf
    )
  }
  
  return(stats)
}


#' Helper: Calculate Drawdown
#' @keywords internal
calculate_drawdown <- function(equity) {
  cummax_equity <- cummax(equity)
  drawdown <- (equity - cummax_equity) / cummax_equity
  return(drawdown)
}


#' Print Method for Trading Session
#'
#' @param x A trading_session object
#' @param ... Additional arguments
#' @export
print.trading_session <- function(x, ...) {
  
  cat("\n")
  cat(str_repeat("=", 70), "\n", sep = "")
  cat("  TRADING SESSION:", x$name, "\n")
  cat(str_repeat("=", 70), "\n", sep = "")
  cat("\n")
  
  # Description
  if (!is.null(x$description)) {
    cat("Description:", x$description, "\n")
  }
  cat("Created:    ", format(x$created, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("\n")
  
  # Data info
  cat("---[ DATA ]", str_repeat("-", 60), "\n", sep = "")
  cat("  Symbols:      ", paste(x$metadata$symbols, collapse = ", "), "\n")
  cat("  Date Range:   ", format(x$metadata$date_range[1], "%Y-%m-%d"), " to ",
      format(x$metadata$date_range[2], "%Y-%m-%d"), "\n")
  cat("  Observations: ", format(x$metadata$n_observations, big.mark = ","), "\n")
  cat("\n")
  
  # Features
  cat("---[ FEATURES ]", str_repeat("-", 57), "\n", sep = "")
  if (length(x$feature_names) > 0) {
    for (i in seq_along(x$feature_names)) {
      cat("  [", i, "] ", x$feature_names[i], "\n", sep = "")
    }
  } else {
    cat("  (none added yet)\n")
  }
  cat("\n")
  
  # Strategy
  cat("---[ STRATEGY ]", str_repeat("-", 57), "\n", sep = "")
  if (!is.null(x$strategy)) {
    cat("  Name:  ", x$strategy$name, "\n")
    cat("  Entry: ", x$strategy$entry, "\n")
    cat("  Exit:  ", x$strategy$exit, "\n")
  } else {
    cat("  (none added yet)\n")
  }
  cat("\n")
  
  # Backtest Results
  cat("---[ BACKTEST ]", str_repeat("-", 57), "\n", sep = "")
  if (!is.null(x$backtest)) {
    cat("  Initial Capital: $", format(x$stats$initial_capital, big.mark = ","), "\n", sep = "")
    cat("  Final Equity:    $", format(round(x$stats$final_equity), big.mark = ","), "\n", sep = "")
    cat("  Total Return:    ", sprintf("%+.2f%%", x$stats$total_return_pct), "\n", sep = "")
    cat("  Sharpe Ratio:    ", sprintf("%.2f", x$stats$sharpe), "\n")
    cat("  Max Drawdown:    ", sprintf("%.2f%%", x$stats$max_drawdown_pct), "\n")
    cat("  Trades:          ", x$stats$n_trades, "\n")
    
    if (!is.null(x$stats$win_rate)) {
      cat("  Win Rate:        ", sprintf("%.1f%%", x$stats$win_rate), "\n")
      cat("  Profit Factor:   ", sprintf("%.2f", x$stats$profit_factor), "\n")
    }
  } else {
    cat("  (not run yet - use run_backtest())\n")
  }
  cat("\n")
  
  cat(str_repeat("=", 70), "\n", sep = "")
  cat("\n")
  cat("Use summary(session) for detailed statistics\n")
  cat("Use plot(session) for visualizations\n")
  cat("Use dashboard(session) to launch interactive dashboard\n")
  cat("\n")
  
  invisible(x)
}


#' Summary Method for Trading Session
#'
#' @param object A trading_session object
#' @param ... Additional arguments
#' @export
summary.trading_session <- function(object, ...) {
  
  print(object)
  
  # Additional detailed statistics
  if (!is.null(object$backtest)) {
    cat("\n")
    cat("=== DETAILED STATISTICS ===\n\n")
    
    cat("Returns:\n")
    cat("  Average Daily Return: ", sprintf("%.4f%%", object$stats$avg_return * 100), "\n")
    cat("  Daily Std Dev:        ", sprintf("%.4f%%", object$stats$sd_return * 100), "\n")
    cat("  Annualized Return:    ", sprintf("%.2f%%", object$stats$avg_return * 252 * 100), "\n")
    cat("  Annualized Volatility:", sprintf("%.2f%%", object$stats$sd_return * sqrt(252) * 100), "\n")
    cat("\n")
    
    if (!is.null(object$stats$win_rate)) {
      cat("Trade Analysis:\n")
      cat("  Total Trades:    ", object$stats$n_trades, "\n")
      cat("  Win Rate:        ", sprintf("%.1f%%", object$stats$win_rate), "\n")
      cat("  Average Win:     $", sprintf("%.2f", object$stats$avg_win), "\n")
      cat("  Average Loss:    $", sprintf("%.2f", object$stats$avg_loss), "\n")
      cat("  Profit Factor:   ", sprintf("%.2f", object$stats$profit_factor), "\n")
      cat("\n")
    }
  }
  
  invisible(object)
}


#' Plot Method for Trading Session
#'
#' @param x A trading_session object
#' @param type Plot type: "equity", "drawdown", "returns", or "all"
#' @param ... Additional arguments passed to plotting functions
#' @export
plot.trading_session <- function(x, type = "equity", ...) {
  
  if (is.null(x$backtest)) {
    stop("No backtest results. Run run_backtest() first.")
  }
  
  if (!requireNamespace("tradeviz", quietly = TRUE)) {
    stop("tradeviz package required. Install with: devtools::install_github('Traderverse/tradeviz')")
  }
  
  type <- match.arg(type, c("equity", "drawdown", "returns", "all"))
  
  if (type == "equity") {
    tradeviz::plot_equity_curve(x$backtest, title = paste("Equity Curve -", x$name), ...)
    
  } else if (type == "drawdown") {
    tradeviz::plot_drawdown(x$backtest, title = paste("Drawdown -", x$name), ...)
    
  } else if (type == "returns") {
    tradeviz::plot_returns_distribution(x$backtest, title = paste("Returns -", x$name), ...)
    
  } else if (type == "all") {
    # Create multi-panel plot
    p1 <- tradeviz::plot_equity_curve(x$backtest, drawdown_panel = FALSE)
    p2 <- tradeviz::plot_drawdown(x$backtest, highlight_max = TRUE)
    p3 <- tradeviz::plot_returns_distribution(x$backtest)
    
    if (requireNamespace("patchwork", quietly = TRUE)) {
      return((p1 / p2 / p3) + patchwork::plot_annotation(title = x$name))
    } else {
      print(p1)
      print(p2)
      print(p3)
    }
  }
}


#' Launch Dashboard for Trading Session
#'
#' @param session A trading_session object
#' @param ... Additional arguments passed to launch_dashboard()
#' @export
dashboard <- function(session, ...) {
  
  if (!inherits(session, "trading_session")) {
    stop("Input must be a trading_session object")
  }
  
  if (!requireNamespace("tradedash", quietly = TRUE)) {
    stop("tradedash package required. Install with: devtools::install_github('Traderverse/tradedash')")
  }
  
  # TODO: Implement session-aware dashboard in tradedash v0.2
  message("[!] Session-aware dashboard coming in tradedash v0.2")
  message("    For now, launching standard dashboard...")
  
  tradedash::launch_dashboard(...)
}


# Helper function for string repeat
str_repeat <- function(str, n) {
  paste(rep(str, n), collapse = "")
}
