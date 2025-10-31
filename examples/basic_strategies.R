# TradingVerse: tradeengine Examples
# 
# This script demonstrates the core functionality of the tradeengine package
# with practical trading strategy examples.

library(tradeengine)
library(dplyr)

# =============================================================================
# Example 1: Simple Moving Average Crossover
# =============================================================================

cat("Example 1: SMA Crossover Strategy\n")
cat("==================================\n\n")

# Generate synthetic market data
prices <- generate_synthetic_data(
  n_days = 252,
  start_price = 100,
  volatility = 0.20,
  drift = 0.08,
  symbol = "DEMO"
)

# Add technical indicators
strategy_data <- prices |>
  mutate(
    sma_20 = sma(close, 20),
    sma_50 = sma(close, 50)
  )

# Define strategy
strategy <- strategy_data |>
  add_strategy(
    name = "SMA_Crossover",
    entry_rules = sma_20 > sma_50,
    exit_rules = sma_20 < sma_50,
    position_size = 1000,
    commission = 0.001,
    slippage = 0.0005
  )

# Run backtest
results <- backtest(
  strategy,
  initial_capital = 10000,
  method = "vectorized"
)

# Display results
summary(results)

cat("\nFinal Equity: $", format(results$final_equity, big.mark = ","), "\n")
cat("Total Return: ", round(results$total_return * 100, 2), "%\n")
cat("Number of Trades: ", results$n_trades, "\n\n")


# =============================================================================
# Example 2: Mean Reversion Strategy
# =============================================================================

cat("\nExample 2: Mean Reversion Strategy\n")
cat("===================================\n\n")

# Calculate indicators for mean reversion
mean_reversion <- prices |>
  mutate(
    sma_20 = sma(close, 20),
    distance = (close - sma_20) / sma_20,  # % distance from mean
    ema_10 = ema(close, 10)
  )

# Strategy: Buy when price is 5% below SMA, sell when back to mean
mean_rev_strategy <- mean_reversion |>
  add_strategy(
    name = "Mean_Reversion",
    entry_rules = distance < -0.05,  # 5% below mean
    exit_rules = distance > -0.01,   # Back near mean
    position_size = 2000,
    commission = 0.001
  )

results_mr <- backtest(mean_rev_strategy, initial_capital = 10000)

cat("Final Equity: $", format(results_mr$final_equity, big.mark = ","), "\n")
cat("Total Return: ", round(results_mr$total_return * 100, 2), "%\n")
cat("Number of Trades: ", results_mr$n_trades, "\n\n")


# =============================================================================
# Example 3: Volatility Breakout Strategy
# =============================================================================

cat("\nExample 3: Volatility Breakout\n")
cat("===============================\n\n")

# Calculate volatility-based indicators
breakout <- prices |>
  mutate(
    sma_20 = sma(close, 20),
    # Simple rolling standard deviation
    roll_sd = sapply(1:n(), function(i) {
      if (i < 20) return(NA)
      sd(close[max(1, i-19):i])
    }),
    upper_band = sma_20 + 2 * roll_sd,
    lower_band = sma_20 - 2 * roll_sd
  )

# Strategy: Buy breakout above upper band
breakout_strategy <- breakout |>
  add_strategy(
    name = "Volatility_Breakout",
    entry_rules = close > upper_band,
    exit_rules = close < sma_20,
    position_size = 1500
  )

results_breakout <- backtest(breakout_strategy, initial_capital = 10000)

cat("Final Equity: $", format(results_breakout$final_equity, big.mark = ","), "\n")
cat("Total Return: ", round(results_breakout$total_return * 100, 2), "%\n")
cat("Number of Trades: ", results_breakout$n_trades, "\n\n")


# =============================================================================
# Example 4: Position Sizing Comparison
# =============================================================================

cat("\nExample 4: Position Sizing Methods\n")
cat("===================================\n\n")

capital <- 10000
price <- 100

# Fixed dollar amount
fixed_size <- calc_position_size(
  "fixed", 
  capital = capital, 
  price = price, 
  risk_per_trade = 0.02
)

# Percentage of equity
percent_size <- calc_position_size(
  "percent_equity",
  capital = capital,
  price = price,
  risk_per_trade = 0.10
)

# Volatility targeted
vol_size <- calc_position_size(
  "volatility_target",
  capital = capital,
  price = price,
  volatility = 0.30,
  target_volatility = 0.10
)

cat("Position Sizing for $10,000 capital, $100 price:\n")
cat("  Fixed (2% risk):     ", fixed_size, " shares\n")
cat("  Percent (10%):       ", percent_size, " shares\n")
cat("  Vol Target (10%):    ", vol_size, " shares\n\n")


# =============================================================================
# Example 5: Portfolio Creation
# =============================================================================

cat("\nExample 5: Multi-Asset Portfolio\n")
cat("==================================\n\n")

# Create a diversified portfolio
portfolio <- create_portfolio(
  symbols = c("STOCK_A", "STOCK_B", "STOCK_C"),
  weights = c(0.5, 0.3, 0.2),
  rebalance_frequency = "monthly",
  initial_capital = 100000
)

cat("Portfolio Configuration:\n")
cat("  Symbols: ", paste(portfolio$symbols, collapse = ", "), "\n")
cat("  Weights: ", paste(round(portfolio$weights, 2), collapse = ", "), "\n")
cat("  Initial Capital: $", format(portfolio$initial_capital, big.mark = ","), "\n")
cat("  Rebalance: ", portfolio$rebalance_frequency, "\n\n")


# =============================================================================
# Example 6: Strategy Comparison
# =============================================================================

cat("\nExample 6: Comparing Multiple Strategies\n")
cat("=========================================\n\n")

# Create synthetic data for fair comparison
test_data <- generate_synthetic_data(
  n_days = 252,
  start_price = 100,
  volatility = 0.25,
  drift = 0.10
)

# Prepare data with all indicators
test_data <- test_data |>
  mutate(
    sma_20 = sma(close, 20),
    sma_50 = sma(close, 50),
    ema_10 = ema(close, 10),
    ema_30 = ema(close, 30)
  )

# Strategy 1: SMA Crossover
s1 <- test_data |>
  add_strategy(
    name = "SMA",
    entry_rules = sma_20 > sma_50,
    exit_rules = sma_20 < sma_50,
    position_size = 1000
  ) |>
  backtest(initial_capital = 10000)

# Strategy 2: EMA Crossover
s2 <- test_data |>
  add_strategy(
    name = "EMA",
    entry_rules = ema_10 > ema_30,
    exit_rules = ema_10 < ema_30,
    position_size = 1000
  ) |>
  backtest(initial_capital = 10000)

# Strategy 3: Buy and Hold
s3 <- test_data |>
  add_strategy(
    name = "Buy_Hold",
    entry_rules = row_number() == 1,  # Buy on first day
    exit_rules = FALSE,               # Never sell
    position_size = 10000
  ) |>
  backtest(initial_capital = 10000)

# Compare results
cat("Strategy Comparison:\n")
cat("--------------------\n")
cat(sprintf("%-15s %12s %10s %8s\n", "Strategy", "Final Equity", "Return %", "Trades"))
cat(sprintf("%-15s $%11s %9.2f%% %8d\n", 
            "SMA Crossover", 
            format(s1$final_equity, big.mark = ","),
            s1$total_return * 100,
            s1$n_trades))
cat(sprintf("%-15s $%11s %9.2f%% %8d\n",
            "EMA Crossover",
            format(s2$final_equity, big.mark = ","),
            s2$total_return * 100,
            s2$n_trades))
cat(sprintf("%-15s $%11s %9.2f%% %8d\n",
            "Buy & Hold",
            format(s3$final_equity, big.mark = ","),
            s3$total_return * 100,
            s3$n_trades))
cat("\n")


# =============================================================================
# Summary
# =============================================================================

cat("\n" , rep("=", 70), "\n")
cat("TradingVerse tradeengine - Examples Complete!\n")
cat(rep("=", 70), "\n\n")

cat("Next Steps:\n")
cat("  1. Explore the vignettes: vignette('getting-started', package='tradeengine')\n")
cat("  2. Check out tradefeatures for 100+ technical indicators\n")
cat("  3. Use tradeviz for beautiful trading visualizations\n")
cat("  4. Analyze with trademetrics for risk/return analytics\n\n")

cat("Documentation: https://tradingverse.github.io/tradeengine\n")
cat("Community: https://discord.gg/tradingverse\n\n")
