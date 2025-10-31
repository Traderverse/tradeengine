# ⚡ TradingVerse Quick Start Guide

Welcome to the **TradingVerse**! This guide will get you up and running with `tradeengine` in under 5 minutes.

## 🚀 Installation

```r
# Install from GitHub (development version)
devtools::install_github("tradingverse/tradeengine")

# Load package
library(tradeengine)
library(dplyr)
```

## 📊 Your First Backtest (2 minutes)

### Step 1: Get Some Data

```r
# Generate synthetic data (or use tradeio to fetch real data)
prices <- generate_synthetic_data(
  n_days = 252,
  start_price = 100,
  volatility = 0.20,
  symbol = "DEMO"
)

# Take a look
head(prices)
```

### Step 2: Add Indicators

```r
# Calculate simple moving averages
strategy_data <- prices |>
  mutate(
    sma_20 = sma(close, 20),  # Fast moving average
    sma_50 = sma(close, 50)   # Slow moving average
  )
```

### Step 3: Define Your Strategy

```r
# Create a simple crossover strategy
strategy <- strategy_data |>
  add_strategy(
    name = "SMA_Crossover",
    entry_rules = sma_20 > sma_50,  # Buy when fast > slow
    exit_rules = sma_20 < sma_50,   # Sell when fast < slow
    position_size = 1000,            # $1000 per trade
    commission = 0.001               # 0.1% commission
  )
```

### Step 4: Run Backtest

```r
# Execute the backtest
results <- backtest(
  strategy,
  initial_capital = 10000,
  method = "vectorized"
)
```

### Step 5: View Results

```r
# Summary statistics
summary(results)

# Plot equity curve
plot(results)

# Check specific metrics
results$total_return          # Total return percentage
results$final_equity          # Ending capital
results$n_trades              # Number of trades executed
```

## 🎯 Common Strategies

### Mean Reversion

```r
strategy <- prices |>
  mutate(
    sma = sma(close, 20),
    distance = (close - sma) / sma
  ) |>
  add_strategy(
    entry_rules = distance < -0.05,  # Buy 5% below mean
    exit_rules = distance > 0,       # Sell at mean
    position_size = 2000
  )
```

### Momentum

```r
strategy <- prices |>
  mutate(
    returns_5d = (close / lag(close, 5) - 1),
    sma_20 = sma(close, 20)
  ) |>
  add_strategy(
    entry_rules = (returns_5d > 0.02) & (close > sma_20),
    exit_rules = returns_5d < -0.01,
    position_size = 1500
  )
```

### Volatility Breakout

```r
strategy <- prices |>
  mutate(
    sma_20 = sma(close, 20),
    roll_sd = sapply(seq_len(n()), function(i) {
      if (i < 20) return(NA)
      sd(close[max(1, i-19):i])
    }),
    upper = sma_20 + 2 * roll_sd,
    lower = sma_20 - 2 * roll_sd
  ) |>
  add_strategy(
    entry_rules = close > upper,  # Breakout above
    exit_rules = close < sma_20,  # Exit at mean
    position_size = 1000
  )
```

## 💰 Position Sizing

```r
# Fixed amount
calc_position_size("fixed", capital = 10000, price = 100, risk_per_trade = 0.02)
# → Risk 2% of capital

# Percentage of portfolio
calc_position_size("percent_equity", capital = 10000, price = 100, risk_per_trade = 0.10)
# → Allocate 10% of portfolio

# Volatility targeting
calc_position_size(
  "volatility_target",
  capital = 10000,
  price = 100,
  volatility = 0.30,
  target_volatility = 0.10
)
# → Target 10% portfolio volatility
```

## 📦 Multi-Asset Portfolios

```r
# Create a portfolio
portfolio <- create_portfolio(
  symbols = c("STOCK_A", "STOCK_B", "BOND_A"),
  weights = c(0.4, 0.4, 0.2),
  rebalance_frequency = "monthly",
  initial_capital = 100000
)
```

## 🔍 Analyzing Results

```r
# Run backtest
results <- backtest(strategy, initial_capital = 10000)

# Access results
results$equity_curve          # Full equity curve data
results$trades                # Individual trade details
results$final_equity          # Ending equity
results$total_return          # Overall return
results$n_trades              # Number of trades

# Equity curve details
head(results$equity_curve)
# → datetime, equity, cash, position, returns

# Trade details
head(results$trades)
# → entry/exit dates, prices, PnL, commission
```

## 🎨 Visualization

```r
# Requires ggplot2
library(ggplot2)

# Basic equity curve
plot(results)

# Custom plots
ggplot(results$equity_curve, aes(x = datetime, y = equity)) +
  geom_line(color = "blue") +
  labs(title = "Strategy Performance", y = "Equity ($)")

# Plot returns distribution
ggplot(results$equity_curve, aes(x = returns)) +
  geom_histogram(bins = 50, fill = "steelblue") +
  labs(title = "Returns Distribution")
```

## 📚 Next Steps

### Learn More
- Read the [full vignette](vignettes/getting-started.Rmd)
- Check out [example strategies](examples/basic_strategies.R)
- Explore the [API documentation](https://tradingverse.github.io/tradeengine)

### Try These Examples
1. **Compare strategies**: Test SMA vs EMA crossovers
2. **Optimize parameters**: Find best moving average periods
3. **Add filters**: Combine multiple indicators
4. **Risk management**: Implement stop losses and position limits

### Join the Community
- **GitHub**: https://github.com/Traderverse/tradeengine
- **Discord**: https://discord.gg/tradingverse
- **Blog**: https://tradingverse.org/blog

### Other TradingVerse Packages

Once comfortable with `tradeengine`, explore:

- **tradeio**: Fetch real market data from multiple sources
- **tradefeatures**: 100+ technical indicators
- **tradeviz**: Beautiful trading visualizations
- **trademetrics**: Advanced performance analytics

## 💡 Pro Tips

1. **Start simple**: Master basic strategies before adding complexity
2. **Transaction costs matter**: Always include commissions and slippage
3. **Test thoroughly**: Use different market conditions
4. **Keep learning**: Markets change, strategies must adapt
5. **Risk management**: Never risk more than you can afford to lose

## ⚠️ Important Notes

- **Educational purpose**: This is for learning and research
- **Past ≠ Future**: Historical performance doesn't guarantee future results
- **Paper trade first**: Test strategies before using real money
- **Start small**: Begin with small position sizes
- **Continuous monitoring**: Markets change, review regularly

## 🐛 Having Issues?

- Check the [FAQ](https://tradingverse.github.io/tradeengine/FAQ.html)
- Search [existing issues](https://github.com/Traderverse/tradeengine/issues)
- Ask on [Discord](https://discord.gg/tradingverse)
- [Report a bug](https://github.com/Traderverse/tradeengine/issues/new)

---

**Happy Trading!** 📈

*Built with ❤️ by the TradingVerse community*
