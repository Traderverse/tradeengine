# tradeengine <img src="man/figures/logo.png" align="right" height="139" />

> **The Backtesting Engine for Quantitative Trading**  
> Part of the [TradingVerse](https://github.com/Traderverse) ecosystem

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/Traderverse/tradeengine/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Traderverse/tradeengine/actions/workflows/R-CMD-check.yaml)

## Overview

**tradeengine** is a powerful and flexible backtesting engine for quantitative trading strategies in R. It provides:

- 🚀 **Vectorized & Event-Driven Backtesting** - Choose the approach that fits your strategy
- 💼 **Multi-Asset Support** - Equities, crypto, futures, FX, and more
- 💰 **Realistic Cost Modeling** - Commissions, slippage, market impact
- 📊 **Portfolio Management** - Position sizing, rebalancing, risk controls
- 🔄 **Walk-Forward Analysis** - Out-of-sample testing
- 🧩 **Pipe-Friendly** - Works seamlessly with tidyverse workflows

## Installation

```r
# Install from GitHub (development version)
# install.packages("devtools")
devtools::install_github("tradingverse/tradeengine")
```

## Quick Start

```r
library(tradeengine)
library(dplyr)

# Create market data (or use tradeio package to fetch)
prices <- market_tbl(
  symbol = rep("AAPL", 100),
  datetime = seq.POSIXt(as.POSIXct("2024-01-01"), by = "day", length.out = 100),
  open = rnorm(100, 150, 5),
  high = rnorm(100, 152, 5),
  low = rnorm(100, 148, 5),
  close = rnorm(100, 150, 5),
  volume = rnorm(100, 1e6, 1e5)
)

# Define a simple moving average crossover strategy
strategy <- prices |>
  add_strategy(
    name = "SMA_Crossover",
    entry_rules = sma_short > sma_long,
    exit_rules = sma_short < sma_long,
    position_size = 1000,  # $1000 per trade
    commission = 0.001      # 0.1% commission
  )

# Run backtest
results <- backtest(
  data = strategy,
  initial_capital = 10000,
  method = "vectorized"
)

# View summary
summary(results)
```

## Key Features

### 1. Standard Data Structure: `market_tbl`

All tradeengine functions work with a standardized `market_tbl` tibble:

```r
data <- market_tbl(
  symbol = c("AAPL", "AAPL"),
  datetime = as.POSIXct(c("2024-01-01", "2024-01-02")),
  open = c(150, 151),
  high = c(152, 153),
  low = c(149, 150),
  close = c(151, 152),
  volume = c(1e6, 1.1e6)
)
```

### 2. Flexible Strategy Definition

```r
# Simple strategy
strategy <- add_strategy(
  data,
  entry_rules = close > sma(close, 20),
  exit_rules = close < sma(close, 20)
)

# Complex multi-condition strategy
strategy <- add_strategy(
  data,
  entry_rules = (rsi < 30) & (volume > volume_sma),
  exit_rules = (rsi > 70) | (stop_loss_hit),
  position_size_func = function(equity, price) {
    risk_per_trade <- equity * 0.02
    position_size <- risk_per_trade / (price * 0.05)
    return(position_size)
  }
)
```

### 3. Realistic Transaction Costs

```r
results <- backtest(
  data,
  commission = 0.001,           # 0.1% per trade
  slippage = 0.0005,           # 0.05% slippage
  market_impact = function(volume, avg_volume) {
    ifelse(volume > avg_volume * 0.01, 0.001, 0)
  }
)
```

### 4. Portfolio Management

```r
# Create multi-asset portfolio
portfolio <- create_portfolio(
  symbols = c("AAPL", "MSFT", "GOOGL"),
  weights = c(0.4, 0.3, 0.3),
  rebalance_frequency = "monthly"
)

# Run portfolio backtest
results <- backtest(
  portfolio,
  initial_capital = 100000,
  rebalance = TRUE
)
```

## Backtesting Methods

### Vectorized Backtesting
Fast, ideal for simple signal-based strategies:

```r
results <- backtest(data, method = "vectorized")
```

### Event-Driven Backtesting
More realistic, handles complex order logic:

```r
results <- backtest(
  data, 
  method = "event_driven",
  order_types = c("market", "limit", "stop")
)
```

## Integration with TradingVerse

tradeengine is designed to work seamlessly with other TradingVerse packages:

```r
library(tradeio)        # Data acquisition
library(tradefeatures)  # Technical indicators
library(tradeviz)       # Visualization
library(trademetrics)   # Performance analytics

# Complete workflow
results <- fetch_prices("AAPL", from = "2023-01-01") |>  # tradeio
  add_sma(20) |>                                         # tradefeatures
  add_rsi(14) |>                                         # tradefeatures
  add_strategy(entry = rsi < 30, exit = rsi > 70) |>    # tradeengine
  backtest(initial_capital = 10000) |>                   # tradeengine
  calc_sharpe() |>                                       # trademetrics
  plot_equity_curve()                                    # tradeviz
```

## Documentation

- [Getting Started Guide](https://tradingverse.github.io/tradeengine/articles/getting-started.html)
- [Strategy Design](https://tradingverse.github.io/tradeengine/articles/strategy-design.html)
- [Transaction Costs](https://tradingverse.github.io/tradeengine/articles/transaction-costs.html)
- [Portfolio Management](https://tradingverse.github.io/tradeengine/articles/portfolio-management.html)
- [API Reference](https://tradingverse.github.io/tradeengine/reference/index.html)

## Philosophy

tradeengine follows the TradingVerse design principles:

- **Tidy**: Works with tibbles and supports pipe workflows
- **Consistent**: Predictable function names and parameters
- **Flexible**: Vectorized or event-driven, simple or complex
- **Realistic**: Transaction costs and market constraints
- **Professional**: Production-ready performance

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## Related Packages

- [tradeio](https://github.com/Traderverse/tradeio) - Data acquisition
- [tradefeatures](https://github.com/Traderverse/tradefeatures) - Technical indicators
- [tradeviz](https://github.com/Traderverse/tradeviz) - Visualization
- [trademetrics](https://github.com/Traderverse/trademetrics) - Performance analytics

## License

MIT © TradingVerse Core Team

---

**Disclaimer**: This software is for educational and research purposes only. Past performance does not guarantee future results. Use at your own risk.
