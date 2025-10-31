# News and Release Notes

## tradeengine 0.1.0 (Development)

*Initial release - October 31, 2025*

### Features

#### Core Data Structure
- **market_tbl**: Standardized tibble for financial time series data
  - Support for OHLCV data across all asset classes
  - Timezone-aware datetime handling
  - Automatic validation and consistency checks
  - Metadata attributes (frequency, timezone, asset_class)

#### Strategy Definition
- **add_strategy()**: Flexible strategy rule definition
  - Expression-based entry and exit rules
  - Multiple position sizing methods
  - Commission and slippage modeling
  - Long, short, and long/short strategies

#### Backtesting Engine
- **backtest()**: Vectorized backtesting (v0.1)
  - Fast signal-based backtesting
  - Realistic transaction cost simulation
  - Multiple position management
  - Compound or simple returns
  - Complete trade history and equity curve

#### Position Sizing
- **calc_position_size()**: Multiple sizing methods
  - Fixed dollar amount
  - Percentage of equity
  - Risk parity
  - Volatility targeting

#### Portfolio Management
- **create_portfolio()**: Multi-asset portfolios
  - Custom weight allocation
  - Rebalancing frequencies
  - Multiple rebalancing methods

#### Utility Functions
- **sma()**: Simple moving average
- **ema()**: Exponential moving average
- **calc_returns_simple()**: Return calculations
- **generate_synthetic_data()**: Synthetic data generation for testing

### Documentation

- Comprehensive README with quick start guide
- Getting Started vignette with detailed examples
- Full roxygen2 documentation for all exported functions
- Example scripts demonstrating common strategies
- Contributing guidelines

### Testing

- Unit tests for all core functionality
- Edge case handling
- Data validation tests
- Strategy execution tests

### Known Limitations

- Event-driven backtesting not yet implemented (coming in v0.2)
- Limited to simple order types (market orders only in v0.1)
- No options or derivatives support yet
- Single-threaded execution

### Future Plans

#### v0.2 (Planned)
- Event-driven backtesting engine
- Advanced order types (limit, stop, trailing stop)
- Walk-forward analysis
- Performance optimization (C++ backend)
- Better integration with other TradingVerse packages

#### v0.3 (Future)
- Multi-timeframe analysis
- Options and derivatives support
- Real-time data streaming
- Live trading simulation
- Advanced portfolio analytics

### Dependencies

**Imports:**
- tibble >= 3.1.0
- dplyr >= 1.0.0
- tidyr >= 1.2.0
- purrr >= 1.0.0
- lubridate >= 1.9.0
- rlang >= 1.0.0
- cli >= 3.4.0
- glue >= 1.6.0

**Suggests:**
- testthat >= 3.0.0
- knitr
- rmarkdown
- ggplot2

### Breaking Changes

None (initial release)

### Contributors

- TradingVerse Core Team

---

## Development Roadmap

### Short Term (3 months)
- [ ] Complete transaction cost modeling
- [ ] Implement event-driven engine
- [ ] Add more example strategies
- [ ] Performance benchmarking
- [ ] Integration tests with tradeio

### Medium Term (6 months)
- [ ] Options backtesting support
- [ ] Multi-currency support
- [ ] Advanced portfolio analytics
- [ ] Parallel processing support
- [ ] Walk-forward optimization

### Long Term (12 months)
- [ ] Real-time trading integration (tradeexec)
- [ ] Machine learning strategy support
- [ ] Advanced risk management
- [ ] Institutional-grade features
- [ ] Cloud deployment support

---

For the latest updates, visit: https://github.com/Traderverse/tradeengine
