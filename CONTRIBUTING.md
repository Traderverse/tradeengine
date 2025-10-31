# Contributing to tradeengine

Thank you for your interest in contributing to tradeengine and the TradingVerse ecosystem! 🚀

## Code of Conduct

This project follows a Code of Conduct that we expect all contributors to adhere to. Please be respectful and professional in all interactions.

## How Can I Contribute?

### Reporting Bugs

Before creating bug reports, please check existing issues. When creating a bug report, include:

- **Clear title and description**
- **Steps to reproduce** the problem
- **Expected vs actual behavior**
- **R version** and package versions
- **Minimal reproducible example** (reprex)

Example:
```r
library(tradeengine)

# Minimal code that reproduces the issue
data <- market_tbl(...)
# Error occurs here
```

### Suggesting Enhancements

Enhancement suggestions are tracked as GitHub issues. Include:

- **Clear title and description** of the enhancement
- **Rationale**: Why is this enhancement useful?
- **Examples**: How would it work?
- **Alternatives**: Have you considered other approaches?

### Pull Requests

1. **Fork the repository** and create your branch from `main`
2. **Write code** following our style guide (see below)
3. **Add tests** for new functionality
4. **Update documentation** (roxygen2 comments and vignettes)
5. **Run checks**: `devtools::check()` should pass with no errors
6. **Submit PR** with clear description

## Development Setup

```r
# Install development dependencies
install.packages(c("devtools", "testthat", "roxygen2", "pkgdown"))

# Clone repository
# git clone https://github.com/Traderverse/tradeengine.git

# Load package for development
devtools::load_all()

# Run tests
devtools::test()

# Check package
devtools::check()

# Build documentation
devtools::document()
pkgdown::build_site()
```

## Style Guide

### R Code Style

We follow the [tidyverse style guide](https://style.tidyverse.org/):

```r
# Good
calculate_position_size <- function(capital, risk_percent) {
  position <- capital * risk_percent
  return(position)
}

# Bad
calc_pos_size<-function(x,y){
x*y
}
```

**Key points:**
- Use `snake_case` for function and variable names
- 2 spaces for indentation (no tabs)
- Spaces around operators (`x + y`, not `x+y`)
- Use `<-` for assignment, not `=`
- Maximum line length: 80 characters
- Use explicit returns

### Documentation

All exported functions must have roxygen2 documentation:

```r
#' Calculate Position Size
#'
#' Calculates the position size based on capital and risk parameters.
#'
#' @param capital Numeric: available capital
#' @param risk_percent Numeric: risk as percentage (e.g., 0.02 for 2%)
#' @return Numeric: position size in dollars
#' @export
#' @examples
#' calculate_position_size(10000, 0.02)
calculate_position_size <- function(capital, risk_percent) {
  position <- capital * risk_percent
  return(position)
}
```

### Testing

Every new function should have tests:

```r
test_that("calculate_position_size works correctly", {
  result <- calculate_position_size(10000, 0.02)
  expect_equal(result, 200)
  
  # Edge cases
  expect_equal(calculate_position_size(0, 0.02), 0)
  expect_error(calculate_position_size(-10000, 0.02))
})
```

## Package Architecture

### Core Principles

1. **Standard Data Structure**: Everything uses `market_tbl`
2. **Pipe-Friendly**: Functions work with `|>` and `%>%`
3. **Minimal Dependencies**: Only essential packages
4. **Extensible**: Easy to add new features
5. **Well-Documented**: Clear examples and vignettes

### Adding New Features

When adding features, consider:

1. **Does it fit the package scope?** (backtesting and simulation)
2. **Is it generalizable?** (works across asset classes)
3. **Does it integrate well?** (works with existing functions)
4. **Is it well-tested?** (comprehensive test coverage)

### File Organization

```
tradeengine/
├── R/
│   ├── market_tbl.R          # Data structure
│   ├── strategy.R            # Strategy definition
│   ├── backtest.R            # Backtesting engine
│   ├── utils.R               # Utility functions
│   └── data.R                # Example data
├── tests/
│   └── testthat/
│       ├── test-market_tbl.R
│       ├── test-strategy.R
│       └── test-backtest.R
├── vignettes/
│   └── getting-started.Rmd
└── examples/
    └── basic_strategies.R
```

## Roadmap Priorities

### v0.1 (Current)
- [x] Basic vectorized backtesting
- [x] Standard data structure (market_tbl)
- [x] Simple strategy definition
- [ ] Transaction cost modeling
- [ ] Portfolio support

### v0.2 (Next)
- [ ] Event-driven backtesting
- [ ] Advanced order types (limit, stop)
- [ ] Walk-forward analysis
- [ ] Performance optimization

### v0.3 (Future)
- [ ] Multi-timeframe support
- [ ] Options and derivatives
- [ ] Real-time simulation
- [ ] Integration with tradeexec

## Areas We Need Help With

- **Performance optimization**: Making backtests faster
- **Advanced order types**: Limit, stop, trailing stop, etc.
- **Documentation**: More examples and use cases
- **Testing**: Edge cases and error handling
- **Visualization**: Better result plots
- **Integration**: With other TradingVerse packages

## Questions?

- **GitHub Issues**: For bugs and feature requests
- **Discussions**: For questions and ideas
- **Discord**: Join our community at https://discord.gg/tradingverse
- **Email**: info@tradingverse.org

## Recognition

All contributors will be recognized in:
- Package AUTHORS file
- DESCRIPTION file
- Release notes
- Website contributors page

Thank you for helping make TradingVerse better! 🎉
