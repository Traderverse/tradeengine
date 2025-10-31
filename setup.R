#!/usr/bin/env Rscript

# TradingVerse: tradeengine Package Setup Script
# This script helps you verify and test your new package

cat("
╔═══════════════════════════════════════════════════════════════╗
║                                                               ║
║            🚀 TradingVerse: tradeengine Setup 🚀             ║
║                                                               ║
║                  The Backtesting Engine                       ║
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝

")

# Check if we're in the right directory
if (!file.exists("DESCRIPTION")) {
  stop("❌ Please run this script from the tradeengine package directory")
}

cat("\n📋 Checking Dependencies...\n")

# List of required packages
required_packages <- c(
  "devtools", "roxygen2", "testthat", "knitr", "rmarkdown",
  "tibble", "dplyr", "tidyr", "purrr", "lubridate", "rlang", "cli", "glue"
)

# Check which packages are missing
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("\n📦 Installing missing packages...\n")
  cat("   Packages:", paste(missing_packages, collapse = ", "), "\n")
  
  install.packages(missing_packages, repos = "https://cran.r-project.org")
  cat("✅ Packages installed!\n")
} else {
  cat("✅ All required packages are installed!\n")
}

# Load devtools
library(devtools)

cat("\n🔨 Building Package Documentation...\n")
try({
  document()
  cat("✅ Documentation generated!\n")
}, silent = FALSE)

cat("\n🧪 Running Tests...\n")
try({
  test_results <- test()
  cat("✅ Tests completed!\n")
}, silent = FALSE)

cat("\n📦 Checking Package...\n")
cat("   (This may take a minute...)\n")
try({
  check_results <- check(quiet = TRUE)
  cat("✅ Package check completed!\n")
}, silent = FALSE)

cat("\n🔄 Loading Package...\n")
try({
  load_all()
  cat("✅ Package loaded!\n")
}, silent = FALSE)

cat("\n
╔═══════════════════════════════════════════════════════════════╗
║                    🎉 Setup Complete! 🎉                     ║
╚═══════════════════════════════════════════════════════════════╝

📚 Next Steps:

1. Try the Quick Start:
   > source('examples/basic_strategies.R')

2. Load the package:
   > library(tradeengine)

3. Run a simple example:
   > data <- generate_synthetic_data(n_days = 100)
   > data |> 
       mutate(sma = sma(close, 20)) |>
       add_strategy(
         entry_rules = close > sma,
         exit_rules = close < sma
       ) |>
       backtest(initial_capital = 10000)

4. Read the vignettes:
   > vignette('getting-started', package = 'tradeengine')

5. Check the documentation:
   - README.md
   - QUICKSTART.md
   - IMPLEMENTATION_SUMMARY.md

6. View function help:
   > ?market_tbl
   > ?add_strategy
   > ?backtest

📞 Need Help?
   - GitHub: github.com/tradingverse/tradeengine
   - Docs: QUICKSTART.md
   - Examples: examples/basic_strategies.R

⚠️  Important Reminders:
   - This is for educational purposes only
   - Past performance ≠ future results
   - Always test thoroughly before live trading
   - Start with small position sizes

🚀 Happy Trading!

")

cat("💡 Pro Tip: Run 'devtools::load_all()' to reload the package after making changes\n\n")
