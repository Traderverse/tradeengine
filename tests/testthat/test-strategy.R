# Test strategy definition

test_that("add_strategy adds signal columns", {
        
        data <- generate_synthetic_data(n_days = 50, start_price = 100)
        
        data <- data |>
                dplyr::mutate(sma_20 = sma(close, 20))
        
        strategy <- add_strategy(
                data,
                name = "Test_Strategy",
                entry_rules = close > sma_20,
                exit_rules = close < sma_20,
                position_size = 1000
        )
        
        expect_true("entry_signal" %in% names(strategy))
        expect_true("exit_signal" %in% names(strategy))
        expect_true("strategy_name" %in% names(strategy))
        expect_equal(strategy$strategy_name[1], "Test_Strategy")
})


test_that("calc_position_size works with different methods", {
        
        # Fixed method
        size <- calc_position_size("fixed", capital = 10000, price = 100, risk_per_trade = 0.02)
        expect_equal(size, 2)
        
        # Percent equity
        size <- calc_position_size("percent_equity", capital = 10000, price = 50, risk_per_trade = 0.10)
        expect_equal(size, 20)
        
        # Volatility target
        size <- calc_position_size(
                "volatility_target",
                capital = 10000,
                price = 100,
                volatility = 0.30,
                target_volatility = 0.10
        )
        expect_true(size > 0)
})


test_that("create_portfolio validates inputs", {
        
        portfolio <- create_portfolio(
                symbols = c("AAPL", "MSFT"),
                weights = c(0.6, 0.4),
                initial_capital = 10000
        )
        
        expect_s3_class(portfolio, "trade_portfolio")
        expect_equal(portfolio$n_assets, 2)
        expect_equal(sum(portfolio$weights), 1)
})


test_that("create_portfolio normalizes weights", {
        
        expect_warning(
                portfolio <- create_portfolio(
                        symbols = c("A", "B", "C"),
                        weights = c(0.5, 0.3, 0.3),  # Sums to 1.1
                        initial_capital = 10000
                ),
                "do not sum to 1"
        )
})


test_that("create_portfolio uses equal weights by default", {
        
        portfolio <- create_portfolio(
                symbols = c("A", "B", "C", "D"),
                initial_capital = 10000
        )
        
        expect_equal(portfolio$weights, rep(0.25, 4))
})
