# Test backtesting functionality

test_that("backtest runs with valid strategy", {
        
        skip_if_not_installed("dplyr")
        
        data <- generate_synthetic_data(n_days = 100, start_price = 100)
        
        strategy <- data |>
                dplyr::mutate(sma_20 = sma(close, 20)) |>
                add_strategy(
                        entry_rules = close > sma_20,
                        exit_rules = close < sma_20,
                        position_size = 1000
                )
        
        results <- backtest(strategy, initial_capital = 10000, method = "vectorized")
        
        expect_s3_class(results, "backtest_results")
        expect_true("equity_curve" %in% names(results))
        expect_true("trades" %in% names(results))
        expect_true("final_equity" %in% names(results))
        expect_true("total_return" %in% names(results))
})


test_that("backtest requires strategy signals", {
        
        data <- generate_synthetic_data(n_days = 50)
        
        expect_error(
                backtest(data, initial_capital = 10000),
                "No strategy signals found"
        )
})


test_that("backtest calculates equity correctly", {
        
        data <- generate_synthetic_data(n_days = 100, start_price = 100)
        
        strategy <- data |>
                dplyr::mutate(sma_20 = sma(close, 20)) |>
                add_strategy(
                        entry_rules = close > sma_20,
                        exit_rules = close < sma_20,
                        position_size = 1000
                )
        
        results <- backtest(strategy, initial_capital = 10000)
        
        # Final equity should be different from initial
        expect_true(results$final_equity != 10000)
        
        # Equity curve should have same length as input data
        expect_equal(nrow(results$equity_curve), nrow(data))
        
        # First equity value should equal initial capital
        expect_equal(results$equity_curve$equity[1], 10000)
})


test_that("backtest respects commission and slippage", {
        
        data <- generate_synthetic_data(n_days = 50, start_price = 100)
        
        strategy_no_costs <- data |>
                dplyr::mutate(sma_20 = sma(close, 20)) |>
                add_strategy(
                        entry_rules = close > sma_20,
                        exit_rules = close < sma_20,
                        commission = 0,
                        slippage = 0
                )
        
        strategy_with_costs <- data |>
                dplyr::mutate(sma_20 = sma(close, 20)) |>
                add_strategy(
                        entry_rules = close > sma_20,
                        exit_rules = close < sma_20,
                        commission = 0.01,
                        slippage = 0.01
                )
        
        results_no_costs <- backtest(strategy_no_costs, initial_capital = 10000)
        results_with_costs <- backtest(strategy_with_costs, initial_capital = 10000)
        
        # Results with costs should have lower final equity (assuming any trades)
        if (results_no_costs$n_trades > 0) {
                expect_true(results_with_costs$final_equity <= results_no_costs$final_equity)
        }
})


test_that("simulate_orders executes market orders", {
        
        execution <- simulate_orders(
                order_type = "market",
                side = "buy",
                quantity = 100,
                market_price = 100,
                market_volume = 1000000
        )
        
        expect_true(execution$filled)
        expect_equal(execution$filled_quantity, 100)
        expect_true(execution$fill_price > 0)
})


test_that("simulate_orders handles limit orders", {
        
        # Limit buy that should fill
        execution <- simulate_orders(
                order_type = "limit",
                side = "buy",
                quantity = 100,
                price = 101,
                market_price = 100,
                market_volume = 1000000
        )
        
        expect_true(execution$filled)
        
        # Limit buy that should not fill
        execution <- simulate_orders(
                order_type = "limit",
                side = "buy",
                quantity = 100,
                price = 99,
                market_price = 100,
                market_volume = 1000000
        )
        
        expect_false(execution$filled)
})
