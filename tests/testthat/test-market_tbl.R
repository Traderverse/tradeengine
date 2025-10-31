# Test market_tbl creation and validation

test_that("market_tbl creates valid object", {
        
        data <- market_tbl(
                symbol = rep("AAPL", 5),
                datetime = seq.POSIXt(as.POSIXct("2024-01-01"), by = "day", length.out = 5),
                open = c(150, 151, 152, 151, 153),
                high = c(152, 153, 154, 153, 155),
                low = c(149, 150, 151, 150, 152),
                close = c(151, 152, 153, 152, 154),
                volume = c(1e6, 1.1e6, 1.2e6, 1.15e6, 1.3e6)
        )
        
        expect_s3_class(data, "market_tbl")
        expect_s3_class(data, "tbl_df")
        expect_equal(nrow(data), 5)
        expect_equal(ncol(data), 8)
        expect_true(is_market_tbl(data))
})


test_that("market_tbl validates OHLC consistency", {
        
        expect_warning(
                market_tbl(
                        symbol = rep("TEST", 3),
                        datetime = seq.POSIXt(as.POSIXct("2024-01-01"), by = "day", length.out = 3),
                        open = c(100, 101, 102),
                        high = c(98, 99, 100),  # High less than open - should warn
                        low = c(97, 98, 99),
                        close = c(99, 100, 101),
                        volume = c(1e6, 1e6, 1e6)
                ),
                "Inconsistent OHLC prices"
        )
})


test_that("market_tbl rejects negative prices", {
        
        expect_error(
                market_tbl(
                        symbol = rep("TEST", 3),
                        datetime = seq.POSIXt(as.POSIXct("2024-01-01"), by = "day", length.out = 3),
                        open = c(100, 101, -102),  # Negative price
                        high = c(102, 103, 104),
                        low = c(99, 100, 101),
                        close = c(101, 102, 103),
                        volume = c(1e6, 1e6, 1e6)
                ),
                "Negative prices or volumes"
        )
})


test_that("as_market_tbl converts data frames", {
        
        df <- data.frame(
                symbol = rep("AAPL", 3),
                datetime = as.POSIXct(c("2024-01-01", "2024-01-02", "2024-01-03")),
                open = c(150, 151, 152),
                high = c(152, 153, 154),
                low = c(149, 150, 151),
                close = c(151, 152, 153),
                volume = c(1e6, 1.1e6, 1.2e6)
        )
        
        data <- as_market_tbl(df)
        
        expect_s3_class(data, "market_tbl")
        expect_equal(nrow(data), 3)
})


test_that("market_tbl attributes are set correctly", {
        
        data <- market_tbl(
                symbol = rep("BTC", 3),
                datetime = seq.POSIXt(as.POSIXct("2024-01-01"), by = "hour", length.out = 3),
                open = c(50000, 50100, 50200),
                high = c(50200, 50300, 50400),
                low = c(49900, 50000, 50100),
                close = c(50100, 50200, 50300),
                volume = c(100, 110, 120),
                frequency = "1hour",
                timezone = "UTC",
                asset_class = "crypto"
        )
        
        expect_equal(attr(data, "frequency"), "1hour")
        expect_equal(attr(data, "timezone"), "UTC")
        expect_equal(attr(data, "asset_class"), "crypto")
})
