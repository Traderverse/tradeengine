#' Create a Market Data Tibble
#'
#' Creates a standardized market data tibble (`market_tbl`) that serves as the 
#' foundation for all tradeengine operations. This is the core data structure 
#' used across the entire TradingVerse ecosystem.
#'
#' @param symbol Character vector of asset symbols/tickers
#' @param datetime POSIXct vector of timestamps (timezone-aware)
#' @param open Numeric vector of opening prices
#' @param high Numeric vector of high prices
#' @param low Numeric vector of low prices
#' @param close Numeric vector of closing prices
#' @param volume Numeric vector of trading volume
#' @param adjusted Numeric vector of adjusted closing prices (optional)
#' @param frequency Character string indicating data frequency (e.g., "1min", "1hour", "daily")
#' @param timezone Character string for timezone (default: "UTC")
#' @param asset_class Character string for asset class (e.g., "equity", "crypto", "futures", "fx")
#'
#' @return A `market_tbl` object (enhanced tibble) with standardized market data
#'
#' @export
#' @importFrom tibble tibble
#' @importFrom lubridate as_datetime
#' @importFrom rlang .data
#'
#' @examples
#' # Create daily stock data
#' prices <- market_tbl(
#'   symbol = rep("AAPL", 5),
#'   datetime = seq.POSIXt(as.POSIXct("2024-01-01"), by = "day", length.out = 5),
#'   open = c(150, 151, 152, 151, 153),
#'   high = c(152, 153, 154, 153, 155),
#'   low = c(149, 150, 151, 150, 152),
#'   close = c(151, 152, 153, 152, 154),
#'   volume = c(1e6, 1.1e6, 1.2e6, 1.15e6, 1.3e6)
#' )
#'
#' # Create crypto data with 1-hour frequency
#' btc <- market_tbl(
#'   symbol = rep("BTC-USD", 24),
#'   datetime = seq.POSIXt(as.POSIXct("2024-01-01"), by = "hour", length.out = 24),
#'   open = rnorm(24, 50000, 500),
#'   high = rnorm(24, 50500, 500),
#'   low = rnorm(24, 49500, 500),
#'   close = rnorm(24, 50000, 500),
#'   volume = rnorm(24, 100, 10),
#'   frequency = "1hour",
#'   asset_class = "crypto"
#' )
market_tbl <- function(symbol,
                       datetime,
                       open,
                       high,
                       low,
                       close,
                       volume,
                       adjusted = NULL,
                       frequency = "daily",
                       timezone = "UTC",
                       asset_class = "equity") {
        
        # Input validation
        n <- length(symbol)
        stopifnot(
                "All inputs must have the same length" = 
                        length(datetime) == n && length(open) == n &&
                        length(high) == n && length(low) == n &&
                        length(close) == n && length(volume) == n
        )
        
        # Convert datetime to POSIXct if needed
        if (!inherits(datetime, "POSIXct")) {
                datetime <- as_datetime(datetime, tz = timezone)
        }
        
        # If adjusted not provided, use close
        if (is.null(adjusted)) {
                adjusted <- close
        }
        
        # Create tibble
        tbl <- tibble(
                symbol = as.character(symbol),
                datetime = datetime,
                open = as.numeric(open),
                high = as.numeric(high),
                low = as.numeric(low),
                close = as.numeric(close),
                volume = as.numeric(volume),
                adjusted = as.numeric(adjusted)
        )
        
        # Add attributes
        attr(tbl, "frequency") <- frequency
        attr(tbl, "timezone") <- timezone
        attr(tbl, "asset_class") <- asset_class
        
        # Add class
        class(tbl) <- c("market_tbl", class(tbl))
        
        # Validate
        validate_market_tbl(tbl)
        
        return(tbl)
}


#' Convert to Market Data Tibble
#'
#' Converts various data formats to a standardized `market_tbl` object.
#'
#' @param data A data frame or tibble with market data
#' @param symbol_col Name of the column containing symbols (default: "symbol")
#' @param datetime_col Name of the column containing timestamps (default: "datetime")
#' @param ... Additional arguments passed to `market_tbl()`
#'
#' @return A `market_tbl` object
#' @export
#'
#' @examples
#' # Convert a regular data frame
#' df <- data.frame(
#'   symbol = rep("AAPL", 3),
#'   datetime = as.POSIXct(c("2024-01-01", "2024-01-02", "2024-01-03")),
#'   open = c(150, 151, 152),
#'   high = c(152, 153, 154),
#'   low = c(149, 150, 151),
#'   close = c(151, 152, 153),
#'   volume = c(1e6, 1.1e6, 1.2e6)
#' )
#' 
#' market_data <- as_market_tbl(df)
as_market_tbl <- function(data, 
                          symbol_col = "symbol",
                          datetime_col = "datetime",
                          ...) {
        
        # Check required columns
        required_cols <- c(symbol_col, datetime_col, "open", "high", "low", "close", "volume")
        missing_cols <- setdiff(required_cols, names(data))
        
        if (length(missing_cols) > 0) {
                stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
        }
        
        # Create market_tbl
        market_tbl(
                symbol = data[[symbol_col]],
                datetime = data[[datetime_col]],
                open = data$open,
                high = data$high,
                low = data$low,
                close = data$close,
                volume = data$volume,
                adjusted = if ("adjusted" %in% names(data)) data$adjusted else NULL,
                ...
        )
}


#' Check if Object is a Market Tibble
#'
#' @param x An object to test
#' @return Logical value indicating if object is a `market_tbl`
#' @export
is_market_tbl <- function(x) {
        inherits(x, "market_tbl")
}


#' Validate Market Data Tibble
#'
#' Validates that a `market_tbl` object meets all requirements.
#'
#' @param data A `market_tbl` object
#' @return Invisible TRUE if valid, throws error otherwise
#' @export
validate_market_tbl <- function(data) {
        
        # Check class
        if (!is_market_tbl(data)) {
                stop("Object is not a market_tbl")
        }
        
        # Check required columns
        required_cols <- c("symbol", "datetime", "open", "high", "low", "close", "volume", "adjusted")
        missing_cols <- setdiff(required_cols, names(data))
        
        if (length(missing_cols) > 0) {
                stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
        }
        
        # Check data types
        if (!inherits(data$datetime, "POSIXct")) {
                stop("datetime column must be POSIXct")
        }
        
        # Check for NA values in critical columns
        if (any(is.na(data$close))) {
                warning("NA values detected in close prices")
        }
        
        # Check price consistency (high >= low, high >= open, high >= close, etc.)
        invalid_prices <- with(data, {
                (high < low) | (high < open) | (high < close) | 
                        (low > open) | (low > close)
        })
        
        if (any(invalid_prices, na.rm = TRUE)) {
                warning("Inconsistent OHLC prices detected (high < low or similar)")
        }
        
        # Check for negative prices or volumes
        if (any(data$open < 0, na.rm = TRUE) || 
            any(data$high < 0, na.rm = TRUE) || 
            any(data$low < 0, na.rm = TRUE) || 
            any(data$close < 0, na.rm = TRUE) || 
            any(data$volume < 0, na.rm = TRUE)) {
                stop("Negative prices or volumes detected")
        }
        
        invisible(TRUE)
}


#' Print Method for Market Tibble
#'
#' @param x A `market_tbl` object
#' @param ... Additional arguments passed to print
#' @export
print.market_tbl <- function(x, ...) {
        cat("# Market Data Tibble (market_tbl)\n")
        cat("# Frequency:", attr(x, "frequency"), "\n")
        cat("# Timezone:", attr(x, "timezone"), "\n")
        cat("# Asset Class:", attr(x, "asset_class"), "\n")
        cat("# Symbols:", length(unique(x$symbol)), "unique\n")
        cat("# Observations:", nrow(x), "\n")
        cat("# Date Range:", as.character(min(x$datetime)), "to", as.character(max(x$datetime)), "\n")
        cat("\n")
        NextMethod()
}
