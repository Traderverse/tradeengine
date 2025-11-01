#' Generate Synthetic Market Data
#'
#' Creates synthetic OHLCV market data using geometric Brownian motion for 
#' testing and examples.
#'
#' @param n_days Integer: number of days to generate
#' @param start_price Numeric: starting price
#' @param volatility Numeric: annual volatility (e.g., 0.20 for 20%)
#' @param drift Numeric: annual drift/return (e.g., 0.08 for 8%)
#' @param start_date POSIXct or Date: starting date
#' @param symbol Character: stock symbol
#'
#' @return A `market_tbl` object with synthetic data
#' @export
#'
#' @examples
#' # Generate 1 year of data
#' synthetic_data <- generate_synthetic_data(
#'   n_days = 252,
#'   start_price = 100,
#'   volatility = 0.20,
#'   drift = 0.08
#' )
#' 
#' # Generate high-volatility crypto-like data
#' crypto_data <- generate_synthetic_data(
#'   n_days = 365,
#'   start_price = 50000,
#'   volatility = 0.80,
#'   drift = 0.50,
#'   symbol = "BTC-USD"
#' )
generate_synthetic_data <- function(n_days = 252,
                                   start_price = 100,
                                   volatility = 0.20,
                                   drift = 0.08,
                                   start_date = as.POSIXct("2024-01-01"),
                                   symbol = "DEMO") {
        
        # Generate dates (skip weekends for stock-like data)
        dates <- seq.POSIXt(start_date, by = "day", length.out = n_days * 1.5)
        weekdays_only <- dates[!weekday(dates) %in% c(1, 7)]  # Remove Sat/Sun
        dates <- weekdays_only[1:n_days]
        
        # Parameters for daily returns
        dt <- 1/252  # daily time step
        mu <- drift * dt
        sigma <- volatility * sqrt(dt)
        
        # Generate random returns using geometric Brownian motion
        set.seed(42)  # For reproducibility in examples
        returns <- rnorm(n_days, mean = mu, sd = sigma)
        
        # Generate close prices
        log_prices <- cumsum(c(log(start_price), returns))
        close_prices <- exp(log_prices)[1:n_days]  # Take only n_days prices
        
        # Generate OHLC from close
        # High is close + some random positive move
        high_moves <- abs(rnorm(n_days, mean = 0, sd = sigma * start_price * 0.5))
        high_prices <- close_prices + high_moves
        
        # Low is close - some random negative move  
        low_moves <- abs(rnorm(n_days, mean = 0, sd = sigma * start_price * 0.5))
        low_prices <- close_prices - low_moves
        
        # Open is somewhere between yesterday's close and today's close
        open_prices <- c(start_price, close_prices[-n_days] + rnorm(n_days - 1, 0, sigma * start_price * 0.3))
        
        # Ensure OHLC consistency
        for (i in 1:n_days) {
                high_prices[i] <- max(open_prices[i], close_prices[i], high_prices[i])
                low_prices[i] <- min(open_prices[i], close_prices[i], low_prices[i])
        }
        
        # Generate volume (log-normal distribution)
        avg_volume <- 1000000
        volumes <- exp(rnorm(n_days, mean = log(avg_volume), sd = 0.3))
        
        # Create market_tbl
        data <- market_tbl(
                symbol = rep(symbol, n_days),
                datetime = dates,
                open = open_prices,
                high = high_prices,
                low = low_prices,
                close = close_prices,
                volume = volumes,
                adjusted = close_prices
        )
        
        return(data)
}


# Helper function for weekday calculation
weekday <- function(dates) {
        as.integer(format(dates, "%u"))
}
