#' TradingVerse Package Utilities
#'
#' Internal utility functions for the tradeengine package.
#'
#' @keywords internal
#' @name utils


#' Check if Package is Available
#'
#' @param pkg Character string of package name
#' @return Logical
#' @keywords internal
has_package <- function(pkg) {
        requireNamespace(pkg, quietly = TRUE)
}


#' Format Currency
#'
#' @param x Numeric value
#' @param digits Number of decimal places
#' @return Character string formatted as currency
#' @keywords internal
format_currency <- function(x, digits = 2) {
        paste0("$", format(round(x, digits), big.mark = ",", scientific = FALSE))
}


#' Format Percentage
#'
#' @param x Numeric value (e.g., 0.05 for 5%)
#' @param digits Number of decimal places
#' @return Character string formatted as percentage
#' @keywords internal
format_percentage <- function(x, digits = 2) {
        paste0(format(round(x * 100, digits), nsmall = digits), "%")
}


#' Calculate Simple Moving Average
#'
#' Helper function to calculate SMA for examples and testing
#'
#' @param x Numeric vector
#' @param n Window size
#' @return Numeric vector of same length with SMA values
#' @export
#' @examples
#' prices <- c(100, 102, 101, 103, 105, 104, 106)
#' sma(prices, n = 3)
sma <- function(x, n = 20) {
        if (length(x) < n) {
                return(rep(NA, length(x)))
        }
        
        result <- rep(NA, length(x))
        
        for (i in n:length(x)) {
                result[i] <- mean(x[(i - n + 1):i], na.rm = TRUE)
        }
        
        return(result)
}


#' Calculate Exponential Moving Average
#'
#' Helper function to calculate EMA for examples and testing
#'
#' @param x Numeric vector
#' @param n Window size
#' @return Numeric vector of same length with EMA values
#' @export
#' @examples
#' prices <- c(100, 102, 101, 103, 105, 104, 106)
#' ema(prices, n = 3)
ema <- function(x, n = 20) {
        if (length(x) < n) {
                return(rep(NA, length(x)))
        }
        
        alpha <- 2 / (n + 1)
        result <- rep(NA, length(x))
        result[n] <- mean(x[1:n], na.rm = TRUE)
        
        for (i in (n + 1):length(x)) {
                result[i] <- alpha * x[i] + (1 - alpha) * result[i - 1]
        }
        
        return(result)
}


#' Calculate Returns
#'
#' Calculate simple or log returns from price series
#'
#' @param prices Numeric vector of prices
#' @param type Character: "simple" or "log"
#' @param lag Integer: number of periods for return calculation
#' @return Numeric vector of returns
#' @export
#' @examples
#' prices <- c(100, 102, 101, 103, 105)
#' calc_returns_simple(prices, type = "simple")
#' calc_returns_simple(prices, type = "log")
calc_returns_simple <- function(prices, type = "simple", lag = 1) {
        type <- match.arg(type, c("simple", "log"))
        
        if (type == "simple") {
                returns <- diff(prices, lag = lag) / prices[1:(length(prices) - lag)]
        } else {
                returns <- diff(log(prices), lag = lag)
        }
        
        return(c(rep(NA, lag), returns))
}


#' Validate Strategy Signals
#'
#' Checks if strategy signals are valid and consistent
#'
#' @param entry_signal Logical vector of entry signals
#' @param exit_signal Logical vector of exit signals
#' @return Logical TRUE if valid, stops with error otherwise
#' @keywords internal
validate_signals <- function(entry_signal, exit_signal) {
        
        if (length(entry_signal) != length(exit_signal)) {
                stop("Entry and exit signals must have same length")
        }
        
        if (!is.logical(entry_signal) && !is.numeric(entry_signal)) {
                stop("Signals must be logical or numeric")
        }
        
        # Check for simultaneous signals
        simultaneous <- entry_signal & exit_signal
        if (any(simultaneous, na.rm = TRUE)) {
                warning("Simultaneous entry and exit signals detected. Exit will take precedence.")
        }
        
        invisible(TRUE)
}


#' Convert Frequency String to Seconds
#'
#' @param freq Character: frequency string like "1min", "1hour", "daily"
#' @return Numeric: number of seconds
#' @keywords internal
freq_to_seconds <- function(freq) {
        
        conversions <- c(
                "1min" = 60,
                "5min" = 300,
                "15min" = 900,
                "30min" = 1800,
                "1hour" = 3600,
                "4hour" = 14400,
                "daily" = 86400,
                "weekly" = 604800,
                "monthly" = 2592000
        )
        
        if (freq %in% names(conversions)) {
                return(conversions[[freq]])
        } else {
                stop("Unknown frequency: ", freq)
        }
}


#' Safe Division
#'
#' Performs division with protection against division by zero
#'
#' @param x Numeric vector (numerator)
#' @param y Numeric vector (denominator)
#' @param default Value to return when denominator is zero
#' @return Numeric vector
#' @keywords internal
safe_divide <- function(x, y, default = 0) {
        result <- ifelse(y == 0 | is.na(y), default, x / y)
        return(result)
}
