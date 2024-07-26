library(PerformanceAnalytics)
library(boot)

portfolio_env <- new.env()

#' Add an Existing Portfolio to the Environment
#'
#' This function adds an existing portfolio to the global environment.
#'
#' @param portfolio_name A string representing the name of the portfolio.
#' @param portfolio The portfolio object to add to the environment.
#' @return None
#' @export
add_existing_portfolio_to_env <- function(portfolio_name, portfolio) {
  portfolio_env[[portfolio_name]] <- portfolio
}

#' Add a Return to a Portfolio in the Environment
#'
#' This function adds a return value to a specified portfolio in the global environment.
#'
#' @param portfolio_name A string representing the name of the portfolio.
#' @param return_value The return value to add to the portfolio.
#' @param ... Additional arguments.
#' @return None
#' @export
add_return_to_env_portfolio <- function(portfolio_name, return_value, ...) {
  portfolio <- portfolio_env[[portfolio_name]]
  stopifnot(!is.null(portfolio), inherits(portfolio, "xts"))

  # Ensure the new return is added as a new date entry
  new_return <- xts(matrix(return_value, ncol = 1), order.by = Sys.Date())
  colnames(new_return) <- colnames(portfolio)

  portfolio <- rbind(portfolio, new_return)
  portfolio_env[[portfolio_name]] <- portfolio  # Update the environment with the modified portfolio
}


#' Get Returns from a Portfolio in the Environment
#'
#' This function retrieves the returns of a specified portfolio from the global environment.
#'
#' @param portfolio_name A string representing the name of the portfolio.
#' @param ... Additional arguments.
#' @return Returns an xts object containing the portfolio returns.
#' @export
get_env_portfolio_returns <- function(portfolio_name, ...) {
  portfolio <- portfolio_env[[portfolio_name]]
  stopifnot(!is.null(portfolio), inherits(portfolio, "xts"))

  portfolio
}

#' Calculate Total Return of a Portfolio in the Environment
#'
#' This function calculates the total return of a specified portfolio in the global environment.
#'
#' @param portfolio_name A string representing the name of the portfolio.
#' @param ... Additional arguments.
#' @return The total return of the portfolio.
#' @export
calculate_env_portfolio_total_return <- function(portfolio_name, ...) {
  portfolio <- portfolio_env[[portfolio_name]]
  stopifnot(!is.null(portfolio), inherits(portfolio, "xts"))

  total_return <- Return.cumulative(portfolio, ...)
  total_return
}

#' Create an Account
#'
#' This function creates an account object.
#'
#' @param account_name The name of the account.
#' @param person The person associated with the account.
#' @param portfolios A list of portfolio objects associated with the account. Default is an empty list.
#' @param balance Initial balance of the account.
#' @param taxable Logical or character indicating if the account is taxable. Can be FALSE, TRUE, or "deferred". Default is FALSE.
#' @param ... Additional arguments.
#' @return An account object.
#' @export
create_account <- function(account_name, person, portfolios = list(), balance = 0, taxable = FALSE, ...) {
  stopifnot(is.character(account_name), is.character(person), is.numeric(balance))
  valid_taxable_values <- c(FALSE, TRUE, "deferred")
  stopifnot(taxable %in% valid_taxable_values)
  stopifnot(is.list(portfolios))

  for (portfolio in portfolios) {
    stopifnot(inherits(portfolio, "xts"))
  }

  account <- list(
    account_name = account_name,
    person = person,
    portfolios = portfolios,
    balance = balance,
    taxable = taxable,
    history = list()
  )

  structure(account, class = c("account", "list"))
}


library(quantmod)

#' Fetch and Store Historical Data
#'
#' This function fetches historical data for a given symbol and stores it in the portfolio environment.
#'
#' @param symbol A string representing the financial instrument's symbol.
#' @param source The data source, e.g., "tiingo" or "quandl".
#' @param ... Additional arguments passed to getSymbols.
#' @return None
#' @export
fetch_and_store_data <- function(symbol, source = "tiingo", ...) {
  if (source == "tiingo") {
    api_key <- Sys.getenv("TIINGO_API_KEY")
    if (api_key == "") stop("Tiingo API key not found.")
    getSymbols(symbol, src = "tiingo", api.key = api_key, auto.assign = TRUE, ...)
  } else if (source == "quandl") {
    api_key <- Sys.getenv("QUANDL_API_KEY")
    if (api_key == "") stop("Quandl API key not found.")
    getSymbols(symbol, src = "quandl", api.key = api_key, auto.assign = TRUE, ...)
  } else {
    stop("Unsupported data source.")
  }
  portfolio_env[[symbol]] <- get(symbol)
}





#' Add Portfolio to the Account
#'
#' This function adds a portfolio to the account and environment.
#'
#' @param account The account object.
#' @param portfolio_name The name of the portfolio.
#' @param portfolio The portfolio object (from the portfolio analytics package).
#' @param ... Additional arguments.
#' @return The updated account object.
#' @export
add_portfolio_to_account <- function(account, portfolio_name, portfolio, ...) {
  stopifnot(inherits(account, "account"), is.character(portfolio_name), inherits(portfolio, "xts"))

  account$portfolios[[portfolio_name]] <- portfolio
  account$history[[length(account$history) + 1]] <- paste("Added portfolio:", portfolio_name)

  add_existing_portfolio_to_env(portfolio_name, portfolio)

  return(account)
}

#' Rebalance Portfolio
#'
#' This function rebalances the portfolio using the specified target allocation.
#'
#' @param portfolio_name A string representing the name of the portfolio.
#' @param target_allocation A named vector of target weights for each asset in the portfolio.
#' @param rebalance_dates A vector of dates when rebalancing should occur.
#' @param ... Additional arguments passed to Return.portfolio.
#' @return A list containing the rebalanced portfolio returns.
#' @export
rebalance_portfolio <- function(portfolio_name, target_allocation, rebalance_dates, ...) {
  portfolio <- portfolio_env[[portfolio_name]]
  stopifnot(!is.null(portfolio), inherits(portfolio, "xts"))

  if (!is.vector(target_allocation) || !all(names(target_allocation) %in% colnames(portfolio))) {
    stop("Target allocation must be a named vector with names matching portfolio columns.")
  }

  rebalanced_returns <- Return.portfolio(R = portfolio, weights = target_allocation, rebalance_on = rebalance_dates, ...)
  rebalanced_returns
}


#' Estimate Portfolio Return using Multivariate Bootstrap
#'
#' This function estimates the return of a portfolio using Multivariate Bootstrap.
#'
#' @param account The account object.
#' @param n_simulations The number of bootstrap simulations. Default is 1000.
#' @param rebalance Logical indicating if rebalancing should be done after each simulation. Default is TRUE.
#' @param withdraw_amount The fixed amount to withdraw in each period.
#' @param ... Additional arguments.
#' @return A list containing the simulated final balances and the probability of success.
#' @export
estimate_portfolio_return <- function(account, n_simulations = 1000, target_allocation, rebalance_dates, withdraw_amount, ...) {
  stopifnot(inherits(account, "account"), is.numeric(n_simulations), is.numeric(withdraw_amount))

  portfolio_names <- names(account$portfolios)
  initial_balance <- account$balance

  # Function to perform a single bootstrap sample
  bootstrap_sample <- function(data, indices) {
    data[indices, ]
  }

  # Function to calculate the block length based on historical autocorrelation
  calculate_block_length <- function(returns) {
    acf_values <- acf(returns, plot = FALSE)$acf[-1]
    autocorrelation <- sum(acf_values) / length(acf_values)
    block_length <- max(1, round(1 / (1 - autocorrelation)))
    return(block_length)
  }

  # Function to simulate the portfolio returns
  simulate_returns <- function() {
    balance <- initial_balance
    for (name in portfolio_names) {
      portfolio <- portfolio_env[[name]]
      returns <- rebalance_portfolio(name, target_allocation, rebalance_dates, ...)

      for (ret in returns) {
        balance <- balance * (1 + ret) - withdraw_amount
      }
    }
    balance
  }

  # Run the simulations
  final_balances <- replicate(n_simulations, simulate_returns())

  # Calculate the probability of success (final balance > 0)
  prob_success <- mean(final_balances > 0)

  list(final_balances = final_balances, probability_of_success = prob_success)
}
