library(PerformanceAnalytics)
library(boot)
library(quantmod)

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
#' @param tax_rate Numeric value representing the tax rate. This should be provided if taxable is TRUE or "deferred".
#' @param ... Additional arguments.
#' @return An account object.
#' @export
create_account <- function(account_name, person, portfolios = list(), balance = 0, taxable = FALSE, tax_rate = 0, ...) {
  stopifnot(is.character(account_name), is.character(person), is.numeric(balance))
  valid_taxable_values <- c(FALSE, TRUE, "deferred")
  stopifnot(taxable %in% valid_taxable_values)
  stopifnot(is.list(portfolios))
  if (taxable != FALSE) stopifnot(is.numeric(tax_rate) && tax_rate >= 0 && tax_rate <= 1)

  for (portfolio in portfolios) {
    stopifnot(inherits(portfolio, "xts"))
  }

  account <- list(
    account_name = account_name,
    person = person,
    portfolios = portfolios,
    balance = balance,
    taxable = taxable,
    tax_rate = ifelse(taxable == FALSE, 0, tax_rate),
    history = list()
  )

  structure(account, class = c("account", "list"))
}


#' Withdraw Funds from Account
#'
#' This function withdraws funds from the account, applying taxes if necessary.
#'
#' @param account The account object.
#' @param amount The amount to withdraw.
#' @param ... Additional arguments.
#' @return The updated account object and the actual withdrawn amount after taxes (if any).
#' @export
withdraw_from_account <- function(account, amount, ...) {
  stopifnot(inherits(account, "account"), is.numeric(amount), amount > 0)

  actual_withdrawal <- amount
  if (account$taxable == "deferred") {
    tax_amount <- amount * account$tax_rate
    actual_withdrawal <- amount - tax_amount
    account$balance <- account$balance - amount  # Subtract full amount including tax
  } else if (account$taxable == TRUE) {
    tax_amount <- amount * account$tax_rate
    actual_withdrawal <- amount - tax_amount
    account$balance <- account$balance - actual_withdrawal
  } else {
    account$balance <- account$balance - amount
  }

  account$history[[length(account$history) + 1]] <- paste("Withdrew", amount, "from account:", account$account_name)

  return(list(account = account, actual_withdrawal = actual_withdrawal))
}






#' Fetch and Store Historical Data
#'
#' This function fetches historical data for a given symbol and stores it in the portfolio environment.
#'
#' @param symbol A string representing the financial instrument's symbol.
#' @param source The data source, e.g., "tiingo" or "quandl".
#' @param ... Additional arguments passed to getSymbols.
#' @return None
#' @export
fetch <- function(symbol, source = "tiingo", ...) {
  source("private_creds.R")  # Load the API keys from the private_creds.R file

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
#' @param target_allocation A named vector of target weights for each asset in the portfolio. Default is 60-40 allocation.
#' @param rebalance_dates A vector of dates when rebalancing should occur. Default is "months".
#' @param ... Additional arguments.
#' @return A list containing the simulated final balances and the probability of success.
#' @export
estimate_portfolio_return <- function(account, n_simulations = 1000, rebalance = TRUE, withdraw_amount,
                                      target_allocation = c(0.6, 0.4), rebalance_dates = "months", ...) {
  stopifnot(inherits(account, "account"), is.numeric(n_simulations), is.numeric(withdraw_amount))

  portfolio_names <- names(account$portfolios)
  initial_balance <- account$balance

  # Function to perform a single bootstrap sample
  bootstrap_sample <- function(data, indices) {
    data[indices, ]
  }

  # Function to calculate the block length based on historical autocorrelation
  calculate_block_length <- function(returns) {
    correlation_matrix <- cor(returns)
    avg_correlation <- mean(correlation_matrix[lower.tri(correlation_matrix)])
    block_length <- max(1, round(1 / (1 - avg_correlation)))
    return(block_length)
  }

  # Function to simulate the portfolio returns
  simulate_returns <- function() {
    balance <- initial_balance
    for (name in portfolio_names) {
      portfolio <- portfolio_env[[name]]
      returns <- rebalance_portfolio(name, target_allocation, rebalance_dates, ...)

      block_length <- calculate_block_length(returns)
      bootstrapped_returns <- tsbootstrap(returns, statistic = bootstrap_sample,
                                          R = 1, l = block_length, sim = "fixed")

      for (ret in bootstrapped_returns) {
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

