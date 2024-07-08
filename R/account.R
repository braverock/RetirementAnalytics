library(PerformanceAnalytics)
library(boot)

portfolio_env <- new.env()

# Function to add an existing portfolio to the environment
add_existing_portfolio_to_env <- function(portfolio_name, portfolio) {
  portfolio_env[[portfolio_name]] <- portfolio
}

# Function to add a return to a portfolio in the environment
add_return_to_env_portfolio <- function(portfolio_name, return_value, ...) {
  portfolio <- portfolio_env[[portfolio_name]]
  stopifnot(!is.null(portfolio), inherits(portfolio, "xts"))

  # Add return to portfolio
  portfolio <- rbind(portfolio, xts(return_value, order.by = Sys.Date()))
  portfolio_env[[portfolio_name]] <- portfolio  # Update the environment with the modified portfolio
}

# Function to get returns from a portfolio in the environment
get_env_portfolio_returns <- function(portfolio_name, ...) {
  portfolio <- portfolio_env[[portfolio_name]]
  stopifnot(!is.null(portfolio), inherits(portfolio, "xts"))

  portfolio
}

# Function to calculate total return of a portfolio in the environment
calculate_env_portfolio_total_return <- function(portfolio_name, ...) {
  portfolio <- portfolio_env[[portfolio_name]]
  stopifnot(!is.null(portfolio), inherits(portfolio, "xts"))

  total_return <- Return.cumulative(portfolio, ...)
  total_return
}

create_account <- function(account_name, person, portfolios = list(), balance = 0, taxable = FALSE, ...) {
  stopifnot(is.character(account_name), is.character(person), is.numeric(balance))
  valid_taxable_values <- c(FALSE, TRUE, "deferred")
  stopifnot(taxable %in% valid_taxable_values)

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

add_portfolio_to_account <- function(account, portfolio_name, portfolio, ...) {
  stopifnot(inherits(account, "account"), is.character(portfolio_name), inherits(portfolio, "xts"))

  account$portfolios[[portfolio_name]] <- portfolio
  account$history[[length(account$history) + 1]] <- paste("Added portfolio:", portfolio_name)

  add_existing_portfolio_to_env(portfolio_name, portfolio)

  account
}

estimate_portfolio_return <- function(account, n_simulations = 1000, rebalance = TRUE, withdraw_amount, ...) {
  stopifnot(inherits(account, "account"), is.numeric(n_simulations), is.numeric(withdraw_amount))

  portfolio_names <- names(account$portfolios)
  initial_balance <- account$balance

  bootstrap_sample <- function(data, indices) {
    data[indices, ]
  }

  calculate_block_length <- function(returns) {
    acf_values <- acf(returns, plot = FALSE)$acf[-1]
    autocorrelation <- sum(acf_values) / length(acf_values)
    block_length <- max(1, round(1 / (1 - autocorrelation)))
    return(block_length)
  }

  simulate_returns <- function() {
    balance <- initial_balance
    for (name in portfolio_names) {
      returns <- get_env_portfolio_returns(name, ...)
      block_length <- calculate_block_length(returns)
      boot_obj <- tsboot(data = returns, statistic = bootstrap_sample, R = n_simulations, l = block_length, sim = "geom")
      simulated_returns <- apply(boot_obj$t, 2, mean)

      for (ret in simulated_returns) {
        balance <- balance * (1 + ret) - withdraw_amount
        if (rebalance) {
          balance <- balance / length(portfolio_names)
        }
      }
    }
    balance
  }

  final_balances <- replicate(n_simulations, simulate_returns())

  prob_success <- mean(final_balances > 0)

  list(final_balances = final_balances, probability_of_success = prob_success)
}






