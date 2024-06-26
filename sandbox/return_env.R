# Define the environment for portfolio returns
portfolio_env <- new.env()

# Function to add an existing portfolio to the environment
add_existing_portfolio_to_env <- function(portfolio_name, portfolio) {
  portfolio_env[[portfolio_name]] <- portfolio
}

# Function to add a return to a portfolio in the environment
add_return_to_env_portfolio <- function(portfolio_name, return_value) {
  portfolio <- portfolio_env[[portfolio_name]]
  stopifnot(!is.null(portfolio), inherits(portfolio, "portfolio"))

  # Use the package's function to add return (placeholder check the actual portfolio an. function)
  portfolio_add_return(portfolio, return_value)
  portfolio_env[[portfolio_name]] <- portfolio  # Update the environment with the modified portfolio
}

# Function to get returns from a portfolio in the environment
get_env_portfolio_returns <- function(portfolio_name) {
  portfolio <- portfolio_env[[portfolio_name]]
  stopifnot(!is.null(portfolio), inherits(portfolio, "portfolio"))

  # Use the package's function to get returns (placeholder check the actual portfolio an. function)
  portfolio_get_returns(portfolio)
}

# Function to calculate total return of a portfolio in the environment
calculate_env_portfolio_total_return <- function(portfolio_name) {
  portfolio <- portfolio_env[[portfolio_name]]
  stopifnot(!is.null(portfolio), inherits(portfolio, "portfolio"))

  # Use the package's function to calculate total return (placeholder check the actual portfolio an. function)
  portfolio_calculate_total_return(portfolio)
}
