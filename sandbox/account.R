#' Create an account
#'
#' This function creates an account object.
#'
#' @param account_name The name of the account.
#' @param person The person associated with the account.
#' @param portfolios A list of portfolio objects associated with the account. Default is an empty list.
#' @param balance Initial balance of the account.
#' @param taxable Logical or character indicating if the account is taxable. Can be FALSE, TRUE, or "deferred". Default is FALSE.
#' @return An account object.
#' @export
create_account <- function(account_name, person, portfolios = list(), balance = 0, taxable = FALSE) {
  # Check input validity
  stopifnot(is.character(account_name), is.character(person), is.numeric(balance))

  # Validate taxable input
  valid_taxable_values <- c(FALSE, TRUE, "deferred")
  stopifnot(taxable %in% valid_taxable_values)

  # Create the account object
  account <- list(
    account_name = account_name,
    person = person,
    portfolios = portfolios,
    balance = balance,
    taxable = taxable,
    history = list()  # Initialize empty history list
  )

  # Set class
  structure(account, class = c("account", "list"))
}



#' Add portfolio to the account
#'
#' This function adds a portfolio to the account and environment.
#'
#' @param account The account object.
#' @param portfolio_name The name of the portfolio.
#' @param portfolio The portfolio object (from the portfolio analytics package).
#' @return The updated account object.
#' @export
add_portfolio_to_account <- function(account, portfolio_name, portfolio) {
  # Check input validity
  stopifnot(inherits(account, "account"), is.character(portfolio_name), inherits(portfolio, "portfolio"))

  account$portfolios[[portfolio_name]] <- portfolio
  account$history[[length(account$history) + 1]] <- paste("Added portfolio:", portfolio_name)

  # Add the portfolio to the environment
  add_existing_portfolio_to_env(portfolio_name, portfolio)

  account
}








