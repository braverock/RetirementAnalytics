#' Create a Household
#'
#' This function creates a household object.
#'
#' @param household_name The name of the household.
#' @param persons A list of persons in the household.
#' @return A Household object.
#' @export



create_household <- function(household_name,person=list()) {
  stopifnot(is.character(household_name))
  household <- list(
    household_name = household_name,
    person = person
  )
  class(household)<- "Household"
  return(household)
}


#' Add a Person to a Household
#'
#' This function adds a person to the household and assigns a role.
#'
#' @param household The household object.
#' @param person The person object to add.
#' @param role The role of the person in the household.
#' @return The updated Household object.
#' @export



add_person <- function(household, person, role) {
  person$role <- role
  household$person <- append(household$person, list(person))
  return(household)
}

#' Add an Account to a Person
#'
#' This function adds an account to a person in the household.
#'
#' @param person The person object.
#' @param account_name The name of the account.
#' @param balance The balance of the account.
#' @return The updated Person object.
#' @export

add_account <- function(person, account_name, balance) {
  stopifnot(is.character(account_name))
  stopifnot(is.numeric(balance))
  account <- list(
    account_name = account_name,
    balance = balance
  )
  person$accounts <- append(person$accounts, list(account))
  return(person)
}


#' Sum Balances of Accounts for a Person
#'
#' This function sums the balances of all accounts for a person.
#'
#' @param person The person object.
#' @return The total balance of the person's accounts.
#' @export

sum_account_balances <- function(person) {
  total_balance <- sum(sapply(person$accounts, function(account) account$balance))
  return(total_balance)
}




#' Sum Income of Household
#'
#' This function sums the income of all persons in the household.
#'
#' @param household The household object.
#' @return The total income of the household.
#' @export

sum_income <- function(household, ...) {
  UseMethod("sum_income")
}

#' @export
sum_income.Household <- function(household, ...) {
  total_income <- sum(sapply(household$person, function(person) person$income))
  return(total_income)
}



#' Summary of Household
#'
#' This function provides a summary of the household.
#'
#' @param object The household object.
#' @export

summary.Household <- function(object, ...) {
  cat("Household Name:", object$household_name, "\n")
  cat("Number of Persons:", length(object$person), "\n")
  cat("Persons Details:\n")
  for (i in 1:length(object$person)) {
    person <- object$person[[i]]
    cat("Role:", person$role, "\n")
    summary.Person(person)
    cat("\n")
    summary.Accounts(person)
  }
  cat("Total Household Income:", sum_income(object), "\n")
}


#' Summary of Accounts for a Person
#'
#' This function provides a summary of the accounts for a person.
#'
#' @param person The person object.
#' @expor


summary.Accounts <- function(person, ...) {
  if (length(person$accounts) == 0) {
    cat("No accounts available for this person.\n")
    return()
  }
  cat("Accounts:\n")
  for (i in 1:length(person$accounts)) {
    account <- person$accounts[[i]]
    cat("Account Name:", account$account_name, "\n")
    cat("Balance:", account$balance, "\n")
    cat("\n")
  }
  cat("Total Account Balance:", sum_account_balances(person), "\n")
}



##example
# Create a sample household
household <- create_household("Smith Family")

# Create a sample person
person <- create_person(
  name = "John Smith",
  birth_date = "1980-01-01",
  income = 50000,
  retirement_date = "2045-01-01",
  longevity = 85
)

# Add the person to the household
household <- add_person(household, person, "Dad")

# Add accounts to the person
household$person[[1]] <- add_account(household$persons[[1]], "Savings Account", 10000)
household$person[[1]] <- add_account(household$persons[[1]], "Checking Account", 2000)

# Display summary of the household
summary(household)

summary(person)





