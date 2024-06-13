#' Create a household
#'
#' This function creates a household object.
#'
#' @param household_name The name of the household.
#' @param persons A list of persons in the household.
#' @return A household object.
#' @export

#### Constructor ####

create_household <- function(household_name, persons = list()) {
  stopifnot(is.character(household_name))
  household <- list(
    household_name = household_name,
    persons = persons
  )
  structure(household, class = c("household", "list"))
}

#' Add a person to a household
#'
#' This function adds a person to the household and assigns a role.
#'
#' @param household The household object.
#' @param person The person object to add.
#' @param role The role of the person in the household.
#' @return The updated household object.
#' @export
add_person <- function(household, person, role) {
  person$role <- role
  household$persons <- append(household$persons, list(person))
  return(household)
}

#' Sum income of household
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
sum_income.household <- function(household, ...) {
  total_income <- sum(sapply(household$persons, function(person) person$income))
  return(total_income)
}

#' Summary of household
#'
#' This function provides a summary of the household.
#'
#' @param object The household object.
#' @export
summary.household <- function(object, ...) {
  cat("Household Name:", object$household_name, "\n")
  cat("Number of Persons:", length(object$persons), "\n")
  cat("Persons Details:\n")
  for (i in 1:length(object$persons)) {
    person <- object$persons[[i]]
    cat("Role:", person$role, "\n")
    summary.person(person)
    cat("\n")
  }
  cat("Total Household Income:", sum_income(object), "\n")
}
