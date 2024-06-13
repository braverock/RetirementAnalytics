#' Create a person
#'
#' This function creates a person object.
#'
#' @param name The name of the person.
#' @param birth_date The birth date of the person.
#' @param income The income of the person.
#' @param retirement_date The retirement date of the person.
#' @param longevity The expected longevity of the person.
#' @return A person object.
#' @export
create_person <- function(name, birth_date, income, retirement_date, longevity) {
  person <- list(
    name = name,
    birth_date = as.Date(birth_date),
    income = income,
    retirement_date = as.Date(retirement_date),
    longevity = longevity
  )
  structure(person, class = c("Person", "list"))
}

#' Calculate age
#'
#' This function calculates the age of a person.
#'
#' @param person A person object.
#' @return The age of the person.
#' @export
calculate_age <- function(person, ...) {
  UseMethod("calculate_age")
}

#' @export
calculate_age.Person <- function(person) {
  age <- as.numeric(difftime(Sys.Date(), person[["birth_date"]], units = "weeks")) / 52.25
  return(floor(age))
}

#' Calculate retirement age
#'
#' This function calculates the age of a person at retirement.
#'
#' @param person A person object.
#' @return The age of the person at retirement.
#' @export
calculate_retirement_age <- function(person, ...) {
  UseMethod("calculate_retirement_age")
}

#' @export
calculate_retirement_age.Person <- function(person) {
  age_at_retirement <- as.numeric(difftime(person[["retirement_date"]], person[["birth_date"]], units = "weeks")) / 52.25
  return(floor(age_at_retirement))
}

#' Summary of person
#'
#' This function provides a summary of the person.
#'
#' @param object A person object.
#' @return A list containing the summary information of the person.
#' @export
summary.Person <- function(object) {
  age <- calculate_age(object)
  summary_list <- list(
    Name = object[["name"]],
    Birth_Date = format(object[["birth_date"]], "%Y-%m-%d"),
    Income = object[["income"]],
    Retirement_Date = format(object[["retirement_date"]], "%Y-%m-%d"),
    Longevity = object[["longevity"]],
    Current_Age = age
  )
  return(summary_list)
}
