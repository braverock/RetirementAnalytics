#' Create a Person
#'
#' This function creates a person object.
#'
#' @param name The name of the person.
#' @param birth_date The birth date of the person.
#' @param income The income of the person.
#' @param retirement_date The retirement date of the person.
#' @param longevity The expected longevity of the person.
#' @return A Person object.
#' @export

create_person <- function(name,birth_date,income,retirement_date,longevity){
  person <- list(
    name = name,
    birth_date = as.Date(birth_date),
    income=income,
    retirement_date= as.Date(retirement_date),
    longevity=longevity
  )
  class(person) <- "Person"
  return(person)
}

#' Calculate Age
#'
#' This function calculates the age of a person.
#'
#' @param person A Person object.
#' @return The age of the person.
#' @export

calculate_age<-function(person,...){
  UseMethod("calculate_age")
}

#' @export
calculate_age.Person<-function(person){
  age <- as.numeric(difftime(Sys.Date(),person$birth_date,units = "weeks"))/ 52.25
  return(floor(age))
}

#' Calculate Retirement Age
#'
#' This function calculates the age of a person at retirement.
#'
#' @param person A Person object.
#' @param retirement_date The retirement date.
#' @return The age of the person at retirement.
#' @export
retirement <- function(person,retirement_date,...){
  UseMethod("retirement")
}

#' @export
retirement.Person<-function(person,retirement_date){
  age_at_retirement<-as.numeric(difftime(person$retirement_date,person$birth_date,units = "weeks"))/52.25
  return(floor(age_at_retirement))

}


#' Summary of Person
#'
#' This function provides a summary of the person.
#'
#' @param object A Person object.
#' @export

summary.Person<-function(object){
  age<-calculate_age(object)
  summary <- cat(
    "Name:", object$name, "\n",
    "Birth Date:", format(object$birth_date, "%Y-%m-%d"), "\n",
    "Income:", object$income, "\n",
    "Retirement_date:", format(object$retirement_date, "%Y-%m-%d"), "\n",
    "Longevity:", object$longevity, "years\n",
    "Current Age:", age, "years\n"
  )

}









