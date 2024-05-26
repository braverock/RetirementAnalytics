#### Test for S3 oop Person class: ####

# constructor for person class
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

# Generic function to calculate age

calculate_age<-function(person,...){
  UseMethod("calculate_age")
}

#specific method for Person class
calculate_age.Person<-function(person,...){
  age <- as.numeric(difftime(Sys.Date(),person$birth_date,units = "weeks"))/ 52.25
  return(floor(age))
}

#Generic function to calculate retirement age
retirement <- function(person,retirement_date,...){
  UseMethod("retirement")
}

#specific method for Person class
retirement.Person<-function(person,...){
  age_at_retirement<-as.numeric(difftime(person$retirement_date,person$birth_date,units = "weeks"))/52.25
  return(floor(age_at_retirement))

}


#summary function for Person

summary.Person<-function(object,...){
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



#### Example usage ####
person <- create_person(name = "John Doe", birth_date = "1980-01-01", income = 60000, retirement_date = "2045-01-01", longevity = 85)

# Test summary function
summary(person)

# Test calculate_age function
cat("Calculated Age: ", calculate_age(person), " years\n")

# Test retirement function
cat("Age at Retirement: ", retirement(person), " years\n")





