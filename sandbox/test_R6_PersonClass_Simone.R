#### Test R6 oop person class: ####

library(R6)


Person <-R6Class("Person",
                 public = list(
                   name=NULL,
                   birth_date= NULL,
                   income = NULL,
                   retirement_date = NULL,
                   longevity= NULL,

                   #method start
                   initialize = function(name,birth_date,income,retirement_date,longevity){
                     self$name <- name
                     self$birth_date<-as.Date(birth_date)
                     self$income<- income
                     self$retirement_date <- as.Date(retirement_date)
                     self$longevity <- longevity
                   },

                   #calculate age
                   calculate_age = function(){
                     age = as.numeric(difftime( Sys.time(),self$birth_date,units = "weeks"))/52.25
                     return(floor( age))
                   },

                   #calculate retirement age
                   retirement_age = function() {
                     age_at_retirement <- as.numeric(difftime(self$retirement_date, self$birth_date, units = "weeks")) / 52.25
                     return(floor(age_at_retirement))
                   },

                   #summary method
                   summary = function(){
                     age <-self$calculate_age()
                     cat(
                       "Name:", self$name, "\n",
                       "Birth Date:", format(self$birth_date, "%Y-%m-%d"), "\n",
                       "Income:", self$income, "\n",
                       "Retirement Date:", format(self$retirement_date, "%Y-%m-%d"), "\n",
                       "Longevity:", self$longevity, " years\n",
                       "Current Age:", age, " years\n"
                     )
                   }

                 ))



#### Example usage  ####
person <- Person$new(name = "John Doe", birth_date = "1980-01-01", income = 60000, retirement_date = "2045-01-01", longevity = 85)
person$summary()
retirement_age <- person$retirement_age()
cat("Retirement Age:", retirement_age, "years\n")
