
test_that("Person class methods work correctly", {
  # Create a Person object
  person <- create_person(name = "John Doe", birth_date = "1980-01-01", income = 50000, retirement_date = "2045-01-01", longevity = 85)

  # Test calculate_age method
  age <- calculate_age(person)
  expect_equal(age, floor(as.numeric(difftime(Sys.Date(), as.Date("1980-01-01"), units = "weeks")) / 52.25))

  # Test retirement age calculation
  retirement_age <- calculate_retirement_age(person)
  expect_equal(retirement_age, floor(as.numeric(difftime(as.Date("2045-01-01"), as.Date("1980-01-01"), units = "weeks")) / 52.25))

})

test_that("Household class methods work correctly", {
  # Create Person objects
  person1 <- create_person(name = "John Doe", birth_date = "1980-01-01", income = 50000, retirement_date = "2045-01-01", longevity = 85)
  person2 <- create_person(name = "Jane Doe", birth_date = "1985-01-01", income = 60000, retirement_date = "2050-01-01", longevity = 90)

  # Create a Household object and add persons
  household <- create_household(household_name = "Doe Household")
  household <- add_person(household, person1, "Spouse 1")
  household <- add_person(household, person2, "Spouse 2")

  # Test sum_income method
  total_income <- sum_income(household)
  expect_equal(total_income, 110000)


})
