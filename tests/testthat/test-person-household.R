
test_that("Person class methods work correctly", {
  # Create a Person object
  person <- create_person(name = "John Doe", birth_date = "1980-01-01", income = 50000, retirement_date = "2045-01-01", longevity = 85)

  # Test calculate_age method
  age <- calculate_age(person)
  expect_equal(age, floor(as.numeric(difftime(Sys.Date(), as.Date("1980-01-01"), units = "weeks")) / 52.25))

  # Test retirement age calculation
  retirement_age <- retirement(person)
  expect_equal(retirement_age, floor(as.numeric(difftime(as.Date("2045-01-01"), as.Date("1980-01-01"), units = "weeks")) / 52.25))

  # Test summary output
  expect_output(summary(person), "John Doe")
  expect_output(summary(person), "1980-01-01")
  expect_output(summary(person), "50000")
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

  # Test summary output for household
  expect_output(summary(household), "Doe Household")
  expect_output(summary(household), "John Doe")
  expect_output(summary(household), "Jane Doe")
  expect_output(summary(household), "110000")
})
