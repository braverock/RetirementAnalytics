

# Sample data for testing
sample_returns <- xts(matrix(rnorm(1000), ncol=1), order.by=Sys.Date() - 999:0)
colnames(sample_returns) <- "Returns"

test_that("Adding existing portfolio to environment works", {
  add_existing_portfolio_to_env("test_portfolio", sample_returns)
  expect_true(exists("test_portfolio", envir = portfolio_env))
  expect_equal(portfolio_env$test_portfolio, sample_returns)
})

test_that("Adding return to portfolio in environment works", {
  add_existing_portfolio_to_env("test_portfolio", sample_returns)
  new_return <- 0.01
  add_return_to_env_portfolio("test_portfolio", new_return)
  updated_portfolio <- portfolio_env$test_portfolio

  expected_new_return <- xts(matrix(new_return, ncol = 1), order.by = Sys.Date())
  colnames(expected_new_return) <- colnames(updated_portfolio)

  expect_equal(last(updated_portfolio), expected_new_return)
})

test_that("Getting returns from portfolio in environment works", {
  add_existing_portfolio_to_env("test_portfolio", sample_returns)
  returns <- get_env_portfolio_returns("test_portfolio")
  expect_equal(returns, sample_returns)
})

test_that("Calculating total return of portfolio in environment works", {
  add_existing_portfolio_to_env("test_portfolio", sample_returns)
  total_return <- calculate_env_portfolio_total_return("test_portfolio")
  expect_equal(total_return, Return.cumulative(sample_returns))
})

test_that("Creating account works", {
  account <- create_account("test_account", "John Doe", balance = 1000, taxable = TRUE)
  expect_true(inherits(account, "account"))
  expect_equal(account$account_name, "test_account")
  expect_equal(account$person, "John Doe")
  expect_equal(account$balance, 1000)
  expect_true(account$taxable)
})

test_that("Adding portfolio to account works", {
  account <- create_account("test_account", "John Doe", balance = 1000, taxable = TRUE)
  account <- add_portfolio_to_account(account, "test_portfolio", sample_returns)
  expect_true("test_portfolio" %in% names(account$portfolios))
  expect_equal(account$portfolios[["test_portfolio"]], sample_returns)
})

test_that("Estimating portfolio return works", {
  account <- create_account("test_account", "John Doe", balance = 1000, taxable = TRUE)
  account <- add_portfolio_to_account(account, "test_portfolio", sample_returns)
  result <- estimate_portfolio_return(account, n_simulations = 100, rebalance = FALSE, withdraw_amount = 10)
  expect_true(is.list(result))
  expect_true("final_balances" %in% names(result))
  expect_true("probability_of_success" %in% names(result))
  expect_true(length(result$final_balances) == 100)
})
