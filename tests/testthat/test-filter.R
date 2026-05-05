df <- tibble::tibble(
  name = c("Alice", "Bob", "Charlie", "Dave"),
  role = c("Data Analyst", "Team Lead - Data", "Senior Financial Officer", NA)
)

test_that("filter_in keeps matching rows", {
  result <- filter_in(df, role, "Data")
  expect_equal(nrow(result), 2)
  expect_true(all(grepl("Data", result$role)))
})

test_that("filter_out removes matching rows", {
  result <- filter_out(df, role, "Data")
  expect_equal(nrow(result), 2)
  expect_false(any(grepl("Data", result$role, ignore.case = TRUE), na.rm = TRUE))
})

test_that("filter_in then filter_out works in a pipe chain", {
  result <- df |> filter_in(role, "Data") |> filter_out(role, "financial")
  expect_equal(nrow(result), 2)
})

test_that("filter_out preserves NA rows by default", {
  result <- filter_out(df, role, "Data")
  expect_true(any(is.na(result$role)))
})

test_that("filter_out drops NA rows when na.rm = TRUE", {
  result <- filter_out(df, role, "Data", na.rm = TRUE)
  expect_false(any(is.na(result$role)))
})
