df <- tibble::tibble(
  name = c("Alice", "Bob", "Charlie", "Dave"),
  role = c("Data Analyst", "Team Lead - Data", "Senior Financial Officer", NA)
)

test_that("filter_in_str keeps matching rows", {
  result <- filter_in_str(df, role, "Data")
  expect_equal(nrow(result), 2)
  expect_true(all(grepl("Data", result$role)))
})

test_that("filter_out_str removes matching rows", {
  result <- filter_out_str(df, role, "Data")
  expect_equal(nrow(result), 2)
  expect_false(any(grepl("Data", result$role, ignore.case = TRUE), na.rm = TRUE))
})

test_that("filter_in_str then filter_out_str works in a pipe chain", {
  result <- df |> filter_in_str(role, "Data") |> filter_out_str(role, "financial")
  expect_equal(nrow(result), 2)
})

test_that("filter_out_str preserves NA rows by default", {
  result <- filter_out_str(df, role, "Data")
  expect_true(any(is.na(result$role)))
})

test_that("filter_out_str drops NA rows when na.rm = TRUE", {
  result <- filter_out_str(df, role, "Data", na.rm = TRUE)
  expect_false(any(is.na(result$role)))
})

test_that("filter_in is deprecated", {
  expect_warning(filter_in(df, role, "Data"), class = "lifecycle_warning_deprecated")
})

test_that("filter_out is deprecated", {
  expect_warning(filter_out(df, role, "Data"), class = "lifecycle_warning_deprecated")
})
