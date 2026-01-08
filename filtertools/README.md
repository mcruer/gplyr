# filtertools

Simple and intuitive data filtering tools for R.

## Overview

`filtertools` provides convenient wrapper functions around dplyr filtering operations for common data filtering tasks. These functions are designed to make interactive data exploration faster and more readable.

## Installation

You can install the development version of filtertools from GitHub:

```r
# install.packages("devtools")
devtools::install_github("mcruer/filtertools")
```

## Functions

### String Pattern Filtering

- `filter_in()` - Keep rows where a column contains a string pattern
- `filter_out()` - Remove rows where a column contains a string pattern
- `str_filter()` - Filter a character vector by pattern

### NA Filtering

- `filter_in_na()` - Keep rows where specified columns contain NA
- `filter_out_na()` - Remove rows where specified columns contain NA

### Numeric Filtering

- `filter_out_numeric()` - Remove rows with numeric values in selected columns

### Internal Helpers

- `filter_str()` - Internal string-based filtering helper

## Examples

```r
library(filtertools)
library(dplyr)

# Keep rows where name contains "John"
data %>% filter_in(name, "John")

# Remove rows where status contains "inactive"
data %>% filter_out(status, "inactive")

# Remove rows with NA in specific columns
data %>% filter_out_na(age, income)

# Keep only rows with NA in a column
data %>% filter_in_na(optional_field)
```

## License

MIT © Geordie McRuer
