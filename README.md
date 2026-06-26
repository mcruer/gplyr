# gplyr

> Tidyverse-flavored helpers for the data wrangling you actually do every day.

`gplyr` is a personal toolkit of convenience functions that smooth over the
rough edges of day-to-day data work in R. It builds directly on the tidyverse
(`dplyr`, `tidyr`, `stringr`, `purrr`, `readr`) and adds the small,
high-leverage helpers that turn a five-line idiom into a single readable verb:
move data in and out of the clipboard, filter on strings without ceremony,
peek inside a pipe without breaking it, and clean up the messy columns that
real spreadsheets produce.

## Installation

```r
# install.packages("remotes")
remotes::install_github("mcruer/gplyr")
```

## The major wins

### Clipboard round-tripping: `clip_it()` and `paste_it()`

If your data lives half in R and half in Excel, these two are the headline
features. They let you move a data frame to and from the clipboard so you can
paste straight into a spreadsheet — or pull a spreadsheet selection straight
into a tibble — with no intermediate file.

```r
library(gplyr)
library(dplyr)

# Send a data frame to the clipboard, ready to paste into Excel.
# Returns the data frame invisibly, so it drops cleanly into a pipe.
mtcars %>%
  filter(cyl == 6) %>%
  clip_it()

# Copy a range in Excel, then pull it into a tibble:
df <- paste_it()

# No header row in the copied range? Name the columns yourself:
df <- paste_it(header = FALSE, header_names = c("id", "amount", "date"))
```

`paste_it()` parses tab-separated clipboard data the way spreadsheets actually
write it — multi-line cells, embedded quotes, and apostrophes (e.g.
`d'enfants`) are all handled correctly, and column names are preserved exactly
(spaces and all).

Two more clipboard helpers round out the set:

```r
# Read a single column off the clipboard as a plain vector.
ids <- clipped_vec()

# Convert a copied Excel column into a comma-separated, optionally quoted
# R list — perfect for pasting into dplyr::select() or c(). Writes the
# result back to the clipboard.
clipboard_to_list(quote = TRUE)

# Copied a Windows file path? Flip the backslashes, wrap it in quotes,
# and put it back on the clipboard, ready to paste into R.
fix_path()
```

### String filtering without the boilerplate: the `filter_*` family

Filtering rows by whether a column contains a string is one of the most common
things you do, and base dplyr makes you spell out `str_detect()` + `regex()`
every time. The `filter_*` family makes it a one-liner — case-insensitive by
default, NA-safe by default.

```r
# Keep rows where `name` contains "smith" (case-insensitive)
df %>% filter_in_str(name, "smith")

# Drop rows where `name` contains "test"
df %>% filter_out_str(name, "test")

# Options when you need them:
df %>% filter_in_str(name, "smith", ignore_case = FALSE, drop.col = TRUE, na.rm = TRUE)
```

The family extends to the other filtering chores that don't have a clean
tidyverse idiom:

```r
# Rows where the selected column(s) are NA — or aren't.
df %>% filter_in_na(end_date)
df %>% filter_out_na(start_date, end_date, if_any_or_all = "if_any")

# Rows where a column holds non-numeric junk (great for spotting
# stray text in a column that should be all numbers).
df %>% filter_out_numeric(amount)
```

> Note: `filter_in()` / `filter_out()` are the original names and are now
> deprecated in favor of the explicit `filter_in_str()` / `filter_out_str()`.

There's also `str_filter()`, the vector-level cousin — give it a character
vector and a regex and get back the matching elements (with `negate` to invert):

```r
str_filter(c("apple", "banana", "cherry"), "a")          # all three
str_filter(c("apple", "banana", "cherry"), "^a", negate = TRUE)  # banana, cherry
```

### Peeking inside a pipe: `peek()`, `peek_n()`, `peek_v()`

Debugging a long pipe usually means tearing it apart to inspect the middle.
The `peek` functions let you look at an intermediate result — print it, view it,
transform it — while passing the original object straight through untouched.

```r
mtcars %>%
  filter(cyl == 6) %>%
  peek(~ head(.x)) %>%     # print the first few rows, then keep going
  mutate(kpl = mpg * 0.425) %>%
  peek_n(n = 50) %>%       # print up to 50 rows
  peek_v()                 # pop it open in the RStudio viewer
```

## More helpers worth knowing about

### Cell-level get and set

```r
pull_cell(df, amount, row = 3)        # the value in one cell
mutate_cell(df, amount, 3, 99.95)     # set one cell, return the frame
```

### Cleaning messy real-world data

```r
fix_dates(c("01/02/2003", "2003-02", "44197"))  # many formats -> "YYYY-MM-DD"
parse_guess_all(df)                              # re-guess every column's type
to_number(df, c(a, b))                           # coerce columns to numeric
to_character(df)                                 # coerce columns to character
replace_with_na(x, "n/a")                        # matched strings -> NA
na_to_T(x); na_to_F(x)                           # NA -> TRUE / FALSE
sum_rna(1, 2, NA, 4)                             # sum(), na.rm = TRUE by default
```

### Inspecting and summarizing

```r
na_review(df)        # per-column class, row count, and NA count/percentage
count_all(df)        # value counts for every column, side by side
is_in_df(df, col)    # does this column exist?
add_index(df)        # add a 1..n index column (group-aware)
```

### Reshaping and joining

```r
# Apply a function across many columns in one move.
quickm(df, c(a, b), ~ .x * 2)
quicks(df, c(a, b), mean)

# Row-wise mutate that actually reads well.
df %>% mutate_rowwise(total, starts_with("q"), sum)

# Left join without the .x/.y suffix mess — overlapping columns are dropped.
left_join_unique(df1, df2, by = "id")

# Fill NAs in one column from another, then drop the donor column.
merge_if_na(df, target, fallback)

# Let non-NA values in df2 supersede df1, keyed on a unique id.
merge_supersede(df1, df2, id, value)

# Split a data frame into a named list by a column's values.
listify_df(df, group)

# Tag rows into categories based on string matches in a column.
add_category(df, source, c("here", "target2"), c("One", "Two"))
```

### List-columns

```r
# Mutate selected columns *inside* each nested tibble.
df %>% nested_mutate(data, a, as.character)

# unnest() that keeps empty-list rows by default (no silent data loss).
unnest_keep(df, y)
```

### Conditional pipelines: `pif()` / `%pif%`

Apply a step only when a condition holds, without breaking the pipe.

```r
do_scale <- TRUE

tibble(x = 1:3) %>%
  pif(do_scale ~ mutate(., y = x * 2))     # runs only if do_scale is TRUE

# Infix form (no extra args):
tibble(x = 1:3) %pif% (do_scale ~ mutate(., y = x * 2))
```

### Odds and ends

```r
pipe_assign(df, "snapshot", ~ filter(.x, cyl == 6))  # save a midpipe copy
cloak(value, secrets = list(note = "hi"))            # attach hidden metadata
uncloak(x)                                           # read it back
combine_r_files("R/", "all_code.txt")                # concatenate a folder of .R files
```

## Dependencies

`gplyr` stands on the shoulders of `dplyr`, `tidyr`, `stringr`, `purrr`,
`readr`, `tibble`, `rlang`, `magrittr`, `clipr`, `janitor`, `scales`,
`officer`, `lifecycle`, and `listful`.

## License

MIT © geo.mcruer
