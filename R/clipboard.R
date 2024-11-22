#' Copy data frame to clipboard
#'
#' @param df The data frame or tibble to copy.
#' @param col_names Whether to include column names. Defaults to TRUE.
#' @param sep The separator to use between fields. Defaults to "\\t" (tab-separated).
#' @param na The string to use for NA values. Defaults to an empty string.
#' @param clipboard.size The size of the clipboard in powers of 2. Defaults to 16.
#'
#' @return None. The data is copied to the clipboard.
#' @export
clip_it <- function(df, col_names = TRUE, sep = "\t", na = "", clipboard.size = 16) {

  if (!is.data.frame(df) && !tibble::is_tibble(df)) {
    stop("Input must be a data frame or tibble.")
  }

  clipboard.in <- stringr::str_c("clipboard", 2^clipboard.size, sep = "-")

  tryCatch({
    utils::write.table(df, clipboard.in, sep=sep, row.names=FALSE, col.names=col_names, na = na)
  }, warning = function(war) {
    warning("There was a warning: ", conditionMessage(war))
  }, error = function(err) {
    stop("An error occurred: ", conditionMessage(err))
  })
}

#' Read Data Frame from Clipboard
#'
#' This function reads data from the clipboard and returns a tibble. It can handle data with or without a header row and allows you to specify custom column names.
#'
#' @param header Logical value indicating whether the clipboard data includes a header row. Defaults to \code{TRUE}.
#' @param header_names Optional character vector specifying custom column names. If provided, the length must match the number of columns in the data.
#'
#' @return A tibble containing the clipboard data.
#'
#' @details
#' The \code{paste_it} function reads tab-separated values from the clipboard using \code{readr::read_tsv()} and returns a tibble. The function ensures that:
#' \itemize{
#'   \item Leading and trailing whitespace in data fields is preserved (\code{trim_ws = FALSE}).
#'   \item No characters are treated as quoting characters (\code{quote = ""}), which prevents issues with quotes and apostrophes in the data.
#'   \item Column names are not altered (\code{name_repair = "minimal"}), preserving spaces and special characters.
#' }
#' If \code{header} is \code{TRUE}, the first row of the clipboard data is used as column names. If \code{header_names} is provided, these names replace the existing column names.
#'
#' @examples
#' \dontrun{
#' # Example 1: Reading data with a header row
#' # Copy data from Excel with a header row to the clipboard, then run:
#' df <- paste_it()
#'
#' # Example 2: Reading data without a header row and specifying custom column names
#' df <- paste_it(header = FALSE, header_names = c("Column1", "Column2", "Column3"))
#'
#' # Example 3: Reading data with a header row and replacing column names
#' df <- paste_it(header = TRUE, header_names = c("NewName1", "NewName2", "NewName3"))
#' }
#'
#' @importFrom readr read_tsv
#' @importFrom clipr read_clip
#' @importFrom tibble as_tibble
#' @importFrom purrr set_names
#' @export
paste_it <- function(header = TRUE, header_names = NULL) {
  tryCatch({
    # Ensure required packages are available
    if (!requireNamespace("readr", quietly = TRUE)) {
      stop("The 'readr' package is required but not installed. Please install it using install.packages('readr').")
    }
    if (!requireNamespace("clipr", quietly = TRUE)) {
      stop("The 'clipr' package is required but not installed. Please install it using install.packages('clipr').")
    }
    if (!requireNamespace("tibble", quietly = TRUE)) {
      stop("The 'tibble' package is required but not installed. Please install it using install.packages('tibble').")
    }
    if (!requireNamespace("purrr", quietly = TRUE)) {
      stop("The 'purrr' package is required but not installed. Please install it using install.packages('purrr').")
    }

    # Read clipboard content
    clipboard_content <- clipr::read_clip()
    # Combine into a single string
    clipboard_text <- paste(clipboard_content, collapse = "\n")

    # Read data with 'col_names' set according to 'header' argument
    df <- readr::read_tsv(
      I(clipboard_text),
      col_names = header,
      quote = "",
      name_repair = "minimal",
      trim_ws = FALSE,
      skip_empty_rows = TRUE,
      show_col_types = FALSE
    )

    # Convert to tibble
    df <- tibble::as_tibble(df)

    # If 'header_names' is provided, assign names to columns
    if (!is.null(header_names)) {
      if (length(header_names) != ncol(df)) {
        stop("Length of 'header_names' does not match the number of columns in the data.")
      }
      df <- purrr::set_names(df, header_names)
    }

    return(df)
  }, warning = function(war) {
    warning("There was a warning: ", conditionMessage(war))
    return(NULL)
  }, error = function(err) {
    stop("An error occurred: ", conditionMessage(err))
  })
}


#' Read vector from clipboard
#'
#' @param header Whether the clipboard data includes a header. Defaults to FALSE.
#'
#' @return A vector containing the clipboard data.
#' @export
clipped_vec <- function(header = FALSE) {
  tryCatch({
    # Read data from clipboard as a tibble
    df <- clipr::read_clip_tbl(header = header)

    # Convert the tibble to a vector
    # If there's a header, we take the first column.
    # If no header, we convert the entire tibble to a vector.
    if (header) {
      vec <- df[[1]]
    } else {
      vec <- unlist(df) %>% unname()
    }

    return(vec)
  }, warning = function(war) {
    warning("There was a warning: ", conditionMessage(war))
  }, error = function(err) {
    stop("An error occurred: ", conditionMessage(err))
  })
}

#' Convert Backslashes to Forward Slashes in Clipboard Paths and Quote Them
#'
#' This function reads a string from the clipboard, replaces all backslashes (\\)
#' with forward slashes (/), wraps the modified string in double quotes, and
#' then writes it back to the clipboard. It's intended to convert file paths
#' copied from the file system in Windows to a format suitable for use in R.
#' The function throws an error if the clipboard content is not a single string.
#'
#' @return Invisible NULL. The function's primary effect is to alter the clipboard content.
#'
#' @examples
#' \dontrun{
#'   fix_path() # Reads the clipboard, modifies its content, and updates the clipboard.
#' }
#' @export
#' @importFrom clipr read_clip write_clip
#' @importFrom stringr str_replace_all
fix_path <- function() {
  # Read the current clipboard content
  clipboard_content <- clipr::read_clip()

  # Check if the clipboard content is a string
  if (!is.character(clipboard_content) || length(clipboard_content) != 1) {
    stop("Clipboard content is not a single string.")
  }

  # Replace all backslashes with forward slashes
  modified_content <- stringr::str_replace_all(clipboard_content, "\\\\", "/")

  # Wrap the modified content in quotation marks
  quoted_content <- paste0('"', modified_content, '"')

  # Write the quoted content back to the clipboard
  clipr::write_clip(quoted_content)
}


#' Process Clipboard Content into a List for R Code
#'
#' Converts data copied to the clipboard (from Excel or similar sources) into a formatted list suitable for use in R code.
#' The output is a comma-separated list, optionally quoted, with each element placed on a new line.
#' The processed list is written back to the clipboard for easy pasting.
#'
#' @param quote Logical. Whether to wrap each cell in double quotes. Defaults to \code{FALSE}.
#' @param trailing_comma Logical. Whether to add a trailing comma after the last element. Defaults to \code{TRUE}.
#'
#' @return Invisibly returns the formatted text as a character string. The text is also written to the clipboard for easy pasting.
#'
#' @details
#' The function reads data from the clipboard, splits it into individual elements based on tabs or newlines,
#' trims whitespace, optionally wraps elements in quotes, and appends commas to each element.
#' The resulting text is formatted for use in R code and written back to the clipboard for convenience.
#'
#' This function is particularly useful for preparing column names or values for use in functions like \code{dplyr::select()}.
#'
#' @examples
#' \dontrun{
#' # Copy a row or column of data from Excel, then run:
#' clipboard_to_list()
#'
#' # Wrap each cell in quotes:
#' clipboard_to_list(quote = TRUE)
#'
#' # Skip the trailing comma after the last element:
#' clipboard_to_list(trailing_comma = FALSE)
#' }
#'
#' @importFrom clipr read_clip write_clip
#' @importFrom stringr str_split str_trim str_c
#' @export
clipboard_to_list <- function(quote = FALSE, trailing_comma = TRUE) {
  # Ensure required packages are available
  if (!requireNamespace("clipr", quietly = TRUE)) {
    stop("The 'clipr' package is required but not installed. Please install it using install.packages('clipr').")
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("The 'stringr' package is required but not installed. Please install it using install.packages('stringr').")
  }

  # Read from the clipboard using clipr
  clipboard_content <- clipr::read_clip()

  # Combine the clipboard content into a single string
  clipboard_text <- paste(clipboard_content, collapse = "\n")

  # Split the clipboard contents into individual cells using stringr
  # Use both tab and newline as delimiters to handle rows and columns
  cells <- stringr::str_split(clipboard_text, "\\t|\\n")[[1]]

  # Remove empty strings (in case of extra newlines)
  cells <- cells[cells != ""]

  # Trim whitespace from each cell
  cells <- stringr::str_trim(cells)

  # Optionally add quotes around each cell
  if (quote) {
    cells <- stringr::str_c("\"", cells, "\"")
  }

  # Add a comma to all cells (if trailing_comma is TRUE)
  if (trailing_comma) {
    cells <- paste0(cells, ",")
  } else {
    # Add a comma to all but the last cell
    cells <- ifelse(seq_along(cells) < length(cells), paste0(cells, ","), cells)
  }

  # Place each cell on a new line
  output <- paste(cells, collapse = "\n")

  # Print the output to show what's happening
  cat(output, sep = "\n")

  # Write the modified text back to the clipboard
  clipr::write_clip(output)
}
