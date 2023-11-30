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

#' Read data frame from clipboard
#'
#' @param header Whether the clipboard data includes a header. Defaults to TRUE.
#'
#' @return A tibble containing the clipboard data.
#' @export
clipped <- function(header = TRUE) {

  tryCatch({
    df <- clipr::read_clip_tbl(header = header) %>%
      tibble::tibble()
    return(df)
  }, warning = function(war) {
    warning("There was a warning: ", conditionMessage(war))
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

