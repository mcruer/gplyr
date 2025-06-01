#' Combine all R files in a folder into a single text or Word document
#'
#' This function combines all `.R` files in a given folder into a single output file.
#' The output can be either a plain text file or a Word document.
#'
#' @param input_folder Path to the folder containing R files.
#' @param output_file Path to the output file (must end in `.txt` or `.docx`).
#' @param format Output format. Either `"txt"` (default) or `"docx"`.
#'
#' @return Invisibly returns the path to the output file.
#' @export
#'
#' @importFrom purrr walk
#' @importFrom officer read_docx body_add_par
combine_r_files <- function(input_folder, output_file, format = c("txt", "docx")) {
  format <- match.arg(format)

  r_files <- list.files(input_folder, pattern = "\\.R$", full.names = TRUE)

  if (format == "txt") {
    con <- file(output_file, open = "w")
    purrr::walk(r_files, function(file) {
      writeLines(paste0("\n### File: ", basename(file), " ###\n"), con)
      writeLines(strrep("-", 80), con)
      content <- readLines(file, warn = FALSE)
      writeLines(content, con)
      writeLines(strrep("=", 80), con)
    })
    close(con)

  } else if (format == "docx") {
    doc <- read_docx()

    purrr::walk(r_files, function(file) {
      header <- paste0("### File: ", basename(file), " ###")
      separator <- strrep("-", 80)
      content <- readLines(file, warn = FALSE)
      chunk <- paste(content, collapse = "\n")
      footer <- strrep("=", 80)

      doc <- doc |>
        body_add_par(header, style = "heading 2") |>
        body_add_par(separator, style = "Normal") |>
        body_add_par(chunk, style = "Normal") |>
        body_add_par(footer, style = "Normal")
    })

    print(doc, target = output_file)
  }

  message("Files combined into ", output_file)
  invisible(output_file)
}
