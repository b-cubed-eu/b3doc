#' Update front matter
#'
#' Updates the front matter and replaces the logo of a Markdown file on disk.
#'
#' @param md_file_path Path to the Markdown file on disk.
#' @param rmd_file_path Path to the R Markdown file, either a local path or a
#'   URL.
#' @param title Title of the article, to show on top of the page.
#' @param sidebar_label Title in the sidebar.
#' @param sidebar_order Number indicating the order of the article in the
#'   sidebar.
#' @param replace Named character vector indicating `key:value` pairs. All
#' `key` strings are replaced by their `value`.
#' @return Markdown file with updated front matter, written to disk.
#' @examples
#' \dontrun{
#' update_frontmatter(
#'   md_file_path = file.path(
#'     "output/src/content/docs/software/gcube/occurrence-process.md"
#'   ),
#'   rmd_file_path = file.path(
#'     "https://raw.githubusercontent.com/b-cubed-eu/gcube/refs/heads/main",
#'     "vignettes/articles/occurrence-process.Rmd"
#'   ),
#'   title = "2. Occurrence process",
#'   sidebar_label = "Occurrence process",
#'   sidebar_order = 2;
#'   replace = c("### Changing number of occurrences over time" =
#'               "### How to change the number of occurrences over time")
#' )
#' }
update_frontmatter <- function(md_file_path, rmd_file_path, title = NULL,
                               sidebar_label = NULL, sidebar_order = NULL,
                               replace = NULL) {
  if (!is.null(sidebar_order)) {
    if (!is.numeric(sidebar_order)) {
      cli::cli_abort(
        c(
          "{.arg sidebar_order} must be an integer."
        ),
        class = "b3doc_error_order_invalid"
      )
    }

    decimal_part <- strsplit(as.character(sidebar_order), split = "\\.")[[1]][2]
    if (!is.na(decimal_part) && nchar(decimal_part) > 0) {
      cli::cli_abort(
        c(
          "{.arg sidebar_order} must be an integer."
        ),
        class = "b3doc_error_order_invalid"
      )
    }
  }

  if (!is.null(replace) && !is.character(replace)) {
    cli::cli_abort(
      c(
        "{.arg replace} must be a named character vector.",
        "i" = "{.arg replace} is a {.cls {class(replace)}}."
      ),
      class = "b3doc_error_replace_class"
    )
  }

  if (!is.null(replace) && is.null(names(replace))) {
    cli::cli_abort(
      c(
        "{.arg replace} must be a named character vector.",
        "i" = "Please provide c('key' = 'value') pairs."
      ),
      class = "b3doc_error_replace_pairs"
    )
  }

  # Read markdown
  lines <- readLines(md_file_path)

  # Replace logo URL
  if (!is.null(replace)) {
    lines <- stringr::str_replace_all(lines, replace)
  }

  # Read front matter
  frontmatter_start <- which(lines == "---")[1]
  frontmatter_end <- which(lines == "---")[2]

  # Get current front matter if it exists
  if (is.na(frontmatter_start) || is.na(frontmatter_end)) {
    # No existing front matter
    frontmatter <- NULL

    # Set up for writing updated front matter
    first_lines <- "---"
    last_lines <- c("---\n", lines)
  } else {
    # Existing front matter
    frontmatter_char <- lines[(frontmatter_start + 1):(frontmatter_end - 1)]
    frontmatter <- yaml::yaml.load(frontmatter_char)

    # Set up for writing updated front matter
    first_lines <- lines[1:frontmatter_start]
    last_lines <- lines[frontmatter_end:length(lines)]
  }

  # Update front matter
  if (!is.null(title)) {frontmatter$title <- title}
  frontmatter$lastUpdated <- format(Sys.time(), "%Y-%m-%d")
  if (!is.null(sidebar_label)) {frontmatter$sidebar$label <- sidebar_label}
  if (!is.null(sidebar_order)) {
    frontmatter$sidebar$order <- as.integer(sidebar_order)
  }
  if (R.utils::isUrl(rmd_file_path)) {
    # Transform original file path from raw to edit mode to add as source
    rmd_file_path <- gsub(
      "raw.githubusercontent.com", "github.com", rmd_file_path
    )
    rmd_file_path <- gsub("/refs/heads/", "/blob/", rmd_file_path)
    frontmatter$source <- rmd_file_path
  }
  new_frontmatter <- yaml::as.yaml(frontmatter)
  # as.yaml() converts the date to a string with quotes; remove quotes
  new_frontmatter <- gsub(
    "lastUpdated: '(.*)'", "lastUpdated: \\1", new_frontmatter
  )
  # as.yaml() adds a a newline character at the end: remove newline
  new_frontmatter <- gsub("\n$", "", new_frontmatter)

  # Add updated front matter to markdown
  updated_lines <- c(
    first_lines,
    new_frontmatter,
    last_lines
  )

  # Write the updated file
  writeLines(updated_lines, md_file_path)
}
