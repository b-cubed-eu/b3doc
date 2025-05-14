#' Update front matter
#'
#' Updates the front matter and replaces the logo of a Markdown file on disk.
#'
#' @param md_file_path Path to the Markdown file on disk.
#' @param rmd_file Path to the R Markdown file, either a local path or a URL.
#' @param title Title of the article, to show on top of the page.
#' @param sidebar_label Title in the sidebar.
#' @param sidebar_order Number indicating the order of the article in the
#'   sidebar.
#' @param logo URL to the logo file that replaces `"man/figures/logo.png"`.
#' @return Markdown file with updated front matter, written to disk.
#' @examples
#' \dontrun{
#' update_frontmatter(
#'   md_file_path = file.path(
#'     "output/src/content/docs/software/gcube/occurrence-process.md"
#'   ),
#'   rmd_file = file.path(
#'     "https://raw.githubusercontent.com/b-cubed-eu/gcube/refs/heads/main",
#'     "vignettes/articles/occurrence-process.Rmd"
#'   ),
#'   title = "2. Occurrence process",
#'   sidebar_label = "Occurrence process",
#'   sidebar_order = 2
#' )
#' }
update_frontmatter <- function(md_file_path, rmd_file, title = NULL,
                               sidebar_label = NULL, sidebar_order = NULL, logo = NULL) {
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

  # Read markdown
  lines <- readLines(md_file_path)

  # Replace logo URL
  if (!is.null(logo)) {
    lines <- gsub("man/figures/logo.png", logo, lines)
  }

  # Read front matter
  frontmatter_start <- which(lines == "---")[1]
  frontmatter_end <- which(lines == "---")[2]
  frontmatter_char <- lines[(frontmatter_start + 1):(frontmatter_end - 1)]
  frontmatter <- yaml::yaml.load(frontmatter_char)

  # Update front matter
  if (!is.null(title)) {frontmatter$title <- title}
  frontmatter$lastUpdated <- format(Sys.time(), "%Y-%m-%d")
  if (!is.null(sidebar_label)) {frontmatter$sidebar$label <- sidebar_label}
  if (!is.null(sidebar_order)) {
    frontmatter$sidebar$order <- as.integer(sidebar_order)
    }
  if (R.utils::isUrl(rmd_file)) {
    # Transform original file path from raw to edit mode to add as source
    rmd_file <- gsub("raw.githubusercontent.com", "github.com", rmd_file)
    rmd_file <- gsub("/refs/heads/", "/blob/", rmd_file)
    frontmatter$source <- rmd_file
  }
  new_frontmatter <- yaml::as.yaml(frontmatter)
  # as.yaml() converts the date to a string with quotes; remove quotes
  new_frontmatter <- gsub(
    "lastUpdated: '(.*)'", "lastUpdated: \\1", new_frontmatter
    )
  # as.yaml() adds a a newline character at the end: remove newline
  new_frontmatter <- gsub("\n$", "", new_frontmatter)

  # Write front matter
  updated_lines <- c(
    lines[1:frontmatter_start],
    new_frontmatter,
    lines[frontmatter_end:length(lines)]
  )

  # Write the updated file
  writeLines(updated_lines, md_file_path)
}
