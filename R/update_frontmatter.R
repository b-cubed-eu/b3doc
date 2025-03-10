#' Update front matter
#'
#' Updates the front matter of a markdown file on disk.
#'
#' @param md_file_path Path to the markdown file on disk.
#' @param rmd_file Path to the R Markdown file, either a local path or a URL.
#' @param order Number indicating the order of the article in the menu.
#' @return Markdown file with updated front matter, written to disk.
#' @noRd
#' @examples
#' md_file_path <- here::here(
#' "output", "src", "content", "docs", "software", "gcube",
#' "occurrence-process.md"
#' )
#' rmd_file <- "https://raw.githubusercontent.com/b-cubed-eu/gcube/refs/heads/main/vignettes/articles/occurrence-process.Rmd"
#' order <- 1
#' update_frontmatter(md_file_path, rmd_file, order)
update_frontmatter <- function(md_file_path, rmd_file, order) {
  if (!is.numeric(order)) {
    cli::cli_warn(
      c(
        "{.arg order} must be a number with maximum 1 decimal."
      ),
      class = "b3doc_error_order_invalid"
    )
  }

  decimal_part <- strsplit(as.character(order), split = "\\.")[[1]][2]
  if (!is.na(decimal_part) && nchar(decimal_part) > 1) {
    cli::cli_warn(
      c(
        "{.arg order} must be a number with maximum 1 decimal."
      ),
      class = "b3doc_error_order_invalid"
    )
  }

  # Transform original file path from raw to edit mode
  rmd_file <- gsub("raw.githubusercontent.com", "github.com", rmd_file)
  rmd_file <- gsub("/refs/heads/", "/blob/", rmd_file)

  # Read front matter
  lines <- readLines(md_file_path)
  frontmatter_start <- which(lines == "---")[1]
  frontmatter_end <- which(lines == "---")[2]
  frontmatter_char <- lines[(frontmatter_start + 1):(frontmatter_end - 1)]
  frontmatter <- yaml::yaml.load(frontmatter_char)

  # Update front matter
  frontmatter$lastUpdated <- format(Sys.time(), "%Y-%m-%d")
  frontmatter$sidebar$order <- order
  frontmatter$source <- rmd_file
  new_frontmatter <- yaml::as.yaml(frontmatter)
  # as.yaml() converts the date to a string with quotes; remove quotes
  new_frontmatter <-
    gsub("lastUpdated: '(.*)'", "lastUpdated: \\1", new_frontmatter)

  # Write front matter
  updated_lines <- c(
    lines[1:frontmatter_start],
    new_frontmatter,
    lines[frontmatter_end:length(lines)]
  )

  # Write the updated file
  writeLines(updated_lines, md_file_path)
}
