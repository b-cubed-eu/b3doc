#' Update front matter
#'
#' Updates the front matter of a markdown file on disk.
#'
#' @param file_path Path to the markdown file on disk.
#' @param original_file_path URL of the R Markdown file, either a local path or a URL.
#' @param order Order of the article in the menu.
#' @return Markdown file with updated front matter, written to disk.
#' @noRd
#' @examples
#' file_path <- here::here("output", "src", "content", "docs", "software", "gcube", "occurrence-process.md")
#' rmd_file <- "https://github.com/b-cubed-eu/gcube/blob/main/vignettes/articles/occurrence-process.Rmd"
#' order = 1
#' update_frontmatter(file_path, rmd_file, order)
update_frontmatter <- function(file_path, original_file_path, order) {
  # Check that the required arguments are present
  if(missing(order)){stop("The 'order' argument is required.")}

  # Transform original file path from raw to edit mode
  original_file_path <- gsub("raw.githubusercontent.com", "github.com", original_file_path)
  original_file_path <- gsub("/refs/heads/", "/blob/", original_file_path)

  # Read front matter
  lines <- readLines(file_path)
  frontmatter_start <- which(lines == "---")[1]
  frontmatter_end <- which(lines == "---")[2]
  frontmatter_char <- lines[(frontmatter_start + 1):(frontmatter_end - 1)]
  frontmatter <- yaml::yaml.load(frontmatter_char)

  # Update front matter
  frontmatter$lastUpdated <- format(Sys.time(), "%Y-%M-%d")
  frontmatter$order <- order
  frontmatter$source <- original_file_path
  new_frontmatter <- yaml::as.yaml(frontmatter)

  # Write front matter
  updated_lines <- c(
    lines[1:frontmatter_start],
    new_frontmatter,
    lines[frontmatter_end:length(lines)]
  )

  # Write the updated file
  writeLines(updated_lines, file_path)
}
