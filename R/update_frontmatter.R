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
  frontmatter$comment <- paste0("This file is generated from ", original_file_path, ". Please edit that file.")
  frontmatter$order <- order
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
