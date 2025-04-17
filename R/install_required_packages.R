#' Install required packages
#'
#' Installs or updates the packages used in an R Markdown file with `library()`
#' and `require()` calls.
#'
#' @param input_file Path to the R Markdown file
#'
#' @returns A character vector with the names of the packages used
#' @export
install_required_packages <- function(input_file) {
  # Read the Rmd file from the URL
  rmd_content <- readLines(input_file, warn = FALSE)

  # Extract lines with library() or require() calls
  package_lines <- grep("library\\(|require\\(", rmd_content, value = TRUE)

  # Extract package names
  package_names <- unique(gsub(".*(library|require)\\(([^)]+)\\).*", "\\2", package_lines))

  # Install or update packages
  for (pkg in package_names) {
    if (!require(pkg, character.only = TRUE)) {
      utils::install.packages(pkg, repos = c('https://b-cubed-eu.r-universe.dev', 'https://cloud.r-project.org'))
    } else {
      utils::update.packages(pkg, repos = c('https://b-cubed-eu.r-universe.dev', 'https://cloud.r-project.org'))
    }
  }

  # Return the list of packages used
  return(package_names)
}
