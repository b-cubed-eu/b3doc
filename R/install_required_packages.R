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
      install.packages(pkg)
    } else {
      update.packages(pkg, ask = FALSE)
    }
  }
}
