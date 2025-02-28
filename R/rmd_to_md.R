#' Convert external R Markdown to Markdown
#'
#' Converts an R Markdown (.Rmd) file to a Markdown (.md) file. Both local and
#' remote .Rmd files can be handled. The date this function is called upon is
#' added to the beginning of the Markdown file.
#'
#' @param rmd_file Path to the R Markdown file, either a local path or a URL.
#' @param md_dir Path to local directory to Markdown file to.
#' @param fig_dir Path to local directory to save figures to.
#' @param fig_url_dir URL path that will be used to link to the figures in the
#' markdown output.
#'
#' @return Markdown file and figures written do disk.
#' @export
#'
#' @examples
#' rmd_file <- "https://raw.githubusercontent.com/b-cubed-eu/gcube/refs/heads/main/vignettes/articles/occurrence-process.Rmd"
#'
#' md_dir <- file.path("output", "src", "content", "docs", "r", "gcube")
#' fig_dir <- file.path("output", "public", "r", "gcube")
#' fig_url_dir <- paste0("/astro-docs/", "r", "gcube", "/")
#' rmd_to_md(rmd_file, md_dir, fig_dir, fig_url_dir)
#'
#' # Clean up (don't do this if you want to keep your files)
#' unlink("output", recursive = TRUE)
rmd_to_md <- function(rmd_file, md_dir, fig_dir, fig_url_dir) {
  md_name <- gsub(".Rmd$", "", basename(rmd_file))

  # Set input
  if (grepl("^http", rmd_file)) {
    # Correct mixed slash and backslash in file path (in Windows tempdir() uses
    # double backslashes as separator while file.path() uses regular slashes.)
    tempdir <- gsub("\\\\", "/", tempdir())
    input_file <- file.path(tempdir, basename(rmd_file))
    utils::download.file(
      rmd_file,
      input_file
    )
  } else {
    input_file <- rmd_file
  }

  # Set output
  markdown_file <- file.path(md_dir, paste0(md_name, ".md"))

  # Create directory for markdown file
  if (!dir.exists(md_dir)) {
    dir.create(md_dir, recursive = TRUE)
  }

  # Set options
  knitr::opts_knit$set(
    progress = TRUE,
    # Directory for figures
    base.dir = fig_dir,
    # Path to figures in markdown
    base.url = fig_url_dir
  )
  knitr::opts_chunk$set(
    # Subdirectory for figures, default figure/
    fig.path = paste0(md_name, "-"),
    # Captions for figures, "" will keep existing, but not create new ones
    fig.cap = ""
  )

  # Knit
  knitr::knit(
    input = input_file,
    output = markdown_file
  )

  # Add the current date to the beginning of the markdown file
  current_date <- Sys.Date()
  markdown_content <- readLines(markdown_file)
  markdown_content <- c(paste0("Last update: ", current_date), "", markdown_content)
  writeLines(markdown_content, markdown_file)

  # TODO: reset knitr opts_knit

  # Empty the temporary directory
  # unlink(tempdir, recursive = TRUE) # Uncomment to make test work
}
