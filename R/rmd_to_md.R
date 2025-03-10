#' Convert external R Markdown to Markdown
#'
#' Converts an R Markdown (.Rmd) file to a Markdown (.md) file. Both local and
#' remote .Rmd files can be handled. The date this function is called upon is
#' added to the beginning of the Markdown file.
#'
#' @param rmd_file Path to the R Markdown file, either a local path or a URL.
#' @param md_dir Path to local directory to Markdown file to. If it doesn't
#'   exist it will be created.
#' @param fig_dir Path to local directory to save figures to.
#' @param fig_url_dir URL path that will be used to link to the figures in the
#'   markdown output.
#' @param order Order of the article in the menu.
#'
#' @return Markdown file and figures written do disk.
#' @export
#'
#' @examples
#' rmd_file <- "https://raw.githubusercontent.com/b-cubed-eu/gcube/refs/heads/main/vignettes/articles/occurrence-process.Rmd"
#'
#' md_dir <- file.path("output", "src", "content", "docs", "software", "gcube")
#' fig_dir <- file.path("output", "public", "software", "gcube")
#' fig_url_dir <- "/software/gcube/"
#' order <- 1
#' rmd_to_md(rmd_file, md_dir, fig_dir, fig_url_dir, order)
#'
#' # Clean up (don't do this if you want to keep your files)
#' unlink("output", recursive = TRUE)
rmd_to_md <- function(rmd_file, md_dir, fig_dir, fig_url_dir, order) {
  # Get the basename of the input rmd_file without extension.
  md_name <- fs::path_file(fs::path_ext_remove(rmd_file))

  # Set input
  if (R.utils::isUrl(rmd_file)) {
    # Store the rmd_file in a subdir of the OS tempdir
    temp_dir <- fs::path_temp("rmd_file")
    # Create the temp_dir if it doesn't exist from an earlier run
    if(!fs::dir_exists(temp_dir)){fs::dir_create(temp_dir)}
    temp_rmd_path <-
      fs::file_temp(pattern = md_name,
                    tmp_dir = temp_dir,
                    ext = ".Rmd")

    utils::download.file(rmd_file,
                         temp_rmd_path)

    input_file <- temp_rmd_path
  } else {
    # The file is local.
    input_file <- rmd_file
  }

  # Set output
  md_file_path <- fs::path_ext_set(
    path = fs::path(md_dir, md_name),
    ext = ".md")

  # Create directory for markdown file
  if (!fs::dir_exists(md_dir)) {
    fs::dir_create(md_dir, recurse = TRUE)
  }

  # Save the original knitting options
  original_opts_knit <- knitr::opts_knit$get()

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
    output = md_file_path
  )

  # Update front matter
  update_frontmatter(
    md_file_path,
    rmd_file,
    order = order
  )

  # Reset knitting options to the original settings
  knitr::opts_knit$set(original_opts_knit)

  # Empty the temporary directory and it's contents
  fs::dir_delete(temp_dir)
}
