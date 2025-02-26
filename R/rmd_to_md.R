rmd_to_md <- function(rmd_file, output_dir) {
  md_name <- gsub(".Rmd$", "", basename(rmd_file))

  # Set input
  if (grepl("^http", rmd_file)) {
    # Correct mixed slash and backslash in file path (in Windows tempdir() uses
    # double backslashes as separator while file.path() uses regular slashes.)
    tempdir <- gsub("\\\\", "/", tempdir())
    input_file <- file.path(tempdir, basename(rmd_file))
    download.file(
      rmd_file,
      input_file
    )
  } else {
    input_file <- rmd_file
  }

  # Set output
  figures_dir <- file.path("public", output_dir)
  figures_dir_url <- paste0("/astro-docs/", output_dir, "/")
  markdown_dir <- file.path("src", "content", "docs", output_dir)
  markdown_file <- file.path(markdown_dir, paste0(md_name, ".md"))

  # Set options
  knitr::opts_knit$set(
    progress = TRUE,
    # Directory for figures
    base.dir = figures_dir,
    # Path to figures in markdown
    base.url = figures_dir_url
  )
  knitr::opts_chunk$set(
    # Subdirectory for figures, default figure/
    fig.path = paste(md_name, "-"),
    # Captions for figures, "" will keep existing, but not create new ones
    fig.cap = ""
  )

  # Knit
  knitr::knit(
    input = input_file,
    output = markdown_file
  )

  # TODO: reset knitr opts_knit
  # TODO: empty tempdir
  invisible(file)
}
