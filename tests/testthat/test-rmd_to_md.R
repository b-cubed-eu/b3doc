test_that("rmd_to_md() writes .md to a directory", {
  skip_if_offline()
  rmd_file <- "https://raw.githubusercontent.com/b-cubed-eu/gcube/refs/heads/main/vignettes/articles/occurrence-process.Rmd"

  temp_dir <- tempdir()

  md_dir <- file.path(temp_dir, "src", "content", "docs", "r", "gcube")
  fig_dir <- file.path(temp_dir, "public", "r", "gcube")
  fig_url_dir <- paste0(temp_dir, "/astro-docs/", "r", "gcube", "/")
  order <- 1

  rmd_to_md(rmd_file, md_dir, fig_dir, fig_url_dir, order)

  expect_identical(
    list.files(md_dir),
    c("occurrence-process.md")
  )

  unlink(temp_dir, recursive = TRUE)
})

# test_that("rmd_to_md() writes figures to a directory", {
#   skip_if_offline()
#   rmd_file <- "https://raw.githubusercontent.com/b-cubed-eu/gcube/refs/heads/main/vignettes/articles/occurrence-process.Rmd"
#
#   temp_dir <- tempdir()
#
#   md_dir <- file.path(temp_dir, "src", "content", "docs", "r", "gcube")
#   fig_dir <- file.path(temp_dir, "public", "r", "gcube")
#   fig_url_dir <- paste0(temp_dir, "/astro-docs/r/gcube/")
#   order <- 1
#
#   # Add logging
#   message("Temporary directory: ", temp_dir)
#   message("Markdown directory: ", md_dir)
#   message("Figure directory: ", fig_dir)
#   message("Figure URL directory: ", fig_url_dir)
#
#   rmd_to_md(rmd_file, md_dir, fig_dir, fig_url_dir, order)
#
#   # Add logging for generated files
#   generated_md <- list.files(md_dir)
#   message("Generated md: ", paste(md_dir, collapse = ", "))
#   generated_fig <- list.files(fig_dir)
#   message("Generated figures: ", paste(generated_fig, collapse = ", "))
#
#   expect_equal(
#     list.files(fig_dir),
#     c(
#       "occurrence-process-unnamed-chunk-12-1.png",
#       "occurrence-process-unnamed-chunk-3-1.png",
#       "occurrence-process-unnamed-chunk-7-1.png",
#       "occurrence-process-unnamed-chunk-9-1.png"
#     )
#   )
#
#   unlink(temp_dir, recursive = TRUE)
# })

test_that("rmd_to_md() resets knitting options to the original settings", {
  skip_if_offline()
  rmd_file <- "https://raw.githubusercontent.com/b-cubed-eu/gcube/refs/heads/main/vignettes/articles/occurrence-process.Rmd"

  temp_dir <- tempdir()

  md_dir <- file.path(temp_dir, "src", "content", "docs", "r", "gcube")
  fig_dir <- file.path(temp_dir, "public", "r", "gcube")
  fig_url_dir <- paste0(temp_dir, "/astro-docs/r/gcube/")

  original_opts_knit <- knitr::opts_knit$get()
  rmd_to_md(rmd_file, md_dir, fig_dir, fig_url_dir, order)
  new_opts_knit <- knitr::opts_knit$get()

  expect_identical(original_opts_knit, new_opts_knit)

  unlink(temp_dir, recursive = TRUE)
})

test_that("rmd_to_md() adds the current date to the beginning of the markdown file", {
  skip_if_offline()
  rmd_file <- "https://raw.githubusercontent.com/b-cubed-eu/gcube/refs/heads/main/vignettes/articles/occurrence-process.Rmd"

  temp_dir <- tempdir()

  md_dir <- file.path(temp_dir, "src", "content", "docs", "r", "gcube")
  fig_dir <- file.path(temp_dir, "public", "r", "gcube")
  fig_url_dir <- paste0(temp_dir, "/astro-docs/r/gcube/")
  order <- 1

  rmd_to_md(rmd_file, md_dir, fig_dir, fig_url_dir, order)

  md_file <- file.path(md_dir, "occurrence-process.md")
  md_content <- readLines(md_file)
  expected_line <- paste0("Last update: ", Sys.Date())

  expect_true(grepl(expected_line, md_content[1]))

  unlink(temp_dir, recursive = TRUE)
})
