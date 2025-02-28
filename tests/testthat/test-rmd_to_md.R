test_that("rmd_to_md() writes .md to a directory", {
  skip_if_offline()
  rmd_file <- "https://raw.githubusercontent.com/b-cubed-eu/gcube/refs/heads/main/vignettes/articles/occurrence-process.Rmd"

  temp_dir <- tempdir()

  md_dir <- file.path(temp_dir, "src", "content", "docs", "r", "gcube")
  fig_dir <- file.path(temp_dir, "public", "r", "gcube")
  fig_url_dir <- paste0(temp_dir, "/astro-docs/", "r", "gcube", "/")

  rmd_to_md(rmd_file, md_dir, fig_dir, fig_url_dir)

  expect_identical(
    list.files(md_dir),
    c("occurrence-process.md")
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("rmd_to_md() writes figures to a directory", {
  skip_if_offline()
  rmd_file <- "https://raw.githubusercontent.com/b-cubed-eu/gcube/refs/heads/main/vignettes/articles/occurrence-process.Rmd"

  temp_dir <- tempdir()

  md_dir <- file.path(temp_dir, "src", "content", "docs", "r", "gcube")
  fig_dir <- file.path(temp_dir, "public", "r", "gcube")
  fig_url_dir <- paste0(temp_dir, "/astro-docs/", "r", "gcube", "/")

  rmd_to_md(rmd_file, md_dir, fig_dir, fig_url_dir)

  expect_identical(
    list.files(fig_dir),
    c(
      "occurrence-process-unnamed-chunk-12-1.png",
      "occurrence-process-unnamed-chunk-3-1.png",
      "occurrence-process-unnamed-chunk-7-1.png",
      "occurrence-process-unnamed-chunk-9-1.png"
    )
  )

  unlink(temp_dir, recursive = TRUE)
})

