test_that("rmd_to_md() writes .md to a directory", {
  skip_if_offline()
  rmd_file <- "https://raw.githubusercontent.com/b-cubed-eu/gcube/refs/heads/main/vignettes/articles/occurrence-process.Rmd"

  temp_dir <- tempdir()

  md_dir <- file.path(temp_dir, "src", "content", "docs", "r", "gcube")
  fig_dir <- file.path(temp_dir, "public", "r", "gcube")
  fig_url_dir <- paste0(temp_dir, "/astro-docs/", "r", "gcube", "/")
  order <- 1

  install_required_packages(rmd_file)
  rmd_to_md(rmd_file, md_dir, fig_dir, fig_url_dir, order)

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
  fig_url_dir <- paste0(temp_dir, "/astro-docs/r/gcube/")
  order <- 1

  install_required_packages(rmd_file)
  rmd_to_md(rmd_file, md_dir, fig_dir, fig_url_dir, order)

  expect_equal(
    list.files(fig_dir),
    c(
      "occurrence-process-named-1.png",
      "occurrence-process-unnamed-chunk-12-1.png",
      "occurrence-process-unnamed-chunk-15-1.png",
      "occurrence-process-unnamed-chunk-17-1.png",
      "occurrence-process-unnamed-chunk-19-1.png",
      "occurrence-process-unnamed-chunk-21-1.png",
      "occurrence-process-unnamed-chunk-24-1.png",
      "occurrence-process-unnamed-chunk-25-1.png",
      "occurrence-process-unnamed-chunk-3-1.png",
      "occurrence-process-unnamed-chunk-7-1.png",
      "occurrence-process-unnamed-chunk-9-1.png"
      )
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("rmd_to_md() resets knitting options to the original settings", {
  skip_if_offline()
  rmd_file <- "https://raw.githubusercontent.com/b-cubed-eu/gcube/refs/heads/main/vignettes/articles/occurrence-process.Rmd"

  temp_dir <- tempdir()

  md_dir <- file.path(temp_dir, "src", "content", "docs", "r", "gcube")
  fig_dir <- file.path(temp_dir, "public", "r", "gcube")
  fig_url_dir <- paste0(temp_dir, "/astro-docs/r/gcube/")
  order <- 1

  original_opts_knit <- knitr::opts_knit$get()

  install_required_packages(rmd_file)
  rmd_to_md(rmd_file, md_dir, fig_dir, fig_url_dir, order)

  new_opts_knit <- knitr::opts_knit$get()

  expect_identical(original_opts_knit, new_opts_knit)

  unlink(temp_dir, recursive = TRUE)
})

test_that("rmd_to_md() updates the frontmatter of the markdown file", {
  skip_if_offline()
  rmd_file <- "https://raw.githubusercontent.com/b-cubed-eu/gcube/refs/heads/main/vignettes/articles/occurrence-process.Rmd"

  temp_dir <- tempdir()

  md_dir <- file.path(temp_dir, "src", "content", "docs", "r", "gcube")
  fig_dir <- file.path(temp_dir, "public", "r", "gcube")
  fig_url_dir <- paste0(temp_dir, "/astro-docs/r/gcube/")
  order <- 1
  title <- "blah"

  install_required_packages(rmd_file)
  rmd_to_md(rmd_file, md_dir, fig_dir, fig_url_dir, order, title)

  md_file <- file.path(md_dir, "occurrence-process.md")

  expect_identical(
    rmarkdown::yaml_front_matter(md_file)$lastUpdated,
    format(Sys.time(), "%Y-%m-%d")
  )
  expect_identical(
    rmarkdown::yaml_front_matter(md_file)$sidebar$order,
    1.0
  )
  expect_identical(
    rmarkdown::yaml_front_matter(md_file)$source,
    "https://github.com/b-cubed-eu/gcube/blob/main/vignettes/articles/occurrence-process.Rmd"
  )

  unlink(temp_dir, recursive = TRUE)
})
