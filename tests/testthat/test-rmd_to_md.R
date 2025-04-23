test_that("example.Rmd file is accessible", {
  file_path <- testthat::test_path("example.Rmd")
  expect_true(file.exists(file_path))
})

test_that("rmd_to_md() writes .md to a directory", {
  rmd_file <- testthat::test_path(testthat::test_path("example.Rmd"))

  temp_dir <- tempdir()

  md_dir <- file.path(temp_dir, "src", "content", "docs", "r", "example")
  fig_dir <- file.path(temp_dir, "public", "r", "example")
  fig_url_dir <- paste0(temp_dir, "/astro-docs/r/example/")
  title <- "1. Exploring the Iris Dataset"
  sidebar_label = "Iris"
  sidebar_order <- 1

  rmd_to_md(
    rmd_file, md_dir, fig_dir, fig_url_dir, title, sidebar_label, sidebar_order
    )

  expect_identical(
    list.files(md_dir),
    c("example.md")
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("rmd_to_md() writes figures to a directory", {
  rmd_file <- testthat::test_path("example.Rmd")

  temp_dir <- tempdir()

  md_dir <- file.path(temp_dir, "src", "content", "docs", "r", "example")
  fig_dir <- file.path(temp_dir, "public", "r", "example")
  fig_url_dir <- paste0(temp_dir, "/astro-docs/r/example/")
  title <- "1. Exploring the Iris Dataset"
  sidebar_label <- "Iris"
  sidebar_order <- 1

  rmd_to_md(
    rmd_file, md_dir, fig_dir, fig_url_dir, title, sidebar_label, sidebar_order
  )

  expect_equal(
    list.files(fig_dir),
    c("example-unnamed-chunk-3-1.png")
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("rmd_to_md() resets knitting options to the original settings", {
  rmd_file <- testthat::test_path("example.Rmd")

  temp_dir <- tempdir()

  md_dir <- file.path(temp_dir, "src", "content", "docs", "r", "example")
  fig_dir <- file.path(temp_dir, "public", "r", "example")
  fig_url_dir <- paste0(temp_dir, "/astro-docs/r/example/")
  title <- "1. Exploring the Iris Dataset"
  sidebar_label <- "Iris"
  sidebar_order <- 1

  original_opts_knit <- knitr::opts_knit$get()

  rmd_to_md(
    rmd_file, md_dir, fig_dir, fig_url_dir, title, sidebar_label, sidebar_order
  )

  new_opts_knit <- knitr::opts_knit$get()

  expect_identical(original_opts_knit, new_opts_knit)

  unlink(temp_dir, recursive = TRUE)
})

test_that("rmd_to_md() updates the frontmatter of the markdown file", {
  rmd_file <- testthat::test_path("example.Rmd")

  temp_dir <- tempdir()

  md_dir <- file.path(temp_dir, "src", "content", "docs", "r", "example")
  fig_dir <- file.path(temp_dir, "public", "r", "example")
  fig_url_dir <- paste0(temp_dir, "/astro-docs/r/example/")
  title <- "2. Bla"
  sidebar_label <- "Bla"
  sidebar_order <- 2

  rmd_to_md(
    rmd_file, md_dir, fig_dir, fig_url_dir, title, sidebar_label, sidebar_order
  )

  md_file <- file.path(md_dir, "example.md")

  expect_identical(
    rmarkdown::yaml_front_matter(md_file)$lastUpdated,
    format(Sys.time(), "%Y-%m-%d")
  )
  expect_identical(
    rmarkdown::yaml_front_matter(md_file)$sidebar$order,
    2.0
  )
  expect_identical(
    rmarkdown::yaml_front_matter(md_file)$source,
    rmd_file
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("rmd_to_md() writes the expected markdown file", {
  rmd_file <- testthat::test_path("example.Rmd")

  temp_dir <- tempdir()

  md_dir <- file.path(temp_dir, "src", "content", "docs", "r", "example")
  fig_dir <- file.path(temp_dir, "public", "r", "example")
  fig_url_dir <- paste0(temp_dir, "/astro-docs/r/example/")
  title <- "1. Exploring the Iris Dataset"
  sidebar_label <- "Iris"
  sidebar_order <- 1

  rmd_to_md(
    rmd_file, md_dir, fig_dir, fig_url_dir, title, sidebar_label, sidebar_order
  )

  md_file <- file.path(md_dir, "example.md")

  expect_snapshot_file(md_file)

  unlink(temp_dir, recursive = TRUE)
})
