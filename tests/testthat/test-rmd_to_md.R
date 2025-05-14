test_that("rmd_to_md() returns error on invalid sidebar_order", {
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  rmd_file <- testthat::test_path("example.Rmd")
  md_dir <- file.path(temp_dir, "src/content/docs/software/example")
  fig_dir <- file.path(temp_dir, "public/software/example")
  fig_url_dir <- "/software/example/"

  expect_error(
    rmd_to_md(
      rmd_file = rmd_file,
      md_dir = md_dir,
      fig_dir = fig_dir,
      fig_url_dir = fig_url_dir,
      sidebar_order = "invalid"
    ),
    class = "b3doc_error_order_invalid"
  )

  expect_error(
    rmd_to_md(
      rmd_file = rmd_file,
      md_dir = md_dir,
      fig_dir = fig_dir,
      fig_url_dir = fig_url_dir,
      sidebar_order = "1"
    ),
    class = "b3doc_error_order_invalid"
  )

  expect_error(
    rmd_to_md(
      rmd_file = rmd_file,
      md_dir = md_dir,
      fig_dir = fig_dir,
      fig_url_dir = fig_url_dir,
      sidebar_order = 1.1
    ),
    class = "b3doc_error_order_invalid"
  )
})

test_that("example.Rmd file is accessible", {
  file_path <- testthat::test_path("example.Rmd")
  expect_true(file.exists(file_path))
})

test_that("rmd_to_md() writes .md and figures to the expected directories", {
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  expected_md_dir <- file.path(temp_dir, "src/content/docs/software/example")
  expected_fig_dir <- file.path(temp_dir, "public/software/example")

  rmd_to_md(
    rmd_file = testthat::test_path("example.Rmd"),
    md_dir = expected_md_dir,
    fig_dir = expected_fig_dir,
    fig_url_dir = "/software/example/"
  )

  expect_identical(
    list.files(expected_md_dir),
    c("example.md")
  )
  expect_identical(
    list.files(expected_fig_dir),
    c("example-unnamed-chunk-3-1.png")
  )
})

test_that("rmd_to_md() writes the expected markdown, including custom
           frontmatter and figure paths", {
  temp_dir <- tempdir()
  expected_md_dir <- file.path(temp_dir, "src/content/docs/software/example")
  on.exit(unlink(temp_dir, recursive = TRUE))

  rmd_to_md(
    rmd_file = testthat::test_path("example.Rmd"),
    md_dir = expected_md_dir,
    fig_dir = file.path(temp_dir, "public/software/example"),
    fig_url_dir = "/software/example/",
    title = "Custom title",
    sidebar_label = "Custom sidebar label",
    sidebar_order = 2
  )

  expect_snapshot_file(
    file.path(expected_md_dir, "example.md"),
    transform = function(x) {
      gsub("\\d{4}-\\d{2}-\\d{2}", "<date>", x)
    }
  )
})

test_that("rmd_to_md() resets knitting options to the original settings", {
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  original_opts_knit <- knitr::opts_knit$get()

  rmd_to_md(
    rmd_file = testthat::test_path("example.Rmd"),
    md_dir = file.path(temp_dir, "src/content/docs/software/example"),
    fig_dir = file.path(temp_dir, "public/software/example"),
    fig_url_dir = "/software/example/"
  )
  new_opts_knit <- knitr::opts_knit$get()

  expect_identical(original_opts_knit, new_opts_knit)
})

test_that("rmd_to_md() replaces logo URLs correctly", {
  temp_dir <- tempdir()
  expected_md_dir <- file.path(temp_dir, "src/content/docs/software/example")
  on.exit(unlink(temp_dir, recursive = TRUE))

  logo_from <- "man/figures/logo.png"
  logo_to <- "https://pkgs.rstudio.com/rmarkdown/reference/figures/logo.png"

  rmd_to_md(
    rmd_file = testthat::test_path("example.Rmd"),
    md_dir = expected_md_dir,
    fig_dir = file.path(temp_dir, "public/software/example"),
    fig_url_dir = "/software/example/",
    logo_from = logo_from,
    logo_to = logo_to
  )

  md_content <- readLines(file.path(expected_md_dir, "example.md"))
  expect_true(any(grepl(logo_from, md_content)))
  expect_false(any(grepl(logo_to, md_content)))
})
