test_that("rmd_to_md() writes .md to a directory", {
  skip_if_offline()
  rmd_file <- here::here("examples", "example.Rmd")

  temp_dir <- tempdir()

  md_dir <- file.path(temp_dir, "src", "content", "docs", "r", "example")
  fig_dir <- file.path(temp_dir, "public", "r", "example")
  fig_url_dir <- paste0(temp_dir, "/astro-docs/r/example/")
  order <- 1

  rmd_to_md(rmd_file, md_dir, fig_dir, fig_url_dir, order)

  expect_identical(
    list.files(md_dir),
    c("example.md")
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("rmd_to_md() writes figures to a directory", {
  skip_if_offline()
  rmd_file <- here::here("examples", "example.Rmd")

  temp_dir <- tempdir()

  md_dir <- file.path(temp_dir, "src", "content", "docs", "r", "example")
  fig_dir <- file.path(temp_dir, "public", "r", "example")
  fig_url_dir <- paste0(temp_dir, "/astro-docs/r/example/")
  order <- 1

  rmd_to_md(rmd_file, md_dir, fig_dir, fig_url_dir, order)

  expect_equal(
    list.files(fig_dir),
    c("example-unnamed-chunk-3-1.png")
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("rmd_to_md() resets knitting options to the original settings", {
  skip_if_offline()
  rmd_file <- here::here("examples", "example.Rmd")

  temp_dir <- tempdir()

  md_dir <- file.path(temp_dir, "src", "content", "docs", "r", "example")
  fig_dir <- file.path(temp_dir, "public", "r", "example")
  fig_url_dir <- paste0(temp_dir, "/astro-docs/r/example/")
  order <- 1

  original_opts_knit <- knitr::opts_knit$get()

  rmd_to_md(rmd_file, md_dir, fig_dir, fig_url_dir, order)

  new_opts_knit <- knitr::opts_knit$get()

  expect_identical(original_opts_knit, new_opts_knit)

  unlink(temp_dir, recursive = TRUE)
})

test_that("rmd_to_md() updates the frontmatter of the markdown file", {
  skip_if_offline()
  rmd_file <- here::here("examples", "example.Rmd")

  temp_dir <- tempdir()

  md_dir <- file.path(temp_dir, "src", "content", "docs", "r", "example")
  fig_dir <- file.path(temp_dir, "public", "r", "example")
  fig_url_dir <- paste0(temp_dir, "/astro-docs/r/example/")
  order <- 1
  title <- "blah"

  rmd_to_md(rmd_file, md_dir, fig_dir, fig_url_dir, order, title)

  md_file <- file.path(md_dir, "example.md")

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
    rmd_file
  )

  unlink(temp_dir, recursive = TRUE)
})
