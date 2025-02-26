test_that("rmd_to_md() writes .md to a directory", {
  skip_if_offline()
  rmd_file <- "https://raw.githubusercontent.com/b-cubed-eu/gcube/refs/heads/main/vignettes/articles/occurrence-process.Rmd"
  output_dir <- "r/gcube"

  figures_dir <- file.path("public", output_dir)
  markdown_dir <- file.path("src", "content", "docs", output_dir)

  rmd_to_md(rmd_file, output_dir)

  expect_identical(
    list.files(figures_dir),
    c(
      "occurrence-process-unnamed-chunk-12-1.png",
      "occurrence-process-unnamed-chunk-3-1.png",
      "occurrence-process-unnamed-chunk-7-1.png",
      "occurrence-process-unnamed-chunk-9-1.png"
      )
    )
  expect_identical(
    list.files(markdown_dir),
    c("occurrence-process.md")
  )

  unlink("public", recursive = TRUE)
  unlink("src", recursive = TRUE)
})
