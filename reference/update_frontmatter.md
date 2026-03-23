# Update front matter

Updates the front matter (and optionally content) of a Markdown file on
disk.

## Usage

``` r
update_frontmatter(
  md_file,
  rmd_file,
  title = NULL,
  sidebar_label = NULL,
  sidebar_order = NULL,
  replace = NULL
)
```

## Arguments

- md_file:

  Path to the Markdown file on disk.

- rmd_file:

  Path to the R Markdown file, either a local path or a URL.

- title:

  Title of the article, to show on top of the page.

- sidebar_label:

  Title in the sidebar.

- sidebar_order:

  Number indicating the order of the article in the sidebar.

- replace:

  Named character vector with `c("key" = "value")` pairs. All
  occurrences of `"key"` in the content of the Markdown file will be
  replaced by their respective `"value"` (before updating the front
  matter).

## Value

Markdown file with updated front matter (and optionally content),
written to disk.

## Examples

``` r
if (FALSE) { # \dontrun{
update_frontmatter(
  md_file = file.path(
    "output/src/content/docs/software/gcube/occurrence-process.md"
  ),
  rmd_file = file.path(
    "https://raw.githubusercontent.com/b-cubed-eu/gcube/refs/heads/main",
    "vignettes/articles/occurrence-process.Rmd"
  ),
  title = "2. Occurrence process",
  sidebar_label = "Occurrence process",
  sidebar_order = 2,
  replace = c(
    "### Changing number of occurrences over time" =
    "### How to change the number of occurrences over time",
    "man/figures/logo.png" = "https://b-cubed-eu.github.io/gcube/logo.png"
  )
)
} # }
```
