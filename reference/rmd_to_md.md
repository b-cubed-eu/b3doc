# Convert external R Markdown to Markdown

Converts an R Markdown (`.Rmd`) file to a Markdown (`.md`) file. Both
local and remote `.Rmd` files can be handled. The date this function is
called upon is added to the beginning of the Markdown file.

## Usage

``` r
rmd_to_md(
  rmd_file,
  md_dir,
  fig_dir,
  fig_url_dir,
  title = NULL,
  sidebar_label = NULL,
  sidebar_order = NULL,
  replace = NULL
)
```

## Arguments

- rmd_file:

  Path to the R Markdown file, either a local path or a URL.

- md_dir:

  Path to local directory to write the Markdown file to. If it doesn't
  exist it will be created.

- fig_dir:

  Path to local directory to write the figures to.

- fig_url_dir:

  Link prefix that will be used to refer to figures in Markdown output.

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

Markdown file and figures written do disk.

## Examples

``` r
if (FALSE) { # \dontrun{
# First check and install (or update) packages loaded in the Rmd file

# Then convert Rmd to Markdown
rmd_to_md(
  rmd_file = file.path(
    "https://raw.githubusercontent.com/b-cubed-eu/gcube/refs/heads/main",
    "vignettes/articles/occurrence-process.Rmd"
  ),
  md_dir = "output/src/content/docs/software/gcube",
  fig_dir = "output/public/software/gcube",
  fig_url_dir = "/software/gcube/",
  title = "2. Occurrence process",
  sidebar_label = "occurrence-process",
  sidebar_order = 2,
  replace = c(
    "### Changing number of occurrences over time" =
    "### How to change the number of occurrences over time",
    "man/figures/logo.png" = "https://b-cubed-eu.github.io/gcube/logo.png"
  )
)

# Clean up (don't do this if you want to keep your files)
unlink("output", recursive = TRUE)
} # }
```
