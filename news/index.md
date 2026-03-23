# Changelog

## b3doc (development version)

- [`update_frontmatter()`](https://b-cubed-eu.github.io/b3doc/reference/update_frontmatter.md)’s
  `replace` argument no longer supports regex, avoiding the use of
  escape characters and unintended broad changes
  ([\#25](https://github.com/b-cubed-eu/b3doc/issues/25)).
- Figures now have a standard DPI of 300, to improve figure quality
  ([\#30](https://github.com/b-cubed-eu/b3doc/issues/30)).

## b3doc 0.2.0

- [`update_frontmatter()`](https://b-cubed-eu.github.io/b3doc/reference/update_frontmatter.md)
  can now handle `.md` files
  ([\#20](https://github.com/b-cubed-eu/b3doc/issues/20)).
- [`update_frontmatter()`](https://b-cubed-eu.github.io/b3doc/reference/update_frontmatter.md)
  now has a `replace` argument that replaces all `key`s with their
  `value`. This argument replaces the `logo` argument
  ([\#22](https://github.com/b-cubed-eu/b3doc/issues/22)).
- Add [Ward Langeraert](https://orcid.org/0000-0002-5900-8109) as
  author.

## b3doc 0.1.0

- New
  [`update_frontmatter()`](https://b-cubed-eu.github.io/b3doc/reference/update_frontmatter.md)
  updates the front matter and replaces the logo of a Markdown file on
  disk ([\#1](https://github.com/b-cubed-eu/b3doc/issues/1),
  [\#3](https://github.com/b-cubed-eu/b3doc/issues/3),
  [\#6](https://github.com/b-cubed-eu/b3doc/issues/6)).
- New
  [`rmd_to_md()`](https://b-cubed-eu.github.io/b3doc/reference/rmd_to_md.md)
  converts an external R Markdown to Markdown
  ([\#1](https://github.com/b-cubed-eu/b3doc/issues/1),
  [\#2](https://github.com/b-cubed-eu/b3doc/issues/2),
  [\#3](https://github.com/b-cubed-eu/b3doc/issues/3),
  [\#6](https://github.com/b-cubed-eu/b3doc/issues/6)).
