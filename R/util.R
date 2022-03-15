# util.R
# Utility Functions
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-03-15

#' Translate HTML for Display with the `cli` Package
#'
#' Strips tags and styles, replacing these with styling codes used by `cli`.
#'
#' As part of the `rcm` package, this function intends to handle only those
#' HTML tags that are likely to appear in a REDCap field label.
#'
#' @param chr_html HTML code to be stripped.
#'
#' @return Character string for display with `cli::cli_text()`.
#' @export
html_to_cli <- function (chr_html) {
  chr_html |>
    str_remove_all('<span(\\s.*)?>(.*?)</span>') |>
    str_remove_all('<div(\\s.*)?>(.*?)</div>') |>
    str_replace_all('<h(\\d).*?>(.*?)</h\\1>', '\n{.strong \\2 }\n') |>
    str_replace_all('<p>(.*?)</p>', '\n\\1') |>
    str_replace_all('<(strong|b)>(.*?)</(strong|b)>', '{.strong \\2 }') |>
    str_replace_all('<(em|i)>(.*?)</(em|i)>', '{.emph \\2 }') |>
    str_replace_all('<a href=.*?>(.*?)</a>', '{.url \\1 }')
}
