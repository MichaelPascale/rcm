# util.R
# Utility Functions
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-04-14

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
#' @keywords internal
.html_to_cli <- function (chr_html) {
  chr_html |>
    str_remove_all('<span(\\s.*)?>(.*?)</span>') |>
    str_remove_all('<div(\\s.*)?>(.*?)</div>') |>
    str_replace_all('<h(\\d).*?>(.*?)</h\\1>', '\n{.strong \\2 }\n') |>
    str_replace_all('<p>(.*?)</p>', '\n\\1') |>
    str_replace_all('<(strong|b)>(.*?)</(strong|b)>', '{.strong \\2 }') |>
    str_replace_all('<(em|i)>(.*?)</(em|i)>', '{.emph \\2 }') |>
    str_replace_all('<a href=.*?>(.*?)</a>', '{.url \\1 }')
}

#' Safely extract metadata from an rcm_data object
#'
#' @param df_data A dataframe created with `rcm`.
#'
#' @return The metadata data frame.
#' @keywords internal
.metadata <- function (df_data) {
  if (!is(df_data, 'rcm_data'))
    stop(substitute(df_data), ' is not of class rcm_data.')

  df_m <- attr(df_data, 'rcm-metadata')
  if (is.null(df_m))
    stop('No metadata present in ', substitute(df_data), '.')
  if (!is(df_m, 'rcm_metadata'))
    stop('Metadata attribute of ', substitute(df_data), ' is not of class rcm_metadata.')

  df_m
}

#' Safely extract the form-event map from an rcm_data object
#'
#' @param df_data A dataframe created with `rcm`.
#'
#' @return The form-event map data frame.
#' @keywords internal
.form_event_map <- function (df_data) {
  if (!is(df_data, 'rcm_data'))
    stop(substitute(df_data), ' is not of class rcm_data.')

  df_m <- attr(df_data, 'rcm-form-event-map')
  if (is.null(df_m))
    stop('No form-event map present in ', substitute(df_data), '. Ensure that the instrument event mappings were passed to the constructor.')
  if (!is(df_m, 'rcm_form_event_map'))
    stop('Form-event map attribute of ', substitute(df_data), ' is not of class rcm_form_event_map')

  df_m
}

#' Drop classes matching a regular expression from an object.
#'
#' @param obj_any Any R object.
#' @param chr_pattern A regular expression matching the classes to be dropped.
#'
#' @return The modified class list.
#' @keywords internal
.strip_class <- function (obj_any, chr_pattern) {
  class(obj_any) |> discard(str_detect, chr_pattern)
}

#' Drop attributes matching a regular expression from an object.
#'
#' @param obj_any Any R object.
#' @param chr_pattern A regular expression matching the attributes to be dropped.
#'
#' @return The modified class list.
#' @keywords internal
.strip_attributes <- function (obj_any, chr_pattern) {
  attributes(obj_any) |> discard(str_detect, chr_pattern)
}

#' Extract unique elements from a given column in alphabetical order.
#'
#' @keywords internal
.discrete <- function (df_data, v_col) {
  assert_scalar(v_col)
  suppressWarnings(df_data[, v_col]) |> unique() |> sort()
}

#' Like switch() but match value against regular expressions
#' @keywords internal
.switch_regex <- function(chr_string, ...) {
  li_pairs <- list(...)
  for (int_pair_idx in seq_along(li_pairs)) {
    chr_name <- names(li_pairs)[int_pair_idx]
    exp_result <- li_pairs[[int_pair_idx]]

    if (chr_name == '')
      return(exp_result)

    if (is.na(chr_string))
      next

    if (str_detect(chr_string, chr_name))
      return(exp_result)
  }

  stop('End of option list reached with no default.')
}
