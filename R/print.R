# print.R
# S3 Print Methods for rcm_* Objects
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-04-15
#' @export
print.rcm_data <- function(df_data) {
  cli_h1('REDCap Data')

  cli_par()
  cli_text('{nrow(df_data)} record{?s} in {ncol(df_data)} field{?s}, {attr(df_data, "rcm-fields") |> length()} field{?s} with metadata and {attr(df_data, "rcm-non-fields") |> length()} without.')
  cli_text('Present fields are of the following types: {attr(df_data, "rcm-field-types")}.')
  cli_end()

  cli_par()
  cli_text('Longitudinal: {attr(df_data, "rcm-is-longitudinal")}')
  if (attr(df_data, "rcm-is-longitudinal")) cli_text('Events: {rcm_list_events(df_data)}')
  cli_end()

  cli_par()
  cli_text('Repeating Instruments: {attr(df_data, "rcm-has-repeating")}')
  if (attr(df_data, "rcm-has-repeating")) cli_text('{rcm_list_repeating(df_data)}')
  cli_end()

  df_data |> as_tibble() |>  print(max_footer_lines=2)
  invisible(df_data)
}

#' @export
print.rcm_metadata <- function(df_metadata) {
  cli_h1('REDCap Metadata')
  cli_text('{nrow(df_metadata)} field{?s}')
  invisible(df_metadata)
}

#' @export
print.rcm_field <- function(v_field) {
  cli_h1('REDCap Field')

  chr_type <- rcm_type(v_field)
  chr_desc <- str_glue('{chr_type} field.')

  if (chr_type == 'text' && rcm_validation(v_field) != '')
    chr_desc <- str_glue('text field with {rcm_validation(v_field)} validation.')

  cli_par()
  cli_text('{.emph {rcm_field(v_field)}}, {chr_desc}')
  cli_text('{mode(v_field)} vector of {length(v_field)} datapoint{?s} ({class(v_field)})')
  cli_end()

  cli_par()
  attr(v_field, 'rcm-label') |>
    .html_to_cli() |>
    cli_text()
  cli_end()

  if (chr_type %in% c('radio', 'dropdown', 'checkbox')) {
    cli_par()
    cli_text('Options:')
    rcm_choices(v_field, raw=TRUE) |> str_split('\\|') |> map(str_trim) |> unlist() |> cli_ul()
    cli_end()
  }

  cli_par()
  cli_text('Head:')
  head(v_field) |> cli_ol()
  cli_end()

  cli_par()
  cli_text('Instrument: {rcm_form(v_field)}')
  if (!is.null(rcm_events(v_field))) {
    cli_text('Events: {rcm_events(v_field)}')
  }
  cli_text('Field Annotations: {rcm_annotation(v_field)}')
  cli_end()

  invisible(v_field)
}
