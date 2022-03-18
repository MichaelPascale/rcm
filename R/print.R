# print.R
# S3 Print Methods for rcm_* Objects
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-03-18

#' @export
print.rcm_data <- function(df_data) {
  cli_h1('REDCap Data')
  cli_text('{nrow(df_data)} record{?s} in {ncol(df_data)} field{?s}, {attr(df_data, "rcm-reachable")} field{?s} with metadata.')
  df_data |> head() |> as_tibble() |>  print(max_footer_lines=2)
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
  cli_text('Field Annotations: {rcm_annotation(v_field)}')
  cli_end()

  invisible(v_field)
}
