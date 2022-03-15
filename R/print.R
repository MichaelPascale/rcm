# print.R
# S3 Print Methods for rcm_* Objects
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-03-15

#' @export
print.rcm_data <- function(df_data) {
  cli_h1('REDCap Data')
  cli_text('{nrow(df_data)} record{?s} in {ncol(df_data)} field{?s}, {attr(df_data, "rcm-reachable")} with metadata. ')
  cli_ul(df_data)
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

  cli_par()
  cli_text('{.emph {rcm_field(v_field)}}, {class(v_field)} ({mode(v_field)} of length {length(v_field)} datapoint{?s})')
  attr(v_field, 'rcm-label') |>
    html_to_cli() |>
    cli_text()
  cli_end()

  attr(v_field, "rcm-validation")
  attr(v_field, "rcm-min")
  attr(v_field, "rcm-max")
  attr(v_field, "rcm-choices")

  cli_par()
  cli_text('Head:')
  head(v_field) |> cli_ol()
  cli_end()

  invisible(v_field)
}
