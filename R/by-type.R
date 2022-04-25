# by-type.R
# List fields given a particular field type.
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-04-25



#' List REDCap fields given a field type.
#' @export
rcm_fields_by_type <- function (...) UseMethod('rcm_fields_by_type')

#' @rdname rcm_fields_by_type
#' @export
rcm_fields_by_type.rcm_metadata <- function (df_metadata, chr_type) {
  df_metadata[df_metadata[,4] %in% chr_type, 1]
}

#' @rdname rcm_fields_by_type
#' @export
rcm_fields_by_type.rcm_data <- function (df_data, chr_type) {
  .metadata(df_data) |> rcm_fields_by_type(chr_type)
}
