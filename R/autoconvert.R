# autoconvert.R
# Safely typecast REDCap fields to appropriate R types, inferring format from
# the REDCap data dictionary.
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-04-18

#' Safely typecast REDCap fields given types in data dictionary.
rcm_autoconvert <- function(...) UseMethod('rcm_autoconvert')

#' @rdname rcm_autoconvert
#' @export
rcm_autoconvert.rcm_data <- function(df_data) {
  map_dfc(df_rc,rcm_autoconvert)
}

#' @rdname rcm_autoconvert
#' @export
rcm_autoconvert.rcm_field <- function(v_field) {
  rcm_type(v_field) |>
    switch(
      dropdown=rcm_to_factor(v_field),
      radio=rcm_to_factor(v_field),
      checkbox=rcm_to_logical(v_field),
      calc=rcm_to_numeric(v_field),
      slider=rcm_to_integer(v_field),
      text=.autoconvert_text(v_field),
      notes=rcm_to_text(v_field),
      truefalse=rcm_to_logical(v_field),
      yesno=rcm_to_logical(v_field),
      {
        warning(str_glue('Field type {rcm_type(v_field)} not available for {rcm_field(v_field)}.'))
        v_field |> .strip_class('^rcm_') |> .strip_attributes('^rcm-')
      }
    )
}

#' @rdname rcm_autoconvert
#' @export
rcm_autoconvert.default <- function(obj) {
  if (class(obj) |> str_detect('^rcm_'))
    stop('Non-convertable rcm_ object.')
  obj
}

#' @keywords internal
.autoconvert_text <- function (v_field) {
  (rcm_validation(v_field) |>
    .switch_regex(
      'date|time' =rcm_to_time,
      'integer'   =rcm_to_integer,
      'number'    =rcm_to_numeric,
      rcm_to_text
    ))(v_field)
}
