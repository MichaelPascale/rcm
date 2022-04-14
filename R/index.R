# index.R
# Subset REDCap data frame to extract individual forms, events, etc.
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-04-14

#' Select fields given a form.
#' @export
rcm_select_by_type <- function(df_rc, ...) UseMethod('rcm_select_by_type')

#' @rdname rcm_select_by_type
#' @export
rcm_select_by_type <- function(df_rc, vchr_type, .invert=FALSE) {

  checkmate::assert_subset(vchr_type, rcm_list_types(df_rc))

  df_rc |>
    select(where(
        function(v_field) {

          if (.invert)
            return(!is(v_field, 'rcm_field') || !rcm_type(v_field) %in% vchr_type)

          !is(v_field, 'rcm_field') || rcm_type(v_field) %in% vchr_type

        }
    ))
}

#' Select fields given a form.
#' @export
rcm_select_by_form <- function(df_rc, ...) UseMethod('rcm_select_by_form')

#' @rdname rcm_select_by_form
#' @export
rcm_select_by_form.rcm_data <- function(df_rc, vchr_form, .invert=FALSE) {

  checkmate::assert_subset(vchr_form, rcm_list_forms(df_rc))

  df_rc |>
    select(where(
      function(v_field) {

        if (.invert)
          return(!is(v_field, 'rcm_field') || !rcm_form(v_field) %in% vchr_form)

        !is(v_field, 'rcm_field') || rcm_form(v_field) %in% vchr_form

      }
    ))


}
