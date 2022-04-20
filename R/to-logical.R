# to-logical.R
# Typecast REDCap Fields to Usable R Types
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-03-18


#' Safely typecast numerically coded yes/no and true/false questions to logical.
#' @export
rcm_to_logical <-
  function (...) UseMethod('rcm_to_logical')

#' @rdname rcm_to_logical
#' @export
rcm_to_logical.rcm_data <-
  function(df_data, chr_field) rcm_logical(df_data[[chr_field]])

#' @rdname rcm_to_logical
#' @export
rcm_to_logical.rcm_field_yesno <- function (int_logical) {
  checkmate::assert(int_logical |> rcm_type() == 'yesno')
  checkmate::assert_integer(int_logical, lower=0, upper=1)
  as.logical(int_logical)
}

#' @rdname rcm_to_logical
#' @export
rcm_to_logical.rcm_field_truefalse <- function (int_logical) {
  checkmate::assert(int_logical |> rcm_type() == 'truefalse')
  checkmate::assert_integer(int_logical, lower=0, upper=1)
  as.logical(int_logical)
}

#' @rdname rcm_to_logical
#' @export
rcm_to_logical.rcm_field_checkbox <- function (int_logical) {
  checkmate::assert(int_logical |> rcm_type() == 'checkbox')
  checkmate::assert_integer(int_logical, lower=0, upper=1)
  as.logical(int_logical)
}
