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

#' On rcm_field_truefalse and rcm_field_yesno, expect integer coding 0=FALSE and
#' 1=TRUE.
#'
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

#' On rcm_field_radio, extract logical from a radio field, assuming binary
#' options and provided the label of the option corresponding to TRUE.
#'
#' @rdname rcm_to_logical
#' @export
rcm_to_logical.rcm_field_radio <- function (rcm_radio, chr_label_true) {
  checkmate::assert(rcm_radio |> rcm_type() == 'radio')
  checkmate::assert(n_distinct(rcm_radio, na.rm=TRUE) < 3)

  li_choices <- rcm_choices(rcm_radio)
  checkmate::assert_subset(chr_label_true, names(li_choices))

  chr_value_true <- li_choices[[chr_label_true]]
  vchr_values_false <- li_choices[li_choices != chr_value_true]

  case_when(
    rcm_radio == chr_value_true ~ TRUE,
    rcm_radio %in% vchr_values_false ~ FALSE,
    TRUE ~ NA
  )
}
