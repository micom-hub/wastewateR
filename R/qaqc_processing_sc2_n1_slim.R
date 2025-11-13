
#' Wrapper Function - QAQC for SARS-CoV-2 N1 Testing, Slim
#'
#' This function is meant to run a standard QAQC set-up using the functions available
#' in this package. For increased flexibility, manual construction of the functions
#' would be necessary
#'
#' The default settings of expected count are for the assumption of NO pmmov/bcov testing
#' within the sars-cov-2 n1 testing (so, one plate) with expected controls of:
#'
#' | Sample | Target | Count Wells |
#' | --- | --- | --- |
#' | NEG | N1 | 3 |
#' | EXT | N1 | 3 |
#' | NTC | N1 | 3 |
#' | POS | N1 | 3 |
#'
#' @param data_frame_in A dataframe of laboratory data.
#' @param control_strings A vector of character strings for w0125_expected_controls_present (A vector of character strings that indicate control wells) and w0150_sample_naming_structure (A vector of character strings contained in control sample names)
#' @param expected_count A numeric vector of the expected number of wells for each control type. Default vector is 3, 3, 3, 3.
#' @param pos_rows A Sample-Target dataframe of character strings for w0450_pos_control_breakdown_check
#' @param lab_id A character string vector for w0150_sample_naming_structure (A character string identifying the submitter laboratory)
#' @param site_id_set A vector of character strings for w0150_sample_naming_structure (A vector of character strings identifying the potential site ids)
#' @return A dataframe containing unmerged Sample-Target data points
#' @export

qaqc_processing_sc2_n1_slim <- function(file_in,
                control_strings = c("NEG", "POS", "NTC", "EXT"),
                expected_count = c(3, 3, 3, 3),
                pos_rows = data.frame(Samples = c("POS"),
                                       Targets = c("N1")),
                lab_id,
                site_id_set){

  file_in <- w0110_sample_name_edits(file_in)

  file_in <- w0125_expected_controls_present(file_in,
                                            control_strings,
                                            expected_count)

  file_in <- w0150_sample_naming_structure(file_in,
                                         lab_id,
                                         site_id_set,
                                         control_strings)

  file_in <- w0200_accepted_droplet_count(file_in)

  file_in <- w0300_ntc_control_check(file_in)

  w0400_pos_control_hard_stop(file_in, pos_rows)
  # just a check, no return value

  file_in <- w0450_pos_control_breakdown_check(file_in, pos_rows, 35)

  file_in <- w0600_ext_neg_control_check(file_in, 9, 3, 3, 1)

  w0650_cumulative_count_check(file_in, 3, "yes")

  file_in <- w0900_positives_comparison_rule(file_in, c("POS"), c("N1"), c("POS", "NEG", "EXT", "NTC"), 3)

  file_in <- w1000_remove_rows_as_chosen(file_in)

  return(file_in)

}

