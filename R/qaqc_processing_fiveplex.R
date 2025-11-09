#' Wrapper Function - QAQC for a Fiveplex
#'
#' This function is meant to run a standard QAQC set-up using the functions available
#' in this package. For increased flexibility, manual construction of the functions
#' would be necessary
#'
#' @param data_frame_in A dataframe of laboratory data.
#' @param control_strings A vector of character strings for w0125_expected_controls_present (A vector of character strings that indicate control wells) and w0150_sample_naming_structure (A vector of character strings contained in control sample names)
#' @param pos_rows A Sample-Target dataframe of character strings for w0450_pos_control_breakdown_check
#' @param con_rows A Sample-Target dataframe of character strings for w0500_control_soft_check
#' @param lab_id A character string vector for w0150_sample_naming_structure (A character string identifying the submitter laboratory)
#' @param site_id_set A vector of character strings for w0150_sample_naming_structure (A vector of character strings identifying the potential site ids)
#' @return A dataframe containing unmerged Sample-Target data points
#' @export

qaqc_processing_fiveplex <- function(file_in,
                                   control_strings = c("NEG", "POS", "NTC", "EXT"),
                                   pos_rows = data.frame(Samples = c("POS", "POS", "POS", "POS", "POS"),
                                                         Targets = c("FluA", "FluB", "RSV", "SC2", "H5")),
                                   con_rows = data.frame(Samples = c("BCOV"),
                                                         Targets = c("PMMOV")),
                                   lab_id,
                                   site_id_set){

  file_in <- w0110_sample_name_edits(file_in)

  file_in <- w0125_expected_controls_present(file_in,
                                             control_strings,
                                             c(12, 12, 12, 0))

  file_in <- w0150_sample_naming_structure(file_in,
                                           lab_id,
                                           site_id_set,
                                           control_strings)

  file_in <- w0200_accepted_droplet_count(file_in)

  file_in <- w0300_ntc_control_check(file_in)

  w0400_pos_control_hard_stop(file_in, pos_rows)
  # just a check, no return value

  file_in <- w0450_pos_control_breakdown_check(file_in, pos_rows, 35)

  file_in <- w0500_control_soft_check(file_in, con_rows)

  file_in <- w0600_ext_neg_control_check(file_in, 9, 3, 1)

  #w0650_cumulative_count_check(file_in, 3, "yes")

  #file_in <- w0800_recover_control_check(file_in, "BCOV", c("POS", "NEG", "EXT", "NTC"), 0.3)

  #file_in <- w0900_positives_comparison_rule(file_in, c("POS"), c("N1"), c("POS", "NEG", "EXT", "NTC"), 3)

  #file_in <- w1000_remove_rows_as_chosen(file_in)

  return(file_in)

}

