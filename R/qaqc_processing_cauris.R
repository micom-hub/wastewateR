
#' Wrapper Function - QAQC for C. auris Testing
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

qaqc_processing_cauris <- function(file_in,
                                   control_strings = c("NEG", "AUR", "NTC", "EXT"),
                                   pos_rows = data.frame(Samples = c("AUR", "AUR"),
                                                         Targets = c("CAUR", "CJEJ")),
                                   con_rows = data.frame(Samples = c("AUR", "EXT", "NEG", "NTC", "AUR", "EXT", "NEG", "NTC"), 
                                                         Targets = c("CAUR", "CAUR", "CAUR", "CAUR", "CJEJ", "CJEJ", "CJEJ", "CJEJ")),
                                   lab_id,
                                   site_id_set){
  
    auris1_b <- w0110_sample_name_edits(file_in)
    
    auris1_c <- w0150_sample_naming_structure(auris1_b, 
                                              lab_id, 
                                              site_id_set, 
                                              control_strings)
    
    auris1_c <- w0200_accepted_droplet_count(auris1_c, 10000)
    
    auris1_c <- w0300_ntc_control_check(auris1_c)
    
    w0400_pos_control_hard_stop(auris1_c, pos_rows)
    
    auris1_d <- w0450_pos_control_breakdown_check(auris1_c, pos_rows)
    
    auris1_e <- w0600_ext_neg_control_check(auris1_d, 3, 3)
    
    w0650_cumulative_count_check(auris1_e)
    
    auris1_f <- w0700_pos_droplet_sum(auris1_e, 4, con_rows)
    
    auris1_g <- w1000_remove_rows_as_chosen(auris1_f, c(2, 3, 6))
    
    return(auris1_g)   
}