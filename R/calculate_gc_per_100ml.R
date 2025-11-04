#' Calculation of gene copies per 100mL
#'
#' This function takes in a dataframe of laboratory data, either merged or unmerged,
#' as well as a dataframe of the weight/volume data taken in previous laboratory steps.
#'
#' The dataframe of the weight/volume data is assumed to consist of three columns in the following
#' order: "Sample", "initial_volume_analyzed_mL", "final_concentrate_volume_mL"
#'
#' The dataframe of laboratory data is assumed to have at least one column called Sample,
#' containing the Sample name for each row.
#'
#' Default settings:
#' volume_used_for_extraction_mL = 0.8,
#' final_extraction_volume_uL = 50,
#' positives_limit = 3,
#' adjust_frevu = 0.6,
#' div_frevu = 1,
#' further_adjust = 1
#'
#' Default settings for adjust frevu, div_frevu, and further adjust are for RNA kits currently used
#' for c.auris pilot project kits, would be:
#' volume_used_for_extraction_mL = 0.2,
#' final_extraction_volume_uL = 80,
#' adjust_frevu = 3,
#' div_frevu = 5,
#' further_adjust = 10.32
#'
#' For more visual data on the mathematical calculations, please visit: https://micom-hub.org/wastewateR_documentation/
#'
#' @param lab_df_in Dataframe of laboratory data, must contain a columns: 'Sample', 'Positives', 'CP_uL'
#' @param all_weigh_info Dataframe of weight/volumn data, must consist of "Sample", "initial_volume_analyzed_mL", "final_concentrate_volume_mL" columns
#' @param volume_used_for_extraction_mL Numeric value of the volume used for extraction in mL
#' @param final_extraction_volume_uL Numeric value of the final extraction volume in uL
#' @param positives_limit Positives limit for when to apply the gene copies per 100mL value calculation to the sample. If the Positives value is less than this value, the final gene copies per 100mL column will be filled with the Detection limit value instead
#' @param adjust_frevu Numeric adjustment factor (multiplier) for (final_extraction_volume_uL/div_frevu)
#' @param div_frevu Numeric adjustment factor (divider) for final_extraction_volume_uL
#' @param further_adjust Numeric adjustment factor (multiplier) for entire function
#' @return A dataframe containing additional columns: "initial_volume_analyzed_mL", "final_concentrate_volume_mL", "detection_limit_CP_100mL", "CP_100_mL_of_sample"
#' @export

calculate_gc_per_100ml <- function(lab_df_in, all_weigh_info,
                                   volume_used_for_extraction_mL = 0.2,
                                   final_extraction_volume_uL = 80,
                                   positives_limit = 3,
                                   adjust_frevu = 0.6,
                                   div_frevu = 1,
                                   further_adjust = 1){

  # assumption, have a dataframe of Sample, Volume_mL, ConcVol_mL

  # rename, for simplicity
  colnames(all_weigh_info) <- c("Sample", "initial_volume_analyzed_mL", "final_concentrate_volume_mL")


  # merge all_weigh_info onto lab_df_in by Sample, and report back on samples lost
  original_samples <- unique(lab_df_in$Sample)

  working_calc_set <- merge(lab_df_in, all_weigh_info, by = c("Sample"))

  after_samples <- unique(working_calc_set$Sample)

  # messaging about sample name differences after the merge
  count_diff <- length(original_samples) - length(after_samples)
  message(paste0("There are ", count_diff, " sample names that were in the laboratory data set that were not in the weight info dataset."))
  message("The samples that have been lost due to merging inconsistencies are: ")
  message(setdiff(original_samples, after_samples))

  # confirm numeric
  working_calc_set <- working_calc_set %>% mutate(initial_volume_analyzed_mL = as.numeric(initial_volume_analyzed_mL),
                                                  final_concentrate_volume_mL = as.numeric(final_concentrate_volume_mL))

  working_calc_set <- working_calc_set %>% mutate(detection_limit_CP_100mL = (((adjust_frevu*(final_extraction_volume_uL/div_frevu)*further_adjust*((final_concentrate_volume_mL/volume_used_for_extraction_mL)))/initial_volume_analyzed_mL)*100),
                        CP_100_mL_of_sample = case_when(Positives >= positives_limit ~ (((CP_uL*(final_extraction_volume_uL/div_frevu)*further_adjust*((final_concentrate_volume_mL/volume_used_for_extraction_mL)))/initial_volume_analyzed_mL)*100),
                                                        T ~ detection_limit_CP_100mL))


  return(working_calc_set)

}

