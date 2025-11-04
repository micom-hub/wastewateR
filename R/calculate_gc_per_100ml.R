

#' Default settings are for RNA kits currently used
#' for c.auris pilot project kits, would be
#' adjust_frevu = 3,
#' div_frevu = 5,
#' further_adjust = 10.32


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
                        CP_100_mL_of_sample = case_when(Positives >= positives_limit ~ (((CP_uL*final_extraction_volume_uL)*(final_concentrate_volume_mL/volume_used_for_extraction_mL))/initial_volume_analyzed_mL)*100,
                                                        T ~ detection_limit_CP_100mL))


  return(working_calc_set)

}
