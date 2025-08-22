
#' Sample Positive Droplet Sum Check
#' 
#' This function is meant to be used to check non-control wells for low positive 
#' droplet counts. It is just a check function, it will not stop code from running.
#' 
#' This function takes in a dataframe of laboratory data, a numeric indicator for 
#' the sum of positive droplets check, as well as a dataframe of Sample-Target 
#' pairs to EXCLUDE from this check.   
#' 
#' Sample-Target pairs example:
#' 
#' If the dataframe looks like:
#' 
#' | Sample | Target | 
#' | --- | --- | 
#' | POS | N1 | 
#' | BCOV | BCOV | 
#' | EXT | N1 |
#'  
#' Then the code will look for any instances where the 'Sample' column contains 
#' the string "POS" AND the 'Target' column contains the string "N1", and any 
#' instances where the 'Sample' column contains the string "BCOV" AND the 'Target' 
#' column contains "BCOV", etc. Those rows are REMOVED from consideration, and 
#' only Sample/Target combinations that remain within the dataframe of laboratory 
#' data will be considered further. The goal is for the system to only consider 
#' sample rows, NOT controls.
#' 
#' For each unique Sample/Target pair left for consideration, the sum of the 
#' 'Positives' columns is calculated. For example, if you had run a sample 
#' in triplicate and the 'Positives' value for the three wells were 1, 6, and 12, 
#' then the sum would be 19. If any of those sums calculated are less than the 
#' numeric indicator for the sum of positive droplets check, then the Sample-Target 
#' pairs that are low would be printed to the console. 
#' 
#' The output of this function is a dataframe with a new column added, 
#' called sample_wells_positives. This column set to 0 if the sum of 'Positives' 
#' is greater than or equal to the set limit, set to 1 if the sum of 'Positives' 
#' is less than the set limit, and 'NA' if the row was excluded from consideration.
#'
#' @param new_file_in A dataframe of laboratory data
#' @param sum_pos_dropa numeric indicator for the sum of positive droplets check
#' @param controls_to_drop a dataframe of Sample-Target pairs to EXCLUDE from this check
#' @return A dataframe just like the input data frame, with one new column (sample_wells_positives) added
#' @export

w0700_pos_droplet_sum <- function(new_file_in, sum_pos_drop = 4, controls_to_drop){
  
  # add warning for LOD
  message(paste0("CHECK #7: IF ANY SAMPLES HAVE A SUM OF POSITIVES DROPLET COUNT LESS THAN ", sum_pos_drop))
  
  ### need to filter out controls from consideration
  SAM_wells <- data.frame()
  
  for (each_row in seq(1, nrow(controls_to_drop))){
    
    sample1 <- controls_to_drop[each_row, 1]
    target1 <- controls_to_drop[each_row, 2]
    
    sam_rows <- filter(new_file_in, grepl(sample1, Sample) & grepl(target1, Target))
    
    SAM_wells <- rbind(SAM_wells, sam_rows)
    
  }
  
  SAM_wells2 <- anti_join(new_file_in, SAM_wells)
  new_file_in <- SAM_wells
  
  positives_check <- SAM_wells2 %>% group_by(Sample, Target) %>% summarize(sum_positives = sum(Positives, na.rm = TRUE))
  
  if (any(positives_check$sum_positives < sum_pos_drop)){
    
    message(paste0("Samples with sum of positive droplet count less than ", sum_pos_drop, ":"))
    
    belows <- filter(positives_check, sum_positives < sum_pos_drop) 
    
    message("Sample | Target | Sum of Positives")
    
    for (each_one in seq(1, nrow(belows))){
      
      message(belows[each_one, 1]," | ", belows[each_one, 2]," | ", belows[each_one, 3])
      
    }
    
    message(paste0(nrow(belows), " of ", nrow(positives_check), " sample/target combinations have sum of positive droplet count less than ", sum_pos_drop, "."))
    
  }
  
  
  # need to edit new_file_in to have a new marker column
  new_file_in$sample_wells_positives <- NA_real_
  
  SAM_wells2 <- SAM_wells2 %>% group_by(Sample, Target) %>% 
    mutate(sample_wells_positives = case_when(sum(Positives, na.rm = TRUE) >= sum_pos_drop ~ 0, 
                                              T ~ 1))
  
  new_file_in <- rbind(new_file_in, SAM_wells2)
  
  message("Through CHECK #7.")
  
  return(new_file_in)
  
}
