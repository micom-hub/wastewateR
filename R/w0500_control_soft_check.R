
#' Control Warning - Positives Droplets 
#' 
#' Checks if Positives Droplet counts are greater than indicated
#' 
#' This function takes in a laboratory data frame, as well as a dataframe of 
#' Sample-Target pairs to apply this check to. It also takes in a numeric positives 
#' droplet limit. The default positives droplet limit is 3.
#' 
#' Sample-Target pairs example:
#'  
#' If the dataframe looks like: 
#' 
#' | Sample | Target | 
#' | --- | --- | 
#' | BCOV | PMMOV | 
#'  
#' This check looks at the indicated control rows and marks them if the number 
#' of positive droplets is greater than or equal to the numeric droplet limit. 
#' The mark occurs in a column called 'control_pos_drop_soft', which will contain a 
#' value of 1 if the 'Positives' column of the indicated Sample-Target pairs is 
#' >= the droplet limit, and otherwise will contain zeros. Any rows that were not 
#' considered in this check will have 'NA' filled in this column. 
#' 
#' This function returns a dataframe with a new column. 
#'
#' @param new_file_in A dataframe of laboratory data
#' @param samples_targets A dataframe of Sample-Target pairs to apply this check to
#' @param pos_drop_limit A numeric positives droplet limit. Default value set to 3
#' @return A dataframe just like the input data frame, with one new column (control_pos_drop_soft) added
#' @export

w0500_control_soft_check <- function(new_file_in, samples_targets, pos_drop_limit = 3){
  
  message("CHECK #5: Control Well - Soft Check")
  message("") # just for visual clarity
  
  POS_wells <- data.frame()
  # get down to only options that are positive control wells
  for (each_row in seq(1, nrow(samples_targets))){
    
    sample1 <- samples_targets[each_row, 1]
    target1 <- samples_targets[each_row, 2]
    
    pos_rows <- filter(new_file_in, grepl(sample1, Sample) & grepl(target1, Target))
    
    # take those rows out of our main file
    new_file_in <- anti_join(new_file_in, pos_rows)
    
    POS_wells <- rbind(POS_wells, pos_rows)
    
  }
  
  if (nrow(POS_wells) == 0){
    
    message("No rows with the indicated Sample-Target combinations were present in this dataset.")
    message("Sample-Target combinations provided:")
    for (i in seq(1, nrow(samples_targets))){
      message(paste0(samples_targets[i, 1], " - ", samples_targets[i, 2]))
    }
    
    new_file_in$control_pos_drop_soft <- NA_real_
    
  } else {
    
    # count how many
    POS_wells_count_over <- nrow(filter(POS_wells, Positives >= pos_drop_limit))
    
    if (POS_wells_count_over > 1){
      
      # if there is more than one, we stop.
      POS_wells2 <- filter(POS_wells, Positives >= pos_drop_limit) %>% select(Sample, Target, Positives)
      message('Sample | Target | Positives')
      
      for (i in seq(1, nrow(POS_wells2))){
        message(paste0(POS_wells2[i, 1], " | ", POS_wells2[i, 2], " | ", POS_wells2[i, 3]))
      }
      
      message("") # just for visual clarity
      message(paste0("At least one indicated row has ", pos_drop_limit, " or more positive droplets."))
      
      
    } else {
      
      message("All indicated rows have fewer than ", pos_drop_limit, " positive droplets.")
      
    }
    
    new_file_in$control_pos_drop_soft <- NA_real_
    
    POS_well <- POS_wells %>% mutate(control_pos_drop_soft = case_when(Positives >= pos_drop_limit ~ 1, 
                                                                       T ~ 0))
    
    new_file_in <- rbind(new_file_in, POS_well)
    
  }
  
  message("") # just for visual clarity
  message("Through Check #5")
  
  return(new_file_in)
  
}

