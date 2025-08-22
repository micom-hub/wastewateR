
#' Control Hard Stop
#' 
#' Checking if Positive Droplet counts are too low
#' 
#' This function takes in a data frame of laboratory data, as well as a dataframe of Sample-Target pairs to apply 
#' this check to. It also takes in a numeric positives droplet limit. The default 
#' positives droplet limit is 3. It is a Positive Control Well Check for any 'Sample' 
#' and 'Target' pair that contains the strings indicated in the rows of the indicating data frame.  
#' 
#' Sample-Target pairs example:
#' If the dataframe looks like: 
#'  
#' | Sample | Target | 
#' | --- | --- | 
#' | POS | N1 | 
#' | BCOV | BCOV | 
#' 
#' Then the code will look for any instances where the 'Sample' column contains 
#' the string "POS" AND the 'Target' column contains the string "N1", and any 
#' instances where the 'Sample' column contains the string "BCOV" AND the 'Target' 
#' column contains "BCOV". Those rows are exclusively looked at for applying the 
#' positive droplet limit to the values in the 'Positives' column. 
#' 
#' STOP ALERT: If any row has fewer positive droplets than the positives droplet 
#' limit, the offending Sample, Target, and Positives column values will be 
#' printed to the console. The code will STOP RUNNING if this occurs.
#' 
#' Nothing is returned from this function, it is only a check.
#'
#' @param new_file_in A dataframe of laboratory data
#' @param samples_targets A dataframe of Sample-Target pairs to apply this check to
#' @param pos_drop_lim A numeric positives droplet limit. Default value set to 3
#' @return Nothing
#' @export

w0400_pos_control_hard_stop <- function(new_file_in, samples_targets, pos_drop_lim = 3){
  
  message("CHECK #4: Positive Control Well Check - Hard Stop")
  message("") # just for visual clarity
  
  POS_wells <- data.frame()
  # get down to only options that are positive control wells
  for (each_row in seq(1, nrow(samples_targets))){
    
    sample1 <- samples_targets[each_row, 1]
    target1 <- samples_targets[each_row, 2]
    
    pos_rows <- filter(new_file_in, grepl(sample1, Sample) & grepl(target1, Target))
    
    POS_wells <- rbind(POS_wells, pos_rows)
    
  }
  
  # if anything in this set has a well with a Positive droplet count less than 3
  if (any(POS_wells$Positives < pos_drop_lim)){
    # figure out what the samples are
    POS_wells2 <- filter(POS_wells, Positives < pos_drop_lim) %>% select(Sample, Target, Positives)
    
    message("Sample | Target | Positives")
    
    for (i in seq(1, nrow(POS_wells2))){
      
      message(paste0(POS_wells2[i, 1], " | ", POS_wells2[i, 2], " | ", POS_wells2[i, 3]))
      
    }
    
    stop_message <- paste0("At least one indicated Positive Well row has fewer than ", pos_drop_lim, " positive droplets.")
    
    stop(stop_message) # and exit
    
  } else {
    
    message(paste0("All indicated Positive Well rows have ", pos_drop_lim, " or more positive droplets."))
    
  }
  
  message("") # just for visual clarity
  message("Through Check #4")
  
}