
#' Extraction & Negative Control Check 
#' 
#' This function is looking for extraction control (EXT) and negative control (NEG) 
#' 'Sample' rows. It takes in: 
#' - a laboratory data frame
#' - a numeric indicator for how many control wells are expected (for example, if you'd 
#' expect there to be 3 EXT Sample wells and 3 NEG control wells, you would enter 3); 
#' default value is 3
#' - a numeric positive droplet limit; default value is 3
#' - and an acceptable number of wells that you'd allow to be over the positive 
#' droplet limit (for example, if you had run a control in quaduplicate, you might 
#' still accept the plate data results if 2 of the 4 controls were over the positive 
#' droplet limit, so you'd enter 2); default value is 1
#'
#' Extraction controls and negative controls are identified as any rows that have 
#' "EXT" or "NEG" in the character string of the 'Sample' column. 
#' 
#' STOP ALERT: If either the negative controls or the extraction controls do not 
#' have the number of rows indicated, the 'Well', 'Sample', and 'Target' column 
#' values will be printed to the console. The code will STOP RUNNING if this occurs.
#'
#' STOP ALERT: If either the negative controls or the extraction controls have 
#' more than the indicated acceptable number of wells with a 'Positives' (positive 
#' droplet measurement) greater than or equal to the indicated positive droplet 
#' limit, the 'Sample', 'Target', and 'Positives' column values will be printed 
#' to the console. The code will STOP RUNNING if this occurs.
#' 
#' If either the negative controls or the extraction controls have more than zero 
#' wells with a 'Positives' (positive droplet measurement) greater than or equal 
#' to the indicated positive droplet limit, the 'Sample', 'Target', and 'Positives' 
#' column values will be printed to the console.
#' 
#' A new column called ext_neg_control_check6 is added to the dataframe, where 
#' NEG or EXT control rows with a 'Positives' (positive droplet measurement) 
#' greater than or equal to the indicated positive droplet limit are marked with 1, 
#' otherwise they are marked with 0. Non-relevant rows are marked with 'NA'. 
#' 
#' A data frame is returned from this function.
#'
#' @param new_file_in A dataframe of laboratory data
#' @param control_well_count A numeric indicator for how many control wells are expected (for example, if you'd 
#' expect there to be 3 EXT Sample wells and 3 NEG control wells, you would enter 3); 
#' default value is 3
#' @param positive_droplet A numeric positives droplet limit. Default value set to 3
#' @param wells_over A numeric indicator for the acceptable number of wells that you'd 
#' allow to be over the positive droplet limit (for example, if you had run a control 
#' in quaduplicate, you might still accept the plate data results if 2 of the 4 controls 
#' were over the positive droplet limit, so you'd enter 2); default value is 1
#' @return A dataframe just like the input data frame, with one new column (ext_neg_control_check6) added
#' @export


w0600_ext_neg_control_check <- function(new_file_in, control_well_count = 3, positive_droplet = 3, wells_over = 1){
  
  message("CHECK #6: Extraction Control & Negative Control Well Check")
  
  x <- 0
  
  for (each_control_type in c("EXT", "NEG")){
    
    controls <- filter(new_file_in, grepl(each_control_type, Sample))
    
    controls_g <- controls %>% group_by(Sample) %>% summarize(count = length(Well))
    
    if (any(controls_g$count != control_well_count)){
      controls2 <- controls %>% select(Well, Sample, Target)
      message("Well | Sample | Target")
      for (i in seq(1, nrow(controls2))){
        
        message(paste0(controls2[i, 1], " | ", controls2[i, 2], " | ", controls2[i, 3]))
        
      }
      
      stop_message <- paste0("Not ", control_well_count, " rows with ", each_control_type, " in Sample name")
      
      message(stop_message)
      
      x <- x + 1
    }
    
  }
  
  if (x != 0){
    stop()
  }
  
  #####
  
  y <- 0
  
  for (each_control_type in c("EXT", "NEG")){
    
    controls <- filter(new_file_in, grepl(each_control_type, Sample))
    
    count_controls <- controls %>% mutate(count_over = case_when(Positives >= positive_droplet ~ 1, 
                                                                 T ~ 0)) %>% 
      group_by(Sample, Target) %>% 
      summarize(count_over_2 = sum(count_over))
    
    
    if (any(count_controls$count_over_2 != 0)){
      if (any(count_controls$count_over_2 > wells_over)){
        # find out which ones are >= 2
        bad_ones <- filter(count_controls, count_over_2 > wells_over)
        example_set <- filter(controls, Sample %in% bad_ones$Sample) %>% select(Sample, Target, Positives)
        example_set <- filter(example_set, Target %in% bad_ones$Target)
        
        message("Sample | Target | Positives")
        
        for (i in seq(1, nrow(example_set))){
          
          message(paste0(example_set[i, 1], " | ", example_set[i, 2], " | ", example_set[i, 3]))
          
        }
        
        stop_message <- paste0("More than ", wells_over, " ", each_control_type, " control replicates have ", positive_droplet, " or more positive droplets.")
        
        message(stop_message)
        
        y <- y + 1
        
      } else {
        
        # just a warning printed out
        message(paste0(wells_over, " or fewer but more than 0 ", each_control_type, " control replicates have ", positive_droplet, " or more positive droplets."))
        
        bad_ones <- filter(count_controls, count_over_2 >= wells_over)
        example_set <- filter(controls, Sample %in% bad_ones$Sample) %>% select(Sample, Target, Positives)
        example_set <- filter(example_set, Target %in% bad_ones$Target)
        
        message("Sample | Target | Positives")
        
        for (i in seq(1, nrow(example_set))){
          
          message(paste0(example_set[i, 1], " | ", example_set[i, 2], " | ", example_set[i, 3]))
          
        }
        
      }
    } else {
      
      message(paste0("All ", each_control_type, " control replicates have fewer than ", positive_droplet, " positive droplets."))
      
    }
    
  }
  
  if (y != 0){
    stop()
  }
  
  
  new_file_in <- new_file_in %>% mutate(ext_neg_control_check6 = case_when(grepl("NEG", Sample) & Positives >= positive_droplet ~ 1, 
                                                                           grepl("EXT", Sample) & Positives >= positive_droplet ~ 1, 
                                                                           grepl("NEG", Sample) ~ 0, 
                                                                           grepl("EXT", Sample) ~ 0, 
                                                                           T ~ NA_real_))
  message("") # just for visual clarity
  message("Through Check #6")
  return(new_file_in)
  
}
