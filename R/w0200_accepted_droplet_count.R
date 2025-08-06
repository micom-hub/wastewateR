#'  Check Accepted Droplet Count
#'
#' This function takes in a data frame of laboratory data and a numeric droplet 
#' count limit that by default is set at 10,000. It checks if every well has an 
#' accepted droplet count greater than or equal to the droplet count limit.
#'
#' Two new columns are added to the data frame: 
#'  - accepted_droplet_limit2: assigned a value of 1 if the row has less than 
#'  the droplet count limit in the AcceptedDroplets column
#'  - accepted_droplet_count2: for each Sample and Target combination, 
#'  contains the value of the number of wells that were below the limit. 
#'  If all the repetitions were above the limit, the value will be 0, if one 
#'  well in the set of repetitions was below the limit, the value will be 1 
#'  for all rows of the set, etc.
#' 
#' If all rows in the data frame are above the limit, there will be a message 
#' printed to the console. If there are any that were below the limit, the 
#' Sample, Target, and the AcceptedDroplet values will be printed to the 
#' console as well.
#'
#' @param df_in A dataframe of laboratory data
#' @param droplet_count_limit A numeric droplet count limit, inclusive, and default set at 10,000
#' @return A dataframe identical to df_in with two additional columns
#' @export


w0200_accepted_droplet_count <- function(df_in, droplet_count_limit = 10000){
      
    
    ################################################################################
    # Check #2: Accepted Droplet count 
    message("") # just for visual clarity
    message("CHECK #2: Accepted Droplets")
    
    # mark all wells with too low droplet counts with 1, else 0
    df_in <- df_in %>% 
      mutate(accepted_droplet_limit2 = case_when(AcceptedDroplets < droplet_count_limit ~ 1, 
                                                 T ~ 0))
    
    # sum those by sample and target, to see how many break the limit
    df_in <- df_in %>% group_by(Sample, Target) %>%
      mutate(accepted_droplet_count2 = sum(accepted_droplet_limit2, na.rm = TRUE))
  
    # add in messaging
    messaging1 <- df_in %>% mutate(sample_part = paste(Sample, Target)) %>%
      group_by(accepted_droplet_count2) %>% summarize(count_unique = length(unique(sample_part)))
    
    
    for (each_row in seq(1, nrow(messaging1))){
      
      message(paste0("There were ", messaging1[each_row, 2], " sample/target combinations with ", messaging1[each_row, 1], " wells below the Accepted Droplet Limit of ", droplet_count_limit)) 
      
      if (messaging1[each_row, 1] != 0){
        
        message("") # just for visual clarity
        message("Sample | Target | AcceptedDroplets")
        value1 <- messaging1[each_row, 1]
        set <- filter(df_in, accepted_droplet_count2 == value1)
        set <- set %>% select(Sample, Target, AcceptedDroplets) %>% distinct()
        for (i in seq(1, nrow(set))){
          
          message(paste0(set[i, 1], " | ", set[i, 2], " | ", set[i, 3]))
          
        }
        
        
      }
      
    }
    
    message("") # just for visual clarity
    message("Through Check #2")
    
    return(df_in)
}

