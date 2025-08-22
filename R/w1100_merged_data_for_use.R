
#' Merging Multiples into Single Data Point
#' 
#' This function is meant to be of use when samples or data is run in duplicate, 
#' triplicate, etc. and ideally after QAQC has been completed on the data. 
#' 
#' This function takes as input: 
#' - A data frame, with at least columns of: Well, Sample, Target, Concentration, 
#' CopiesPer20uLWell, Positives, Negatives, and AcceptedDroplets
#' - A character string indicating the method of well merging you'd like to complete, 
#' either "average" or "biorad_merge"
#' - A numeric droplet adjustment factor, if you are using the "biorad_merge" method. 
#' The default value set for this is 0.00085. 
#' 
#' For both methods, the dataframe is grouped by Sample and Target, then the following 
#' are calculated: 
#' 
#' Well = paste(Well)
#' count_wells = length(Well) 
#' CopiesPer20uLWell_sum = sum(CopiesPer20uLWell, na.rm = TRUE)
#' CopiesPer20uLWell_avg = mean(CopiesPer20uLWell, na.rm = TRUE) 
#' Positives_sum = sum(Positives, na.rm = TRUE)
#' Positives_avg = mean(Positives, na.rm = TRUE)
#' Negatives_sum = sum(Negatives, na.rm = TRUE)
#' Negatives_avg = mean(Negatives, na.rm = TRUE) 
#' AcceptedDroplets_sum = sum(AcceptedDroplets, na.rm = TRUE) 
#' AcceptedDroplets_avg = mean(AcceptedDroplets, na.rm = TRUE)
#'                 
#' The biorad method of merging data is applied only to the Concentration value, 
#' and is calculated as: 
#' 
#' concentration = (negative natural logarithm of (number of negative droplets / total number of droplets)) / (volume of droplet)
#' 
#' If you are using a Biorad machine, the droplet size is usually: 
#' For QX200 = 0.00085 
#' For QX600 = 0.000795
#' 
#' If you are using the "average" method, concentration is just calculated as the 
#' mean of the Concentration column. 
#' 
#' @param data_frame_in A dataframe of laboratory data. Needs to have columns of Well, Sample, Target, Concentration, CopiesPer20uLWell, Positives, Negatives, and AcceptedDroplets
#' @param method A character string indicating the method of well merging you'd like to complete, either "average" or "biorad_merge"
#' @param droplet_adjust A numeric droplet adjustment factor, if you are using the "biorad_merge" method. The default value set for this is 0.00085.
#' @return A dataframe containing merged Sample-Target data points
#' @export

w1100_merged_data_for_use <- function(data_frame_in, method = "average", droplet_adjust = 0.00085){
  
  if (method == "average"){
    
    data_frame_in_merge <- data_frame_in %>% group_by(Sample, Target) %>% 
      summarize(count_wells = length(Well),
                Well = paste(Well, collapse = ", "),
                Concentration = mean(Concentration, na.rm = TRUE), 
                CopiesPer20uLWell_sum = sum(CopiesPer20uLWell, na.rm = TRUE),
                CopiesPer20uLWell_avg = mean(CopiesPer20uLWell, na.rm = TRUE), 
                Positives_sum = sum(Positives, na.rm = TRUE), 
                Positives_avg = mean(Positives, na.rm = TRUE),
                Negatives_sum = sum(Negatives, na.rm = TRUE), 
                Negatives_avg = mean(Negatives, na.rm = TRUE), 
                AcceptedDroplets_sum = sum(AcceptedDroplets, na.rm = TRUE), 
                AcceptedDroplets_avg = mean(AcceptedDroplets, na.rm = TRUE))
    
  } else if (method == "biorad_merge"){
    
    message(paste0("Your droplet adjustment factor is = ", droplet_adjust))
    message("Please ensure this aligns with the machine / methodology you're using.")
    
    data_frame_in_merge <- data_frame_in %>% group_by(Sample, Target) %>% 
      summarize(count_wells = length(Well),
                Well = paste(Well, collapse = ", "),
                CopiesPer20uLWell_sum = sum(CopiesPer20uLWell, na.rm = TRUE),
                CopiesPer20uLWell_avg = mean(CopiesPer20uLWell, na.rm = TRUE), 
                Positives_sum = sum(Positives, na.rm = TRUE), 
                Positives_avg = mean(Positives, na.rm = TRUE),
                Negatives_sum = sum(Negatives, na.rm = TRUE), 
                Negatives_avg = mean(Negatives, na.rm = TRUE), 
                AcceptedDroplets_sum = sum(AcceptedDroplets, na.rm = TRUE), 
                AcceptedDroplets_avg = mean(AcceptedDroplets, na.rm = TRUE))
    
    data_frame_in_merge <- data_frame_in_merge %>% 
      mutate(Concentration = (-log(Negatives_sum / AcceptedDroplets_sum))/droplet_adjust)
    
    
  } else {
    
    message("method = ", method)
    stop("Method input not recognized. Should be 'average' or 'biorad_merge'.")
    
  }
  
  data_frame_in_merge_out <- data_frame_in_merge %>% 
    select(Sample, Target, Well, count_wells, Concentration,
           CopiesPer20uLWell_sum, CopiesPer20uLWell_avg, 
           Positives_sum, Positives_avg,
           Negatives_sum, Negatives_avg, 
           AcceptedDroplets_sum, AcceptedDroplets_avg)
  
  return(data_frame_in_merge_out)
  
}

