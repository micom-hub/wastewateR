
#' Target Positives Droplet High Limit
#'
#' This function is optional. It takes as input:
#'
#' - a laboratory data frame
#' - a vector of character strings indicating the 'Sample' values of interest
#' that indicate the positive control samples (ex. "POS")
#' - a vector of character strings indicating the 'Target' values of interest (ex. "N1")
#' - a vector of character strings that indicate the control samples to exclude
#' - a numeric threshold limit (default value = 3)
#'
#' For the two 'Sample' and 'Target' character string vectors, the two should occur
#' in order. For example, if you're looking to check N1 samples and RV samples on
#' the same data plate:
#'
#' | Sample | Target |
#' | --- | --- |
#' | POS | N1 |
#' | RV | RV |
#'
#' Then the two vectors should be:
#'
#' Sample vector: c("POS", "RV")
#' Target vector: c("N1", "RV")
#'
#' The threshold for marking sample rows is calculated as the average number of
#' positive droplets in all wells where the positive control indicator string is
#' in the 'Sample' column and the indicated target string is in the 'Target' column,
#' multiplied by the numeric threshold limit. Well rows that are NOT controls,
#' where the 'Target' contains the indicated target string(s), that have a 'Positives'
#' column value that is greater than this value are marked.
#'
#' This is just a warning/notification function. The 'Sample', 'Target', and 'Positives'
#' value for each row that violates the threshold will be printed to the console.
#' A dataframe is returned from this function, and it will contain a new variable
#' called 'sample_pos_limit_flag9' that will have a value of 1 if the row violated
#' the threshold and a value of 0 if it did not violate the threshold OR if it
#' was not considered as part of the check.
#'
#' @param new_file_in A dataframe of laboratory data
#' @param pos_samp_str A vector of character strings indicating the 'Sample' values of interest that indicate the positive control samples (ex. "POS")
#' @param target_str a vector of character strings indicating the 'Target' values of interest (ex. "N1")
#' @param control_ids a vector of character strings that indicate the control samples to exclude
#' @param thresh_size a numeric threshold limit (default value = 3)
#' @return A dataframe just like the input data frame, with one new column (sample_pos_limit_flag9) added
#' @export

w0900_positives_comparison_rule <- function(new_file_in, pos_samp_str, target_str, control_ids, thresh_size = 3){

  message("CHECK #9: Positives Comparison Rule")

  pos_to_flag <- data.frame()

  for (each_item in seq(1, length(pos_samp_str))){

    POS_target_N1 <- filter(new_file_in, grepl(pos_samp_str[each_item], Sample) & grepl(target_str[each_item], Target))
    POS_limit <- mean(POS_target_N1$Positives)

    n1_samples <- filter(new_file_in, grepl(target_str[each_item], Target))

    n1_samples2 <- data.frame()

    for (i in control_ids){

      set <- filter(n1_samples, grepl(i, Sample))
      n1_samples2 <- rbind(n1_samples2, set)

    }

    n1_samples <- anti_join(n1_samples, n1_samples2)

    n1_samples <- n1_samples %>% mutate(positives_more_than_limit = case_when(Positives > (thresh_size * POS_limit) ~ 1,
                                                                              T ~ 0))

    if (any(n1_samples$positives_more_than_limit == 1)){

      message(paste0("Some ", target_str[each_item], " targets above positives limit."))
      pos_out <- filter(n1_samples, positives_more_than_limit == 1)
      pos_out <- pos_out %>% select(Sample, Target, Positives)
      ### print those out
      message(paste0("Threshold = ", thresh_size, " * ", POS_limit))
      message("Sample | Target | Positives")

      for (i in seq(1, nrow(pos_out))){

        message(pos_out[i, 1], " | ", pos_out[i, 2], " | ", pos_out[i, 3])

      }

      pos_to_flag <- rbind(pos_to_flag, pos_out)

    } else {
      message(paste0("No ", target_str[each_item], " targets above positives limit."))

    }
  }

  if (nrow(pos_to_flag) > 0){
    ### add pos flag
    pos_to_flag$sample_pos_limit_flag9 <- 1
    new_file_in <- merge(new_file_in, pos_to_flag, by = c("Sample", "Target", "Positives"), all.x = TRUE, all.y = FALSE)
    new_file_in <- new_file_in %>% mutate(sample_pos_limit_flag9 = case_when(is.na(sample_pos_limit_flag9) ~ 0,
                                                                            T ~ sample_pos_limit_flag9))

  } else {

    new_file_in$sample_pos_limit_flag9 <- 0

  }

  message("Through Check #9.")

  return(new_file_in)

}



