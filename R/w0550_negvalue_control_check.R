#' Control Check, Ensuring Controls that Should be Negative, Are Negative; Optional **stop**.
#'
#' Checks if Positives Droplet counts are greater than or equal to indicated value.
#'
#' This function takes in a laboratory data frame, as well as a dataframe of Sample-Target pairs to apply this check to. It takes in a numeric positives droplet limit. The default positives droplet limit is 3. It also takes in a character "yes" or "no" stop choice to indicate whether to treat the rule as a hard stop (yes) or a soft stop (no).
#'
#' Sample-Target pairs example:
#'
#' Sample-Target pairs example, for pairs that are expected to be negative:
#'
#' | Sample | Target |
#' | --- | --- |
#' | NEG | PMMOV |
#' | EXT | PMMOV |
#' | NEG | N1 |
#' | EXT | N1 |
#'
#' The input to the samples_target parameter would be: as.dataframe(Sample = c("NEG", "EXT", "NEG", "EXT"), Target = c("PMMOV", "PMMOV", "N1", "N1"))
#'
#' This check looks at the indicated control rows and marks them if the number of positive droplets is greater than or equal to the numeric droplet limit (pos_drop_limit parameter). The mark occurs in a column called 'negvalue_control_check55', which will contain a value of 1 if the 'Positives' column of the indicated Sample-Target pairs is greater than or equal to the droplet limit, and otherwise will contain zeros. Any rows that were not considered in this check will have 'NA' filled in this column.
#'
#' If the stop_choice parameter is set to "yes", the function will stop running (i.e. the code will stop running) if there are instances where negvalue_control_check55 is equal to 1. If the stop_choice parameter is set to "no", the function will not stop running if there are instances where negvalue_control_check55 is equal to 1, but instead will record the "fail" in the second element of the returned list.
#'
#' This function returns a list with the first element being a dataframe just like the input data frame, with one new column (negvalue_control_check55) added, and the second element being either 0 (for no failure stop) or 1 (for failure stop).
#'
#' @param new_file_in A dataframe of laboratory data
#' @param samples_targets A dataframe of Sample-Target pairs to apply this check to
#' @param pos_drop_limit A numeric positives droplet limit. Default value set to 3
#' @param stop_choice A character string of "yes" or "no" to indicate whether this should be a hard stop function or not
#' @return A list with the first element being a dataframe just like the input data frame, with one new column (negvalue_control_check55) added, and the second element being either 0 (for no failure stop) or 1 (for failure stop)
#' @export

w0550_negvalue_control_check <- function(new_file_in,
                                         samples_targets,
                                         pos_drop_limit = 3,
                                         stop_choice = "no"){

  message("CHECK #5.5: Control Check - Should be Negative")
  message("") # just for visual clarity

  stop_indicator <- 0

  POS_wells <- data.frame()
  # get down to only options that are control wells
  for (each_row in seq(1, nrow(samples_targets))){

    sample1 <- samples_targets[each_row, 1]
    target1 <- samples_targets[each_row, 2]

    pos_rows <- filter(new_file_in, grepl(sample1, Sample) & grepl(target1, Target))

    # take those rows out of our main file
    new_file_in <- suppressMessages(anti_join(new_file_in, pos_rows))

    POS_wells <- rbind(POS_wells, pos_rows)

  }

  if (nrow(POS_wells) == 0){

    message("No rows with the indicated Sample-Target combinations were present in this dataset.")
    message("Sample-Target combinations provided:")
    for (i in seq(1, nrow(samples_targets))){
      message(paste0(samples_targets[i, 1], " - ", samples_targets[i, 2]))
    }

    new_file_in$negvalue_control_check55 <- NA_real_

  } else {

    ### need to figure out how many wells in each set are off expectation

    POS_wells_by_set <- POS_wells %>% mutate(overPOS = case_when(Positives >= pos_drop_limit ~ 1,
                                                                 T ~ 0)) %>%
                                               group_by(Sample, Target) %>%
                                               summarize(count_wells = length(Well),
                                                         count_over = sum(overPOS))

    # if it's just one, print out the warning, but keep moving
    # if it's more than one, print out the warning, but instigate a stop alert

    for (each_row in seq(1, nrow(POS_wells_by_set))){

      # look at the row of interest
      row_oi <- POS_wells_by_set[each_row, ] # get the row, all columns

      if (row_oi$count_over > 1){

        # alert/stop
        stop_indicator <- 1

        message("Stop error encountered - #5.5")
        message(paste0("Sample = ", row_oi$Sample, " - Target = ", row_oi$Target, " has > 1 well over the Positive droplet limit of ", pos_drop_limit))
        message("")

      } else if (row_oi$count_over == 1){

        # just a warning
        message(paste0("Sample = ", row_oi$Sample, " - Target = ", row_oi$Target, " has 1 well over the Positive droplet limit of ", pos_drop_limit))
        message("")

      }


    }

    ####

    new_file_in$negvalue_control_check55 <- NA_real_

    POS_well <- POS_wells %>% mutate(negvalue_control_check55 = case_when(Positives >= pos_drop_limit ~ 1,
                                                                        T ~ 0))

    new_file_in <- rbind(new_file_in, POS_well)

  }

  if (stop_choice == "yes" & stop_indicator == 1){
    stop("Stop error implemented - #5.5")
  }

  if (stop_indicator == 0){

    message("")
    message("Sample | Target | Positives")

    for (i in seq(1, nrow(POS_wells))){

      message(paste0(POS_wells[i, 2], " | ", POS_wells[i, 3], " | ", POS_wells[i, 7]))

    }

    message("")
    message(paste0("All indicated controls had fewer than ", pos_drop_limit, " positive droplets."))

  }

  message("") # just for visual clarity
  message("Through Check #5.5")
  message("")

  return(list(new_file_in, stop_indicator))

}

