#'
#' Sample Negative Droplet Sum Check
#'
#' This function is meant to be used to check non-control wells for low negative
#' droplet counts. It can be used as just a check function, or as a check that will
#' stop code from running.
#'
#' This function takes in a dataframe of laboratory data, a numeric indicator for
#' the sum of negative droplets check, as well as a dataframe of Sample-Target
#' pairs to EXCLUDE from this check. Finally, it takes in a character string,
#' either "yes" or "no", to indicate whether the user does (yes) or does not (no)
#' want to treat the check as a stop check.
#'
#' Sample-Target pairs example:
#'
#'  If the dataframe looks like:
#'
#'   | Sample | Target |
#'   | --- | --- |
#'   | POS | N1 |
#'   | BCOV | BCOV |
#'   | EXT | N1 |
#'
#' Then the code will look for any instances where the 'Sample' column contains
#' the string "POS" AND the 'Target' column contains the string "N1", and any
#' instances where the 'Sample' column contains the string "BCOV" AND the 'Target'
#' column contains "BCOV", etc. Those rows are REMOVED from consideration, and only
#' Sample/Target combinations that remain within the dataframe of laboratory data
#' will be considered further. The intended purpose is for the system to only consider sample
#' rows, NOT controls.
#'
#' For each unique Sample/Target pair left for consideration, the sum of the
#' 'Negatives' columns is calculated. For example, if you had run a sample in
#' triplicate and the 'Negatives' value for the three wells were 1, 6, and 12,
#' then the sum would be 19. If any of those sums calculated are less than the
#' numeric indicator for the sum of negative droplets check, then the Sample-Target
#' pairs that are low would be printed to the console.
#'
#' STOP ALERT: If "yes" is entered as stop_choice, and any
#' of the samples have fewer than the numeric indicator for the sum of negative
#' droplets check, the code will STOP RUNNING.
#'
#' If "no" is entered instead, the output of this function will be a dataframe
#' with a new column added, called 'sample_wells_negatives75'. This column set to 0
#' if the sum of 'Negatives' is greater than or equal to the set limit, set to 1
#' if the sum of 'Negatives' is less than the set limit, and 'NA' if the row was
#' excluded from consideration.
#'
#' @param new_file_in A dataframe of laboratory data
#' @param sum_neg_drop a numeric indicator for the sum of negative droplets check
#' @param controls_to_drop a dataframe of Sample-Target pairs to EXCLUDE from this check
#' @param stop_choice a character string, either 'yes' or 'no' to indicate whether you'd like to treat this as a STOP check.
#' @return A list with the first element being a dataframe just like the input data frame, with one new column (sample_wells_negatives75) added, and the second element being either 0 (for no failure stop) or 1 (for failure stop)
#' @export

w0750_neg_droplet_samples <- function(new_file_in, sum_neg_drop = 4, controls_to_drop, stop_choice = "no"){

    # add warning for LOD
    message(paste0("CHECK #7.5: IF ANY SAMPLES HAVE A SUM OF NEGATIVES DROPLET COUNT LESS THAN ", sum_neg_drop))
    message("") # just for visual clarity

    if (!trimws(tolower(stop_choice)) %in% c("yes", "no")){

      stop("Stop choice entry is incorrect. Please use either 'yes' or 'no'.")

    }

    stop_indicator <- 0

    ### need to filter out controls from consideration
    SAM_wells <- data.frame()

    for (each_row in seq(1, nrow(controls_to_drop))){

      sample1 <- controls_to_drop[each_row, 1]
      target1 <- controls_to_drop[each_row, 2]

      sam_rows <- filter(new_file_in, grepl(sample1, Sample) & grepl(target1, Target))

      SAM_wells <- rbind(SAM_wells, sam_rows)

    }

    SAM_wells2 <- suppressMessages(anti_join(new_file_in, SAM_wells))
    new_file_in <- SAM_wells

    negatives_check <- SAM_wells2 %>% group_by(Sample, Target) %>% summarize(sum_negatives = sum(Negatives, na.rm = TRUE))

    if (any(negatives_check$sum_negatives < sum_neg_drop)){

      message(paste0("Samples with sum of negative droplet count less than ", sum_neg_drop, ":"))

      belows <- filter(negatives_check, sum_negatives < sum_neg_drop)

      message("Sample | Target | Sum of Negatives")

      for (each_one in seq(1, nrow(belows))){

        message(belows[each_one, 1]," | ", belows[each_one, 2]," | ", belows[each_one, 3])

      }

      message("") # just for visual clarity
      message(paste0(nrow(belows), " of ", nrow(negatives_check), " sample/target combinations have sum of negative droplet count less than ", sum_neg_drop, "."))

      # stop check implementation - leaving it flexible, in case this is something you
      # just want to be notified on, rather than always being a stop the code alert
      if (tolower(trimws(stop_choice)) == "yes"){

        stop_message <- "STOP - Consider disregarding results and rerun ddPCR to determine whether the sample may be a false positive or if it needs to be diluted."

        message(stop_message)
        stop_indicator <- 1

      }

    }


    # need to edit new_file_in to have a new marker column
    new_file_in$sample_wells_negatives75 <- NA_real_

    SAM_wells2 <- SAM_wells2 %>% group_by(Sample, Target) %>%
      mutate(sample_wells_negatives75 = case_when(sum(Negatives, na.rm = TRUE) >= sum_neg_drop ~ 0,
                                                T ~ 1))

    if (sum(SAM_wells2$sample_wells_negatives75, na.rm = TRUE) == 0){

      message(paste0("No samples had a sum of negative droplets less than ", sum_neg_drop, "."))

    }

    new_file_in <- rbind(new_file_in, SAM_wells2)

    message("") # just for visual clarity
    message("Through CHECK #7.5")
    message("")

    if (stop_indicator == 1 & stop_choice == "yes"){
      stop("Stop error encountered - #7.5")
    }

    return(list(new_file_in, stop_indicator))

}

