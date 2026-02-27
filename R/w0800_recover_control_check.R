
#' Recovery Control Check
#'
#' This function is used as a recovery control check, indicating if there are any
#' wells where any  wells that contain a tested sample with a 'Target' of the
#' recovery control (such as BCOV) are particularly low. The function takes in a
#' laboratory dataframe, a character string indicating your recovery control abbreviation
#' (ex. "BCOV"), a vector of character strings that indicate the control samples to exclude,
#' and a limit percent in decimal form. The default limit percent is 0.3 (30%). If
#' there are no recovery wells to consider based on the information provided,
#' "There were no Recovery Control / Sample Wells to consider." will be written
#' out to the console.
#'
#' For the vector of character strings to indicate control sample exclusion, an
#' example might be:
#'
#' c("POS", "NEG", "EXT", "NV")
#'
#' This is used to exclude the control 'Sample' from rows where "BCOV" is the
#' 'Target'. The goal is to leave only 'Sample' == extraction control ("BCOV") &
#' 'Target' == extraction control ("BCOV") rows, as well as all sample testing rows
#' with extraction control ("BCOV") as the 'Target'.
#'
#' The threshold is calculated as the average number of positive droplets
#' ('Positives') in the Recovery Control Sample-Target pairs (i.e. both the
#' Sample and the Target are the recovery control; i.e. BCOV-BCOV) multiplied
#' by the set limit percent.
#'
#' For the wells that contain a tested sample with a 'Target' of the recovery
#' control (such as BCOV), the average number of positive droplets ('Positives')
#' is calculated for all Sample-Target pairs (run in duplicate, triplicate, etc.).
#' If any of those averages is less than the threshold, the rows are marked.
#' The sample names are printed to the console in this case.
#'
#' This function returns a dataframe with a new column called recovery_flag8 that
#' has a value of 1 if the relevant row was below the set limit and otherwise
#' has a value of 0. If the row wasn't involved in the check, it will have a value of 'NA'.
#'
#' @param new_file_in A dataframe of laboratory data
#' @param recovery_control A character string indicating your recovery control abbreviation (ex. "BCOV")
#' @param control_ids A vector of character strings that indicate the control samples to exclude
#' @param limit_percent A limit percent in decimal form. The default limit percent is 0.3 (30%)
#' @return A dataframe just like the input data frame, with one new column (recovery_flag8) added
#' @export

w0800_recover_control_check <- function(new_file_in, recovery_control, control_ids, limit_percent = 0.3){

  message(paste0("CHECK #8: ", recovery_control, " ", limit_percent*100, "% Rule"))

  bcov_targets <- filter(new_file_in, Target == recovery_control & !grepl(recovery_control, Sample))
  bcov_targets <- filter(bcov_targets, control_check == "NOT A CONTROL")

  bcov_targets2 <- data.frame()

  for (i in control_ids){

    set <- filter(bcov_targets, grepl(i, Sample))
    bcov_targets2 <- rbind(bcov_targets2, set)

  }

  bcov_targets <- suppressMessages(anti_join(bcov_targets, bcov_targets2))


  if (nrow(bcov_targets) > 0){

    bcov_sample_bcov_target <- filter(new_file_in, Target == recovery_control & grepl(recovery_control, Sample))

    if (nrow(bcov_sample_bcov_target) > 0){

      bcov_30_limit <- mean(bcov_sample_bcov_target$Positives) * limit_percent

      bcov_targets <- bcov_targets %>% group_by(Sample) %>% mutate(average_positives = mean(Positives))
      bcov_targets <- bcov_targets %>% select(Sample, Target, Positives, average_positives)
      bcov_targets <- bcov_targets %>% mutate(recovery_flag8 = case_when(average_positives < bcov_30_limit ~ 1,
                                                                    T ~ 0))

      if (any(bcov_targets$recovery_flag8 == 1)){

        message(paste0("SOME ",recovery_control,  " TARGETS BELOW ", limit_percent*100, "% LIMIT"))
        message(paste0(recovery_control, " ", limit_percent*100, "% Limit =", bcov_30_limit))
        bcov_out <- filter(bcov_targets, recovery_flag8 == 1)

        ### print those out
        message("Sample | Target | Positives | Avg. Positives")
        for (every_row_in in seq(1, nrow(bcov_out))){

          message(paste0(bcov_out[every_row_in, 1], " | ", bcov_out[every_row_in, 2], " | ", bcov_out[every_row_in, 3], " | ", bcov_out[every_row_in, 4]))

        }

        bcov_out2 <- bcov_out %>% select(Sample, Target)
        bcov_out2$recovery_flag8 <- 1

        new_file_in <- merge(new_file_in, bcov_out2, by = c("Sample", "Target"), all.x = TRUE)

      } else {
        new_file_in$recovery_flag8 <- 0
        message(paste0("NO ", recovery_control, " TARGETS BELOW ", limit_percent*100, "% LIMIT"))

      }

    } else {

      message("There were no Recovery Control / Recovery Control wells to consider.")

    }

  } else {

    message("There were no Recovery Control / Sample Wells to consider.")
    new_file_in$recovery_flag8 <- NA

  }

  return(new_file_in)

  message("")
  message("Through Check #8")
  message("")

}
