#' Check for the presence of expected control labeling in Sample column
#'
#' This function takes in a data frame of laboratory data
#' and checks for the presence of control wells. This function also
#' takes in a vector of character strings to use to identify control wells,
#' and a numeric vector of the expected number of wells for each control type.
#' The two vectors should be the same length. All it does is provide some alerts
#' regarding control sample well presence/absence it does not edit the input - input
#' and output are identical in this case.
#'
#' The default controls the system is looking for are Sample names that
#' contain: "NEG", "POS", "NTC", "EXT", "BCOV"
#'
#' The default numeric vector of the expected number of wells is: 3, 3, 3, 3, 3
#'
#' Please note: This check will not alert if there is a control that is NOT
#' mentioned.
#'
#' The system will print out a table of the Sample, Target, and well count of each for reference,
#' for all rows identified as containing the Control strings.
#'
#' The system is only alerting for well counts that are LESS THAN the indicated value.
#'
#' if stop_choice is "yes" then:
#' STOP ALERT: If there are any instances of inconsistencies between the actual
#' number of control wells identified and the expected number as indicated by
#' the two input vectors, the offending control type and the well counts will be
#' printed to the console. The code will STOP RUNNING if this occurs.
#'
#' if stop_choice is "no" then:
#' The error information will print to the console and the return list will have the
#' output data frame as the first element in the list and a stop indicator of 1 as the
#' second element in the list.
#'
#' @param df_in A dataframe of laboratory data, must contain a column called Sample
#' @param exp_cntrl_v A vector of character strings that indicate control wells. Default vector is "NEG", "POS", "NTC", "BCOV", "EXT"
#' @param expected_count A numeric vector of the expected number of wells for each control type. Default vector is 3, 3, 3, 3, 3
#' @param stop_choice A character string of "yes" or "no" to indicate whether this should be a hard stop function or not
#' @return A list with the first element being a dataframe identical to df_in, and the second element being either 0 (for no failure stop) or 1 (for failure stop)
#' @export

w0125_expected_controls_present <- function(df_in,
                                            exp_cntrl_v = c("NEG", "POS", "NTC", "BCOV", "EXT"),
                                            expected_count = c(3, 3, 3, 3, 3),
                                            stop_choice = "no"){

    message("CHECK #1.25: Control Samples Present")
    message("") # just for visual clarity

    ################################################################################
    # Check #1.25: Make sure expected control sample names are present (essentially, that
    # there are controls present)

    control_samples_here <- data.frame()

    message("Control | Target | Well Count")

    for (each_control in exp_cntrl_v){

      fin <- filter(df_in, grepl(each_control, Sample))

      for (each_target in unique(fin$Target)){

          fin2 <- filter(fin, Target == each_target)

          message(paste0(each_control, " | ", each_target, " | ", nrow(fin2)))
          control_samples_here <- rbind(control_samples_here, fin2) #list the control well type, as well as the number of wells for each control type, for each target

      }

    }

    message("") # just for visual clarity

    alert_set <- data.frame(ControlValue = NA,
                            TargetValue = NA,
                            ExpectedWellCount = NA,
                            ActualWellCount = NA)
    alert_set <- alert_set[-1, ]

    # checking against given numbers
    for (each_num in seq(1, length(exp_cntrl_v))){

        fin <- filter(df_in, grepl(exp_cntrl_v[each_num], Sample))

        if (nrow(fin) == 0){
          # no identified samples

          vect <- data.frame(ControlValue = exp_cntrl_v[each_num],
                             TargetValue = NA_character_,
                             ExpectedWellCount = expected_count[each_num],
                             ActualWellCount = 0)

          alert_set <- rbind(alert_set, vect)

        } else {

            fin <- fin %>% group_by(Sample, Target) %>% summarize(count_wells = length(Well))

            for (each_row in seq(1, nrow(fin))){

                control_wells <- fin[each_row, 3]
                alert_num <- expected_count[each_num]

                if (control_wells < alert_num){
                    # if there are fewer than expected

                    vect <- data.frame(ControlValue = exp_cntrl_v[each_num],
                                       TargetValue = fin[each_row, 2],
                               ExpectedWellCount = alert_num,
                               ActualWellCount = control_wells)
                    alert_set <- rbind(alert_set, vect)

                }

            }
        }

    }

    if (nrow(alert_set) > 0){

      message("There are discrepancies between the actual number of control wells and the expected number of control wells:")
      message("Control | Target | Expected | Actual")

      for (i in seq(1, nrow(alert_set))){

        message(paste0(alert_set[i, 1], " | ", alert_set[i, 2], " | ", alert_set[i, 3], " | ", alert_set[i, 4]))

      }

      if (stop_choice == "yes"){
          stop()
      } else if (stop_choice == "no"){
          stopper_ind <- 1
          message("Stop error encountered - #1.25")
      }

    } else {

      stopper_ind <- 0

    }


    message("") # just for visual clarity
    message("Through Check #1.25") # notify the check is over
    message("")

    return(list(df_in, stopper_ind)) # return our input

}

