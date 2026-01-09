
#' Control Potential Breakdown Check
#'
#' This is just a check/warning function.
#'
#' This function takes in a dataframe of laboratory data, as well as a dataframe
#' of Sample-Target pairs to apply this check to. It also takes in a numeric
#' positives droplet limit. The default positives droplet limit is 20. It is
#' a Positive Control Well Check for any Sample and Target pair that contains
#' the strings indicated in the rows of the indicating data frame.
#'
#' Sample-Target pairs example:
#'
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
#' If there are any wells that are below the set threshold, a warning note will
#' be printed to the console, and the 'Sample', 'Target', 'Concentration', 'CopiesPer20uLWell',
#' and 'Positives' column values for all indicated positive control wells will
#' be printed to the console as well. The rows that triggered the alert will be
#' marked with a "***".
#'
#' If there are not any wells that are below the set threshold, the 'Sample',
#' 'Target', 'Concentration', 'CopiesPer20uLWell', and 'Positives' column values
#' for all indicated positive control wells will be printed to the console just
#' for informational purposes.
#'
#' A new column will be added to the dataframe, called 'pos_control_breakdown',
#' recording any possible positive control breakdowns as ones, and otherwise
#' being filled with zeros. Any rows that were not considered in this check will
#' have 'NA' filled in this column.
#'
#' @param new_file_in A dataframe of laboratory data
#' @param samples_targets A dataframe of Sample-Target pairs to apply this check to
#' @param limit_number A numeric positives droplet limit. Default value set to 20
#' @return A dataframe just like the input data frame, with one new column (pos_control_breakdown) added
#' @export

w0450_pos_control_breakdown_check <- function(new_file_in, samples_targets, limit_number = 20){

  #new_file_in <- after_ntc
  # samples_targets <- pos_rows
  # limit_number <- 20

  message("CHECK #4.5: Positive Control Breakdown Check")
  message("") # just for visual clarity

  POS_wells <- data.frame()
  # get down to only options that are positive control wells
  for (each_row in seq(1, nrow(samples_targets))){

    sample1 <- samples_targets[each_row, 1]
    target1 <- samples_targets[each_row, 2]

    pos_rows <- filter(new_file_in, grepl(sample1, Sample) & grepl(target1, Target))

    # take those rows out of our main file
    new_file_in <- suppressMessages(anti_join(new_file_in, pos_rows))

    POS_wells <- rbind(POS_wells, pos_rows)

  }

  ### alert for general positive control breakdown
  # so look at our smaller data set
  POS_wells_breakdown <- POS_wells %>% select(Sample,
                                              Target,
                                              Concentration,
                                              CopiesPer20uLWell,
                                              Positives,
                                              Well)
  # and check the Positives droplet column. if there are fewer droplets than
  # the limit number set, make note of it!
  if (any(POS_wells_breakdown$Positives < limit_number)){
    message(paste0("Possible Positive Control Breakdown. Positive Droplet column detected with a value less than ", limit_number, ": "))

    POS_wells_breakdown <- POS_wells_breakdown %>% mutate(Mark = case_when(Positives < limit_number ~ "***",
                                                                           T ~ ""))

    message("Sample | Target | Concentration | CopiesPer20uLWell | Positives | Well | Mark")
    for (i in seq(1, nrow(POS_wells_breakdown))){

      message(paste0(POS_wells_breakdown[i, 1], " | ",
                     POS_wells_breakdown[i, 2], " | ",
                     POS_wells_breakdown[i, 3], " | ",
                     POS_wells_breakdown[i, 4], " | ",
                     POS_wells_breakdown[i, 5], " | ",
                     POS_wells_breakdown[i, 6], " | ",
                     POS_wells_breakdown[i, 7]))

    }


  } else {
    message("Positive Control Data here for reference.")

    message("Sample | Target | Concentration | CopiesPer20uLWell | Positives")
    for (i in seq(1, nrow(POS_wells_breakdown))){

      message(paste0(POS_wells_breakdown[i, 1], " | ",
                     POS_wells_breakdown[i, 2], " | ",
                     POS_wells_breakdown[i, 3], " | ",
                     POS_wells_breakdown[i, 4], " | ",
                     POS_wells_breakdown[i, 5]))

    }
  }

  new_file_in$pos_control_breakdown <- NA_real_

  POS_well <- POS_wells %>% mutate(pos_control_breakdown = case_when(Positives < limit_number ~ 1,
                                                                     T ~ 0))

  new_file_in <- rbind(new_file_in, POS_well)

  message("") # just for visual clarity
  message("Through Check #4.5")

  return(new_file_in)

}
