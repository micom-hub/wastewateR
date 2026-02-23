
#' Extraction & Negative Control Check
#'
#' This function is looking for extraction control (EXT) and negative control (NEG)
#' 'Sample' rows. It takes in:
#' - a laboratory data frame
#' - a numeric indicator for how many EXT control wells are expected
#' - a numeric indicator for how many NEG control wells are expected
#' - a numeric positive droplet limit; default value is 3
#' - an acceptable number of wells that you'd allow to be over the positive
#' droplet limit (for example, if you had run a control in quaduplicate, you might
#' still accept the plate data results if 2 of the 4 controls were over the positive
#' droplet limit, so you'd enter 2); default value is 1
#' - a character string of "yes" or "no" for whether the user would like to treat this
#' function as a hard stop function
#'
#' Extraction controls and negative controls are identified as any rows that have
#' "EXT" or "NEG" in the character string of the 'Sample' column.
#'
#' if stop_choice == "yes":
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
#' A list of a data frame and a numeric indicator of 0 (if no stop trigger was encountered)
#' or 1 (if a stop trigger was encountered) is returned from this function.
#'
#' @param new_file_in A dataframe of laboratory data
#' @param two_control_options A vector of two control abbreviations that should be checked for NO positive signal; default value = c("EXT", "NEG")
#' @param extraction_control_value A character string that is the extraction control Target that should be checked for NO positive signal; default value is "BCOV"
#' @param pos_or_neg A character string of "positive" or "negative" depending on what the user is checking for (i.e. are the wells expected to be negative - no/few positive droplets - or expected to be positive - lots of Positive droplets)
#' @param positive_droplet A numeric positives droplet limit. Default value set to 3
#' @param wells_over A numeric indicator for the acceptable number of wells that you'd allow to be over the positive droplet limit (for example, if you had run a control in quaduplicate, you might still accept the plate data results if 2 of the 4 controls were over the positive droplet limit, so you'd enter 2); default value is 1
#' @param stop_choice A character string of "yes" or "no" to indicate whether this should be a hard stop function or not
#' @return A list with the first element being a dataframe just like the input data frame, with one new column (ext_neg_control_check6) added, and the second element being either 0 (for no failure stop) or 1 (for failure stop)
#' @export


w0600_ext_neg_control_check <- function(new_file_in, two_control_options = c("EXT", "NEG"), extraction_control_value = "BCOV", pos_or_neg = "negative", positive_droplet = 3, wells_over = 1, stop_choice = "no"){

  # initial messaging
  message("CHECK #6: Control Well Check - Extraction Negatives")
  message("") #aesthetics

  stop_indicator <- 0 # setting up stop notification holder

  x <- 0 # setting up error catcher
  # this allows us to iterate through both EXT and NEG scenarios, then make note
  # of the error (rather than erroring out on one, having a situation where that's fixed,
  # then running again, and the second one errors out on the same thing)

  # do the following for both EXT and NEG samples, closed with '### ;'
  # note: this is hard coded to only work for "EXT" and "NEG" naming convention, however
  # it is flexible enough that it's looking for these character strings inside the
  # sample name - so "NEG1" would still get picked up, or "EXT 2026"
  for (each_control_type in two_control_options){

    # looking only at either our EXT or NEG sample names. Note this is looking at
    # all possible Target values for these wells
    controls <- filter(new_file_in, grepl(each_control_type, Sample) & Target == extraction_control_value)

    # group either our EXT or NEG samples by Sample name and Target, then count how
    # many Wells are in each
    controls_g <- controls %>% group_by(Sample, Target) %>% summarize(count = length(Well))

    # if there are discrepancies, we want to see them. so we grab all of them
    # (not JUST the sample/target combination that alerted.)
    controls2 <- controls %>% select(Well, Sample, Target, Positives, Negatives)

    message("Well | Sample | Target | Positives | Negatives ") # print out a header

    for (i in seq(1, nrow(controls2))){

        message(paste0(controls2[i, 1], " | ", controls2[i, 2], " | ", controls2[i, 3], " | ", controls2[i, 4], " | ", controls2[i, 5]))

    } # and message out a row for every well, sample, and target line that was pulled
  }
  #####

  # second half of this test/check
  y <- 0 # setting up error catcher

  # do the following for both EXT and NEG samples, closed with '### **'
  for (each_control_type in two_control_options){

    # again get the EXT or NEG samples only (agnostic of Target type)
    controls <- filter(new_file_in, grepl(each_control_type, Sample))

    if (pos_or_neg == "negative"){
      # for each row, mark with 1 if the Positives column is greater than or equal to the
      # positive droplet limit set (default value is 3)
      count_controls <- controls %>% mutate(count_over = case_when(Positives >= positive_droplet ~ 1,
                                                                   T ~ 0))
      # expected to be negative, so alert if too much positive
    } else if (pos_or_neg == "positive"){
      count_controls <- controls %>% mutate(count_over = case_when(Positives <= positive_droplet ~ 1,
                                                                   T ~ 0))
      # expected to be positive, so alert if too much negative
    }
    # then group by Sample/Target combinations, and sum them
    # so we should have a single row for each sample name and target type combination
    # with a third column that is the sum of the number of wells that are over the limit.
    #
    count_controls <- count_controls %>%
      group_by(Sample, Target) %>%
      summarize(count_over_2 = sum(count_over, na.rm = TRUE))

    # if that sum is not zero then ...
    if (any(count_controls$count_over_2 != 0)){ # end of this if/else is marked with '## ---'

      if (any(count_controls$count_over_2 > wells_over)){ # wells over is the number of wells we're willing to allow
        # to "fail"

        bad_ones <- filter(count_controls, count_over_2 > wells_over) # get the rows that are over
        example_set <- filter(controls, Sample %in% bad_ones$Sample) %>% select(Sample, Target, Positives)
        example_set <- filter(example_set, Target %in% bad_ones$Target)


        message("Sample | Target | Positives")

        for (i in seq(1, nrow(example_set))){

          message(paste0(example_set[i, 1], " | ", example_set[i, 2], " | ", example_set[i, 3]))

        }
        message("") #aesthetics

        if (pos_or_neg == "negative"){
            stop_message <- paste0("More than ", wells_over, " ", each_control_type, " control replicates have ", positive_droplet, " or more positive droplets.")
        } else if (pos_or_neg == "positive"){
          stop_message <- paste0("More than ", wells_over, " ", each_control_type, " control replicates have ", positive_droplet, " or fewer positive droplets.")
        }
        message(stop_message)

        y <- y + 1

      } else {

        # just a warning printed out

        bad_ones <- filter(count_controls, count_over_2 <= wells_over)
        example_set <- filter(controls, Sample %in% bad_ones$Sample) %>% select(Sample, Target, Positives)
        example_set <- filter(example_set, Target %in% bad_ones$Target)


        message("Sample | Target | Positives")

        for (i in seq(1, nrow(example_set))){

          message(paste0(example_set[i, 1], " | ", example_set[i, 2], " | ", example_set[i, 3]))

        }
        message("") #aesthetics

        if (pos_or_neg == "negative"){
            message(paste0(wells_over, " or fewer but more than 0 ", each_control_type, " control replicates have ", positive_droplet, " or more positive droplets."))
        } else if (pos_or_neg == "positive"){
          message(paste0(wells_over, " or fewer but more than 0 ", each_control_type, " control replicates have ", positive_droplet, " or fewer positive droplets."))
        }

      }
    } else {

      if (pos_or_neg == "negative"){
          message(paste0("All ", each_control_type, " control replicates have fewer than ", positive_droplet, " positive droplets."))
      } else if (pos_or_neg == "positive"){
        message(paste0("All ", each_control_type, " control replicates have more than ", positive_droplet, " positive droplets."))
      }


    } ## ---

  } ### **

  if (y != 0){
    stop_indicator <- 1
  }


      if (pos_or_neg == "negative"){
            new_file_in <- new_file_in %>% mutate(ext_neg_control_check6 = case_when(grepl(two_control_options[2], Sample) & Positives >= positive_droplet ~ 1,
                                                                           grepl(two_control_options[1], Sample) & Positives >= positive_droplet ~ 1,
                                                                           grepl(two_control_options[2], Sample) ~ 0,
                                                                           grepl(two_control_options[1], Sample) ~ 0,
                                                                           T ~ NA_real_))
      } else if (pos_or_neg == "positive"){
            new_file_in <- new_file_in %>% mutate(ext_neg_control_check6 = case_when(grepl(two_control_options[2], Sample) & Positives <= positive_droplet ~ 1,
                                                                                 grepl(two_control_options[1], Sample) & Positives <= positive_droplet ~ 1,
                                                                                 grepl(two_control_options[2], Sample) ~ 0,
                                                                                 grepl(two_control_options[1], Sample) ~ 0,
                                                                                 T ~ NA_real_))

      }

  message("") # just for visual clarity
  message("Through Check #6")
  message("")

  if (stop_choice == "yes" & stop_indicator == 1){
    stop("Stop error encountered - #6")
  }

  return(list(new_file_in, stop_indicator))

}
