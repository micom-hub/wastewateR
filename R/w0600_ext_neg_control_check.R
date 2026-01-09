
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
#' @param ext_well_count A numeric indicator for how many EXT control wells are expected (for example, if you'd expect there to be 3 EXT Sample wells, you would enter 3); default value is 3
#' @param neg_well_count A numeric indicator for how many NEG control wells are expected (for example, if you'd expect there to be 3 NEG Sample wells, you would enter 3); default value is 3
#' @param positive_droplet A numeric positives droplet limit. Default value set to 3
#' @param wells_over A numeric indicator for the acceptable number of wells that you'd allow to be over the positive droplet limit (for example, if you had run a control in quaduplicate, you might still accept the plate data results if 2 of the 4 controls were over the positive droplet limit, so you'd enter 2); default value is 1
#' @param stop_choice A character string of "yes" or "no" to indicate whether this should be a hard stop function or not
#' @return A list with the first element being a dataframe just like the input data frame, with one new column (ext_neg_control_check6) added, and the second element being either 0 (for no failure stop) or 1 (for failure stop)
#' @export


w0600_ext_neg_control_check <- function(new_file_in, ext_well_count = 3, neg_well_count = 3, positive_droplet = 3, wells_over = 1, stop_choice = "no"){

  message("CHECK #6: Extraction Control & Negative Control Well Check")
  message("") #aesthetics

  stop_indicator <- 0

  x <- 0

  for (each_control_type in c("EXT", "NEG")){

    if (each_control_type == "EXT"){
      control_well_count <- ext_well_count
    } else if (each_control_type == "NEG"){
      control_well_count <- neg_well_count
    }


    controls <- filter(new_file_in, grepl(each_control_type, Sample))

    controls_g <- controls %>% group_by(Sample, Target) %>% summarize(count = length(Well))

    if (any(controls_g$count != control_well_count)){
      controls2 <- controls %>% select(Well, Sample, Target)

      message("Well | Sample | Target")
      for (i in seq(1, nrow(controls2))){

        message(paste0(controls2[i, 1], " | ", controls2[i, 2], " | ", controls2[i, 3]))

      }
      message("") #aesthetics
      stop_message <- paste0("Not ", control_well_count, " rows with ", each_control_type, " in Sample name")

      message(stop_message)

      x <- x + 1
    }

  }

  if (x != 0){
    stop_indicator <- 1
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
        message("") #aesthetics
        stop_message <- paste0("More than ", wells_over, " ", each_control_type, " control replicates have ", positive_droplet, " or more positive droplets.")

        message(stop_message)

        y <- y + 1

      } else {

        # just a warning printed out

        bad_ones <- filter(count_controls, count_over_2 >= wells_over)
        example_set <- filter(controls, Sample %in% bad_ones$Sample) %>% select(Sample, Target, Positives)
        example_set <- filter(example_set, Target %in% bad_ones$Target)


        message("Sample | Target | Positives")

        for (i in seq(1, nrow(example_set))){

          message(paste0(example_set[i, 1], " | ", example_set[i, 2], " | ", example_set[i, 3]))

        }
        message("") #aesthetics
        message(paste0(wells_over, " or fewer but more than 0 ", each_control_type, " control replicates have ", positive_droplet, " or more positive droplets."))

      }
    } else {

      message(paste0("All ", each_control_type, " control replicates have fewer than ", positive_droplet, " positive droplets."))

    }

  }

  if (y != 0){
    stop_indicator <- 1
  }


  new_file_in <- new_file_in %>% mutate(ext_neg_control_check6 = case_when(grepl("NEG", Sample) & Positives >= positive_droplet ~ 1,
                                                                           grepl("EXT", Sample) & Positives >= positive_droplet ~ 1,
                                                                           grepl("NEG", Sample) ~ 0,
                                                                           grepl("EXT", Sample) ~ 0,
                                                                           T ~ NA_real_))
  message("") # just for visual clarity
  message("Through Check #6")

  if (stop_choice == "yes" & stop_indicator == 1){
    stop("Stop error encountered - #6")
  }

  return(list(new_file_in, stop_indicator))

}
