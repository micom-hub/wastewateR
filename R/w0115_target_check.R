#' Check #1.15: Target Confirmation and optional Sample write-out
#'
#' The purpose of this function is to check that the Targets included in the data file
#' provided are the Targets the user is expecting, to remove any Targets that are
#' not expected, and to have an optional message stating the Sample & Targets included
#' in the run.
#'
#' This function takes in:
#'
#' - new_file_in: a dataframe of laboratory results; a column called "Sample" and a column called "Target" are required components
#' - targets_allowed: a vector of character strings indicating the Target values the user would like to allow
#' - message_samples: a character string "yes" or "no" indicating whether the user would like a message write-out of all samples continuing to be used
#' - stop_choice: A character string of "yes" or "no" to indicate whether this should be a hard stop function or not
#'
#' This function will print to the console the list of targets the user provided as being allowed,
#' followed by the list of targets that are in the provided dataframe of laboratory results (new_file_in).
#'
#' It will then filter the dataframe of laboratory results so that it only contains rows
#' where the Target values are in the list of targets the user provided as being allowed. It
#' will print out the number of rows that were removed. If all rows were removed, that is a
#' stop indicator option.
#'
#' If message_smaples is "yes", then the samples that are still in the data frame will be
#' printed to the console, by Target (if multiple targets remain in the data file).
#'
#' If the stop_choice is "yes" and all rows were removed due to the Target filtering, then
#' the code will hard stop. If the stop_choice is no, the code will continue, and a list of the
#' filtered dataframe and the stop indicator of 0 (no stop rule reached) or 1 (stop rule reached)
#' will be returned by the function.
#'
#'
#' @param new_file_in A dataframe of laboratory results; a column called "Sample" and a column called "Target" are required components
#' @param targets_allowed A vector of character strings indicating the Target values the user would like to allow; Should be all capitalized values with no spaces
#' @param message_samples A character string "yes" or "no" indicating whether the user would like a message write-out of all samples continuing to be used; Default value is "yes"
#' @param stop_choice A character string of "yes" or "no" to indicate whether this should be a hard stop function or not; Default value is "no"
#' @return A list with the first element being a dataframe filtered by Target value, and the second element being either 0 (for no failure stop) or 1 (for failure stop)
#' @export

w0115_target_check <- function(new_file_in, targets_allowed,
                               message_samples = "yes", stop_choice = "no"){

  stop_indicator <- 0

  message("Check #1.15: Target Confirmation and optional Sample write-out")
  message("")

  message("Targets Allowed:")
  for (each_target in targets_allowed){
      message(each_target)
  }
  message("")

  message("Targets In Provided Data File:")
  for (each_target in unique(new_file_in$Target)){
    message(each_target)
  }
  message("")

  original_length <- nrow(new_file_in)
  new_file_in2 <- filter(new_file_in, Target %in% targets_allowed)
  new_length <- nrow(new_file_in2)

  message(paste0("There were ", (original_length - new_length), " rows removed with Targets that were not in the provided list."))
  message("")

  if (new_length == 0){

    # then all rows were removed/no rows matched the allowed targets
    # this indicates a stop error
    stop_indicator <- 1

  }

  if (tolower(trimws(message_samples)) == "yes"){

        message("Sample/Targets Continuing Through System: ")



        for (each_target in unique(new_file_in2$Target)){

            build_message <- ""

            build_message <- paste0(build_message, each_target, ": ")

            sample_in_target_set <- filter(new_file_in2, Target == each_target)

            for (each_sample in unique(sample_in_target_set$Sample)){

              build_message <- paste0(build_message, each_sample, ", ")

            }

            # need to remove last two characters because of the extra ", " addition for last one
            build_message <- substr(build_message, 1, nchar(build_message) - 2)

            message(build_message)
            message("")

        }




    }


    if (stop_choice == "yes"){

      if (stop_indicator == 1){

          message("Stopped because no rows matched indicated Targets to include.")
          stop("Stop error encountered - #1.15")
      }

    }


    if (stop_indicator == 1){

      message("Stop error encountered - #1.15")
      message("")

    }

  message("Through Check #1.15")

  return(list(new_file_in2, stop_indicator))


}
