
#' NTC Control Check
#'
#' This function takes in a data frame of laboratory data, as well as a numeric
#' positives droplet limit. It also takes in a character string "yes" or "no"
#' to indicate if the function should act as a hard stop or not.
#'
#' This function only needs to be run if there are NTC control
#' well data in the data file.
#'
#' It looks only at rows where the character string "NTC" is present in the
#' Sample column. If the Positives column for any of those rows is greater
#' than or equal to the positives droplet limit, it will mark those rows.
#' The default positives droplet limit is 3.
#'
#' A new column ntc_control_check3 will be added to the data frame,
#' which will contain a value of 1 if the sample name contains "NTC"
#' AND the Positives value is greater than or equal to the positives droplet
#' limit set by the user. Otherwise, the value will be 0.
#'
#' if stop_choice == "yes":
#' STOP ALERT: If the NTC rows have more than one well / row that is over
#' the droplet limit, all NTC rows will be printed to the console - Sample,
#' Target, Positives, and AcceptedDroplets columns. The code will STOP RUNNING
#'  if this occurs.
#'
#' @param df_in A dataframe of laboratory data
#' @param positive_droplet_limit A number indicating the limit for the number of positive droplets
#' @param stop_choice A character string of "yes" or "no" to indicate whether this should be a hard stop function or not
#' @return A list with the first element being a dataframe identical to df_in with one additional column, and the second element being either 0 (for no failure stop) or 1 (for failure stop)
#' @export

w0300_ntc_control_check <- function(df_in, positive_droplet_limit = 3, stop_choice = "yes"){
    ################################################################################
    # Check #3: NTC Well Check
    message("") # just for visual clarity
    message("CHECK #3: NTC Well Check")

    stop_indicator <- 0

    if (any(grepl("NTC", df_in$Sample))){

          df_in <- df_in %>% mutate(ntc_control_check3 = case_when(grepl("NTC", Sample) & Positives >= positive_droplet_limit ~ 1,
                                                                               T ~ 0))
          ### Looking only at the NTC Control wells, inspect the Positives column
          only_NTC <- filter(df_in, grepl("NTC", Sample))

          if (sum(only_NTC$ntc_control_check3, na.rm = TRUE) > 1){

                message("Sample | Target | Positives | AcceptedDroplets")

                on2 <- only_NTC %>% select(Sample, Target, Positives, AcceptedDroplets)

                for (i in seq(1, nrow(on2))){

                  message(paste0(on2[i, 1], " | ", on2[i, 2], " | ", on2[i, 3], " | ", on2[i, 4]))

                }

                stop_message <- paste0("NTC Sample wells have more than one well with more than ", positive_droplet_limit, " positive droplets.")

                message(stop_message)

                stop_indicator <- 1

          } else if (sum(only_NTC$ntc_control_check3, na.rm = TRUE) > 1){

                message("Sample | Target | Positives | AcceptedDroplets")

                on2 <- only_NTC %>% select(Sample, Target, Positives, AcceptedDroplets)

                for (i in seq(1, nrow(on2))){

                  message(paste0(on2[i, 1], " | ", on2[i, 2], " | ", on2[i, 3], " | ", on2[i, 4]))

                }

                message(paste0("NTC Sample wells have one well with more than ", positive_droplet_limit, " positive droplets."))



          } else {

                message(paste0("NTC Sample wells have zero wells with more than ", positive_droplet_limit, " positive droplets."))

          }

    } else {

          df_in$ntc_control_check3 <- 0
          message("There are no NTC Sample wells in this data file.")
    }

  message("") # just for visual clarity
  message("Through Check #3")

  if (stop_choice == "yes" & stop_indicator == 1){
    stop("Stop error encountered - #3")
  }


  return(list(df_in, stop_indicator))



}
