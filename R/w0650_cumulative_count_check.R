
#' Cumulative Error Check
#'
#' This function is just a "check" function. This
#' function takes in a dataframe that has at least been through checks 2, 3, 5.5, and 6 prior.
#' It also takes in a numerical value corresponding to the number of acceptable
#' failed well checks that the user is willing to accept across checks 2, 3, 5.5, and 6.
#' The default set value for this is 3. In addition, the function accepts either a
#' "yes" or "no" character string, indicating whether the limit should be a hard
#' stop limit, or just a warning.
#'
#' This function is checking the sum of:
#'
#' * accepted_droplet_limit2 / 3
#'
#' * ntc_control_check3
#'
#' * negvalue_control_check55
#'
#' * ext_neg_control_check6
#'
#' if stop_choice == "yes":
#' STOP ALERT: If "yes" is entered as the last input into this function, and
#' the sum of wells that failed checks 2, 3, 5.5, and 6 is greater than the set
#' numeric limit, the code will STOP RUNNING.
#'
#' Regardless, this function will print a message to the console with the number
#' of wells that failed each of checks 2, 3, 5.5, and 6. It will then note whether
#' the sum of these was greater than the set numeric limit, or not.
#'
#'  A zero or a one is returned from this function - A 1 if the stop check was triggered
#' and the stop_choice was "no", and a zero if the stop check was not triggered

#' @param df_file_in A dataframe of laboratory data
#' @param set_limit A numerical value corresponding to the number of acceptable failed well checks that the user is willing to accept across checks 2, 3, 5.5, and 6. The default set value for this is 3.
#' @param stop_choice A character string of "yes" or "no" to indicate whether this should be a hard stop function or not
#' @return A numeric 0 or 1 value
#' @export

w0650_cumulative_count_check <- function(df_file_in, set_limit = 3, stop_choice = "no"){

  if (!trimws(tolower(stop_choice)) %in% c("yes", "no")){
    stop("Unaccepted value provided for stop_q; must be either 'yes' or 'no'")
  }

  stop_indicator <- 0

  message("CHECK #6.5: Rule Count Check")

  # checks 2, 3, 6 make:
  # columns = accepted_droplet_limit2, ntc_control_check3, ext_neg_control_check6

  ### need to make this more specific to the Well
  two_check <- as.data.frame(df_file_in) %>% select(Well, Sample, accepted_droplet_limit2) %>% distinct()
  two <- sum(two_check$accepted_droplet_limit2, na.rm = TRUE)/3

  ### need to make this more specific to the sample/target combination
  three_check <- as.data.frame(df_file_in) %>% select(Sample, Target, ntc_control_check3) %>% distinct()
  three <- sum(three_check$ntc_control_check3, na.rm = TRUE)

  fivefive_check <- as.data.frame(df_file_in) %>% group_by(Sample, Target) %>% summarize(count = sum(negvalue_control_check55, na.rm = TRUE))
  fivefive <- sum(fivefive_check$count, na.rm = TRUE)

  ### need to make this more specific to the sample/target combination
  six_check <- as.data.frame(df_file_in) %>% select(Sample, Target, ext_neg_control_check6) %>% distinct()
  six <- sum(six_check$ext_neg_control_check6, na.rm = TRUE)

  message("") # just for visual clarity
  # message regardless

  message("Check | Number Failed")
  message(paste0(" #2 | ", two))
  message(paste0(" #3 | ", three))
  message(paste0(" #3 | ", fivefive))
  message(paste0(" #6 | ", six))

  if (sum(two, three, fivefive, six, na.rm = TRUE) > set_limit){

    message(paste0("The total number of failed wells/Samples across these rules was greater than the set limit of ", set_limit))

    stop_indicator <- 1

  } else {

    # note everything was below acceptable/set limits
    message(paste0("The total number of failed wells/Samples across these rules was less than or equal to the set limit of ", set_limit))

  }

  message("")
  message("Through Check #6.5")
  message("")

  if (trimws(tolower(stop_choice)) == "yes" & stop_indicator == 1){

    stop("Stop error encountered - #6.5")

  }

  return(stop_indicator)

}
