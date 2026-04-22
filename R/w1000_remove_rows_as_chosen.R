
#' Removing QAQC Rule Breakers
#'
#' This function takes in a data frame of laboratory data that has been through
#' most if not all of the prior QAQC checks. It also takes as input, a vector
#' of numbers that correspond to the rules that, if there were only "mild" rule breaks
#' (aka that did not previously trigger a STOP), you'd like to remove those rule breaking rows.
#' The default value for this vector is 'c(2, 3, 4, 5.5, 6)'.
#'
#' This function can currently accommodate this process for rules:
#'
#' - 2.0 Accepted Droplet Count Check (2)
#' - 3.0 NTC Control Check (3)
#' - 5.0 Control Warning - Positives Droplets (5)
#' - 5.5 Control Check - Ensuring Negatives (5.5)
#' - 6.0 Extraction & Negative Control Check (6)
#' - 7.0 Sample Positive Droplet Sum Check (7)
#' - 7.5 Sample Negative Droplet Sum Check (7.5)
#' - 8.0 Recovery Control Check (8)
#' - 9.0 Target Positives Droplet High Limit (9)
#'
#' For all of these rules, this function removes any rows with a value of 1 in
#' the relevant mark column. In addition, for 2.0 Accepted Droplet Count Check,
#' if any sample set had more than one well that broke the rule within the set,
#' the entire sample set is removed.
#'
#' @param data_file_in A dataframe of laboratory data
#' @param rules_out_v A vector of numbers that correspond to the rules that, if there were only "mild" rule breaks (aka that did not previously trigger a STOP), you'd like to remove those rule breaking rows. The default value for this vector is 'c(2, 3, 5.5, 6)'.
#' @return A dataframe just like the input data frame, with any indicated rule breaker rows removed
#' @export

w1000_remove_rows_as_chosen <- function(data_file_in, rules_out_v = c(2, 3, 4, 5.5, 6)){

  message("CHECK #10: Removing QAQC Rule Breakers")
  message("")

  counter <- 0

#remove any row with a value of 1 for each rule

  if (2 %in% rules_out_v){

    nrows_1 <- nrow(data_file_in)

    data_file_in <- filter(data_file_in, accepted_droplet_limit2 != 1)
    data_file_in <- filter(data_file_in, accepted_droplet_count2 < 2)

    nrows_2 <- nrow(data_file_in)
    if (nrows_2 < nrows_1){

      message(paste0(nrows_1 - nrows_2, " rows removed from Rule #2 criteria."))
      counter <- counter + 1
    }



  }

  if (3 %in% rules_out_v){

    nrows_1 <- nrow(data_file_in)

    data_file_in <- filter(data_file_in, ntc_control_check3 != 1)

    nrows_2 <- nrow(data_file_in)
    if (nrows_2 < nrows_1){

      message(paste0(nrows_1 - nrows_2, " rows removed from Rule #3 criteria."))
      counter <- counter + 1
    }



  }


  if (4 %in% rules_out_v){

    nrows_1 <- nrow(data_file_in)

    data_file_in <- filter(data_file_in, pos_control_check4 != 1  | is.na(pos_control_check4))

    nrows_2 <- nrow(data_file_in)
    if (nrows_2 < nrows_1){

      message(paste0(nrows_1 - nrows_2, " rows removed from Rule #4 criteria."))
      counter <- counter + 1
    }



  }


  if (5 %in% rules_out_v){

    nrows_1 <- nrow(data_file_in)

    data_file_in <- filter(data_file_in, control_pos_drop_soft5 != 1 | is.na(control_pos_drop_soft5))


    nrows_2 <- nrow(data_file_in)
    if (nrows_2 < nrows_1){

      message(paste0(nrows_1 - nrows_2, " rows removed from Rule #5 criteria."))
      counter <- counter + 1

    }



  }

  if (5.5 %in% rules_out_v){

    nrows_1 <- nrow(data_file_in)

    data_file_in <- filter(data_file_in, negvalue_control_check55 != 1 | is.na(negvalue_control_check55))


    nrows_2 <- nrow(data_file_in)
    if (nrows_2 < nrows_1){

      message(paste0(nrows_1 - nrows_2, " rows removed from Rule #5.5 criteria."))
      counter <- counter + 1

    }



  }

  if (6 %in% rules_out_v){

    nrows_1 <- nrow(data_file_in)

    data_file_in <- filter(data_file_in, ext_neg_control_check6 != 1 | is.na(ext_neg_control_check6 ))

    nrows_2 <- nrow(data_file_in)
    if (nrows_2 < nrows_1){

      message(paste0(nrows_1 - nrows_2, " rows removed from Rule #6 criteria."))
      counter <- counter + 1
    }



  }

  if (7 %in% rules_out_v){

    nrows_1 <- nrow(data_file_in)

    data_file_in <- filter(data_file_in, sample_wells_positives7 != 1 | is.na(sample_wells_positives7))

    nrows_2 <- nrow(data_file_in)
    if (nrows_2 < nrows_1){

      message(paste0(nrows_1 - nrows_2, " rows removed from Rule #7 criteria."))
      counter <- counter + 1
    }



  }

  if (7.5 %in% rules_out_v){

    nrows_1 <- nrow(data_file_in)

    data_file_in <- filter(data_file_in, sample_wells_negatives75 != 1 | is.na(sample_wells_negatives75))

    nrows_2 <- nrow(data_file_in)
    if (nrows_2 < nrows_1){

      message(paste0(nrows_1 - nrows_2, " rows removed from Rule #7.5 criteria."))
      counter <- counter + 1

    }



  }

  if (8 %in% rules_out_v){

    nrows_1 <- nrow(data_file_in)

    data_file_in <- filter(data_file_in, recovery_flag8 != 1)

    nrows_2 <- nrow(data_file_in)
    if (nrows_2 < nrows_1){

      message(paste0(nrows_1 - nrows_2, " rows removed from Rule #8 criteria."))
      counter <- counter + 1

    }



  }


  if (9 %in% rules_out_v){

    nrows_1 <- nrow(data_file_in)

    data_file_in <- filter(data_file_in, sample_pos_limit_flag9 != 1)

    nrows_2 <- nrow(data_file_in)
    if (nrows_2 < nrows_1){

      message(paste0(nrows_1 - nrows_2, " rows removed from Rule #9 criteria."))
      counter <- counter + 1

    }



  }


  if (counter == 0){

    message("No rows were removed based upon criteria.")
    message("Rules checker:")
    for (i in rules_out_v){
      message(i)
    }

  }

  message("")
  message("Through Check #10.")
  message("")

  return(data_file_in)

}


