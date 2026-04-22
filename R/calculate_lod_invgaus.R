#' Calculating a Limit of Detection
#'
#' This is a function available to calculate a limit of detection differently than the limit of detection
#' value contained in the calculate_gc_per_100ml() function. This is meant to be used
#' with unmerged data. For reference, a similar logic to this can be calculated with the
#' TotalConfMax output on merged data.
#'
#' Only negative control well data should be input to this function.
#'
#' This function takes in a dataframe of negative control measurements from different plates of data.
#' This dataframe is expected to have columns of:
#'
#' * Sample = Negative Control name
#' * Target = Target pathogen abbreviation/name. All to be considered together should be named the same
#' * plate = Name of the plate / plate file
#' * plate_date = Date the plate was from
#' * Positives = Number of measured positive droplets
#' * AcceptedDroplets = Number of measured Accepted Droplets
#'
#' The data is grouped by Target value, plate, and plate_date. The sum of the positive
#' droplets and the sum of the accepted droplets are calculated by each defined group.
#'
#' Continuing in the same defined groups, the quantile function (inverse cumulative
#' distribution function) of the gamma distribution is calculated with a p of the input
#' threshold value, a shape of the sum of the positive droplets + 1, and a scale of 1. This
#' value is saved in the column posdrops.
#'
#' The negative limit is then calculated as the
#'
#' -log( (sum_accepted_drops - posdrops)/sum_accepted_drops  ) / vdroplet
#'
#' For additional plate to plate stability, the data is then grouped by Target value and
#' arranged by plate_date. The rolling three plate average value of the negative limit
#' is calculated. For each plate, the larger of the two values (the three plate rolling
#' average of the negative limit, or the plate's negative limit) is provided as the
#' final negative limit.
#'
#' A dataframe is returned from this function, with the following columns:
#'
#' * Target = Target pathogen abbreviation/name. All to be considered together should be named the same
#'
#' * plate = Name of the plate / plate file
#'
#' * plate_date = Date the plate was from
#'
#' * sum_pos_drops = Positive droplets sum per Target, plate, and plate_date group
#'
#' * sum_accepted_drops = Accepted Droplets sum per Target, plate, and plate_date group
#'
#' * posdrops = the quantile function (inverse cumulative distribution function) of the gamma distribution calculated with a p of the input threshold value, a shape of the sum of the positive droplets + 1, and a scale of 1 per Target, plate, and plate_date group
#'
#' * NEG_limit = -log( (sum_accepted_drops - posdrops)/sum_accepted_drops  ) / vdroplet per Target, plate, and plate_date group
#'
#' * rolling_avg = rolling three plate average value of NEG_limit
#'
#' * NEG_limit_final = larger value of either NEG_limit and rolling_avg
#'
#'
#' @param negative_controls A dataframe of negative control measurements. Expected columns = Sample, Target, plate, plate_date, Positives, AcceptedDroplets
#' @param threshold The decimal measurement of the Confidence Interval threshold you'd like to calculate (for example, the upper limit of the 95% Confidence Interval would be 0.95); Default value = 0.95
#' @param vdroplet The droplet size measurement from the machine the laboratory tests were run on; Default value = 0.00085
#' @return A dataframe with all original input columns plus new columns included for the limit of detection calculations
#' @export

calculate_lod_invgaus <- function(negative_controls, threshold = 0.95, vdroplet = 0.00085){

    negative_controls2 <- negative_controls %>%
      group_by(Target, plate, plate_date) %>%
      summarize(sum_pos_drops = sum(Positives, na.rm = TRUE),
                sum_accepted_drops = sum(AcceptedDroplets, na.rm = TRUE)) %>%
      ungroup()

    negative_controls3 <- negative_controls2 %>%
      group_by(Target, plate, plate_date) %>%
      mutate(posdrops = qgamma(p = threshold, shape = (sum_pos_drops + 1), scale = 1),
             NEG_limit = -log( (sum_accepted_drops - posdrops)/sum_accepted_drops  ) / vdroplet)

    negative_controls4 <- negative_controls3 %>%
      group_by(Target) %>%
      arrange(plate_date) %>%
      mutate(rolling_avg = zoo::rollsum(as.numeric(NEG_limit), k = 3, fill = NA, align = "right"),
             rolling_avg = rolling_avg/3,
             NEG_limit_final = case_when(rolling_avg > as.numeric(NEG_limit) ~ rolling_avg,
                                         as.numeric(NEG_limit) > rolling_avg ~ as.numeric(NEG_limit),
                                         T ~ as.numeric(NEG_limit)))


    return(negative_controls4)

}
