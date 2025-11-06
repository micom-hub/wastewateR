#' Calculating wVal for individual site(s)
#'
#' Function that takes in wastewater dataframe with columns of:
#' - log_value
#' - baseline
#' - stdev
#' - date
#' - id
#' (generated as output from r0100 to r0400)
#'
#' @param wastewater_data_in A dataframe of wastewater data; must at least have columns of "log_value", "baseline", "stdev", "date", "id"; most likely is output of r0100 to r0400
#' @param org A character string, either "SC2_v1", "FLU_v1", "RSV_v1" indicating what pathogen the wastewater data represents, and the CDC methodology version of level determination the user would like to use
#' @return A data frame
#' @export

r0500_wval_sitecalc <- function(wastewater_data_in, org){

  # calculate first part of wval calculation
  # log value minus baseline value, all divided by st. dev
  wastewater_data_in2 <- wastewater_data_in %>% mutate(wval_calc = (log_value - baseline)/stdev)

  # then exponentiate that value to finalize wval calculation
  wastewater_data_in2 <- wastewater_data_in2 %>% mutate(wval_calc = exp(wval_calc))

  # at this point, you have individual wval measurements per row (day and site)

  # want to create some additional informational columns - first, the min date and
  # max date included for each id/year/week combination
  second_range <- wastewater_data_in2 %>%
    mutate(week = epiweek(date)) %>%
    mutate(year = case_when(week >= 51 & month(date) == 1 ~ year - 1,
                            T ~ year)) %>%
    group_by(id, year, week) %>%
    summarize(avg_min = min(date),
              avg_max = max(date)) %>% distinct()

  # then turn the wval calculation into the average wval per WEEK per site
  # and count the samples included in each week's calculation
  wastewater_data_in2 <- wastewater_data_in2 %>%
    mutate(week = epiweek(date)) %>%
    mutate(year = case_when(week >= 51 & month(date) == 1 ~ year - 1,
                            T ~ year)) %>%
    group_by(id, year, week) %>%
    summarize(average_wval_calc = mean(wval_calc, na.rm = TRUE),
              count_samples = length(id))

  # combine that, so we have one large set
  wastewater_data_in2 <- merge(wastewater_data_in2, second_range, by = c("id", "year", "week"))

  # dataframe at this point is id, year, week, avg_min, avg_max,
  # average_wval_calc, count_samples


  # these values are assigning wval "level" based on the average_wval_calc value
  # these are the original cdc v1 methodology cutoffs
  if (org == "SC2_v1"){

    wastewater_data_in2 <- wastewater_data_in2 %>% mutate(wval_level = case_when(average_wval_calc <= 1.5 ~ "1 - Minimal",
                                                                                 average_wval_calc <= 3 ~ "2 - Low",
                                                                                 average_wval_calc <= 4.5 ~ "3 - Moderate",
                                                                                 average_wval_calc <= 8 ~ "4 - High",
                                                                                 average_wval_calc > 8 ~ "5 - Very High"))
  } else if (org == "FLU_v1"){

    wastewater_data_in2 <- wastewater_data_in2 %>% mutate(wval_level = case_when(average_wval_calc <= 1.6 ~ "1 - Minimal",
                                                                                 average_wval_calc <= 4.5 ~ "2 - Low",
                                                                                 average_wval_calc <= 12.2 ~ "3 - Moderate",
                                                                                 average_wval_calc <= 20.1 ~ "4 - High",
                                                                                 average_wval_calc > 20.1 ~ "5 - Very High"))

  } else if (org == "RSV_v1"){

    wastewater_data_in2 <- wastewater_data_in2 %>% mutate(wval_level = case_when(average_wval_calc <= 4 ~ "1 - Minimal",
                                                                                 average_wval_calc <= 8 ~ "2 - Low",
                                                                                 average_wval_calc <= 12 ~ "3 - Moderate",
                                                                                 average_wval_calc <= 20 ~ "4 - High",
                                                                                 average_wval_calc > 20 ~ "5 - Very High"))

  }


  wastewater_data_in2 <- wastewater_data_in2 %>% mutate(average_wval_calc = case_when(!is.finite(average_wval_calc) ~ NA_real_,
                                                                                      T ~ average_wval_calc))

  return(wastewater_data_in2)

}
