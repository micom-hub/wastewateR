

r0500_wval_sitecalc <- function(wastewater_data_in, org){

  #wastewater_data_in <- out_put_3

  wastewater_data_in2 <- wastewater_data_in %>% mutate(wval_calc = (log_value - baseline)/stdev)


  wastewater_data_in2 <- wastewater_data_in2 %>% mutate(wval_calc = exp(wval_calc))

  second_range <- wastewater_data_in2 %>%
    mutate(week = epiweek(date)) %>%
    mutate(year = case_when(week >= 51 & month(date) == 1 ~ year - 1,
                            T ~ year)) %>%
    group_by(id, year, week) %>%
    summarize(avg_min = min(date),
              avg_max = max(date)) %>% distinct()


  wastewater_data_in2 <- wastewater_data_in2 %>%
    mutate(week = epiweek(date)) %>%
    mutate(year = case_when(week >= 51 & month(date) == 1 ~ year - 1,
                            T ~ year)) %>%
    group_by(id, year, week) %>%
    summarize(average_wval_calc = mean(wval_calc, na.rm = TRUE),
              count_samples = length(id))

  wastewater_data_in2 <- merge(wastewater_data_in2, second_range, by = c("id", "year", "week"))

  if (org == "SC2"){
    wastewater_data_in2 <- wastewater_data_in2 %>% mutate(wval_level = case_when(average_wval_calc <= 1.5 ~ "1 - Minimal",
                                                                                 average_wval_calc <= 3 ~ "2 - Low",
                                                                                 average_wval_calc <= 4.5 ~ "3 - Moderate",
                                                                                 average_wval_calc <= 8 ~ "4 - High",
                                                                                 average_wval_calc > 8 ~ "5 - Very High"))
  } else if (org == "FLU"){

    wastewater_data_in2 <- wastewater_data_in2 %>% mutate(wval_level = case_when(average_wval_calc <= 1.6 ~ "1 - Minimal",
                                                                                 average_wval_calc <= 4.5 ~ "2 - Low",
                                                                                 average_wval_calc <= 12.2 ~ "3 - Moderate",
                                                                                 average_wval_calc <= 20.1 ~ "4 - High",
                                                                                 average_wval_calc > 20.1 ~ "5 - Very High"))

  }


  wastewater_data_in2 <- wastewater_data_in2 %>% mutate(average_wval_calc = case_when(!is.finite(average_wval_calc) ~ NA_real_,
                                                                                      T ~ average_wval_calc))

  return(wastewater_data_in2)

}
