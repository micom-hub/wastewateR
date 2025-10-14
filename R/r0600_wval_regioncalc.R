

r0600_wval_regioncalc <- function(){

step5_wvalregioncalc_median <- function(wastewater_data_in, site_to_region_crosswalk){

  wastewater_data_in2 <- merge(wastewater_data_in, site_to_region_crosswalk, by = c("id"), all.x = TRUE)

  regional <- wastewater_data_in2 %>%
    group_by(Geography, year, week) %>%
    summarize(avg_min = min(avg_min),
              avg_max = max(avg_max),
              median_wval_calc = median(average_wval_calc, na.rm = TRUE),
              contributing_site_count = length(unique(id)))

  regional <- regional %>% mutate(median_wval_level = case_when(median_wval_calc <= 1.5 ~ "1 - Minimal",
                                                                median_wval_calc <= 3 ~ "2 - Low",
                                                                median_wval_calc <= 4.5 ~ "3 - Moderate",
                                                                median_wval_calc <= 8 ~ "4 - High",
                                                                median_wval_calc > 8 ~ "5 - Very High"))


  return(regional)

}



step5_wvalregioncalc_mean <- function(wastewater_data_in, site_to_region_crosswalk){

  wastewater_data_in2 <- merge(wastewater_data_in, site_to_region_crosswalk, by = c("id"), all.x = TRUE)

  regional <- wastewater_data_in2 %>%
    group_by(Geography, year, week) %>%
    summarize(avg_min = min(avg_min),
              avg_max = max(avg_max),
              mean_wval_calc = mean(average_wval_calc, na.rm = TRUE),
              contributing_site_count = length(unique(id)))

  regional <- regional %>% mutate(mean_wval_level = case_when(mean_wval_calc <= 1.5 ~ "1 - Minimal",
                                                              mean_wval_calc <= 3 ~ "2 - Low",
                                                              mean_wval_calc <= 4.5 ~ "3 - Moderate",
                                                              mean_wval_calc <= 8 ~ "4 - High",
                                                              mean_wval_calc > 8 ~ "5 - Very High"))

  regional <- regional %>% mutate(mean_wval_calc = case_when(!is.finite(mean_wval_calc) ~ NA_real_,
                                                             T ~ mean_wval_calc))


  return(regional)

}



# if this method, need to provide weights as well
# dataframe of "GEOID", "Geography", "weight"
step5_wvalregioncalc_wtavg <- function(wastewater_data_in, site_to_region_crosswalk, weight_df, org){

  #wastewater_data_in <- out_put_4
  #weight_df <- wt_avg
  #site_to_region_crosswalk <- cross

  colnames(weight_df) <- c("id", "weight")
  weight_df <- filter(weight_df, !is.na(weight))

  wastewater_data_in2 <- merge(wastewater_data_in, site_to_region_crosswalk, by = c("id"), all.x = TRUE)

  wastewater_data_in2 <- merge(wastewater_data_in2, weight_df, by.x = c("id"))

  # set <- wastewater_data_in2 %>% select(year, week) %>% distinct()
  # set2 <- filter(wastewater_data_in2, !is.na(average_wval_calc)) %>% select(year, week) %>% distinct()
  #
  regional_weighted1 <- filter(wastewater_data_in2, !is.na(average_wval_calc)) %>%
    group_by(Geography, year, week) %>%
    summarize(weighted_avg_wval_calc = weighted.mean(average_wval_calc, weight),
              avg_min = min(avg_min),
              avg_max = max(avg_max))

  regional_weighted2 <- wastewater_data_in2 %>%
    group_by(Geography, year, week) %>%
    summarize(contributing_site_count = length(unique(id)))

  regional_weighted <- merge(regional_weighted1, regional_weighted2, all.y = TRUE)

  if (org == "SC2"){
    regional_weighted <- regional_weighted %>% mutate(weighted_avg_wval_level = case_when(weighted_avg_wval_calc <= 1.5 ~ "1 - Minimal",
                                                                                          weighted_avg_wval_calc <= 3 ~ "2 - Low",
                                                                                          weighted_avg_wval_calc <= 4.5 ~ "3 - Moderate",
                                                                                          weighted_avg_wval_calc <= 8 ~ "4 - High",
                                                                                          weighted_avg_wval_calc > 8 ~ "5 - Very High"))
  } else if (org == "FLU"){
    regional_weighted <- regional_weighted %>% mutate(weighted_avg_wval_level = case_when(weighted_avg_wval_calc <= 1.6 ~ "1 - Minimal",
                                                                                          weighted_avg_wval_calc <= 4.5 ~ "2 - Low",
                                                                                          weighted_avg_wval_calc <= 12.2 ~ "3 - Moderate",
                                                                                          weighted_avg_wval_calc <= 20.1 ~ "4 - High",
                                                                                          weighted_avg_wval_calc > 20.1 ~ "5 - Very High"))

  }

  regional_weighted <- regional_weighted %>% mutate(weighted_avg_wval_calc = case_when(!is.finite(weighted_avg_wval_calc) ~ NA_real_,
                                                                                       T ~ weighted_avg_wval_calc))

  return(regional_weighted)

}

}
