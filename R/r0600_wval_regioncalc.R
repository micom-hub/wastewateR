

r0600_wval_regioncalc <- function(wastewater_data_in, site_to_region_crosswalk = NA, method = "median", org){

  # site_to_region_crosswalk defaults to NA, expects Geography in wastewater_data_in
  # by default, defaults to all weights = 1 if mean is used
  # option to add in site_to_region_crosswalk as it's own dataframe, if used,
  # weights must be provided in crosswalk dataframe
  # method = median, mean
  # site_to_region_crosswalk must have columns = id, Geography, weight

  if (!is.na(site_to_region_crosswalk)){

    colnames(site_to_region_crosswalk) <- c("id", "Geography", "weight")
    wastewater_data_in <- merge(wastewater_data_in, site_to_region_crosswalk, by = c("id"), all.x = TRUE)

  } else {

    wastewater_data_in$weight <- 1

  }


  if (method == "median"){

      regional <- wastewater_data_in %>%
        group_by(Geography, year, week) %>%
        summarize(avg_min = min(avg_min),
                  avg_max = max(avg_max),
                  region_wval_calc = median(average_wval_calc, na.rm = TRUE),
                  contributing_site_count = length(unique(id)))

  } else if (method == "mean"){

    regional_weighted1 <- filter(wastewater_data_in, !is.na(average_wval_calc)) %>%
      group_by(Geography, year, week) %>%
      summarize(region_wval_calc = weighted.mean(average_wval_calc, weight),
                avg_min = min(avg_min),
                avg_max = max(avg_max))

    regional_weighted2 <- wastewater_data_in %>%
      group_by(Geography, year, week) %>%
      summarize(contributing_site_count = length(unique(id)))

    regional <- merge(regional_weighted1, regional_weighted2, all.y = TRUE)


  }


# apply levels to regional based on org - same as in r0500
  if (org == "SC2_v1"){

    regional2 <- regional %>% mutate(wval_level = case_when(region_wval_calc <= 1.5 ~ "1 - Minimal",
                                                            region_wval_calc <= 3 ~ "2 - Low",
                                                            region_wval_calc <= 4.5 ~ "3 - Moderate",
                                                            region_wval_calc <= 8 ~ "4 - High",
                                                            region_wval_calc > 8 ~ "5 - Very High"))
  } else if (org == "FLU_v1"){

    regional2 <- regional %>% mutate(wval_level = case_when(region_wval_calc <= 1.6 ~ "1 - Minimal",
                                                            region_wval_calc <= 4.5 ~ "2 - Low",
                                                            region_wval_calc <= 12.2 ~ "3 - Moderate",
                                                            region_wval_calc <= 20.1 ~ "4 - High",
                                                            region_wval_calc > 20.1 ~ "5 - Very High"))

  } else if (org == "RSV_v1"){

    regional2 <- regional %>% mutate(wval_level = case_when(region_wval_calc <= 4 ~ "1 - Minimal",
                                                            region_wval_calc <= 8 ~ "2 - Low",
                                                            region_wval_calc <= 12 ~ "3 - Moderate",
                                                            region_wval_calc <= 20 ~ "4 - High",
                                                            region_wval_calc > 20 ~ "5 - Very High"))

  } else if (org == "SC2_v2"){

    regional2 <- regional %>% mutate(wval_level = case_when(region_wval_calc <= 2 ~ "1 - Minimal",
                                                            region_wval_calc <= 3.4 ~ "2 - Low",
                                                            region_wval_calc <= 5.3 ~ "3 - Moderate",
                                                            region_wval_calc <= 7.8 ~ "4 - High",
                                                            region_wval_calc > 7.8 ~ "5 - Very High"))
  } else if (org == "FLU_v2"){

    regional2 <- regional %>% mutate(wval_level = case_when(region_wval_calc <= 2.7 ~ "1 - Minimal",
                                                            region_wval_calc <= 6.2 ~ "2 - Low",
                                                            region_wval_calc <= 11.2 ~ "3 - Moderate",
                                                            region_wval_calc <= 17.6 ~ "4 - High",
                                                            region_wval_calc > 17.6 ~ "5 - Very High"))

  } else if (org == "RSV_v2"){

    regional2 <- regional %>% mutate(wval_level = case_when(region_wval_calc <= 2.5 ~ "1 - Minimal",
                                                            region_wval_calc <= 5.2 ~ "2 - Low",
                                                            region_wval_calc <= 8 ~ "3 - Moderate",
                                                            region_wval_calc <= 11 ~ "4 - High",
                                                            region_wval_calc > 11 ~ "5 - Very High"))

  }


  regional2 <- regional2 %>% mutate(region_wval_calc = case_when(!is.finite(region_wval_calc) ~ NA_real_,
                                                                T ~ region_wval_calc))


  return(regional2)

}
