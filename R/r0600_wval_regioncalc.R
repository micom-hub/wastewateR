#' Calculating wVal for a User Defined Region
#'
#' Function that takes in wastewater dataframe with columns of: "id", "year",
#' "week", "average_wval_calc", also must have "Geography" if site_to_regional_crosswalk
#' is not used. (generated as output from r0100 to r0500)
#'
#' Function also takes in site_to_region_crosswalk, method, and org.
#'
#' If site_to_regional_crosswalk is provided, it must be a dataframe with columns of:
#'
#' - id: Site identifier or name
#'
#' - Geography: Region(s) the user is attempting to roll-up multiple sites to.
#'
#' - weight: Weight to apply to the `id` within the region wval calculation. Only relevant if using "mean" as `method`. If the user wants standard mean calculation applied, all `weight` should be 1.
#'
#' If site_to_regional_crosswalk is provided, it is merged onto the wastewater dataframe
#' by the `id` variable. Only instances where the `id` is in both dataframes will continue
#' through the remainder of the calculations.
#'
#' The resulting dataframe is grouped by `Geography`, `week`, and `year` into:
#'
#' - Geography: Region identifier or name
#'
#' - year: year of the sample period
#'
#' - week: week of the sample period
#'
#' - avg_min: minimum sample date within the week sample period
#'
#' - avg_max: maximum sample date within the week sample period
#'
#' - region_wval_calc: if `method` is 'median', then it's the median value of `average_wval_calc` with any `NA`s left out of consideration. if `method` is 'mean' then it's the weighted average value of `average_wval_calc` with any `NA`s left out of consideration
#'
#' - contributing_site_count: number of sites (`id`s) included in the calculation for the sample period week
#'
#'
#'
#' Using the region_wval_calc, a `wval_level` is assigned to each `Geography`'s week of data
#'
#' | Function Input | Minimal | Low | Moderate | High | Very High |
#' | --- | --- | --- | --- | --- | --- |
#' | SC2_v1 | Up to 1.5 | > 1.5 and <= 3 | > 3 and <= 4.5 | > 4.5 and <= 8 | > 8 |
#' | FLU_v1 | Up to 1.6 | > 1.6 and <= 4.5 | > 4.5 and <= 12.2 | > 12.2 and <= 20.1 | > 20.1 |
#' | RSV_v1 | Up to 4 | > 4 and <= 8 | > 8 and <= 12 | > 12 and <= 20 | > 20 |
#' | SC2_v2 | Up to 2 | > 2 and <= 3.4 | > 3.4 and <= 5.3 | > 5.3 and <= 7.8 | > 7.8 |
#' | FLU_v2 | Up to 2.7 | > 2.7 and <= 6.2 | > 6.2 and <= 11.2 | > 11.2 and <= 17.6 | > 17.6 |
#' | RSV_v2 | Up to 2.5 | > 2.5 and <= 5.2 | > 5.2 and <= 8 | > 8 and <= 11 | > 11 |
#'
#' Finally, any instances where the `region_wval_calc` is not a finite value are replaced
#' with `NA`.
#'
#'
#' @param wastewater_data_in A dataframe of wastewater data; must at least have columns of "id", "year", "week", "average_wval_calc", also must have "Geography" if site_to_regional_crosswalk is not used; most likely is output of r0100 to r0500
#' @param site_to_region_crosswalk Defaults to NA. If provided, should be a dataframe of "id", "Geography", "weight"
#' @param method Character string, either "median" or "mean", defaults to 'median'
#' @param org A character string, either "SC2_v1", "FLU_v1", "RSV_v1", "SC2_v2", "FLU_v2", "RSV_v2" indicating what pathogen the wastewater data represents, and the CDC methodology version of level determination the user would like to use
#' @return A data frame of weekly wval levels per geography
#' @export

r0600_wval_regioncalc <- function(wastewater_data_in, site_to_region_crosswalk = NA, method = "median", org){

  # site_to_region_crosswalk defaults to NA, expects Geography in wastewater_data_in
  # by default, defaults to all weights = 1 if mean is used
  # option to add in site_to_region_crosswalk as it's own dataframe, if used,
  # weights must be provided in crosswalk dataframe
  # method = median, mean
  # site_to_region_crosswalk must have columns = id, Geography, weight

  if (length(site_to_region_crosswalk) == 1){

    if (is.na(site_to_region_crosswalk)){
        wastewater_data_in$weight <- 1
    } else {

      message("Input provided for site_to_region_crosswalk by user: ")
      message(site_to_region_crosswalk)
      stop("Crosswalk dataframe or input is not an accepted format.")

    }

  } else {

    colnames(site_to_region_crosswalk) <- c("id", "Geography", "weight")
    wastewater_data_in <- merge(wastewater_data_in, site_to_region_crosswalk, by = c("id"))

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


  } else {

    message(paste0("'method' input not recognized. User input = ", method, ". System accepts only 'mean' or 'median'."))

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
