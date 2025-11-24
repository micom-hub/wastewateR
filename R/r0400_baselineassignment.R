#' Determining Baseline Values
#'
#' This function takes in a dataframe of wastewater sample data by site and a character
#' string of "cdc_v1", "cdc_v2", "cdc_v3", "cdc_v4", or "all_data" to determine the method of determining
#' the "baseline" values for every site over time for the dataframe.
#'
#' If `method_choice` is set as "cdc_v1" then the baseline is set according to the original CDC baseline
#' rules for SARS-CoV-2:
#'
#' * For each combination of site, data submitter, PCR target, lab methods, and
#' normalization method, a baseline is established. The “baseline” is the 10th percentile
#' of the log-transformed and normalized concentration data within a specific time frame.
#'
#' * SARS-CoV-2: For site and method combinations (as listed above) with over six
#' months of data, baselines are re-calculated every six calendar months (January 1st and July 1st)
#' using the past 12 months of data. For sites and method combinations with less than six months of data, baselines
#' are computed weekly until reaching six months, after which they remain unchanged
#' until the next January 1st or July 1st, at which time baselines are re-calculated.
#'
#' If `method_choice` is set as "cdc_v2" then the baseline is set according to the original
#' baseline rules, using the 18 month look-back period used for Influenza A and RSV.
#'
#' * For sites and method combinations with less than twelve months of data, baselines
#' are computed weekly until reaching twelve months, after which they remain unchanged
#' until the next August 1st, at which time baselines are re-calculated.
#'
#' * For site and method combinations (as listed above) with over twelve months of data, baselines are re-
#' calculated every August 1st using all available data in the previous 18 months.
#'
#' For "cdc_v1" and "cdc_v2", `week_required` must be considered. In the original
#' CDC wval calculations, this value was set to 6 for COVID data, and 10 for influenza A
#' and RSV.
#'
#' August 2025 updates - RSV & Flu
#'
#' For "cdc_v3", baseline calculations are made based on the CDC August 2025 updates,
#' including, aligning site-level baselines for COVID-19, influenza A, and RSV to 24 months.
#' `week_required` for this method would be set to 8 weeks.
#'
#' August 2025 updates - COVID
#'
#' For "cdc_v4", baseline calculations are made based on the CDC August 2025 updates,
#' including, aligning site-level baselines for COVID-19, influenza A, and RSV to 24 months,
#' and shifting timing of the biannual COVID-19 WVAL updates to April and October.
#' `week_required` for this method would be set to 8 weeks.
#'
#'
#' If `method_choice` is set as "all_data" then the baseline is set as the 10th
#' percentile of all `log_values` for each site. The standard deviation of all `log_values` is calculated,
#' and the baseline minimum date and maximum date are set as the min and max available date
#' for all data per site. Any rows where the baseline is `NA` are removed, and any
#' rows where the standard deviation is zero are removed. The final dataframe is returned.
#'
#' Per CDC methodology, as of the August 15, 2025 methodology change, "all historical
#' wVal values are updated with the new baseline when recalculated". If you'd like
#' to use that method with the "cdc_v4" option, `hist_override` can be set equal to 1.
#' By default, this behavior is not chosen, and `hist_override` is set equal to 0.
#'
#' For all methods, the returned dataframe consists of: `id`, `date`, `sampletype`,
#' `sitetype`, `population_served`, `gcper100ml`, `microbial_val`, `flow_val`,
#' `normalized_measurement`, `log_value`, `oldest_date`, `days_since_first`,
#' `x_months_data_yn`, `sample_counter`, `multiple_durations`, `baseline`,
#' `stdev`, `baseline_mindate`, `baseline_maxdate`, `baseline_datapoints`
#'
#' @param wastewater_data_in A dataframe of wastewater site, metadata, and measurement values
#' @param method_choice A character string of "cdc_v1", "cdc_v2", "cdc_v3", "cdc_v4", or "all_data" to determine method of baseline assignment
#' @param week_required A numeric value required if using "cdc_v1", "cdc_v2", "cdc_v3", or "cdc_v4" that sets the minimum number of weeks of data a site must have in order to calculate baselines. If a site has fewer weeks of data than this number, they are removed from consideration. Default value is 6
#' @param hist_override A numeric value of 0 if the user does not want to override all historic baselines with the most recent baseline calculation. If set equal to 1, all sites with more than six months of data will have their historic baselines overwritten with the most recently calculated baseline.
#' @return A data frame
#' @export

r0400_baselineassignment <- function(wastewater_data_in, method_choice, week_required = 6, hist_override = 0){


  if (method_choice == "cdc_v1"){

    wastewater_data_in2 <- wastewater_data_in %>%
      group_by(id) %>%
      mutate(oldest_date = min(date))

    wastewater_data_in2 <- wastewater_data_in2 %>%
      mutate(days_since_first = as.numeric(difftime(date, oldest_date, units = "days")))

    wastewater_data_in2 <- wastewater_data_in2 %>%
      group_by(id) %>%
      mutate(x_months_data_yn = case_when(days_since_first/365.25 > 0.5 ~ "yes",
                                          T ~ "no"))

    # account for situation where dates have length between them, but there aren't enough samples in that range to justify moving to
    # the six months methodology
    wastewater_data_in2 <- wastewater_data_in2 %>%
      group_by(id) %>%
      arrange(date) %>%
      mutate(sample_counter = seq_along(gcper100ml))

    wastewater_data_in2 <- wastewater_data_in2 %>%
      mutate(x_months_data_yn = case_when(sample_counter < 24 ~ "no",
                                          T ~ x_months_data_yn))

    wastewater_data_in2 <- wastewater_data_in2 %>%
      group_by(id) %>%
      mutate(multiple_durations = length(unique(x_months_data_yn)))

    if (any(wastewater_data_in2$x_months_data_yn == "yes")){

      # still need all the data to calculate these baselines for the times immediately after
      # the site has enough data, but only for sites that have both "durations" of data
      baseline_lots <- filter(wastewater_data_in2, multiple_durations == 2) %>%
        mutate(year_half = case_when(month(date) < 7 ~ 1,
                                     month(date) >= 7 ~ 2,
                                     T ~ 9999),
               year = year(date))

      if (any(baseline_lots$year_half == 9999)){
        message("Sample dates corrupted, assigned 9999 value.")
        stop()
      }

      # if your year half is 2 (i.e. it's after july 1)
      # then your baseline should be the 10th percentile of all samples with your current
      # year and year half == 1 AND year prior with year half == 2

      # if your year half is 1 (i.e. it's after january 1)
      # then your baseline should be the 10th percentile of all samples with the year prior

      lots_baseline_set <- data.frame()

      for (every_site in unique(baseline_lots$id)){

        working_site <- filter(baseline_lots, id == every_site) %>% arrange(date)
        combinations <- as.data.frame(working_site) %>% select(year, year_half) %>% distinct()

        saved_baselines <- data.frame()

        for (every_combination in seq(1, nrow(combinations))){
          combo1 <- combinations[every_combination, ]

          if (combo1$year_half == 2){

            set1 <- filter(working_site, year == combo1$year & year_half == 1)
            set2 <- filter(working_site, year == combo1$year - 1 & year_half == 2)
            full_set <- rbind(set1, set2)

          } else if (combo1$year_half == 1){

            full_set <- filter(working_site, year == combo1$year - 1)

          }

          combo1$baseline <- quantile(full_set$log_value, 0.1)[[1]][1]
          combo1$stdev <- sd(full_set$log_value, na.rm = TRUE)
          combo1$baseline_mindate <- min(full_set$date)
          combo1$baseline_maxdate <- max(full_set$date)
          combo1$baseline_datapoints <- nrow(full_set)
          saved_baselines <- rbind(saved_baselines, combo1)

        }

        saved_baselines$id <- every_site
        lots_baseline_set <- rbind(lots_baseline_set, saved_baselines)

      }

      # have NAs in this, for the periods that are too new
      lots_baseline_set <- filter(lots_baseline_set, !is.na(baseline))

      # # also remove anything out of here that has too few days determining
      # # baseline
      # lots_baseline_set <- lots_baseline_set %>% mutate(baseline_days_range = as.numeric(difftime(baseline_maxdate, baseline_mindate, units = "days"))/365.25)
      #
      # lots_baseline_set <- filter(lots_baseline_set, baseline_days_range > 0.5)
      #
      # lots_baseline_set <- lots_baseline_set %>% select(-baseline_days_range)

    } else {

      lots_baseline_set <- data.frame()

    }




    ################################################################################
    # set up our baselines for new/short-term sites
    if (any(wastewater_data_in2$x_months_data_yn == "no")){
      baseline_few <- filter(wastewater_data_in2, x_months_data_yn == "no") %>% group_by(id) %>%
        mutate(week = epiweek(date),
               year = year(date),
               total_weeks = length(unique(week)))

      baseline_few <- baseline_few %>% mutate(year = case_when(week >= 51 & month(date) == 1 ~ year - 1,
                                                               T ~ year))


      # small set site names
      start_site_names <- unique(baseline_few$id)

      baseline_few <- filter(baseline_few, total_weeks > week_required)

      # site names after we remove everything with 6 or fewer weeks of data
      end_site_names <- unique(baseline_few$id)

      # messaging
      lost_sites <- setdiff(start_site_names, end_site_names)
      message(paste0("These sites were removed as they have ", week_required, " or fewer weeks of data:"))
      if (length(lost_sites) > 0){
        for (es in lost_sites){
          message(es)
        }
      } else {
        message("None")
      }

      if (nrow(baseline_few) > 0){

        few_baseline_set <- data.frame()

        for (every_site in unique(baseline_few$id)){

          working_site <- filter(baseline_few, id == every_site)
          combinations <- as.data.frame(working_site) %>% arrange(date) %>%
            group_by(year, week) %>% summarize(max_week_date = max(date))

          combinations <- combinations %>% mutate(week_number = row_number())

          combinations <- filter(combinations, week_number >= week_required)

          saved_baselines <- data.frame()

          for (every_combination in seq(1, nrow(combinations))){
            combo1 <- combinations[every_combination, ]

            full_set <- filter(working_site, date <= combo1$max_week_date)

            combo1$baseline <- quantile(full_set$log_value, 0.1)[[1]][1]
            combo1$stdev <- sd(full_set$log_value, na.rm = TRUE)
            combo1$baseline_mindate <- min(full_set$date)
            combo1$baseline_maxdate <- max(full_set$date)
            combo1$baseline_datapoints <- nrow(full_set)
            saved_baselines <- rbind(saved_baselines, combo1)

          }

          saved_baselines$id <- every_site
          few_baseline_set <- rbind(few_baseline_set, saved_baselines)

        }

        ################################################################################

        #colnames(few_baseline_set)
        #colnames(lots_baseline_set)

        few_baseline_set <- few_baseline_set %>% select(year, week, baseline, stdev, baseline_mindate, baseline_maxdate, baseline_datapoints, id)
        colnames(few_baseline_set) <- colnames(lots_baseline_set)

      } else {

        few_baseline_set <- data.frame()

      }

    } else {

      few_baseline_set <- data.frame()

    }

    #all_baselines <- rbind(lots_baseline_set, few_baseline_set)

    ##############################################################################

    #wastewater_data_in2 <- filter(wastewater_data_in2, year(date) >= year(Sys.Date()) - 1)

    wastewater_data_in2 <- wastewater_data_in2 %>% mutate(month_or_week = case_when(x_months_data_yn == "yes" & month(date) < 7 ~ 1,
                                                                                    x_months_data_yn == "yes" & month(date) >= 7 ~ 2,
                                                                                    x_months_data_yn == "no" ~ epiweek(date),
                                                                                    T ~ 9999),
                                                          year = year(date))


    if (any(wastewater_data_in2$month_or_week == 9999)){
      message("Six month data or sample date data corrupted, assigned 9999 value.")
      stop()
    }

    wastewater_data_in2 <- wastewater_data_in2 %>% mutate(year = case_when(x_months_data_yn == "no" & month_or_week >= 51 & month(date) == 1 ~ year - 1,
                                                                           T ~ year))

    # lots_baseline_set, few_baseline_set
    ### no data, needs to merge on the few dataset

    wastewater_few <- merge(filter(wastewater_data_in2, x_months_data_yn == "no"), few_baseline_set, by.x = c("id", "year", "month_or_week"),
                            by.y = c("id", "year", "year_half"), all.x = TRUE)

    ### yes data, needs to merge on the lots dataset
    wastewater_lots <- merge(filter(wastewater_data_in2, x_months_data_yn == "yes"), lots_baseline_set, by.x = c("id", "year", "month_or_week"),
                             by.y = c("id", "year", "year_half"), all.x = TRUE)

    # wastewater_data_in2 <- merge(wastewater_data_in2, all_baselines, by.x = c("id", "year", "month_or_week"),
    #                     by.y = c("id", "year", "year_half"), all.x = TRUE)

    wastewater_data_in2 <- rbind(wastewater_few, wastewater_lots)

    wastewater_data_in2 <- wastewater_data_in2 %>% select(id, date, sampletype,
                                                          sitetype, population_served,
                                                          gcper100ml, microbial_val, flow_val, normalized_measurement,
                                                          log_value, oldest_date,
                                                          days_since_first, x_months_data_yn, sample_counter,
                                                          multiple_durations, baseline, stdev,
                                                          baseline_mindate, baseline_maxdate, baseline_datapoints)


  }



  if (method_choice == "cdc_v2"){

    wastewater_data_in2 <- wastewater_data_in %>%
      group_by(id) %>%
      mutate(oldest_date = min(date))

    wastewater_data_in2 <- wastewater_data_in2 %>% group_by(id) %>% arrange(date) %>%
      mutate(days_since_first = as.numeric(difftime(date, oldest_date, units = "days")))

    wastewater_data_in2 <- wastewater_data_in2 %>%
      group_by(id) %>%
      mutate(x_months_data_yn = case_when(days_since_first/365.25 > 1 ~ "yes",
                                          T ~ "no"))

    # account for situation where dates have length between them, but there aren't enough samples in that range to justify moving to
    # the twelve months methodology
    wastewater_data_in2 <- wastewater_data_in2 %>%
      group_by(id) %>%
      arrange(date) %>%
      mutate(sample_counter = seq_along(gcper100ml))

    wastewater_data_in2 <- wastewater_data_in2 %>%
      mutate(x_months_data_yn = case_when(sample_counter < 48 ~ "no",
                                          T ~ x_months_data_yn))

    wastewater_data_in2 <- wastewater_data_in2 %>%
      group_by(id) %>%
      mutate(multiple_durations = length(unique(x_months_data_yn)))


    # For sites and method combinations with less than twelve months of data, baselines
    # are computed weekly until reaching twelve months, after which they remain unchanged
    # until the next August 1st, at which time baselines are re-calculated.

    if (any(wastewater_data_in2$x_months_data_yn == "no")){

      baseline_few <- filter(wastewater_data_in2, x_months_data_yn == "no") %>%
        group_by(id) %>%
        mutate(week = epiweek(date),
               year = year(date),
               total_weeks = length(unique(week)))

      baseline_few <- baseline_few %>%
        mutate(year = case_when(week >= 51 & month(date) == 1 ~ year - 1,
                                T ~ year))


      # small set site names
      start_site_names <- unique(baseline_few$id)

      #week_required <- 8
      baseline_few <- filter(baseline_few, total_weeks > week_required)

      # site names after we remove everything with x or fewer weeks of data
      end_site_names <- unique(baseline_few$id)

      # messaging
      lost_sites <- setdiff(start_site_names, end_site_names)
      message(paste0("These sites were removed as they have ", week_required, " or fewer weeks of data:"))
      if (length(lost_sites) > 0){
        for (es in lost_sites){
          message(es)
        }
      } else {
        message("None")
      }

      if (nrow(baseline_few) > 0){

        few_baseline_set <- data.frame()

        for (every_site in unique(baseline_few$id)){

          working_site <- filter(baseline_few, id == every_site)

          combinations <- as.data.frame(working_site) %>% arrange(date) %>%
            group_by(year, week) %>% summarize(max_week_date = max(date))

          combinations <- combinations %>% mutate(week_number = row_number())

          combinations <- filter(combinations, week_number >= week_required)

          saved_baselines <- data.frame()

          for (every_combination in seq(1, nrow(combinations))){
            combo1 <- combinations[every_combination, ]

            full_set <- filter(working_site, date <= combo1$max_week_date)

            combo1$baseline <- quantile(full_set$log_value, 0.1)[[1]][1]
            combo1$stdev <- sd(full_set$log_value, na.rm = TRUE)
            combo1$baseline_mindate <- min(full_set$date)
            combo1$baseline_maxdate <- max(full_set$date)
            combo1$baseline_datapoints <- nrow(full_set)
            saved_baselines <- rbind(saved_baselines, combo1)

          }

          saved_baselines$id <- every_site
          few_baseline_set <- rbind(few_baseline_set, saved_baselines)

        }

        ################################################################################

        #colnames(few_baseline_set)
        #colnames(lots_baseline_set)

        few_baseline_set <- as.data.frame(few_baseline_set) %>% select(baseline, stdev, baseline_mindate, baseline_maxdate, baseline_datapoints, id)
        #colnames(few_baseline_set) <- colnames(lots_baseline_set)

      } else {

        few_baseline_set <- data.frame()

      }

    } else {

      few_baseline_set <- data.frame()

    }



    # For site and method combinations (as listed above) with over twelve months of data, baselines are re-
    #   calculated every August 1st using all available data in the previous 18 months.


    if (any(wastewater_data_in2$x_months_data_yn == "yes")){

      baseline_lots <- filter(wastewater_data_in2, x_months_data_yn == "yes")

      # mark every august 1st
      baseline_lots <- baseline_lots %>% group_by(id) %>% arrange(date) %>%
        mutate(august1_id = case_when(as_date(date) >= as_date(paste0(year(as_date(date)), "-08-01")) & as_date(lag(date)) < as_date(paste0(year(as_date(date)), "-08-01")) ~ 1,
                                      T ~ 0))

      # then number the august 1st's in order
      august1 <- filter(baseline_lots, august1_id == 1)
      august1 <- august1 %>% group_by(id) %>% arrange(date) %>% mutate(number_aug = seq_along(date))

      baseline_lots <- merge(baseline_lots, august1, all = TRUE)

      # need to add these back in, in theory, since we want them to contribute to
      # the baseline calculation
      baseline_add <- filter(wastewater_data_in2, x_months_data_yn == "no") %>%
        group_by(id) %>%
        mutate(week = epiweek(date),
               year = year(date),
               total_weeks = length(unique(week)))

      baseline_add <- filter(baseline_add, total_weeks > week_required)
      baseline_add <- baseline_add %>% select(-week, -year, -total_weeks)

      baseline_add$august1_id <- 0
      baseline_add$number_aug <- NA

      all_baseline_lots <- rbind(baseline_add, baseline_lots)


      saved_baselines <- data.frame()
      # anything prior to the first august 1st we're going to leave alone, since that's
      # got to be filled with the last weekly baseline info

      # anything after the first august 1st, 18 months = baseline, every august 1st
      for (id_set in unique(all_baseline_lots$id)){

        all_baseline_lots2 <- filter(all_baseline_lots, id == id_set)

        for (i in seq(1:max(all_baseline_lots2$number_aug, na.rm = TRUE))){

          combo1 <- data.frame(id = id_set)
          date_interest_line <- filter(all_baseline_lots2, number_aug == i)

          baseline_for_period_set <- filter(all_baseline_lots2, as_date(date) >= as_date(date_interest_line$date[1]) %m-% months(18) & as_date(date) <= as_date(date_interest_line$date[1]))

          combo1$baseline <- quantile(baseline_for_period_set$log_value, 0.1)[[1]][1]
          combo1$stdev <- sd(baseline_for_period_set$log_value, na.rm = TRUE)
          combo1$baseline_mindate <- min(baseline_for_period_set$date, na.rm = TRUE)
          combo1$baseline_maxdate <- max(baseline_for_period_set$date, na.rm = TRUE)
          combo1$baseline_datapoints <- nrow(baseline_for_period_set)
          saved_baselines <- rbind(saved_baselines, combo1)


        }
      }


    } else {

      saved_baselines <- data.frame()

    }


    all_baselines <- rbind(few_baseline_set, saved_baselines)


    w_w_base <- merge(wastewater_data_in2, all_baselines, by.x = c("date", "id"), by.y = c("baseline_maxdate", "id"), all.x = TRUE, all.y = TRUE)
    w_w_base <- merge(w_w_base, all_baselines, all.x = TRUE)

    w_w_base <- w_w_base %>% group_by(id) %>% arrange(date) %>% fill(baseline, .direction = c("down"))
    w_w_base <- w_w_base %>% group_by(id) %>% arrange(date) %>% fill(stdev, .direction = c("down"))
    w_w_base <- w_w_base %>% group_by(id) %>% arrange(date) %>% fill(baseline_mindate, .direction = c("down"))
    w_w_base <- w_w_base %>% group_by(id) %>% arrange(date) %>% fill(baseline_datapoints, .direction = c("down"))
    w_w_base <- w_w_base %>% group_by(id) %>% arrange(date) %>% fill(baseline_maxdate, .direction = c("down"))

    # need to make this select the correct columns in proper order to
    # match other formats
    wastewater_data_in2 <- w_w_base %>% select(id, date, sampletype,
                                               sitetype, population_served,
                                               gcper100ml, microbial_val, flow_val, normalized_measurement,
                                               log_value, oldest_date,
                                               days_since_first, x_months_data_yn, sample_counter,
                                               multiple_durations, baseline, stdev,
                                               baseline_mindate, baseline_maxdate, baseline_datapoints)



  }



  if (method_choice == "cdc_v3"){

    wastewater_data_in2 <- wastewater_data_in %>%
      group_by(id) %>%
      mutate(oldest_date = min(date))

    wastewater_data_in2 <- wastewater_data_in2 %>% group_by(id) %>% arrange(date) %>%
      mutate(days_since_first = as.numeric(difftime(date, oldest_date, units = "days")))

    wastewater_data_in2 <- wastewater_data_in2 %>%
      group_by(id) %>%
      mutate(x_months_data_yn = case_when(days_since_first/365.25 > 1 ~ "yes",
                                          T ~ "no"))

    # account for situation where dates have length between them, but there aren't enough samples in that range to justify moving to
    # the twelve months methodology
    wastewater_data_in2 <- wastewater_data_in2 %>%
      group_by(id) %>%
      arrange(date) %>%
      mutate(sample_counter = seq_along(gcper100ml))

    wastewater_data_in2 <- wastewater_data_in2 %>%
      mutate(x_months_data_yn = case_when(sample_counter < 48 ~ "no",
                                          T ~ x_months_data_yn))

    wastewater_data_in2 <- wastewater_data_in2 %>%
      group_by(id) %>%
      mutate(multiple_durations = length(unique(x_months_data_yn)))


    # For sites and method combinations with less than twelve months of data, baselines
    # are computed weekly until reaching twelve months, after which they remain unchanged
    # until the next August 1st, at which time baselines are re-calculated.

    if (any(wastewater_data_in2$x_months_data_yn == "no")){

      baseline_few <- filter(wastewater_data_in2, x_months_data_yn == "no") %>%
        group_by(id) %>%
        mutate(week = epiweek(date),
               year = year(date),
               total_weeks = length(unique(week)))

      baseline_few <- baseline_few %>%
        mutate(year = case_when(week >= 51 & month(date) == 1 ~ year - 1,
                                T ~ year))


      # small set site names
      start_site_names <- unique(baseline_few$id)

      baseline_few <- filter(baseline_few, total_weeks > week_required)

      # site names after we remove everything with x or fewer weeks of data
      end_site_names <- unique(baseline_few$id)

      # messaging
      lost_sites <- setdiff(start_site_names, end_site_names)
      message(paste0("These sites were removed as they have ", week_required, " or fewer weeks of data:"))
      if (length(lost_sites) > 0){
        for (es in lost_sites){
          message(es)
        }
      } else {
        message("None")
      }

      if (nrow(baseline_few) > 0){

        few_baseline_set <- data.frame()

        for (every_site in unique(baseline_few$id)){

          working_site <- filter(baseline_few, id == every_site)

          combinations <- as.data.frame(working_site) %>% arrange(date) %>%
            group_by(year, week) %>% summarize(max_week_date = max(date))

          combinations <- combinations %>% mutate(week_number = row_number())

          combinations <- filter(combinations, week_number >= week_required)

          saved_baselines <- data.frame()

          for (every_combination in seq(1, nrow(combinations))){
            combo1 <- combinations[every_combination, ]

            full_set <- filter(working_site, date <= combo1$max_week_date)

            combo1$baseline <- quantile(full_set$log_value, 0.1)[[1]][1]
            combo1$stdev <- sd(full_set$log_value, na.rm = TRUE)
            combo1$baseline_mindate <- min(full_set$date)
            combo1$baseline_maxdate <- max(full_set$date)
            combo1$baseline_datapoints <- nrow(full_set)
            saved_baselines <- rbind(saved_baselines, combo1)

          }

          saved_baselines$id <- every_site
          few_baseline_set <- rbind(few_baseline_set, saved_baselines)

        }

        ################################################################################

        #colnames(few_baseline_set)
        #colnames(lots_baseline_set)

        few_baseline_set <- as.data.frame(few_baseline_set) %>% select(baseline, stdev, baseline_mindate, baseline_maxdate, baseline_datapoints, id)
        #colnames(few_baseline_set) <- colnames(lots_baseline_set)

      } else {

        few_baseline_set <- data.frame()

      }

    } else {

      few_baseline_set <- data.frame()

    }



    # For site and method combinations (as listed above) with over twelve months of data, baselines are re-
    #   calculated every August 1st using all available data in the previous 18 months.


    if (any(wastewater_data_in2$x_months_data_yn == "yes")){

      baseline_lots <- filter(wastewater_data_in2, x_months_data_yn == "yes")

      # mark every august 1st
      baseline_lots <- baseline_lots %>% group_by(id) %>% arrange(date) %>%
        mutate(august1_id = case_when(as_date(date) >= as_date(paste0(year(as_date(date)), "-08-01")) & as_date(lag(date)) < as_date(paste0(year(as_date(date)), "-08-01")) ~ 1,
                                      T ~ 0))

      # then number the august 1st's in order
      august1 <- filter(baseline_lots, august1_id == 1)
      august1 <- august1 %>% group_by(id) %>% arrange(date) %>% mutate(number_aug = seq_along(date))

      baseline_lots <- merge(baseline_lots, august1, all = TRUE)

      baseline_add <- filter(wastewater_data_in2, x_months_data_yn == "no")
      baseline_add$august1_id <- 0
      baseline_add$number_aug <- NA

      all_baseline_lots <- rbind(baseline_add, baseline_lots)


      saved_baselines <- data.frame()
      # anything prior to the first august 1st we're going to leave alone, since that's
      # got to be filled with the last weekly baseline info

      # anything after the first august 1st, 18 months = baseline, every august 1st
      for (id_set in unique(all_baseline_lots$id)){

        all_baseline_lots2 <- filter(all_baseline_lots, id == id_set)

        for (i in seq(1:max(all_baseline_lots$number_aug, na.rm = TRUE))){

          combo1 <- data.frame(id = id_set)
          date_interest_line <- filter(all_baseline_lots2, number_aug == i)

          baseline_for_period_set <- filter(all_baseline_lots2, as_date(date) >= as_date(date_interest_line$date[1]) %m-% months(24) & as_date(date) <= as_date(date_interest_line$date[1]))

          combo1$baseline <- quantile(baseline_for_period_set$log_value, 0.1)[[1]][1]
          combo1$stdev <- sd(baseline_for_period_set$log_value, na.rm = TRUE)
          combo1$baseline_mindate <- min(baseline_for_period_set$date, na.rm = TRUE)
          combo1$baseline_maxdate <- max(baseline_for_period_set$date, na.rm = TRUE)
          combo1$baseline_datapoints <- nrow(baseline_for_period_set)
          saved_baselines <- rbind(saved_baselines, combo1)


        }
      }


    } else {

      saved_baselines <- data.frame()

    }


    all_baselines <- rbind(few_baseline_set, saved_baselines)


    w_w_base <- merge(wastewater_data_in2, all_baselines, by.x = c("date", "id"), by.y = c("baseline_maxdate", "id"), all.x = TRUE, all.y = TRUE)
    w_w_base <- merge(w_w_base, all_baselines, all.x = TRUE)

    w_w_base <- w_w_base %>% group_by(id) %>% arrange(date) %>% fill(baseline, .direction = c("down"))
    w_w_base <- w_w_base %>% group_by(id) %>% arrange(date) %>% fill(stdev, .direction = c("down"))
    w_w_base <- w_w_base %>% group_by(id) %>% arrange(date) %>% fill(baseline_mindate, .direction = c("down"))
    w_w_base <- w_w_base %>% group_by(id) %>% arrange(date) %>% fill(baseline_datapoints, .direction = c("down"))
    w_w_base <- w_w_base %>% group_by(id) %>% arrange(date) %>% fill(baseline_maxdate, .direction = c("down"))


    wastewater_data_in2 <- w_w_base %>% select(id, date, sampletype,
                                               sitetype, population_served,
                                               gcper100ml, microbial_val, flow_val, normalized_measurement,
                                               log_value, oldest_date,
                                               days_since_first, x_months_data_yn, sample_counter,
                                               multiple_durations, baseline, stdev,
                                               baseline_mindate, baseline_maxdate, baseline_datapoints)



  }


  if (method_choice == "cdc_v4"){

    wastewater_data_in2 <- wastewater_data_in %>%
      group_by(id) %>%
      mutate(oldest_date = min(date))

    wastewater_data_in2 <- wastewater_data_in2 %>%
      mutate(days_since_first = as.numeric(difftime(date, oldest_date, units = "days")))

    wastewater_data_in2 <- wastewater_data_in2 %>%
      group_by(id) %>%
      mutate(x_months_data_yn = case_when(days_since_first/365.25 > 1 ~ "yes",
                                          T ~ "no"))

    # account for situation where dates have length between them, but there aren't enough samples in that range to justify moving to
    # the twelve months methodology
    wastewater_data_in2 <- wastewater_data_in2 %>%
      group_by(id) %>%
      arrange(date) %>%
      mutate(sample_counter = seq_along(gcper100ml))

    wastewater_data_in2 <- wastewater_data_in2 %>%
      mutate(x_months_data_yn = case_when(sample_counter < 48 ~ "no",
                                          T ~ x_months_data_yn))

    wastewater_data_in2 <- wastewater_data_in2 %>%
      group_by(id) %>%
      mutate(multiple_durations = length(unique(x_months_data_yn)))

    if (any(wastewater_data_in2$x_months_data_yn == "yes")){

      # still need all the data to calculate these baselines for the times immediately after
      # the site has enough data, but only for sites that have both "durations" of data
      baseline_lots <- filter(wastewater_data_in2, multiple_durations == 2) %>%
        mutate(year_half = case_when(month(date) < 4 ~ 2,
                                     month(date) >= 4 & month(date) < 10 ~ 3,
                                     month(date) >= 10 ~ 1,
                                     T ~ 9999),
               year = year(date))

      if (any(baseline_lots$year_half == 9999)){
        message("Sample dates corrupted, assigned 9999 value.")
        stop()
      }

      # april 1 & october 1 = reset dates

      # if your year half is 1 (it's jan, feb, march)
      # then your baseline should be the 10th percentile of all samples
      # from 24 months prior to october 1 of your current year minus 1

      # if your year half is 2 (i.e. it's apr, may, june, july, august, september)
      # then your baseline should be the 10th percentile of all samples
      # from 24 months prior to april 1 of your current year

      # if your year half is 3 (it's oct, nov, dec)
      # then your baseline should be the 10th percentile of all samples
      # from 24 months prior to october 1 of your current year

      lots_baseline_set <- data.frame()

      for (every_site in unique(baseline_lots$id)){

        working_site <- filter(baseline_lots, id == every_site) %>% arrange(date)
        combinations <- as.data.frame(working_site) %>% select(year, year_half) %>% distinct()

        saved_baselines <- data.frame()

        for (every_combination in seq(1, nrow(combinations))){
          combo1 <- combinations[every_combination, ]

          if (combo1$year_half == 1){

            # x to oct 1 (year)
            full_set <- filter(working_site, Date >= (as_date(paste0(combo1$year, "-10-01")) %m-% months(24)) &
                                 Date < as_date(paste0(combo1$year, "-10-01")))

          } else if (combo1$year_half == 2){

            # x to oct 1 (year - 1)
            full_set <- filter(working_site, Date >= (as_date(paste0((combo1$year - 1), "-10-01")) %m-% months(24)) &
                                 Date < as_date(paste0((combo1$year - 1), "-10-01")))

          } else if (combo1$year_half == 3){

            # x to apr 1 (year)
            full_set <- filter(working_site, Date >= (as_date(paste0(combo1$year, "-04-01")) %m-% months(24)) &
                                 Date < as_date(paste0(combo1$year, "-04-01")))

          }

          combo1$baseline <- quantile(full_set$log_value, 0.1)[[1]][1]
          combo1$stdev <- sd(full_set$log_value, na.rm = TRUE)
          combo1$baseline_mindate <- min(full_set$date)
          combo1$baseline_maxdate <- max(full_set$date)
          combo1$baseline_datapoints <- nrow(full_set)
          saved_baselines <- rbind(saved_baselines, combo1)

        }

        saved_baselines$id <- every_site
        lots_baseline_set <- rbind(lots_baseline_set, saved_baselines)

      }

      # have NAs in this, for the periods that are too new
      lots_baseline_set <- filter(lots_baseline_set, !is.na(baseline))

      # # also remove anything out of here that has too few days determining
      # # baseline
      # lots_baseline_set <- lots_baseline_set %>% mutate(baseline_days_range = as.numeric(difftime(baseline_maxdate, baseline_mindate, units = "days"))/365.25)
      #
      # lots_baseline_set <- filter(lots_baseline_set, baseline_days_range > 0.5)
      #
      # lots_baseline_set <- lots_baseline_set %>% select(-baseline_days_range)

    } else {

      lots_baseline_set <- data.frame()

    }




    ################################################################################
    # set up our baselines for new/short-term sites
    if (any(wastewater_data_in2$x_months_data_yn == "no")){
      baseline_few <- filter(wastewater_data_in2, x_months_data_yn == "no") %>% group_by(id) %>%
        mutate(week = epiweek(date),
               year = year(date),
               total_weeks = length(unique(week)))

      baseline_few <- baseline_few %>% mutate(year = case_when(week >= 51 & month(date) == 1 ~ year - 1,
                                                               T ~ year))


      # small set site names
      start_site_names <- unique(baseline_few$id)

      baseline_few <- filter(baseline_few, total_weeks > week_required)

      # site names after we remove everything with 6 or fewer weeks of data
      end_site_names <- unique(baseline_few$id)

      # messaging
      lost_sites <- setdiff(start_site_names, end_site_names)
      message(paste0("These sites were removed as they have ", week_required, " or fewer weeks of data:"))
      if (length(lost_sites) > 0){
        for (es in lost_sites){
          message(es)
        }
      } else {
        message("None")
      }

      if (nrow(baseline_few) > 0){

        few_baseline_set <- data.frame()

        for (every_site in unique(baseline_few$id)){

          working_site <- filter(baseline_few, id == every_site)
          combinations <- as.data.frame(working_site) %>% arrange(date) %>%
            group_by(year, week) %>% summarize(max_week_date = max(date))

          combinations <- combinations %>% mutate(week_number = row_number())

          combinations <- filter(combinations, week_number >= week_required)

          saved_baselines <- data.frame()

          for (every_combination in seq(1, nrow(combinations))){
            combo1 <- combinations[every_combination, ]

            full_set <- filter(working_site, date <= combo1$max_week_date)

            combo1$baseline <- quantile(full_set$log_value, 0.1)[[1]][1]
            combo1$stdev <- sd(full_set$log_value, na.rm = TRUE)
            combo1$baseline_mindate <- min(full_set$date)
            combo1$baseline_maxdate <- max(full_set$date)
            combo1$baseline_datapoints <- nrow(full_set)
            saved_baselines <- rbind(saved_baselines, combo1)

          }

          saved_baselines$id <- every_site
          few_baseline_set <- rbind(few_baseline_set, saved_baselines)

        }

        ################################################################################

        #colnames(few_baseline_set)
        #colnames(lots_baseline_set)

        few_baseline_set <- few_baseline_set %>% select(year, week, baseline, stdev, baseline_mindate, baseline_maxdate, baseline_datapoints, id)
        colnames(few_baseline_set) <- colnames(lots_baseline_set)

      } else {

        few_baseline_set <- data.frame()

      }

    } else {

      few_baseline_set <- data.frame()

    }

    #all_baselines <- rbind(lots_baseline_set, few_baseline_set)

    ##############################################################################

    #wastewater_data_in2 <- filter(wastewater_data_in2, year(date) >= year(Sys.Date()) - 1)



        wastewater_data_in2 <- wastewater_data_in2 %>% mutate(month_or_week = case_when(x_months_data_yn == "yes" & month(date) < 4 ~ 2,
                                                                                        x_months_data_yn == "yes" & month(date) >= 4 & month(date) < 10 ~ 3,
                                                                                        x_months_data_yn == "yes" & month(date) >= 10 ~ 1,
                                                                                        x_months_data_yn == "no" ~ epiweek(date),
                                                                                        T ~ 9999),
                                                              year = year(date))


        if (any(wastewater_data_in2$month_or_week == 9999)){
          message("Six month data or sample date data corrupted, assigned 9999 value.")
          stop()
        }

        wastewater_data_in2 <- wastewater_data_in2 %>% mutate(year = case_when(x_months_data_yn == "no" & month_or_week >= 51 & month(date) == 1 ~ year - 1,
                                                                               T ~ year))

        # lots_baseline_set, few_baseline_set
        ### no data, needs to merge on the few dataset

        wastewater_few <- merge(filter(wastewater_data_in2, x_months_data_yn == "no"), few_baseline_set, by.x = c("id", "year", "month_or_week"),
                                by.y = c("id", "year", "year_half"), all.x = TRUE)

        ### yes data, needs to merge on the lots dataset
        wastewater_lots <- merge(filter(wastewater_data_in2, x_months_data_yn == "yes"), lots_baseline_set, by.x = c("id", "year", "month_or_week"),
                                 by.y = c("id", "year", "year_half"), all.x = TRUE)


        wastewater_data_in2 <- rbind(wastewater_few, wastewater_lots)


        ###### account for optional
        # "All historical WVAL values are updated with the new baseline when recalculated."
        # rule

        if (hist_override == 1){

          # if there is only low data, keep that
          # otherwise if it has more, override all previous with the most recent baseline


          most_recent_baselines <- filter(wastewater_data_in2, multiple_durations == 2) %>%
            group_by(id) %>%
            summarize(baseline_maxdate = max(baseline_maxdate, na.rm = TRUE))

          most_recent_baselines <- merge(most_recent_baselines, wastewater_data_in2, by = c("id", "baseline_maxdate"), all.x = TRUE) %>%
            select(id, baseline, stdev,
                   baseline_mindate, baseline_maxdate, baseline_datapoints) %>% distinct()


          override_set <- filter(wastewater_data_in2, id %in% unique(most_recent_baselines$id))
          override_set <- override_set %>% select(id, date, sampletype,
                                                  sitetype, population_served,
                                                  gcper100ml, microbial_val, flow_val, normalized_measurement,
                                                  log_value, oldest_date,
                                                  days_since_first, x_months_data_yn, sample_counter,
                                                  multiple_durations)
          override_set <- merge(override_set, most_recent_baselines, by = c("id"), all.x = TRUE)

          non_override <- filter(wastewater_data_in2, !id %in% unique(most_recent_baselines$id))

          wastewater_data_in2 <- rbind(override_set, non_override)


        }


    wastewater_data_in2 <- wastewater_data_in2 %>% select(id, date, sampletype,
                                                          sitetype, population_served,
                                                          gcper100ml, microbial_val, flow_val, normalized_measurement,
                                                          log_value, oldest_date,
                                                          days_since_first, x_months_data_yn, sample_counter,
                                                          multiple_durations, baseline, stdev,
                                                          baseline_mindate, baseline_maxdate, baseline_datapoints)


  }


  if (method_choice == "all_data"){

    wastewater_data_in2 <- wastewater_data_in %>%
      group_by(id) %>%
      arrange(date) %>%
      mutate(baseline = quantile(log_value, 0.1)[[1]][1],
             stdev = sd(log_value, na.rm = TRUE),
             baseline_mindate = min(date),
             baseline_maxdate = max(date),
             year = year(date),
             oldest_date = min(date))

    wastewater_data_in2 <- wastewater_data_in2 %>%
      group_by(id) %>%
      arrange(date) %>%
      mutate(days_since_first = as.numeric(difftime(date, oldest_date, units = "days")),
             x_months_data_yn = case_when(days_since_first/365.25 > 0.5 ~ "yes",
                                          T ~ "no"),
             sample_counter = seq_along(gcper100ml),
             multiple_durations = length(unique(x_months_data_yn)),
             baseline_datapoints = length(date))





    wastewater_data_in2 <- filter(wastewater_data_in2, !is.na(baseline))
    wastewater_data_in2 <- filter(wastewater_data_in2, stdev != 0)

    wastewater_data_in2 <- wastewater_data_in2 %>% select(id, date, sampletype,
                                                          sitetype, population_served,
                                                          gcper100ml, microbial_val, flow_val, normalized_measurement,
                                                          log_value, oldest_date,
                                                          days_since_first, x_months_data_yn, sample_counter,
                                                          multiple_durations, baseline, stdev,
                                                          baseline_mindate, baseline_maxdate, baseline_datapoints)

  }


  return(wastewater_data_in2)

}
