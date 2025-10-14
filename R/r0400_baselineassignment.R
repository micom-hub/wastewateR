

r0400_baselineassignment <- function(wastewater_data_in, method_choice){

  #wastewater_data_in <- out_put_2
  #method_choice <- "cdc"

  if (method_choice == "cdc"){

    wastewater_data_in2 <- wastewater_data_in %>% group_by(id) %>% mutate(oldest_date = min(date))
    wastewater_data_in2 <- wastewater_data_in2 %>% mutate(days_since_first = as.numeric(difftime(date, oldest_date, units = "days")))

    wastewater_data_in2 <- wastewater_data_in2 %>% group_by(id) %>% mutate(six_months_data_yn = case_when(days_since_first/365.25 > 0.5 ~ "yes",
                                                                                                          T ~ "no"))

    # account for situation where dates have length between them, but there aren't enough samples in that range to justify moving to
    # the six months methodology
    wastewater_data_in2 <- wastewater_data_in2 %>% group_by(id) %>% arrange(date) %>% mutate(sample_counter = seq_along(n1gcper100ml))

    wastewater_data_in2 <- wastewater_data_in2 %>% mutate(six_months_data_yn = case_when(sample_counter < 24 ~ "no",
                                                                                         T ~ six_months_data_yn))

    wastewater_data_in2 <- wastewater_data_in2 %>% group_by(id) %>% mutate(multiple_durations = length(unique(six_months_data_yn)))

    if (any(wastewater_data_in2$six_months_data_yn == "yes")){

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
    if (any(wastewater_data_in2$six_months_data_yn == "no")){
      baseline_few <- filter(wastewater_data_in2, six_months_data_yn == "no") %>% group_by(id) %>%
        mutate(week = epiweek(date),
               year = year(date),
               total_weeks = length(unique(week)))

      baseline_few <- baseline_few %>% mutate(year = case_when(week >= 51 & month(date) == 1 ~ year - 1,
                                                               T ~ year))


      # small set site names
      start_site_names <- unique(baseline_few$sitename)

      baseline_few <- filter(baseline_few, total_weeks > 6)

      # site names after we remove everything with 6 or fewer weeks of data
      end_site_names <- unique(baseline_few$sitename)

      # messaging
      lost_sites <- setdiff(start_site_names, end_site_names)
      message("These sites were removed as they have six or fewer weeks of data:")
      if (length(lost_sites) > 0){
        message(lost_sites)
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

          combinations <- filter(combinations, week_number >= 6)

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

    wastewater_data_in2 <- wastewater_data_in2 %>% mutate(month_or_week = case_when(six_months_data_yn == "yes" & month(date) < 7 ~ 1,
                                                                                    six_months_data_yn == "yes" & month(date) >= 7 ~ 2,
                                                                                    six_months_data_yn == "no" ~ epiweek(date),
                                                                                    T ~ 9999),
                                                          year = year(date))


    if (any(wastewater_data_in2$month_or_week == 9999)){
      message("Six month data or sample date data corrupted, assigned 9999 value.")
      stop()
    }

    wastewater_data_in2 <- wastewater_data_in2 %>% mutate(year = case_when(six_months_data_yn == "no" & month_or_week >= 51 & month(date) == 1 ~ year - 1,
                                                                           T ~ year))

    # lots_baseline_set, few_baseline_set
    ### no data, needs to merge on the few dataset

    wastewater_few <- merge(filter(wastewater_data_in2, six_months_data_yn == "no"), few_baseline_set, by.x = c("id", "year", "month_or_week"),
                            by.y = c("id", "year", "year_half"), all.x = TRUE)

    ### yes data, needs to merge on the lots dataset
    wastewater_lots <- merge(filter(wastewater_data_in2, six_months_data_yn == "yes"), lots_baseline_set, by.x = c("id", "year", "month_or_week"),
                             by.y = c("id", "year", "year_half"), all.x = TRUE)

    # wastewater_data_in2 <- merge(wastewater_data_in2, all_baselines, by.x = c("id", "year", "month_or_week"),
    #                     by.y = c("id", "year", "year_half"), all.x = TRUE)

    wastewater_data_in2 <- rbind(wastewater_few, wastewater_lots)

    # wastewater_data_in2 <- filter(wastewater_data_in2, !is.na(baseline))
    # wastewater_data_in2 <- filter(wastewater_data_in2, stdev != 0)

  }


  if (method_choice == "all_data"){

    wastewater_data_in2 <- wastewater_data_in %>%
      group_by(id) %>%
      arrange(date) %>%
      mutate(baseline = quantile(log_value, 0.1)[[1]][1],
             stdev = sd(log_value, na.rm = TRUE),
             baseline_mindate = min(date),
             baseline_maxdate = max(date),
             year = year(date))

    wastewater_data_in2 <- filter(wastewater_data_in2, !is.na(baseline))
    wastewater_data_in2 <- filter(wastewater_data_in2, stdev != 0)


  }


  return(wastewater_data_in2)

}
