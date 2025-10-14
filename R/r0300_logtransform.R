

r0300_logtransform <- function(wastewater_data_in, outlier_removal){

  wastewater_data_in2 <- wastewater_data_in %>% mutate(log_value = log(normalized_measurement))


  count_total_rows <- nrow(wastewater_data_in2)

  wastewater_data_in2 <- filter(wastewater_data_in2, log_value != -Inf)

  after_rows <- nrow(wastewater_data_in2)

  rows_removed_due_to_infinity <- count_total_rows - after_rows

  message(paste0(rows_removed_due_to_infinity, " rows were removed because their log(value) == -Inf"))


  if (outlier_removal == "yes"){

    count_total_rows <- nrow(wastewater_data_in2)

    # removing things with z-score > 4
    # Calculate the mean and standard deviation for each site overall
    mean_stddev_norm_df <- wastewater_data_in2 %>% group_by(id) %>%
      summarize(mean_log_value = mean(log_value, na.rm = TRUE),
                stddev_log_value = sd(log_value, na.rm = TRUE))

    # Join this mean_stddev_norm_df back to the original all_final_filtered DataFrame
    wastewater_data_in2 <- merge(wastewater_data_in2, mean_stddev_norm_df, by = c("id"), all.x = TRUE)

    # Filter out the outliers based on the Z-score threshold
    z_score_threshold <- 4

    # Calculate the Z-score with handling of NaN stddev and division by zero
    wastewater_data_in2 <- wastewater_data_in2 %>% mutate(z_score = case_when(is.na(stddev_log_value) | stddev_log_value == 0 ~ 0,
                                                                              T ~ abs(log_value - mean_log_value) / stddev_log_value))
    # even with nulls --- Assigning a z-score that ensures these rows won't be removed


    wastewater_data_in2 <- filter(wastewater_data_in2, z_score <= z_score_threshold)

    after_rows <- nrow(wastewater_data_in2)

    rows_removed_due_to_zscore <- count_total_rows - after_rows

    message(paste0(rows_removed_due_to_zscore, " rows were removed because the zscore was too high"))

    wastewater_data_in2 <- wastewater_data_in2 %>% select(-z_score, -mean_log_value, -stddev_log_value)

  }


  return(wastewater_data_in2)
}
