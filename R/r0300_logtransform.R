
#' Log Transformation of Values
#'
#' This function takes in a dataframe of wastewater measurement data, as well as
#' a character value of "yes" or "no" depending on whether the user would like to remove
#' outliers in a z-score method. This function also takes in a threshold z score value (numeric)
#' that has a default value set as 4.
#'
#' The log() of the `normalized_measurement` is calculated in a new column called `log_value`.
#' As log(0) = -Inf, those rows are removed and the number of rows that were removed
#' because of this is messaged to the console.
#'
#' If "no" for outlier_removal, then the remaining dataframe is returned from this function.
#'
#' If "yes" for outlier_removal, then the dataframe is grouped by `id` (by site) and the average
#' and standard deviation of `log_value` is calculated. `z_score` is calculated as zero if
#' the overall standard deviation is `NA` or if the overall standard deviation is zero. Otherwise,
#' `z_score` is calculated as the absolute value of the sample `log_value` minus the overall mean log value,
#' divided by the overall standard deviation. Any rows where the calculated z_score is less than
#' or equal to the provided `z_score_threshold` are kept (so, any rows where the calculated z_score
#' is greater than the provided `z_score_threshold` are removed).
#'
#' The number of rows that are removed due to z-score thresholding are messaged to the
#' console. Any extranneous variables created during the z-score calculations are removed
#' and the remaining dataframe is returned from this function.
#'
#' @param wastewater_data_in A dataframe of wastewater site, metadata, and measurement values
#' @param outlier_removal A character string, either "yes" or "no" to indicate that outlier removal should/not occur
#' @param z_score_threshold A number, indicating the upper threshold of the z-scores that should be kept if outlier removal is occurring. Default value is 4
#' @return A data frame with a new column and also potentially filtered rows
#' @export

r0300_logtransform <- function(wastewater_data_in, outlier_removal, z_score_threshold = 4){

  # log transform the normalized measurement
  wastewater_data_in2 <- wastewater_data_in %>%
    mutate(log_value = log(normalized_measurement))

  # count how many rows are in the dataset
  count_total_rows <- nrow(wastewater_data_in2)

  # remove all of them where the log_value turned to -Inf
  # (because log(0) = -Inf)
  wastewater_data_in2 <- filter(wastewater_data_in2, log_value != -Inf)

  # count how many rows are in the dataset now
  after_rows <- nrow(wastewater_data_in2)

  # figure out how many rows were removed due to the 0/infinity thing
  rows_removed_due_to_infinity <- count_total_rows - after_rows

  # message that as part of the function
  message(paste0(rows_removed_due_to_infinity, " rows were removed because their log(value) == -Inf"))

  # if we want to remove outliers per methodology
  if (outlier_removal == "yes"){

    # count the number of rows
    count_total_rows <- nrow(wastewater_data_in2)

    # removing things with z-score > 4
    # Calculate the mean and standard deviation for each site overall
    mean_stddev_norm_df <- wastewater_data_in2 %>% group_by(id) %>%
      summarize(mean_log_value = mean(log_value, na.rm = TRUE),
                stddev_log_value = sd(log_value, na.rm = TRUE))
    # group the dataframe by site id, and calculate the average for the site's data (log value)
    # and the standard deviation of the site's data (log value)

    # Join this mean_stddev_norm_df back to the original all_final_filtered DataFrame
    wastewater_data_in2 <- merge(wastewater_data_in2, mean_stddev_norm_df, by = c("id"), all.x = TRUE)

    # Filter out the outliers based on the Z-score threshold


    # Calculate the Z-score with handling of NaN stddev and division by zero
    wastewater_data_in2 <- wastewater_data_in2 %>% mutate(z_score = case_when(is.na(stddev_log_value) | stddev_log_value == 0 ~ 0,
                                                                              T ~ abs(log_value - mean_log_value) / stddev_log_value))
    # even with nulls --- Assigning a z-score that ensures these rows won't be removed


    wastewater_data_in2 <- filter(wastewater_data_in2, z_score <= z_score_threshold)
    # keep all rows where the z_score is less than or equal to the threshold

    after_rows <- nrow(wastewater_data_in2)
    # count the number of rows

    # calculate how many rows were removed
    rows_removed_due_to_zscore <- count_total_rows - after_rows

    # and report that number via message
    message(paste0(rows_removed_due_to_zscore, " rows were removed because the zscore was too high"))

    # reset columns included in output
    wastewater_data_in2 <- wastewater_data_in2 %>% select(-z_score, -mean_log_value, -stddev_log_value)

  }


  return(wastewater_data_in2)
}
