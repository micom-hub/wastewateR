#' wrapper function for SC2 wval calculations
#'
#' Parameters:
#'
#' - ww_df_in, normal_yesno, and choose_norm must be provided to this function.
#'
#' - method_choice = "cdc_v4", org = "SC2_v2", week_required = 8, method = "mean", outlier_removal = "yes" are already set to run the most recent cdc wval sc2 methodology
#'
#' - site_to_region_crosswalk = NA, pop_serve_num = NA, sample_type_list = NA, testing_site_type_list = NA can be set if desired
#'
#' @param ww_df_in A dataframe of wastewater site, metadata, and measurement values
#' @param normal_yesno A character string, either "yes" or "no", indicating whether the user would like to normalize their data or leave it as is.
#' @param choose_norm A character string, either "microbial", "flow", "mix_flow_first", or "mix_microbial_first", indicating which normalization method to use.
#' @param site_to_region_crosswalk Defaults to NA. If provided, should be a dataframe of "id", "Geography", "weight"
#' @param outlier_removal A character string, either "yes" or "no" to indicate that outlier removal should/not occur. Default value is "yes"
#' @param method_choice A character string of "cdc_v1", "cdc_v2", "cdc_v3", "cdc_v4", or "all_data" to determine method of baseline assignment. Defaults to "cdc_v4"
#' @param week_required A numeric value required if using "cdc_v1", "cdc_v2", "cdc_v3", or "cdc_v4" that sets the minimum number of weeks of data a site must have in order to calculate baselines. If a site has fewer weeks of data than this number, they are removed from consideration. Default value is 8
#' @param org A character string, either "SC2_v1", "FLU_v1", "RSV_v1", "SC2_v2", "FLU_v2", "RSV_v2" indicating what pathogen the wastewater data represents, and the CDC methodology version of level determination the user would like to use. Default value is "SC2_v2"
#' @param method Character string, either "median" or "mean", defaults to "mean"
#' @param pop_serve_num A numeric input indicating the lower bound of acceptable population served values to include in the final data frame; Default value is `NA`
#' @param sample_type_list A character string vector indicating the acceptable sample types to include in the final data frame; Default value is `NA`
#' @param testing_site_type_list A character string vector indicating the acceptable site types to include in the final data frame; Default value is `NA`
#' @return A data frame of weekly wval levels per geography
#' @export
wval_sc2 <- function(ww_df_in,
                     normal_yesno,
                     choose_norm,
                     site_to_region_crosswalk = NA,
                     outlier_removal = "yes",
                     method_choice = "cdc_v4",
                     week_required = 8,
                     org = "SC2_v2",
                     method = "mean",
                     pop_serve_num = NA,
                     sample_type_list = NA,
                     testing_site_type_list = NA){


  wval1 <- r0100_inclusion_exclusion(ww_df_in, pop_serve_num,
                                     sample_type_list,
                                     testing_site_type_list)

  wval1 <- r0200_normalize(wval1, normal_yesno,
                           choose_norm)

  wval1 <- r0300_logtransform(wval1, outlier_removal)

  wval1 <- r0400_baselineassignment(wval1, method_choice, week_required)

  wval1 <- r0500_wval_sitecalc(wval1, org)

  wval1 <- r0600_wval_regioncalc(wval1, site_to_region_crosswalk, method, org)

  return(wval1)
}
