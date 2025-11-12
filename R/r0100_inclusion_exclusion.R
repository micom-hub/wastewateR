
#' Determine factors on which to include multiple wastewater sites into wVal calculation
#'
#' This function takes in a dataframe of wastewater data measurements and site metadata
#' and a series of numeric and character vector values.
#'
#' If one does not want to consider a particular metadata point, the value can be set at NA. This is
#' the default for all values.
#'
#' To use the pop_serve_num input, the dataframe must include a variable called `population served`.
#' If `pop_serve_num` is provided a numeric value, the data frame is filtered to only include
#' sites with a metadata `population served` value of greater than or equal to the provided
#' numeric value.
#'
#' To use the sample_type_list input, the dataframe must include a variable called `sampletype`.
#' If `sample_type_list` is provided a vector of character strings, the data frame is filtered to only include
#' sites with a metadata `sampletype` included in the provided character vector.
#'
#' #' To use the testing_site_type_list input, the dataframe must include a variable called `sitetype`.
#' If `testing_site_type_list` is provided a vector of character strings, the data frame is filtered to only include
#' sites with a metadata `sitetype` included in the provided character vector.
#'
#' For all cases, if the input is left blank, all of the sites will be included regardless of
#' their value for the related dataframe variable.
#'
#' Any/all of the three inputs can be used or not.
#'
#' @param data_file A dataframe of wastewater site, metadata, and measurement values
#' @param pop_serve_num A numeric input indicating the lower bound of acceptable population served values to include in the final data frame; Default value is `NA`
#' @param sample_type_list A character string vector indicating the acceptable sample types to include in the final data frame; Default value is `NA`
#' @param testing_site_type_list A character string vector indicating the acceptable site types to include in the final data frame; Default value is `NA`
#' @return A data frame with (possibly) filtered row results
#' @export

r0100_inclusion_exclusion <- function(data_file, pop_serve_num = NA, sample_type_list = "blank", testing_site_type_list = "blank"){

  if (!is.na(pop_serve_num)){

    data_file <- filter(data_file, population_served >= pop_serve_num)

  }

  if (sample_type_list != "blank"){

    data_file <- filter(data_file, sampletype %in% sample_type_list)

  }


  if (testing_site_type_list != "blank"){

    data_file <- filter(data_file, sitetype %in% testing_site_type_list)

  }

  return(data_file)

}

