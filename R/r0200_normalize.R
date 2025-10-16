#'
#' Normalization Choice and Implementation
#'
#' This function takes in a dataframe of wastewater site data, as well as a (`normal_yesno`) character string indicating "yes" or "no"
#' if the user would like to apply a normalization method, and a (`choose_norm`) character string
#' indicating the method that would be used, if 'yes'. For this function to work,
#' the dataframe must, at a minimum, contain a column called `gcper100ml`. Depending on the use of the
#' normalization methods, `microbial_val` and/or `flow_val` must also be present.
#'
#' * `gcper100ml` = measurement of a given pathogen in the wastewater sample
#' * `microbial_val` = measurement (in the same units as `gcper100ml` values) of the microbial normalization choice in the wastewater sample
#' * `flow_val` = measurement of the water flow to normalize the `gcper100ml` value by
#'
#' If the user inputs 'no' for `normal_yesno`, then a new column called `normalized_measurement`
#' is added, and it is set as the value of `gcper100ml`. Any rows where `normalized_measurement` is `NA`
#' are removed, and the dataframe is returned.
#'
#' If the user inputs 'yes' for `normal_yesno`, then the user must also provide a function
#' input for `choose_norm`. This can be either:
#'
#' * "microbial": A new column called `normalized_measurement` is set as the value of `gcper100ml` / `microbial_val`
#' * "flow": A new column called `normalized_measurement` is set as the value of `gcper100ml` / `flow_val`
#' * "mix_flow_first": Both `gcper100ml` / `microbial_val` and `gcper100ml` / `flow_val` are calculated.
#' `normalized_measurement` is filled with the value of `gcper100ml` / `flow_val` first. Any missing values are filled with `gcper100ml` / `microbial_val`
#' * "mix_microbial_first": Both `gcper100ml` / `microbial_val` and `gcper100ml` / `flow_val` are calculated.
#' `normalized_measurement` is filled with the value of `gcper100ml` / `microbial_val` first. Any missing values are filled with `gcper100ml` / `flow_val`
#'
#' Any rows where `normalized_measurement` is `NA` are removed, and the dataframe is returned.
#'
#' If the user inputs something that is not "microbial", "flow", "mix_flow_first", or
#' "mix_microbial_first", then the system will
#' **STOP** after printing "Unaccepted function input; normalization method must be either 'microbial', 'flow', 'mix_flow_first', or 'mix_microbial_first', not: "
#' and the user input to the console.
#'
#' If the user inputs something that is not 'yes' or 'no' for `normal_yesno`, then the system will
#' **STOP** after printing "Unaccepted function input; normalization choice must be either 'yes' or 'no', not: "
#' and the user input to the console.
#'
#' @param wastewater_data_in A dataframe of wastewater site, metadata, and measurement values
#' @param normal_yesno A character string, either "yes" or "no", indicating whether the user would like to normalize their data or leave it as is.
#' @param choose_norm A character string, either "microbial", "flow", "mix_flow_first", or "mix_microbial_first", indicating which normalization method to use.
#' @return A data frame with a new column and also potentially filtered rows
#' @export

r0200_normalize <- function(wastewater_data_in, normal_yesno, choose_norm){

  if (normal_yesno == "no"){
    # make the "normalized column name" but fill it with non-normalized data
    wastewater_data_in <- wastewater_data_in %>% mutate(normalized_measurement = gcper100ml)
  } else if (normal_yesno == "yes"){


    # preferentially choose
    if (choose_norm == "microbial"){

      wastewater_data_in <- wastewater_data_in %>% mutate(normalized_measurement = gcper100ml / microbial_val)

    } else if (choose_norm == "flow"){

      wastewater_data_in <- wastewater_data_in %>% mutate(normalized_measurement = gcper100ml / flow_val)

    } else if (choose_norm == "mix_flow_first"){

      # calculate the normalized values
      wastewater_data_in <- wastewater_data_in %>% mutate(normalized_microbial = gcper100ml / microbial_val,
                                                          normalized_flow = gcper100ml / flow_val)

      wastewater_data_in <- wastewater_data_in %>% mutate(normalized_measurement = case_when(!is.na(normalized_flow) ~ normalized_flow,
                                                                                             T ~ normalized_microbial))

      wastewater_data_in <- wastewater_data_in %>% select(-normalized_microbial, -normalized_flow)

    } else if (choose_norm == "mix_microbial_first"){

      # calculate the normalized values
      wastewater_data_in <- wastewater_data_in %>% mutate(normalized_microbial = gcper100ml / microbial_val,
                                                          normalized_flow = gcper100ml / flow_val)

      wastewater_data_in <- wastewater_data_in %>% mutate(normalized_measurement = case_when(!is.na(normalized_microbial) ~ normalized_microbial,
                                                                                             T ~ normalized_flow))

      wastewater_data_in <- wastewater_data_in %>% select(-normalized_microbial, -normalized_flow)

    } else {

      message("Unaccepted function input; normalization method must be either 'microbial', 'flow', 'mix_flow_first', or 'mix_microbial_first', not: ")
      message(choose_norm)
      stop()

    }

  } else {

    message("Unaccepted function input; normalization choice must be either 'yes' or 'no', not: ")
    message(normal_yesno)
    stop()

  }


  wastewater_data_in <- filter(wastewater_data_in, !is.na(normalized_measurement))



  return(wastewater_data_in)

}
