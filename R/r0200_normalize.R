

r0200_normalize <- function(wastewater_data_in, normal_yesno, choose_norm){

  if (normal_yesno == "no"){
    # make the "normalized column name" but fill it with non-normalized data
    wastewater_data_in <- wastewater_data_in %>% mutate(normalized_measurement = n1gcper100ml)
  } else {


    # preferentially choose
    if (choose_norm == "microbial"){

      wastewater_data_in <- wastewater_data_in %>% mutate(normalized_measurement = n1gcper100ml / microbial_val)

    } else if (choose_norm == "flow"){

      wastewater_data_in <- wastewater_data_in %>% mutate(normalized_measurement = n1gcper100ml / flow_val)

    } else if (choose_norm == "mix_flow_first"){

      # calculate the normalized values
      wastewater_data_in <- wastewater_data_in %>% mutate(normalized_microbial = n1gcper100ml / microbial_val,
                                                          normalized_flow = n1gcper100ml / flow_val)

      wastewater_data_in <- wastewater_data_in %>% mutate(normalized_measurement = case_when(!is.na(normalized_flow) ~ normalized_flow,
                                                                                             T ~ normalized_microbial))

      wastewater_data_in <- wastewater_data_in %>% select(-normalized_microbial, -normalized_flow)

    } else if (choose_norm == "mix_microbial_first"){

      # calculate the normalized values
      wastewater_data_in <- wastewater_data_in %>% mutate(normalized_microbial = n1gcper100ml / microbial_val,
                                                          normalized_flow = n1gcper100ml / flow_val)

      wastewater_data_in <- wastewater_data_in %>% mutate(normalized_measurement = case_when(!is.na(normalized_microbial) ~ normalized_microbial,
                                                                                             T ~ normalized_flow))

      wastewater_data_in <- wastewater_data_in %>% select(-normalized_microbial, -normalized_flow)

    }

  }


  wastewater_data_in <- filter(wastewater_data_in, !is.na(normalized_measurement))



  return(wastewater_data_in)

}
