

r0100_inclusion_exclusion <- function(data_file, pop_serve_num, sample_type_list, testing_site_type_list){

  if (!is.na(pop_serve_num)){

    data_file <- filter(data_file, population_served >= pop_serve_num)

  }

  if (!is.na(sample_type_list)){

    data_file <- filter(data_file, sampletype %in% sample_type_list)

  }


  if (!is.na(testing_site_type_list)){

    data_file <- filter(data_file, sitetype %in% testing_site_type_list)

  }

  return(data_file)

}

