
#' Standalone Function - Renaming & selecting columns based on initial processing software, for use in other wastewateR functions
#'
#' This function takes in a dataframe and a character string as input. The dataframe should
#' be a laboratory data file. The character string should indicate the software that
#' was used to generate the laboratory data file. Currently, the only style supported in
#' this function is "qx_manager", and that is the default setting.
#' If style is set to "qx_manager", the function will select 8 columns from the dataframe
#' that was provided as input: Well, Sample.description.1, Target, Conc.copies.µL.,
#' Copies.20µLWell, Accepted.Droplets, Positives, and Negatives. It will then rename these to:
#' "Well", "Sample", "Target", "Concentration", "CopiesPer20uLWell", "AcceptedDroplets",
#' "Positives", and "Negatives", respectively.
#' This smaller set is the return value dataframe of this function.
#' If the style is not a recognized option, the function will print "Indicated file style not recognized:"
#' and the style that was input to the console. In this case, the original dataframe that was provided
#' will be returned.
#' @param off_machine A dataframe of laboratory data, directly off the testing machine
#' @param style A character string indicating the laboratory software used to generate the file loaded as off_machine. Currently accepts only "qx_manager"
#' @return A dataframe with 8 columns: Well, Sample, Target, Concentration, CopiesPer20uLWell, AcceptedDroplets, Positives, Negatives OR the original dataframe submitted, if the "style" characters string is not recognized.
#' @export
#'
software_renaming <- function(off_machine, style = "qx_manager"){

  if (style == "qx_manager"){

    smaller_set <- off_machine %>% select(Well, Sample.description.1, Target, Conc.copies.µL.,
                                     Copies.20µLWell, Accepted.Droplets, Positives, Negatives)

    colnames(smaller_set) <- c("Well", "Sample", "Target", "Concentration", "CopiesPer20uLWell",
                               "AcceptedDroplets", "Positives", "Negatives")

    return(smaller_set)

  } else {

    message(paste0("Indicated file style not recognized: ", style))

    return(off_machine)

  }

}
