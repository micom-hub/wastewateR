
#' Wrapper Function - QAQC for a Fiveplex
#'
#' This function is meant to run a standard QAQC set-up using the functions available
#' in this package. For increased flexibility, manual construction of the functions
#' would be necessary
#' @param off_machine A dataframe of laboratory data, directly off the testing machine
#' @param style A character string indicating the laboratory software used to generate the file loaded as off_machine. Currently accepts only "qx_manager"
#' @return A dataframe with 8 columns: Well, Sample, Target, Concentration, CopiesPer20uLWell, AcceptedDroplets, Positives, Negatives
#' @export
#'
software_renaming <- function(off_machine, style = "qx_manager"){

  if (style == "qx_manager"){

    smaller_set <- off_machine %>% select(Well, Sample.description.1, Target, Conc.copies.µL.,
                                     Copies.20µLWell, Accepted.Droplets, Positives, Negatives)

    colnames(smaller_set) <- c("Well", "Sample", "Target", "Concentration", "CopiesPer20uLWell",
                               "AcceptedDroplets", "Positives", "Negatives")

  } else {

    message(paste0("Indicated file style not recognized: ", style))

  }

 return(smaller_set)

}
