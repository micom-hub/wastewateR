
#' Check Sample Naming Structure - SEWER
#'
#' This function was specifically crafted to accommodate the sample naming
#' convention of the SEWER program in Michigan. If you'd like a sample naming
#' function created for your laboratory/program, please reach out to the owner
#' of this repository.
#'
#' This function takes in a data frame. It also takes in a vector of character strings for
#' acceptable site identifiers, and a vector of
#' control strings. It does not edit the input dataframe -- input and output dataframes are identical
#' in this case.
#'
#' The vector of character strings for submitter ids and the vector of acceptable site
#' abbreviations should align with the following sample naming structure:
#'  - there should be 11 characters in the sample name
#'  - submitter identifier, two characters & city/site identifier, two character as one input
#'  - date of sample collection in YYMMDD format
#'  - Sample end indicator: I or S, for influent or solid, or A, for first sample collected in a given day
#'
#' We're working exclusively with the 'Sample' column.
#'
#' In the 'Sample' column, all leading and lagging spaces are removed from
#' each sample name. A new column for the number of characters in each sample
#' name is made, and a new column identifying each row as either "NOT A CONTROL"
#' or "Control" is made.
#'
#' Controls:
#' - Sample name contains any of the character strings indicated
#'
#' Everything else is labelled "NOT A CONTROL". The system then only considers sample names that are labelled "NOT A CONTROL" for the checks outlined here.
#'
#' The system will account for "_2", "_3", or "_4" at the end of a sample
#' name to identify if the same sample is tested multiple times
#' on the same plate, or on different plates. These will run through and
#' will not trigger an "over 11 characters" stop.
#'
#' if stop_choice is "yes":
#' STOP ALERT: All NOT A CONTROL samples that are NOT 11 characters in
#' length are pulled and if there are one or more instances of this,
#' the offending sample names will be printed to the console with "Sample
#' Names are Not 11 Characters". The code will STOP RUNNING if this occurs.
#'
#' Each sample name is then broken down into five new columns, corresponding
#' to the first two characters, the next two characters, the six characters after that,
#' those six characters transformed into an R date data type, and the last character.
#'
#' Individual checks are then run on each of these columns:
#'
#' 1. That the first four characters of non-control rows are an option provided
#'  in the vector of acceptable site identifiers
#' 2. That the next six characters of all non-control rows are numbers, and a
#' date not in the future
#' 3. That the last character of non-control wells is either an I, S, or an A
#'
#' if stop_choice is "yes":
#' STOP ALERT: If any of those three checks do not pass, the offending sample
#'  names will be printed out to the console along with a relevant check
#'  message. The code will STOP RUNNING if this occurs.
#'
#' In addition, the system checks that the sample dates of non-control sample
#'  rows are not older than 6 months ago. This check only notifies if any of them are,
#'  it is not a stop check.
#'
#' This is an example of a function that could be replaced with another function
#' that applied the sample naming rules that different individuals/organizations use.
#'
#' @param df_in A dataframe of laboratory data, must contain a column called 'Sample'
#' @param site_identifiers A vector of four-character character strings identifying the submitter laboratory code(s) and site ids
#' @param control_strs A vector of character strings contained in control sample names
#' @param stop_choice A character string of "yes" or "no" to indicate whether this should be a hard stop function or not
#' @return A list with the first element being a dataframe that is identical to the input dataframe, and the second element being either 0 (for no failure stop) or 1 (for failure stop)
#' @export

w0150_sample_naming_structure <- function(df_in, site_identifiers, control_strs, stop_choice = "no"){

    ################################################################################
    # Check #1.5: Look at sample naming structure, as well as date information

    message("CHECK #1.5: Sample Naming Structure")
    message("") # just for visual clarity
    stop_indicator <- 0
    # there should be 11 characters in the sample name
    # submitter identifier | city/site identifier | YYMMDD | I or S or A

    new_file_in <- df_in %>% mutate(Sample = trimws(Sample),
                                    sample_characters = nchar(Sample),
                                    control_check = "NOT A CONTROL")

    for (each_cont in control_strs){

      new_file_in <- new_file_in %>% mutate(control_check = case_when(grepl(each_cont, Sample) ~ "Control",
                                                                      T ~ control_check))

    }

    check_length_count <- filter(new_file_in, sample_characters != 11 & control_check != "Control")

    # if the sample has "_2" or similar at the end of the name, we don't want it to
    # trigger this error.

    check_length_count <- check_length_count %>% mutate(second_check = case_when(grepl("_2", Sample) ~ nchar(gsub("_2", "", Sample)),
                                                                                 grepl("_3", Sample) ~ nchar(gsub("_3", "", Sample)),
                                                                                 grepl("_4", Sample) ~ nchar(gsub("_4", "", Sample)),
                                                                                 T ~ 999))

    check_length_count <- filter(check_length_count, second_check != 11)

    if (nrow(check_length_count >= 1)){

      for (i in unique(check_length_count$Sample)){
        message(i)
      }

      stop_message <- "Sample Names are Not 11 Characters"
      message(stop_message)
      stop_indicator <- 1

    } else {

      message("Sample Names are all 11 characters (or have expected '_#' format).")

    }

    new_file_in <- new_file_in %>% mutate(first_four = substr(Sample, 1, 4),
                                          next_six = substr(Sample, 5, 10),
                                          next_six_date = as.POSIXct(next_six, format = "%y%m%d"),
                                          last_one = substr(Sample, 11, 11))

    message("") # just for visual clarity

    # quick check that all site_identifiers are 4 characters long
    for (i in site_identifiers){
      if (nchar(i) != 4){
        message(i)
        stop("Site identifier is not four characters long.")
      }
    }


    # check that first four characters align with site
    if (any(!filter(new_file_in, control_check == "NOT A CONTROL")$first_four %in% site_identifiers)){

      for (i in unique(filter(new_file_in, control_check == "NOT A CONTROL" & !first_four %in% site_identifiers)$Sample)){
        message(i)
      }

      stop_message <- "First four characters of sample name are not a known site abbreviation."

      message(stop_message)

      stop_indicator <- 1

    } else {
      message("First four characters of non-control sample rows are all known site abbreviations.")
    }

    message("") # just for visual clarity

    # check that next six characters of all non-control rows are numbers, and a date not in the future
    if (any(is.na(filter(new_file_in, control_check == "NOT A CONTROL")$next_six_date))){

      for (i in unique(filter(new_file_in, control_check == "NOT A CONTROL" & is.na(next_six_date))$Sample)){
        message(i)
      }

      stop_message <- "Unable to convert next six characters of non-control sample rows to date type."

      message(stop_message)

      stop_indicator <- 1

    } else {

      message("Next six characters of all non-control sample rows were able to be converted to Date types.")

    }

    message("") # just for visual clarity

    if (any(filter(new_file_in, control_check == "NOT A CONTROL")$next_six_date > Sys.Date())){

      for (i in unique(filter(new_file_in, next_six_date > Sys.Date)$Sample)){
        message(i)
      }

      stop_message <- "Sample dates of non-control sample rows are in the future."

      message(stop_message)

      stop_indicator <- 1

    } else {

      message("Sample dates of non-control sample rows are not future dated.")

    }

    message("") # just for visual clarity

    if (any(filter(new_file_in, control_check == "NOT A CONTROL")$next_six_date < (Sys.Date() %m-% months(6)))){

      for (i in unique(filter(new_file_in, next_six_date < (Sys.Date() %m-% months(6)))$Sample)){
        message(i)
      }

      alert_message <- "Sample dates of non-control sample rows are older than 6 months."
      message(alert_message)

    } else {

      message("Sample dates of non-control sample rows are not older than 6 months ago.")

    }

    message("") # just for visual clarity

    # check that the last character of non-control wells is either an I or an S or an A
    if (any(filter(new_file_in, control_check == "NOT A CONTROL")$last_one != "S" & filter(new_file_in, control_check == "NOT A CONTROL")$last_one != "I"& filter(new_file_in, control_check == "NOT A CONTROL")$last_one != "A")){

      for (i in unique(filter(new_file_in, control_check == "NOT A CONTROL" & !last_one %in% c("I", "S", "A"))$Sample)){
        message(i)
      }

      stop_message <- "Last character of non-control sample name is not 'I', 'S', or 'A'."
      message(stop_message)
      stop_indicator <- 1

    } else {

      message("Last characters of all non-control sample names is either 'I', 'S', or 'A'.")

    }

    message("") # just for visual clarity
    message("Through Check #1.5")

    if (stop_indicator == 1){
      message("Stop error encountered - #1.5")
    }

    if (stop_choice == "yes" & stop_indicator == 1){
      stop()
    }

    return(list(new_file_in, stop_indicator))



}
