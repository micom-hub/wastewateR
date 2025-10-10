
#' Basic Description: Edit Sample and Target names to remove spaces and change letter-type characters to uppercase.
#'
#' This function takes in a data frame and cleans up sample and target names for consistency. The function does assume the sample names are in a column called `Sample`, and that the target names are in a column called `Target`. It does not create any new columns, it only edits the contents of existing columns. A single dataframe is returned.
#' 1.  Removes all spaces from strings in `Sample` and `Target` (leading, lagging, and interior)
#' 2.  Changes all letter characters in `Sample` and `Target` to capitalized / uppercase letters.
#'
#' @param df_in Data frame of laboratory data, needs to have a column called "Sample" and a column called "Target"
#' @return A data frame with (possibly) edited sample names
#' @export

w0110_sample_name_edits <- function(df_in){

################################################################################
# Check #1.1: Edit Sample Names as necessary

    message("CHECK #1.1: Sample Name Edits for Consistency")

    # remove interior spaces, change BCOV to BCoV, make sure all other sample names
    # have letters that are all capitals
    df_in <- df_in %>% mutate(Sample = gsub(" ", "", trimws(Sample))) # removes interior spaces from all sample names
    # leading and lagging whitespace also removed

    # maintains the BCoV change, and turns all other sample letters to uppercase, if they aren't already
    df_in <- df_in %>% mutate(Sample = toupper(Sample))

    message("Target Name Edits for Consistency")

    df_in <- df_in %>% mutate(Target = gsub(" ", "", trimws(Target)))

    df_in <- df_in %>% mutate(Target = toupper(Target))

    message("Through Check #1.1")

    return(df_in)
################################################################################
}
