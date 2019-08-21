# Description ------------------------------------------------------------------
# Function to read an XLSForm from file.
# Created by Ben Ewing on 2019-08-20

# Libraries --------------------------------------------------------------------
library(readxl)

# Functions --------------------------------------------------------------------

#' Read an XLSForm from a file. The survey must have survey and choices sheets
#' but may omit the settings sheet. Other sheets will be ignored.
#'
#' @param file The filepath.
#'
#' @return An xlss_form object.
read_xls_form <- function(file) {
  # Validate
  sheets <- excel_sheets(file)
  if (!("survey" %in% sheets)) {
    stop("'survey' sheet is not present")
  }
  if (!("choices" %in% sheets)) {
    stop("'choices' sheet is not present")
  }

  # Read sheets
  survey <- read_excel(file, "survey")
  choices <- read_excel(file, "choices")
  # Only read settings if it exists; it is not strictly required
  if ("settings" %in% sheets) {
    settings <- read_excel(file, "settings")
  } else {
    settings <- NULL
  }

  # Create and return the object
  xls_form(survey, choices, settings)
}
