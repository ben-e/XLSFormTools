# Description ------------------------------------------------------------------
# Functions to fuzz an XLSForm survey. In this context, fuzzing means entering
# random data to verify that there are no dead ends or unintended paths
# in the survey. The fuzzed data can also be used to pre-write cleaning and
# analysis code.
# Created by Ben Ewing on 2019-08-20

# Libraries --------------------------------------------------------------------
library(tibble)
library(dplyr)
library(purrr)
library(stringr)

# Functions --------------------------------------------------------------------
#' This is a function to fuzz an xls_form. In other words, generate fake data
#' based on the supplied xls form. This allows us to test the form for dead ends
#' or unintended paths. It also allows for the pre-writing of data cleaning and
#' analysis code.
#'
#' This function treats repeat groups as separate XLSForms, and calls them
#' recursively.
#'
#' @param form An xls_form object.
#'
#' @return A data.frame of the fuzzed form.
fuzz_xls_form <- function(form) {
  # Start at the first question, cq = current question
  cq <- 1
  # Get the total number of rows
  last_q <- nrow(form$survey)
  # How many iterations before we declare a path to be a dead end?
  # Maybe this should be a parameter
  dead_end_attempts <- 100

  # The output dataset
  df <- tibble()

  # Set start time, if needed
  # TODO: Make sure time format conforms to standards
  if ("start" %in% form$survey$type)
    df[1, form$survey$name[form$survey$type == 'start']] <- Sys.time()

  # I'm not sure what the best way to navigate the form might be
  # It can't be a map because the survey is self-referential
  # A for loop makes repeat group annoying
  # So a while loop and a question tracker seems logical, but while(True) is
  # kind of scary to write
  while(TRUE) {
    # Get the variable name
    var <- form$survey$name[cq]

    # TODO: Parse relevant before switch (because we might end up just skipping)
    rel <- form$survey$relevant[cq]

    # Handle each question type
    switch(
      survey$form$type[cq],
      # Metadata
      start = {
        # Ignore
      },
      end = {
        # Ignore
      }
      today = {
        # TODO make sure date format comforms to standards
        df[1, var] <- Sys.Date()
      },
      deviceid = {
        df[1, var] <- "FUZZ"
      },
      deviceid = {
        df[1, var] <- "FUZZ"
      },
      subscriberid = {
        df[1, var] <- "FUZZ"
      },
      simserial = {
        df[1, var] <- "FUZZ"
      },
      phonenumber = {
        df[1, var] <- "FUZZ"
      },
      username = {
        df[1, var] <- "FUZZ"
      },
      email = {
        df[1, var] <- "FUZZ"
      },
      # Questions
      integer = {
        # Get constraint
        cons <- form$survey$constraint[cq]
        # If there is no constraint, choose a random integer
        # TODO: Make the integer range a parameter?
        if (cons == "" | is.na(cons))
          df[1, var] <- sample.int(1:1000000, 1)
        else {
          # TODO: Parsing constraints is going to be tricky...
          # https://docs.opendatakit.org/form-operators-functions/
          !!
        }
      },
      # Groups/RepeatGroups
    )
    # On to the next question
    cq <- cq + 1
  }

  # Set end time, if needed
  # TODO: Make sure time format conforms to standards
  if ("end" %in% form$survey$type)
    df[1, form$survey$name[form$survey$type == 'end']] <- Sys.time()

  df
}
