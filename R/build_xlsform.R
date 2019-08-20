# Description ------------------------------------------------------------------
# Functions to build an XLSForm survey.
# Created by Ben Ewing on 2019-08-20

# Libraries --------------------------------------------------------------------
library(dplyr)

# Functions --------------------------------------------------------------------
#' Add choices to an XLSForm's choices.
#'
#' @param form An xls_form object.
#' @param list_name A string with the list_name for the new choices.
#' @param name A vector of integers, the names for the choices.
#' @param label A vector of strings, the labels for the choices.
#'   TODO: Label can also be a named list, where the names are a given language,
#'   and the entries are the labels in the corresponding language.
#'
#' @return The form with the choices added
add_choices <- function(form, list_name, name, label) {
  # Validate
  if (length(name) != length(label))
    stop("The length of 'name' does not match the length of 'label'.")

  # Create the dataframe holding the new choices
  n_choices <- length(name)
  new_choices <- data.frame(list_name = rep(list_name, n_choices),
                            name = name,
                            label = label)

  # Append to the form's existing choices
  form$choices <- bind_rows(form$choices, new_choices)

  form
}

#' Add a group to an existing xls_form. A group should be able to act as a
#' mostly self-contained set of questions, i.e. it is its own XLSForm.
#' The settings from the group_form will be ignored.
#'
#' TODO: Sort out possibly conflicting label options, e.g. label::English exists
#' in one, but not the other.
#'
#' @param form The form to add the group too
#' @param group_form The XLSForm representing the group to be added. Note that
#'   the group form doesn't need to include choices, it can draw from the form's
#'   choices.
#'   TODO : If there is overlap between the form and group_forms choices, then
#'     a new set of choices will be created.
#' @param group_name The name of the new group.
#' @param relevant The relevant string.
#'
#' @return The form with the group added.
add_group <- function(form, group_form, group_name, relevant = "") {
  # TODO: Verify that group does not already exist.
  # TODO: Verify that group name isn't the same as a variable.

  # Add to the form's choices, first check for intersecting choices
  overlap <- intersect(unique(form$choices$list_name),
                       unique(group_form$choices$list_name))
  # If there is no overlap, we can just add the choices right away.
  if (length(overlap) == 0)
    form$choices <- bind_rows(form$choices, group_form$choices)
  else
    stop("There is overlap between the form and group_form choices.")

  # Add to the form's survey
  # The group is inserted in between these rows
  bookends <- data.frame(
    type = c("begin group", "end group"),
    name = rep(group_name, 2),
    relevant = c(relevant, "")
  )

  form$survey <- bind_rows(bookends[1, ], group_form, bookends[2, ])

  return(form)
}

#' Add a repeat group to an existing xls_form. See ?add_group for details.
#'
#' @param form The form to add the group too
#' @param group_form The XLSForm representing the group to be added. Note that
#'   the group form doesn't need to include choices, it can draw from the form's
#'   choices.
#'   TODO : If there is overlap between the form and group_forms choices, then
#'     a new set of choices will be created.
#' @param group_name The name of the new group.
#' @param repeat_count Either an integer, or a valid string.
#' @param relevant The relevant string.
#'
#' @return The form with the group added.
add_repeat_group <- function(form, group_form, group_name, repeat_count,
                             relevant = "") {
  # Add as a group, as normal
  # I'm using this here so that I don't have to repeat the choices code
  form <- add_group(form, group_form, group_name, relevant)

  # Replace begin and end group with begin and end repeat
  form[form$type == 'begin group' & form$name == group_name,
       'type'] <- "begin repeat"
  form[form$type == 'end group' & form$name == group_name,
       'type'] <- "end repeat"

  # Add the repeat_count
  form[form$type == 'begin repeat' & form$name == group_name,
       'repeat_count'] <- repeat_count

  # Return
  return(form)
}

#' Add an integer question to the form.
#'
#' @param form The XLSForm to be added to.
#' @param name The name of the variable.
#' @param label The question label
#' @param constraint A string containing a valid constraint.
#' @param relevant A string containing a valid relevant.
#'
#' @return The form with the new question.
add_integer <- function(form, name, label, constraint = "", relevant = "") {
  if (name %in% form$survey$name)
    stop(paste0("Variable ", name, " already exists in form."))

  # The new question row
  new_q <- data.frame(type = "integer", name = name, label = label,
                      constraint = constraint, relevant = relevant)

  # Add to the survey
  form$survey <- bind_rows(form$survey, new_q)

  return(form)
}

#' Add an decimal question to the form.
#'
#' @param form The XLSForm to be added to.
#' @param name The name of the variable.
#' @param label The question label
#' @param constraint A string containing a valid constraint.
#' @param relevant A string containing a valid relevant.
#'
#' @return The form with the new question.
add_decimal <- function(form, name, label, constraint = "", relevant = "") {
  if (name %in% form$survey$name)
    stop(paste0("Variable ", name, " already exists in form."))

  # The new question row
  new_q <- data.frame(type = "decimal", name = name, label = label,
                      constraint = constraint, relevant = relevant)

  # Add to the survey
  form$survey <- bind_rows(form$survey, new_q)

  return(form)
}

# add_range
#   TODO: Range input (including rating)
# add_text
#   TODO: Free text response.
# add_select_one
#   TODO: [options] 	Multiple choice question; only one answer can be selected.
# add_select_multiple
#   TODO: [options] 	Multiple choice question; multiple answers can be selected.
# add_rank
#   TODO: [options] 	Rank question; order a list.
# add_note
#   TODO: Display a note on the screen, takes no input.
# add_geopoint
#   TODO: Collect a single GPS coordinate.
# add_geotrace
#   TODO: Record a line of two or more GPS coordinates.
# add_geoshape
#   TODO: Record a polygon of multiple GPS coordinates; the last point is the same as the first point.
# add_date
#   TODO: Date input.
# add_time
#   TODO: Time input.
# add_dateTime
#   TODO: Accepts a date and a time input.
# add_image
#   TODO: Take a picture or upload an image file.
# add_audio
#   TODO: Take an audio recording or upload an audio file.
# add_video
#   TODO: Take a video recording or upload a video file.
# add_file
#   TODO: Generic file input (txt, pdf, xls, xlsx, doc, docx, rtf, zip)
# add_barcode
#   TODO: Scan a barcode, requires the barcode scanner app to be installed.
# add_calculate
#   TODO: Perform a calculation; see the Calculation section below.
# add_acknowledge
#   TODO: Acknowledge prompt that sets value to “OK” if selected.
# add_hidden
#   TODO: A field with no associated UI element
# add_xml
#   TODO: -external 	Adds a reference to an external XML data file
