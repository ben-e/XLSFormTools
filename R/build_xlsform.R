# Description ------------------------------------------------------------------
# Functions to build an XLSForm survey.
# It's definitely overkill to have a function for each type, but I think it
# makes survey building very explicit.
# Reference: http://xlsform.org/en/
# Created by Ben Ewing on 2019-08-20

# Libraries --------------------------------------------------------------------
library(tibble)
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

  # Create the tibble holding the new choices
  n_choices <- length(name)
  new_choices <- tibble(list_name = rep(list_name, n_choices),
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
  bookends <- tibble(
    type = c("begin group", "end group"),
    name = rep(group_name, 2),
    relevant = c(relevant, "")
  )

  form$survey <- bind_rows(form$survey, bookends[1, ],
                           group_form$survey, bookends[2, ])

  form
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
  bookends <- tibble(
    type = c("begin repeat", "end repeat"),
    name = rep(group_name, 2),
    repeat_count = c(repeat_count, ""),
    relevant = c(relevant, "")
  )

  form$survey <- bind_rows(form$survey, bookends[1, ],
                           group_form$survey, bookends[2, ])

  form
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
  new_q <- tibble(type = "integer", name = name, label = label,
                  constraint = constraint, relevant = relevant)

  # Add to the survey
  form$survey <- bind_rows(form$survey, new_q)

  form
}

#' Add a decimal question to the form.
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
  new_q <- tibble(type = "decimal", name = name, label = label,
                  constraint = constraint, relevant = relevant)

  # Add to the survey
  form$survey <- bind_rows(form$survey, new_q)

  form
}

#' Add a range question to the form.
#'
#' @param form The XLSForm to be added to.
#' @param name The name of the variable.
#' @param label The question label
#' @param parameters The start stop and step string, defaults to
#'   "start=0 end=10 step=1"
#' @param constraint A string containing a valid constraint.
#' @param relevant A string containing a valid relevant.
#'
#' @return The form with the new question.
add_range <- function(form, name, label, parameters = "start=0 end=10 step=1",
                      constraint = "", relevant = "") {
  if (name %in% form$survey$name)
    stop(paste0("Variable ", name, " already exists in form."))

  # The new question row
  new_q <- tibble(type = "range", name = name, label = label,
                  parameters = parameters,
                  constraint = constraint, relevant = relevant)

  # Add to the survey
  form$survey <- bind_rows(form$survey, new_q)

  form
}

#' Add a text free-response question to the form.
#'
#' @param form The XLSForm to be added to.
#' @param name The name of the variable.
#' @param label The question label
#' @param constraint A string containing a valid constraint. Note that regex can
#'   be used here, see https://docs.opendatakit.org/form-regex/.
#' @param relevant A string containing a valid relevant.
#'
#' @return The form with the new question.
add_text <- function(form, name, label, constraint = "", relevant = "") {
  if (name %in% form$survey$name)
    stop(paste0("Variable ", name, " already exists in form."))

  # The new question row
  new_q <- tibble(type = "text", name = name, label = label,
                  constraint = constraint, relevant = relevant)

  # Add to the survey
  form$survey <- bind_rows(form$survey, new_q)

  form
}

#' Add a select_one question to the form.
#'
#' @param form The XLSForm to be added to.
#' @param name The name of the variable.
#' @param choices The list_name of the choices.
#' @param label The question label
#' @param constraint A string containing a valid constraint. Note that regex can
#'   be used here, see https://docs.opendatakit.org/form-regex/.
#' @param relevant A string containing a valid relevant.
#'
#' @return The form with the new question.
add_select_one <- function(form, name, label, choices, constraint = "",
                           relevant = "") {
  if (name %in% form$survey$name)
    stop(paste0("Variable ", name, " already exists in form."))

  # The new question row
  new_q <- tibble(type = paste0("select_one ", choices),
                  name = name, label = label,
                  constraint = constraint, relevant = relevant)

  # Add to the survey
  form$survey <- bind_rows(form$survey, new_q)

  form
}

#' Add a select_multiple question to the form.
#'
#' @param form The XLSForm to be added to.
#' @param name The name of the variable.
#' @param choices The list_name of the choices.
#' @param label The question label
#' @param constraint A string containing a valid constraint. Note that regex can
#'   be used here, see https://docs.opendatakit.org/form-regex/.
#' @param relevant A string containing a valid relevant.
#'
#' @return The form with the new question.
add_select_multiple <- function(form, name, label, choices, constraint = "",
                                relevant = "") {
  if (name %in% form$survey$name)
    stop(paste0("Variable ", name, " already exists in form."))

  # The new question row
  new_q <- tibble(type = paste0("select_multiple ", choices),
                  name = name, label = label,
                  constraint = constraint, relevant = relevant)

  # Add to the survey
  form$survey <- bind_rows(form$survey, new_q)

  form
}

#' Add a rank question to the form.
#'
#' TODO: Randomization parameter?
#'
#' @param form The XLSForm to be added to.
#' @param name The name of the variable.
#' @param choices The list_name of the choices.
#' @param label The question label
#' @param constraint A string containing a valid constraint. Note that regex can
#'   be used here, see https://docs.opendatakit.org/form-regex/.
#' @param relevant A string containing a valid relevant.
#'
#' @return The form with the new question.
add_rank <- function(form, name, label, choices, constraint = "",
                     relevant = "") {
  if (name %in% form$survey$name)
    stop(paste0("Variable ", name, " already exists in form."))

  # The new question row
  new_q <- tibble(type = paste0("rank ", choices),
                  name = name, label = label,
                  constraint = constraint, relevant = relevant)

  # Add to the survey
  form$survey <- bind_rows(form$survey, new_q)

  form
}

#' Add start date/time to form.
#'
#' @param form The form to add to.
#'
#' @return The form with the start time.
add_start <- function(form) {
  start <- tibble(type = "start", name = "start")
  form$survey <- bind_rows(form$survey, start)
  form
}

#' Add end date/time to form.
#'
#' @param form The form to add to.
#'
#' @return The form with the end time.
add_end <- function(form) {
  end <- tibble(type = "end", name = "end")
  form$survey <- bind_rows(form$survey, end)
  form
}

#' Add today's date/time to form.
#'
#' @param form The form to add to.
#'
#' @return The form with day's date.
add_today <- function(form) {
  today <- tibble(type = "today", name = "today")
  form$survey <- bind_rows(form$survey, today)
  form
}

#' Add deviceid to the form. Note that deviceid is just the phone's IMEI:
#' https://en.wikipedia.org/wiki/International_Mobile_Equipment_Identity
#'
#' @param form The form to add to.
#'
#' @return The form with the deviceid.
add_deviceid <- function(form) {
  deviceid <- tibble(type = "deviceid", name = "deviceid")
  form$survey <- bind_rows(form$survey, deviceid)
  form
}

#' Add a note.
#'
#' @param form The form to add to.
#' @param name The note identifier
#' @param label The note
#'
#' @return The form with the note added.
add_note <- function(form, name, label) {
  new_note <- tibble(type = "note", name = name, label = label)
  form$survey <- bind_rows(form$survey, new_note)
  form
}

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
