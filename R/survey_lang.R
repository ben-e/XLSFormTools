# Description --------------------------------------------------------------------------------------
# This script implements the `survey` language, a simple embedded DSL for programming XLSForm
# surveys without relying on Excel.
# Reference: https://xlsform.org/en/ref-table/
# Ben Ewing on 2019-12-28

# TODO ---------------------------------------------------------------------------------------------
# [X] Choices
# [ ] Read choices from CSV or script
# [X] Settings
# [X] Labels in multiple languages
# [X] Groups
# [ ] Repeat groups
# [ ] A thorough example

# Libraries ----------------------------------------------------------------------------------------
library(rlang)
library(dplyr)
library(purrr)

# Functions that parse various survey elements -----------------------------------------------------

# Questions types that require choices, i.e. select_one or select_multiple.
choice_q <- function(type) {
  function(choices, name, label,
           hint = "",
           constraint = "", constraint_message = "",
           required = "", required_message = "",
           default = "",
           relevant = "",
           read_only = "",
           calculation = "",
           appearance = "") {

    df <- tibble(type = paste(type, choices),
                 name = name,
                 # label = label,
                 hint = hint,
                 constraint = constraint,
                 constraint_message = constraint_message,
                 required = required,
                 required_message = required_message,
                 default = default,
                 relevant = relevant,
                 read_only = read_only,
                 calculation = calculation,
                 appearance = appearance)
    # Work with labels that may or may not have a language specified
    if (!is.null(names(label))) {
      label_langs <- names(label)
      labels <- bind_cols(
        map(label_langs, ~ tibble(!!paste0("label::", .x) := label[.x]))
      )
      bind_cols(df, labels)
    } else {
      df %>% mutate(label = label)
    }
  }
}

# Meta questions, these are not user facing.
meta_q <- function(type) {
  function(name)
    tibble(type = type, name = name)
}

# Questions that do not take any questions
other_q <- function(type) {
  function(name, label,
           hint = "",
           constraint = "", constraint_message = "",
           required = "", required_message = "",
           default = "",
           relevant = "",
           read_only = "",
           calculation = "",
           appearance = "") {
    df <- tibble(type = type,
                 name = name,
                 # label = label,
                 hint = hint,
                 constraint = constraint,
                 constraint_message = constraint_message,
                 required = required,
                 required_message = required_message,
                 default = default,
                 relevant = relevant,
                 read_only = read_only,
                 calculation = calculation,
                 appearance = appearance)

    # Work with labels that may or may not have a language specified
    if (!is.null(names(label))) {
      label_langs <- names(label)
      labels <- bind_cols(
        map(label_langs, ~ tibble(!!paste0("label::", .x) := label[.x]))
      )
      bind_cols(df, labels)
    } else {
      df %>% mutate(label = label)
    }
  }
}

# Sometimes it's useful to just give a function that returns an empty tibble
empty <- function() {
  tibble()
}

# Define new choices
choice <- function(list_name, ...) {
  dots <- dots_list(...)
  # Check to see if vectors are named, if only some are named there's an issue
  named <- map_lgl(dots, ~ !is.null(names(.x)))
  if (all(named)) {
    label_langs <- map(dots, names) %>%
      unlist() %>%
      unique()
    df <- tibble(type = "choice", list_name = list_name, name = names(dots))
    labels <- bind_cols(
      map(label_langs, ~ tibble(!!paste0("label::", .x) := map_chr(dots, function(.y) .y[.x])))
    )
    bind_cols(df, labels)
  } else if (any(named)) {
    stop("All labels must be either named or unamed.")
  } else {
    tibble(type = "choice", list_name = list_name, name = names(dots), label = unlist(dots))
  }
}
# Define settings
setting <- function(setting, value) {
  setting <- enquo(setting)
  tibble(type = "setting", !!setting := value)
}

# A group
begin_group <- function(name,
                        label,
                        x,
                        hint = "",
                        constraint = "", constraint_message = "",
                        required = "", required_message = "",
                        default = "",
                        relevant = "",
                        read_only = "",
                        calculation = "",
                        appearance = "") {
  expr <- substitute(x)
  parsed_survey <- map(expr, ~ eval(.x, parse_env))
  print(parsed_survey)

  df <- tibble(type = "begin_group", name = name, hint = hint,
               constraint = constraint, constraint_message = constraint_message,
               required = required, required_message = "", default = default,
               relevant = relevant, read_only = read_only, calculation = calculation,
               appearance = appearance)

  # Work with labels that may or may not have a language specified
  if (!is.null(names(label))) {
    label_langs <- names(label)
    labels <- bind_cols(
      map(label_langs, ~ tibble(!!paste0("label::", .x) := label[.x]))
    )
    df <- bind_cols(df, labels)
  } else {
    df <- df %>% mutate(label = label)
  }

  for (i in 1:length(parsed_survey)) {
    if (length(parsed_survey[[i]]) == 0) {
      # Ignore empty rows
      # Check that no settings or choices are set
    } else if (parsed_survey[[i]]$type %in% c("choice", "setting")) {
      stop("Choices and settings can not be set within groups or repeat groups.")
    } else {
      df <- bind_rows(df, parsed_survey[[i]])
    }
  }
  bind_rows(df, tibble(type = "end_group"))
}

# a repeat group
repeat_group <- function(name,
                         label,
                         repeat_count,
                         x,
                         hint = "",
                         constraint = "", constraint_message = "",
                         required = "", required_message = "",
                         default = "",
                         relevant = "",
                         read_only = "",
                         calculation = "",
                         appearance = "") {
  expr <- substitute(x)
  parsed_survey <- map(expr, ~ eval(.x, parse_env))
  print(parsed_survey)

  df <- tibble(type = "begin_repeat", name = name, hint = hint,
               constraint = constraint, constraint_message = constraint_message,
               repeat_count = repeat_count,
               required = required, required_message = "", default = default,
               relevant = relevant, read_only = read_only, calculation = calculation,
               appearance = appearance)

  # Work with labels that may or may not have a language specified
  if (!is.null(names(label))) {
    label_langs <- names(label)
    labels <- bind_cols(
      map(label_langs, ~ tibble(!!paste0("label::", .x) := label[.x]))
    )
    df <- bind_cols(df, labels)
  } else {
    df <- df %>% mutate(label = label)
  }

  for (i in 1:length(parsed_survey)) {
    if (length(parsed_survey[[i]]) == 0) {
      # Ignore empty rows
      # Check that no settings or choices are set
    } else if (parsed_survey[[i]]$type %in% c("choice", "setting")) {
      stop("Choices and settings can not be set within groups or repeat groups.")
    } else {
      df <- bind_rows(df, parsed_survey[[i]])
    }
  }
  bind_rows(df, tibble(type = "end_repeat"))
}

# Define an environment which can parse these functions
parse_list <- list(
  # This is just a hacky way to wrap the whole survey in brackets.
  "{" = empty(),

  # Choices
  "choice" = choice,

  # Settings
  "setting" = setting,

  # Meta questions.
  "start" = meta_q("start"),
  "end" = meta_q("end"),
  "today" = meta_q("today"),
  "deviceid" = meta_q("deviceid"),

  # Choice questions
  "select_one" = choice_q("select_one"),
  "select_multiple" = choice_q("select_multiple"),

  # Other questions
  "text" = other_q("text"),
  "int" = other_q("integer"),
  "decimal" = other_q("decimal"),
  "date" = other_q("date"),
  "time" = other_q("time"),
  "datetime" = other_q("datetime"),
  "geopoint" = other_q("geopoint"),
  "image" = other_q("image"),
  "audio" = other_q("audio"),
  "video" = other_q("video"),
  "note" = other_q("note"),
  "acknowledge" = other_q("acknowledge"),
  "calculate" = other_q("calculate"),
  "geotrace" = other_q("geotrace"),
  "geoshape" = other_q("geoshape")
)
parse_env <- list2env(parse_list)
# Using an emptyenv prevents everything from globalenv being inherited. But allowing everything
# to be inherited allows this to make use of R stuff, so, hmm.
# parse_env <- list2env(parse_list, parent = emptyenv())

# Define a function that passes the user's input to the environment and returns a survey.
to_survey <- function(x) {
  # Parse the user's input.
  expr <- substitute(x)
  parsed_survey <- map(expr, ~ eval(.x, parse_env))

  # Split into different sheets
  survey <- tibble(type = character())
  choices <- tibble(type = character())
  settings <- tibble(type = character())

  for (i in 1:length(parsed_survey)) {
    if (nrow(parsed_survey[[i]]) == 0) {
      # Skip if the row is empty.
    } else if (unique(parsed_survey[[i]]$type) == "choice") {
      choices <- bind_rows(choices, parsed_survey[[i]])
    } else if (unique(parsed_survey[[i]]$type) == "setting") {
      if (nrow(settings) == 0)
        settings <- parsed_survey[[i]]
      else
        settings <- bind_cols(settings, parsed_survey[[i]])
    } else {
      survey <- bind_rows(survey, parsed_survey[[i]])
    }
  }

  # Don't need type in the choices and settings sheets
  choices <- select(choices, -matches("^type"))
  settings <- select(settings, -matches("^type"))

  # Return
  list(survey = survey, choices = choices, settings = settings)
}

# An example
to_survey({
  # Settings
  setting(form_title, "example_form")
  setting("version", "1")

  # Define a choice
  choice("yn",
         "0" = c(English = "No", Spanish = "No"),
         "1" = c(English = "Yes", Spanish = "Si"))

  # Setup
  start("start")

  # Section A
  begin_group("section_a", c(English = "Section A", Spanish = "Seccion A"), {
    select_one("yn", "test_yn",
               c(English = "A1. Is this a test?",
                 Spanish = "A1. Es una prueba?"))
    int("integer_test",
        c(English = "A2. Pick an integer:",
          Spanish = "A2. Elige un numero:"))
  })
  note("a_note", c(English = "Section A Over", Spanish = ""))
})
