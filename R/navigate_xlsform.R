# Description ------------------------------------------------------------------
# Function to navigate an XLSForm survey with Shiny.
# Created by Ben Ewing on 2019-08-20

# Libraries --------------------------------------------------------------------
library(shiny)
library(dplyr)
library(purrr)

# Testing ----------------------------------------------------------------------
# Delete after
survey <- readxl::read_excel("../../../Downloads/TGCC_HP_v1.xlsx", "survey")
survey$label <- survey$`label::English`
choices <- readxl::read_excel("../../../Downloads/TGCC_HP_v1.xlsx", "choices")
test <- list(survey = survey, choices = choices)

# Helpers ----------------------------------------------------------------------

# UI ---------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Survey Title"),

  # Question text
  textOutput("current_question"),

  # Navigation buttons
  actionButton("prevButton", "Previous"),
  actionButton("nextButton", "Next")
)

# Server -----------------------------------------------------------------------
server <- function(input, output) {
  # Question counter - just counts the current row of the XLSForm
  i <- reactiveVal(1)

  observeEvent(input$nextButton, {
    new_i <- i() + 1
    i(new_i)
  })

  observeEvent(input$prevButton, {
    new_i <- i() - 1
    i(new_i)
  })

  output$current_question <- reactive({
    paste0(survey$type[i()],
           " - ",
           survey$label[i()])
  })
}

# Test -------------------------------------------------------------------------
shinyApp(ui, server)
