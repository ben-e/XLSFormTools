# Description ------------------------------------------------------------------
# Function to navigate an XLSForm survey with Shiny.
# Created by Ben Ewing on 2019-08-20

# Libraries --------------------------------------------------------------------
library(shiny)
library(dplyr)

# navigate ---------------------------------------------------------------------
navigate_xlsform <- function(form) {
  # UI -------------------------------------------------------------------------
  ui <- fluidPage(
    titlePanel(form$settings$form_title),

    # Question text
    textOutput("current_question"),

    # Navigation buttons
    actionButton("prevButton", "Previous"),
    actionButton("nextButton", "Next")
  )

  # Server ---------------------------------------------------------------------
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
      paste0(form$survey$type[i()],
             " - ",
             form$survey$label[i()])
    })
  }

  # Serve ----------------------------------------------------------------------
  shinyApp(ui, server)
}
