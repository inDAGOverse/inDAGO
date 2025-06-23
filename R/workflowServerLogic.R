#' Server function for workflow module in Shiny application
#' @param id Shiny module identifier

WorkflowServerLogic <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observeEvent(input$helpWorkflow, {
      rintrojs::introjs(
        session,
        options = list(
          "showBullets" = TRUE,
          "showProgress" = TRUE,
          "showStepNumbers" = FALSE,
          "nextLabel" = "Next",
          "prevLabel" = "Prev",
          "skipLabel" = "Skip"
        )
      )
    })

  })
}
