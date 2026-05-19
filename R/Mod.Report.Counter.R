

# --- MODULE: Report.Counter ---

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module UI component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
Mod.Report.Counter.UI <- function(id)
#-------------------------------------------------------------------------------
{
  ns <- NS(id)

  div(id = ns("Container"),
      style = "padding: 1em;
               overflow: auto;
               min-height: 10em;",

      shiny.semantic::toggle(ns("Tgl.ShowAllStages"),
                             label = "Show detailed stages",
                             is_marked = FALSE),

      h4(class = "ui dividing header"),

      div(id = ns("CounterTableContainer"),
          reactableOutput(outputId = ns("CounterTable")),
          reactableOutput(outputId = ns("CounterTable.WithAllStages"))))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module server component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
Mod.Report.Counter.Server <- function(id)
#-------------------------------------------------------------------------------
{
  moduleServer(id,
               function(input, output, session)
               {
                  ns <- session$ns

                  WaiterScreen <- CreateWaiterScreen(ID = ns("CounterTable"))

                  CounterData <- isolate(session$userData$CurationReport()$Counter)

                  # Toggle button functionality
                  observe({ if (input$Tgl.ShowAllStages == FALSE)
                            {
                                shinyjs::hide(id = "CounterTable.WithAllStages")
                                shinyjs::show(id = "CounterTable")
                            } else {
                                shinyjs::hide(id = "CounterTable")
                                shinyjs::show(id = "CounterTable.WithAllStages")
                            }
                          }) %>%
                      bindEvent(input$Tgl.ShowAllStages)

                  # Main table output
                  output$CounterTable <- renderReactable({ return(CreateTable.Counter(CounterData = CounterData, ShowAllStages = FALSE)) })
                  output$CounterTable.WithAllStages <- renderReactable({ return(CreateTable.Counter(CounterData = CounterData, ShowAllStages = TRUE)) })
               })
}

