

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

      reactableOutput(outputId = ns("NestedView")))
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

                  WaiterScreen <- CreateWaiterScreen(ID = ns("NestedView"))

                  output$NestedView <- renderReactable({  req(session$userData$CurationReport())

                                                          # Assign loading behavior
                                                          WaiterScreen$show()
                                                          on.exit(WaiterScreen$hide())

                                                          ReactableTable <- CreateTable.Counter.NestedView(CounterData = session$userData$CurationReport()$Counter)

                                                          return(ReactableTable)
                                                       })
               })
}


# ui <- fluidPage(
#   titlePanel("Module Template"),
#   Mod.Report.Counter.UI("my_module")
# )
#
# server <- function(input, output, session) {
#   # Pass the fixed local object directly — no reactive() wrapper needed
#   Mod.Report.Counter.Server("my_module", CurationReport = CurationReport)
# }
#
# shinyApp(ui, server)

