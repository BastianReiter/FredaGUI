

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

      shiny.semantic::action_button(ns("ToggleNarrowView"),
                                    label = "Show all stages"),

      h4(class = "ui dividing header"),

      div(id = ns("CounterTableContainer"),
          reactableOutput(outputId = ns("CounterTable"))))
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

                  ReactableTable <- reactive({  req(session$userData$CurationReport())

                                                # Assign loading behavior
                                                WaiterScreen$show()
                                                on.exit(WaiterScreen$hide())

                                                CreateTable.Counter(CounterData = session$userData$CurationReport()$Counter)
                                            })

                  output$CounterTable <- renderReactable({  req(session$userData$CurationReport())
                                                            req(ReactableTable())

                                                            return(ReactableTable())
                                                         })

                  # observe({ shinyjs::toggleClass(id = "Widget.CurationReport-Report.Counter-CounterTableContainer", class = "HideColumns")}) %>%
                  #     bindEvent(input$ToggleNarrowView)


                  ShowStageColumns <- reactiveVal(TRUE)



                  observe({ ShowStageColumns(!ShowStageColumns())

                            Columns.AlwaysHidden <- c("PrimaryTableCleaning.CountRecords.Change.Proportion",
                                                      "PrimaryTableCleaning.CountRootSubjects.Change.Proportion",
                                                      "PrimaryTableCleaning.CountSeedSubjects.Change.Proportion",
                                                      "TableNormalization.CountRecords.Change.Proportion",
                                                      "TableNormalization.CountRootSubjects.Change.Proportion",
                                                      "TableNormalization.CountSeedSubjects.Change.Proportion",
                                                      "SecondaryTableCleaning.CountRecords.Change.Proportion",
                                                      "SecondaryTableCleaning.CountRootSubjects.Change.Proportion",
                                                      "SecondaryTableCleaning.CountSeedSubjects.Change.Proportion",
                                                      "RecordSubsumption.CountRecords.Change.Proportion",
                                                      "RecordSubsumption.CountRootSubjects.Change.Proportion",
                                                      "RecordSubsumption.CountSeedSubjects.Change.Proportion")

                            Columns.ToggleVisibility <- c("PrimaryTableCleaning.CountRecords.Change",
                                                          "PrimaryTableCleaning.CountRootSubjects.Change",
                                                          "PrimaryTableCleaning.CountSeedSubjects.Change",
                                                          "TableNormalization.CountRecords.Change",
                                                          "TableNormalization.CountRootSubjects.Change",
                                                          "TableNormalization.CountSeedSubjects.Change",
                                                          "SecondaryTableCleaning.CountRecords.Change",
                                                          "SecondaryTableCleaning.CountRootSubjects.Change",
                                                          "SecondaryTableCleaning.CountSeedSubjects.Change",
                                                          "RecordSubsumption.CountRecords.Change",
                                                          "RecordSubsumption.CountRootSubjects.Change",
                                                          "RecordSubsumption.CountSeedSubjects.Change")


                            shinyjs::runjs(sprintf("Reactable.setHiddenColumns('Widget.CurationReport-Report.Counter-CounterTable', %s)",
                                                   if (ShowStageColumns() == FALSE)
                                                   {
                                                      paste0("[", paste0("'", c(Columns.AlwaysHidden, Columns.ToggleVisibility), "'", collapse = ", "), "]")
                                                   } else {
                                                      paste0("[", paste0("'", Columns.AlwaysHidden, "'", collapse = ", "), "]")
                                                   }))
                          }) %>%
                      bindEvent(input$ToggleNarrowView)

               })
}

