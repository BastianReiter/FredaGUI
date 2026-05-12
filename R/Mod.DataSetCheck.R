

# --- MODULE: DataSetCheck ---

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module UI component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
Mod.DataSetCheck.UI <- function(id)
#-------------------------------------------------------------------------------
{
  ns <- NS(id)

  #textOutput(outputId = ns("TestMonitor"))

  div(id = ns("Container"),
      style = "padding: 1em;
               min-height: 10em;",

      uiOutput(outputId = ns("TableStatus")),

      div(style = "display: grid;
                   grid-template-columns: 1fr 3fr 1fr;",

          div(),

          div(class = "ui segment",
              style = "margin: 2em;",

              div(class = "ui top attached label",
                  "Table Details"),

              uiOutput(outputId = ns("TableDetails"))),

          div()))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module server component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
Mod.DataSetCheck.Server <- function(id,
                                    DataSetCheckData)   # reactive
#-------------------------------------------------------------------------------
{
  moduleServer(id,
               function(input, output, session)
               {
                  ns <- session$ns

                  # output$TestMonitor <- renderText({  req(DataSetCheckData)
                  #
                  #                                     paste0(names(DataSetCheckData()$TableStatus), collapse = " / ")
                  #                                 })

                  #-------------------------------------------------------------
                  # Render reactive output: Table status overview
                  #-------------------------------------------------------------
                  output$TableStatus <- renderUI({  req(DataSetCheckData())

                                                    # Modify table data
                                                    TableStatusData <- DataSetCheckData()$TableStatus %>%
                                                                            select(-CheckSummary)

                                                    # For HTML table, create vector of column labels with shortened table names to save some horizontal space ()
                                                    ColumnLabels <- str_remove(colnames(TableStatusData), "^.*\\.(?:RDS|CDS|ADS)\\.") %>% set_names(colnames(TableStatusData))
                                                    ColumnLabels[1] <- "Server"

                                                    if (!is.null(TableStatusData))
                                                    {
                                                       DataFrameToHtmlTable(DataFrame = TableStatusData,
                                                                            ColContentHorizontalAlign = "center",
                                                                            ColumnLabels = ColumnLabels,
                                                                            ColumnMaxWidth = 14,
                                                                            SemanticTableCSSClass = "ui small compact celled structured table",
                                                                            TurnColorValuesIntoDots = TRUE)
                                                    }
                                                })


                  #-------------------------------------------------------------
                  # Render reactive output: Table details
                  #-------------------------------------------------------------

                  # Dynamically create empty UI elements
                  output$TableDetails <- renderUI({   req(DataSetCheckData())

                                                      TableNames <- names(DataSetCheckData()$FeatureExistence)

                                                      # Using for-loop instead of purrr-functionality because there is no map-function that can access both names and index of list items
                                                      lapply(TableNames, function(tablename)
                                                      {
                                                          tagList(div(style = "margin-top: 1em;",
                                                                      div(class = "ui small grey ribbon label",
                                                                          tablename),
                                                                      div(style = "width: 1000px; overflow: auto;",
                                                                          uiOutput(outputId = ns(paste0("TableDetails.", tablename))))))
                                                      })
                                                  })

                  # Dynamically assign HTML content to UI elements created above
                  observe({   req(DataSetCheckData())

                              # Process data from 'DataSetCheckData()' to get a list of data.frames (one per data set table) that contain detailed table info
                              TableList <- DataSetCheckData()[c("TableRecordCounts",
                                                                "FeatureExistence",
                                                                "FeatureTypes",
                                                                "NonMissingValueRates")] %>%
                                              list_transpose(simplify = FALSE) %>%
                                              map(\(TableData) CreateTable.TableCheck(TableData))      # Call custom function 'CreateTable.TableCheck()'

                              # Turn prepared data.frames into HTML table code
                              HTMLTables <- TableList %>%
                                                map(function(TableData)
                                                    {
                                                        DataFrameToHtmlTable(DataFrame = TableData$TableDetails,
                                                                             ColContentHorizontalAlign = "center",
                                                                             ColumnLabels = c(ServerName = "Server"),
                                                                             HeaderColspans = TableData$HeaderColspans,
                                                                             SemanticTableCSSClass = "ui small compact inverted scrollable structured table",
                                                                             TurnLogicalsIntoIcons = TRUE,
                                                                             TurnNAsIntoBlanks = TRUE)
                                                    })

                              # Note: Using lapply() instead of for-loop and using local() is essential in dynamic assignment of UI output
                              lapply(names(HTMLTables), function(tablename)
                              {
                                  local({   output[[paste0("TableDetails.", tablename)]] <- renderUI({ HTMLTables[[tablename]] }) })
                              })
                          })
               })
}
