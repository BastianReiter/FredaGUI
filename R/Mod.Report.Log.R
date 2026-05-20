

# --- MODULE: Report.Log ---

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module UI component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
Mod.Report.Log.UI <- function(id)
#-------------------------------------------------------------------------------
{
  ns <- NS(id)

  div(id = ns("Container"),
      style = "padding: 1em;
               overflow: auto;
               min-height: 10em;",

      selectInput(inputId = ns("ServerName"),
                  label = "Select Server",
                  choices = ""),

      h4(class = "ui dividing header"),

      reactableOutput(outputId = ns("LogTable")))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module server component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
Mod.Report.Log.Server <- function(id)
#-------------------------------------------------------------------------------
{
  moduleServer(id,
               function(input, output, session)
               {
                  ns <- session$ns

                  # Update selection input choices
                  #-------------------------------------------------------------
                  observe({ updateSelectInput(session = getDefaultReactiveDomain(),
                                              inputId = "ServerName",
                                              choices = names(session$userData$CurationReport() %>% pluck("Log")),
                                              selected = names(session$userData$CurationReport()%>% pluck("Log"))[1])
                          }) %>%
                      bindEvent(session$userData$CurationReport())


                  # Prepare Log data
                  #-------------------------------------------------------------
                  LogData <- reactive({ req(session$userData$CurationReport())
                                        req(input$ServerName)

                                        if (!is.null(session$userData$CurationReport()))
                                        {
                                            session$userData$CurationReport() %>%
                                                pluck("Log", input$ServerName) %>%
                                                filter(ProcessTopic != "COUNT")
                                        }
                                      })

                  # Create output
                  #-------------------------------------------------------------
                  output$LogTable <- renderReactable({  req(LogData())

                                                        reactable::reactable(data = LogData(),
                                                                             pagination = FALSE,
                                                                             defaultColDef = colDef(align = "center", vAlign = "center"),
                                                                             borderless = TRUE,
                                                                             theme = reactableTheme(style = list(fontFamily = "Inter, sans-serif",
                                                                                                                 fontSize = "12px"),
                                                                                                    rowStyle = list(height = "30px")),
                                                                             columns = list(Timestamp = colDef(style = list(color = dsFredaClient::FredaColors$MediumGrey,
                                                                                                                            fontFamily = "Lucida Console"))
                                                                                            ))

                                                    })


                  # output$LogMessages <- renderUI({  req(LogData())
                  #
                  #                                   MessageData <- LogData() %>%
                  #                                                       select(MessageClass, Message) %>%
                  #                                                       tibble::deframe()
                  #
                  #                                   HtmlReturn <- list()
                  #
                  #                                   for (i in 1:length(MessageData))
                  #                                   {
                  #                                      HtmlReturn <- c(HtmlReturn,
                  #                                                           list(MessageToHtml(MessageData[i])))
                  #                                   }
                  #
                  #                                   HtmlReturn
                  #
                  #                                 })


               })
}

