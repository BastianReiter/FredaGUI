

# --- MODULE: UnivariateExploration ---

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module UI component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
Mod.UnivariateExploration.UI <- function(id)
#-------------------------------------------------------------------------------
{
  ns <- NS(id)

  div(class = "ui scrollable segment",
      style = "height: 100%;
               padding: 0;
               overflow: auto;",

      # div(class = "ui top attached label",
      #     "Univariate Exploration"),

      div(style = "display: grid;
                   height: 100%;
                   grid-template-areas: 'head head'
                                        'options1 featureinfo'
                                        'options2 statistics';
                   grid-template-rows: 3em 14em auto;
                   grid-template-columns: 1fr 4fr;",


          # Head row
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          div(style = "grid-area: head;
                       display: flex;
                       justify-content: space-between;
                       align-items: center;
                       background-color: lightgrey;",

              uiOutput(ns("SelectionLabel")),

              div(shiny.semantic::toggle(input_id = ns("AutoUpdate"),
                                         label = "Auto-update",
                                         is_marked = TRUE),

                  span(style = "display: inline-block;
                                width: 1em;"),

                  shiny.semantic::action_button(input_id = ns("UpdateButton"),
                                                class = "ui icon button",
                                                label = shiny.semantic::icon(class = "sync alternate")))),


          # Options concerning feature selection
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          # div(style = "grid-area: options1;
          #              padding: 10px;",
          #
          #     selectInput(inputId = ns("SelectTable"),
          #                 label = "Table",
          #                 choices = ""),
          #
          #     br(),
          #
          #     selectInput(inputId = ns("SelectFeature"),
          #                 label = "Feature",
          #                 choices = "")),


          # Options concerning statistics display
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          div(style = "grid-area: options2;
                       padding: 10px;",

              shiny.semantic::slider_input(input_id = ns("SliderMaxNumberCategories"),
                                           value = 6,
                                           min = 1,
                                           max = 10,
                                           step = 1)),

          # Feature Info row
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          div(style = "grid-area: featureinfo;
                       padding: 10px;",

              div(style = "display: grid;
                           grid-template-columns: auto auto;
                           grid-gap: 1em;",

                  DT::DTOutput(ns("FeatureInfoTable")))),

                  # plotOutput(ns("FeatureInfoPlot"),
                  #            width = "80%"))),


          # Statistics Row
          #~~~~~~~~~~~~~~~
          div(style = "grid-area: statistics;
                       padding: 10px;",

              div(style = "display: grid;
                           grid-template-rows: auto auto;
                           grid-gap: 2em;",

                  DT::DTOutput(ns("StatisticsTable")),

                  plotOutput(ns("StatisticsPlot"),
                             width = "80%")))))
              #textOutput(outputId = ns("TestMonitor")))))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module server component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
Mod.UnivariateExploration.Server <- function(id,
                                             ObjectSelection)
#-------------------------------------------------------------------------------
{
  moduleServer(id,
               function(input, output, session)
               {
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  # Setting up loading behavior
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  ns <- session$ns
                  WaiterScreen <- CreateWaiterScreen(ID = ns("WaiterScreenContainer"))

                  LoadingOn <- function()
                  {
                      shinyjs::disable("AutoUpdate")
                      shinyjs::disable("UpdateButton")
                      WaiterScreen$show()
                  }

                  LoadingOff <- function()
                  {
                      shinyjs::enable("AutoUpdate")
                      shinyjs::enable("UpdateButton")
                      WaiterScreen$hide()
                  }
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


                  # output$TestMonitor <- renderText({
                  #                                     #req(session$userData$ServerWorkspaceInfo())
                  #                                     #paste(names(session$userData$ServerWorkspaceInfo()), collapse = ", ")
                  #                                     # paste(
                  #                                     #   paste(names(session$userData$ExplorationData), collapse = ", "),
                  #                                     #   paste(names(session$userData$ExplorationData[[ObjectSelection$Object()]]), collapse = ", "),
                  #                                     #   paste(names(session$userData$ExplorationData[[ObjectSelection$Object()]][[ObjectSelection$Element()]]), collapse = ", "),
                  #                                     #   collapse = "   |   ")
                  #
                  #                                     # paste(paste(names(FeatureExplorationData()), collapse = ", "),
                  #                                     #       paste(class(FeatureExplorationData()), collapse = ", "),
                  #                                     #       #class(FeatureExplorationData()[["FeatureInfo"]]),
                  #                                     #       collapse = "; ")
                  #
                  #
                  #
                  #                                     # paste(!is.null(session$userData$ExplorationData),
                  #                                     #       !is.null(session$userData$ExplorationData[[ObjectSelection$Object()]]),
                  #                                     #       !is.null(session$userData$ExplorationData[[ObjectSelection$Object()]][[ObjectSelection$Element()]]),
                  #                                     #       collapse = ", ")
                  #
                  #                                  })


                  # Create reactive expression that returns an expression triggering output rendering (by being bound to various expressions below)
                  TriggerUpdate <- reactive({ if (input$AutoUpdate == TRUE)
                                              {
                                                  list(ObjectSelection$Element(),
                                                       input$SliderMaxNumberCategories)
                                              } else {
                                                  input$UpdateButton
                                              } })


                  # Render selection info label (header)
                  output$SelectionLabel <- renderUI({ req(ObjectSelection)
                                                      req(FeatureType())

                                                      span(style = "margin: auto 10px;
                                                                    font-weight: bold;",
                                                           "Selection: ", ObjectSelection$Element(),
                                                           " (", FeatureType(), ")",
                                                           " from ", ObjectSelection$Object()) }) %>% bindEvent({ TriggerUpdate() })


                  # If selected Object is a data.frame or tibble, run dsFredaClient::ExploreFeature, which returns a list with elements 'FeatureInfo' and 'Statistics'
                  FeatureExplorationData <- reactive({  req(ObjectSelection)

                                                        CurrentExplorationData <- NULL

                                                        # Check if there is pre-collected exploration data (as used in ServerExplorer Widget)
                                                        if (!is.null(session$userData$ExplorationData) &&
                                                              !is.null(session$userData$ExplorationData[[ObjectSelection$Object()]]) &&
                                                              !is.null(session$userData$ExplorationData[[ObjectSelection$Object()]][[ObjectSelection$Element()]]))
                                                        {
                                                            CurrentExplorationData <- session$userData$ExplorationData[[ObjectSelection$Object()]][[ObjectSelection$Element()]]
                                                            #print(paste("Ping: ", names(session$userData$ExplorationData[[ObjectSelection$Object()]][[ObjectSelection$Element()]]), collapse = ", "))

                                                        } else {

                                                          req(session$userData$DSConnections())

                                                          # Get meta data of table object
                                                          TableMetaData <- SafeDS(ds.GetObjectMetaData(ObjectName = ObjectSelection$Object(),
                                                                                                       DSConnections = session$userData$DSConnections()))

                                                          # Handle possible DataSHIELD error ...
                                                          if (inherits(TableMetaData, "dsFail")) { ShowDSError(); return(NULL) }

                                                          # Check if selected object is a tibble / data.frame. If not, return NULL,
                                                          if (TableMetaData$FirstEligible$Class == "data.frame")
                                                          {
                                                              # Returns a list with elements 'FeatureInfo' and 'Statistics'
                                                              CurrentExplorationData <- SafeDS(dsFredaClient::ExploreFeature(TableName = ObjectSelection$Object(),
                                                                                                                             FeatureName = ObjectSelection$Element(),
                                                                                                                             DSConnections = session$userData$DSConnections()))

                                                              # Handle possible DataSHIELD error ...
                                                              if (inherits(CurrentExplorationData, "dsFail")) { ShowDSError(); return(NULL) }
                                                          }
                                                        }

                                                        CurrentExplorationData

                                                      }) %>% bindEvent({ TriggerUpdate() })


                  # Reactive expression containing type of selected feature ('character', 'numeric', ...)
                  FeatureType <- reactive({ req(FeatureExplorationData())
                                            filter(FeatureExplorationData()$FeatureInfo, Server == "All")$DataType })

                  Statistics <- reactive({  req(FeatureExplorationData())
                                            FeatureExplorationData()$Statistics })


                  # Render table containing info about data availability in selected feature
                  output$FeatureInfoTable <- DT::renderDT({  req(FeatureExplorationData())

                                                             # Assign loading behavior
                                                             LoadingOn()
                                                             on.exit(LoadingOff())

                                                             # Restructure table data for table displaying purposes ('Count (Proportion %)')
                                                             TableData <- FeatureExplorationData()$FeatureInfo %>%
                                                                              mutate("N Valid" = paste0(N.Valid, " (", round(ValidProportion * 100, 0), "%)"),
                                                                                     "N Missing" = paste0(N.Missing, " (", round(MissingProportion * 100, 0), "%)"),
                                                                                     .after = N.Total) %>%
                                                                              select(-N.Valid,
                                                                                     -ValidProportion,
                                                                                     -N.Missing,
                                                                                     -MissingProportion) %>%
                                                                              rename("N Total" = N.Total)

                                                             # Create table
                                                             DT::datatable(data = TableData,
                                                                           class = "ui small very compact scrollable table",
                                                                           editable = FALSE,
                                                                           filter = "none",
                                                                           options = list(info = FALSE,
                                                                                          ordering = FALSE,
                                                                                          paging = FALSE,
                                                                                          searching = FALSE,
                                                                                          layout = list(top = NULL)),
                                                                           rownames = FALSE,
                                                                           selection = list(mode = "none"),
                                                                           style = "semanticui")

                                                           }) %>% bindEvent({ TriggerUpdate() })





                  output$StatisticsTable <- DT::renderDT({  req(FeatureExplorationData)

                                                            # Assign loading behavior
                                                            LoadingOn()
                                                            on.exit(LoadingOff())

                                                            TableData <- data.frame()

                                                            if (FeatureType() %in% c("double", "integer", "numeric"))
                                                            {
                                                                TableData <- Statistics()
                                                            }

                                                            if (FeatureType() %in% c("character", "logical"))
                                                            {
                                                                # Format relative frequency values as percentages in brackets for table displaying purposes
                                                                RelativeFrequencies <- Statistics()$RelativeFrequencies %>%
                                                                                            mutate(across(-Server, ~ paste0("(", round(.x * 100, 0), "%)")))

                                                                # Restructure table data for displaying purposes ('AbsoluteFrequency (RelativeFrequency %)')
                                                                TableData <- Statistics()$AbsoluteFrequencies %>%
                                                                                  mutate(across(everything(), as.character)) %>%
                                                                                  bind_rows(RelativeFrequencies) %>%
                                                                                  group_by(Server) %>%
                                                                                      summarize(across(everything(), ~ paste0(.x, collapse = "  ")))
                                                            }

                                                            Table <- DT::datatable(data = TableData,
                                                                                   class = "ui small very compact scrollable table",
                                                                                   editable = FALSE,
                                                                                   filter = "none",
                                                                                   options = list(#columnDefs = list(list(className = "dt-center", targets = "_all")),
                                                                                                  info = FALSE,
                                                                                                  ordering = FALSE,
                                                                                                  paging = FALSE,
                                                                                                  searching = FALSE,
                                                                                                  layout = list(top = NULL)),
                                                                                   rownames = FALSE,
                                                                                   selection = list(mode = "none"),
                                                                                   style = "semanticui")
                                                                        # DT::formatStyle(columns = names(TableData),
                                                                        #                 textAlign = "center")

                                                            if (FeatureType() %in% c("double", "integer", "numeric"))
                                                            {
                                                                Table <- Table %>%
                                                                            DT::formatRound(names(Statistics())[!(names(Statistics()) %in% c("Server", "N"))],      # Apply formatting to all columns except 'Server' and 'N'
                                                                                            digits = 2)
                                                            }

                                                            return(Table)

                                                         }) %>% bindEvent({ TriggerUpdate() })


                  output$StatisticsPlot <- renderPlot({   req(FeatureType())
                                                          req(Statistics())

                                                          # Assign loading behavior
                                                          LoadingOn()
                                                          on.exit(LoadingOff())

                                                          if (FeatureType() %in% c("double", "integer", "numeric"))
                                                          {
                                                              Plot <- dsFredaClient::MakeBoxPlot(SampleStatistics = Statistics())
                                                          }

                                                          if (FeatureType() %in% c("character", "logical"))
                                                          {
                                                              PlotData <- Statistics()$AbsoluteFrequencies %>%
                                                                              pivot_longer(cols = -Server,
                                                                                           names_to = "Value",
                                                                                           values_to = "AbsoluteFrequency") %>%
                                                                              filter(Server != "All")

                                                              Plot <- dsFredaClient::MakeColumnPlot(DataFrame = PlotData,
                                                                                                    XFeature = Value,
                                                                                                    YFeature = AbsoluteFrequency,
                                                                                                    GroupingFeature = Server)


                                                              # Plot <- plotly::plot_ly(data = as.data.frame(Statistics()$AbsoluteFrequencies),      # Must be a data.frame, not a tibble!
                                                              #                 x = ~Stage,
                                                              #                 y = ~Eligible,
                                                              #                 type = "bar",
                                                              #                 name = "Eligible",
                                                              #                 color = I(dsCCPhosClient::CCPhosColors$Green),
                                                              #                 showlegend = FALSE) %>%
                                                              #             add_trace(y = ~Ineligible,
                                                              #                       name = "Ineligible",
                                                              #                       color = I(dsCCPhosClient::CCPhosColors$Red)) %>%
                                                              #             add_trace(y = ~Missing,
                                                              #                       name = "Missing",
                                                              #                       color = I(dsCCPhosClient::CCPhosColors$MediumGrey)) %>%
                                                              #             layout(font = list(size = 11,
                                                              #                                color = I(dsCCPhosClient::CCPhosColors$DarkGrey)),
                                                              #                    xaxis = list(#side = "top",
                                                              #                                 title = "",
                                                              #                                 categoryorder = "array",
                                                              #                                 categoryarray = c("Source", "Harmonized", "Recoded", "Final")),
                                                              #                    yaxis = list(title = "",
                                                              #                                 showticklabels = FALSE),
                                                              #                    barmode = "stack") %>%
                                                              #             plotly::config(displayModeBar = FALSE)
                                                          }

                                                          return(Plot)

                                                        })


                  # observe({ req(session$userData$DSConnections())
                  #           req(ObjectSelection)
                  #
                  #           LoadingOn()
                  #           on.exit(LoadingOff())
                  #
                  #           # if (input$AutoUpdate == TRUE)
                  #           # {
                  #               SampleStatistics <- dsCCPhosClient::ds.GetSampleStatistics(TableName = ObjectSelection$Object(),
                  #                                                                          MetricFeatureName = ObjectSelection$Element(),
                  #                                                                          DSConnections = session$userData$DSConnections())
                  #
                  #               output$StatisticsTable <- renderUI({ DataFrameToHtmlTable(DataFrame = SampleStatistics,
                  #                                                                         SemanticTableClass = "ui small very compact selectable celled table") })
                  #
                  #               output$StatisticsPlot <- renderPlot({ dsCCPhosClient::MakeBoxPlot(SampleStatistics = SampleStatistics,
                  #                                                                                 AxisTitle_y = "Patient age at diagnosis",
                  #                                                                                 FillPalette = c("All" = CCPhosColors$MediumGrey,
                  #                                                                                                 "ServerA" = CCPhosColors$Primary,
                  #                                                                                                 "ServerB" = CCPhosColors$Secondary,
                  #                                                                                                 "ServerC" = CCPhosColors$Tertiary)) })
                  #           # }
                  #           # else
                  #           # {
                  #           #     output$StatisticsTable <- renderUI({ NULL })
                  #           #     output$StatisticsPlot <- renderUI({ NULL })
                  #           # }
                  #
                  #         }) %>%
                  #     bindEvent({ TriggerUpdate() })

               })
}




