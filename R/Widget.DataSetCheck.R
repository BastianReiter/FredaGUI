
#' Widget.DataSetCheck
#'
#' Launch a Shiny app that facilitates interpretation of data set check reports
#'
#' @param DSConnections \code{list} of \code{DSConnection} objects
#' @param RunAutonomously \code{logical} indicating whether the Shiny app is hosted by a background process (default) available as a URL via web browsers or - if set to \code{FALSE} - is hosted by the current running R session.
#' @param RunInViewer \code{logical} indicating whether the Shiny app should be run in the RStudio Viewer pane (Default: \code{FALSE})
#' @param EndProcessWhenClosingApp \code{logical} indicating whether the background process that runs the Shiny app (if it runs autonomously) should end when the app is closed (default) or should be preserved, in which case the process should be ended manually.
#'
#' @return When 'RunAutonomously' is set to \code{TRUE} this function can return the background process to make it assignable to an R symbol. Otherwise it will run/return a \code{shinyApp} object.
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Widget.DataSetCheck <- function(#--- Arguments for app itself ---
                                SDSCheckData = NULL,
                                RDSCheckData = NULL,
                                EDSCheckData = NULL,
                                DSConnections = NULL,
                                #--- Arguments for app wrapper ---
                                EndProcessWhenClosingApp = TRUE,
                                RunAutonomously = FALSE,
                                RunInViewer = FALSE,
                                ...)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # DSConnections <- CCPConnections
  # ServerWorkspaceInfo <- dsFredaClient::GetServerWorkspaceInfo(DSConnections = DSConnections)

  # --- Argument Validation ---
  assert_that(is.flag(EndProcessWhenClosingApp),
              is.flag(RunAutonomously),
              is.flag(RunInViewer))

  if (!is.null(SDSCheckData)) assert_that(is.list(SDSCheckData))
  if (!is.null(RDSCheckData)) assert_that(is.list(RDSCheckData))
  if (!is.null(EDSCheckData)) assert_that(is.list(EDSCheckData))

#-------------------------------------------------------------------------------

  # If no report data is passed, get it programmatically
  if (is.null(SDSCheckData) & is.null(RDSCheckData) & is.null(EDSCheckData))
  {
      # Check validity of 'DSConnections' or find them programmatically if none are passed
      DSConnections <- CheckDSConnections(DSConnections)

      SDSCheckData <- dsFredaClient::ds.GetDataSetCheck(DataSetName = "CCP.SourceDataSet",
                                                        Module = "CCP",
                                                        Stage = "Source",
                                                        DSConnections = DSConnections)

      RDSCheckData <- dsFredaClient::ds.GetDataSetCheck(DataSetName = "CCP.RefinedDataSet",
                                                        Module = "CCP",
                                                        Stage = "Refined",
                                                        DSConnections = DSConnections)

      EDSCheckData <- dsFredaClient::ds.GetDataSetCheck(DataSetName = "CCP.EnrichedDataSet",
                                                        Module = "CCP",
                                                        Stage = "Enriched",
                                                        DSConnections = DSConnections)
  }

#-------------------------------------------------------------------------------

  # Create the app initiating function (UI and server component resulting in a ShinyApp object)
  InitFunction <- function(...)
  {
    # Since the app is deployed as a package, the folder for external resources (e.g. CSS files, static images) needs to be added manually
    shiny::addResourcePath('www', system.file("www", package = "FredaGUI"))

    #---------------------------------------------------------------------------
    # Widget UI component
    #---------------------------------------------------------------------------
    UI <- function()
    {
        Layout <- function(ns)
        {
            div(style = "display: grid;",

                  div(style = "overflow: auto;
                               width: 100%;",

                      shiny.semantic::tabset(tabs = list(list(menu = "Source Data Set (SDS)",
                                                              content = Mod.DataSetCheck.UI(ns("SDSCheck"))),
                                                         list(menu = "Refined Data Set (RDS)",
                                                              content = Mod.DataSetCheck.UI(ns("RDSCheck"))),
                                                         list(menu = "Enriched Data Set (EDS)",
                                                              content = Mod.DataSetCheck.UI(ns("EDSCheck")))),
                                             id = ns("Tabset"))))
         }

         # Call Widget frame module UI and pass widget-specific UI layout
         Mod.Widget.UI(id = "Widget.DataSetCheck",
                       Title = "FREDA Data Set Check",
                       WidgetMainUI = Layout)
    }

    #---------------------------------------------------------------------------
    # Widget Server Logic
    #---------------------------------------------------------------------------
    Server <- function(input, output, session)
    {
        # Hide waiter loading screen after initial app load has finished
        waiter::waiter_hide()

        # Initialize global objects
        session$userData$SDSCheckData <- reactiveVal(NULL)
        session$userData$RDSCheckData <- reactiveVal(NULL)
        session$userData$EDSCheckData <- reactiveVal(NULL)

        # output$TestMonitor <- renderText({  req(session$userData$ServerWorkspaceInfo())
        #                                     paste0(names(session$userData$ServerWorkspaceInfo()), collapse = ", ") })

        # 'Mod.Initialize' assigns content to session$userData objects at app start
        Mod.Initialize(id = "Initialize",
                       SDSCheckData = SDSCheckData,
                       RDSCheckData = RDSCheckData,
                       EDSCheckData = EDSCheckData)

        #-----------------------------------------------------------------------

        # Define widget-specific server logic that is passed to widget frame module
        WidgetServerLogic <- function(session)
                             {
                                # --- Call modules: Data Set Checks ---
                                Mod.DataSetCheck.Server(id = "SDSCheck", DataSetCheckData = session$userData$SDSCheckData)
                                Mod.DataSetCheck.Server(id = "RDSCheck", DataSetCheckData = session$userData$RDSCheckData)
                                Mod.DataSetCheck.Server(id = "EDSCheck", DataSetCheckData = session$userData$EDSCheckData)
                              }

        # Call Widget frame module and pass widget-specific server logic
        Mod.Widget.Server(id = "Widget.DataSetCheck",
                          WidgetServerLogic)

        #-----------------------------------------------------------------------

        # If the option 'EndProcessWhenClosingApp' is TRUE, the following ensures that the background process is automatically ending when the app shuts down
        if (EndProcessWhenClosingApp == TRUE) { session$onSessionEnded(function() { stopApp() }) }
    }

    # Return Mini-App
    shiny::shinyApp(ui = UI,
                    server = Server)
  }

#-------------------------------------------------------------------------------

  # Either use FredaGUI::RunAutonomousApp() to run the app in a separate background process or run it in the hosting session
  if (RunAutonomously == TRUE)
  {
      RunAutonomousApp(ShinyAppInitFunction = InitFunction,
                       AppArguments = list(...),
                       RunInViewer = RunInViewer)
  } else {

      # This returns the app itself
      InitFunction(...)
  }
}
