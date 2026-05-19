
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
                                RDSCheckData = NULL,
                                CDSCheckData = NULL,
                                ADSCheckData = NULL,
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

  if (!is.null(RDSCheckData)) assert_that(is.list(RDSCheckData))
  if (!is.null(CDSCheckData)) assert_that(is.list(CDSCheckData))
  if (!is.null(ADSCheckData)) assert_that(is.list(ADSCheckData))

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # If no monitor data is passed, get it programmatically
  # if (is.null(RDSCheckData) && EnableLiveConnection == FALSE) { RDSCheckData <- dsFredaClient::ds.GetDataSetCheck(DataSetName = "CCP.RawDataSet",
  #                                                                                                                 Module = "CCP",
  #                                                                                                                 Stage = "Raw",
  #                                                                                                                 DSConnections = DSConnections) }
  # if (is.null(CDSCheckData) && EnableLiveConnection == FALSE) { CDSCheckData <- dsFredaClient::ds.GetDataSetCheck(DataSetName = "CCP.CuratedDataSet",
  #                                                                                                                 Module = "CCP",
  #                                                                                                                 Stage = "Curated",
  #                                                                                                                 DSConnections = DSConnections) }
  # if (is.null(ADSCheckData) && EnableLiveConnection == FALSE) { ADSCheckData <- dsFredaClient::ds.GetDataSetCheck(DataSetName = "CCP.AugmentedDataSet",
  #                                                                                                                 Module = "CCP",
  #                                                                                                                 Stage = "Augmented",
  #                                                                                                                 DSConnections = DSConnections) }
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

                      shiny.semantic::tabset(tabs = list(list(menu = "Raw Data Set (RDS)",
                                                              content = Mod.DataSetCheck.UI(ns("RDSCheck"))),
                                                         list(menu = "Curated Data Set (CDS)",
                                                              content = Mod.DataSetCheck.UI(ns("CDSCheck"))),
                                                         list(menu = "Augmented Data Set (ADS)",
                                                              content = Mod.DataSetCheck.UI(ns("ADSCheck")))),
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
        session$userData$RDSCheckData <- reactiveVal(NULL)
        session$userData$CDSCheckData <- reactiveVal(NULL)
        session$userData$ADSCheckData <- reactiveVal(NULL)

        # output$TestMonitor <- renderText({  req(session$userData$ServerWorkspaceInfo())
        #                                     paste0(names(session$userData$ServerWorkspaceInfo()), collapse = ", ") })

        # 'Mod.Initialize' assigns content to session$userData objects at app start
        Mod.Initialize(id = "Initialize",
                       RDSCheckData = RDSCheckData,
                       CDSCheckData = CDSCheckData,
                       ADSCheckData = ADSCheckData)

        #-----------------------------------------------------------------------

        # Define widget-specific server logic that is passed to widget frame module
        WidgetServerLogic <- function(session)
                             {
                                # --- Call modules: Data Set Checks ---
                                Mod.DataSetCheck.Server(id = "RDSCheck", DataSetCheckData = session$userData$RDSCheckData)
                                Mod.DataSetCheck.Server(id = "CDSCheck", DataSetCheckData = session$userData$CDSCheckData)
                                Mod.DataSetCheck.Server(id = "ADSCheck", DataSetCheckData = session$userData$ADSCheckData)
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
