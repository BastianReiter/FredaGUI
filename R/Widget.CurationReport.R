
#' Widget.CurationReport
#'
#' Launch a Shiny app that serves as GUI for Curation Report data.
#'
#' @param CurationReport \code{list}
#' @param DSConnections \code{list} of \code{DSConnection} objects
#' @param DS.async \code{logical} - Value of argument 'async' in \code{DSI::datashield.assign()} / \code{DSI::datashield.aggregate()} - Default: \code{FALSE}
#' @param RunAutonomously \code{logical} indicating whether the Shiny app is hosted by a background process (default) available as a URL via web browsers or - if set to \code{FALSE} - is hosted by the current running R session.
#' @param RunInViewer \code{logical} indicating whether the Shiny app should be run in the RStudio Viewer pane (Default: \code{FALSE})
#' @param EndProcessWhenClosingApp \code{logical} indicating whether the background process that runs the Shiny app (if it runs autonomously) should end when the app is closed (default) or should be preserved, in which case the process should be ended manually.
#'
#' @return If 'RunAutonomously' is set to \code{TRUE} this function can return the background process to make it assignable to an R symbol. Otherwise it will run/return a \code{shinyApp} object.
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Widget.CurationReport <- function(#--- Arguments for app itself ---
                                  Module = "CCP",
                                  CurationReport = NULL,
                                  DSConnections = NULL,
                                  DS.async = FALSE,
                                  #--- Arguments for app wrapper ---
                                  EndProcessWhenClosingApp = TRUE,
                                  RunAutonomously = FALSE,
                                  RunInViewer = FALSE,
                                  ...)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # Module <- "CCP"
  # CurationReport <- CurationReport
  # DSConnections <- CCPConnections
  # DS.async <- FALSE
  # EndProcessWhenClosingApp <- TRUE
  # RunAutonomously <- FALSE
  # RunInViewer <- FALSE

  # --- Argument Validation ---
  assert_that(is.string(Module),
              is.flag(DS.async),
              is.flag(EndProcessWhenClosingApp),
              is.flag(RunAutonomously),
              is.flag(RunInViewer))

  if (!is.null(CurationReport)) assert_that(is.list(CurationReport))

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # If no CurationReport data is passed, get it programmatically
  if (is.null(CurationReport))
  {
      CurationReport <- dsFredaClient::ds.GetCurationReport(Module = Module,
                                                            DSConnections = DSConnections,
                                                            DS.async = DS.async)
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

                      shiny.semantic::tabset(tabs = list(list(menu = "Counter",
                                                              content = Mod.Report.Counter.UI(ns("Report.Counter"))),
                                                    list(menu = "Data Harmonization",
                                                         content = Mod.Report.DataHarmonization.UI(ns("Report.DataHarmonization")))),
                                                    # list(menu = "Log",
                                                    #      content = Mod.Report.Log.UI(ns("Report.Log")))),
                                             id = ns("Tabset"))))
          }

          # Call Widget frame module UI and pass widget-specific UI layout
          Mod.Widget.UI(id = "Widget.CurationReport",
                        Title = "FREDA Curation Report",
                        WidgetMainUI = Layout)
      }

      #---------------------------------------------------------------------------
      # Widget Server Logic
      #---------------------------------------------------------------------------
      Server <- function(input, output, session)
      {
          # Hide waiter loading screen after initial app load has finished
          waiter::waiter_hide()

          # Define widget-specific server logic that is passed to widget frame module
          WidgetServerLogic <- function(session)
                               {
                                  # --- Call modules: DataSet Monitors ---
                                  Mod.Report.Counter.Server(id = "Report.Counter")
                                  Mod.Report.DataHarmonization.Server(id = "Report.DataHarmonization")
                                  #Mod.Report.Log.Server(id = "Report.Log")


                                }

          # Call Widget frame module and pass widget-specific server logic
          Mod.Widget.Server(id = "Widget.CurationReport",
                            WidgetServerLogic)

          #-----------------------------------------------------------------------

          # Initialize global objects
          session$userData$CurationReport <- reactiveVal(NULL)
          session$userData$DSConnections <- reactiveVal(NULL)

          # output$TestMonitor <- renderText({  req(session$userData$CurationReport())
          #                                     paste0(names(session$userData$CurationReport()), collapse = ", ")
          #                                   })

          # 'Mod.Initialize' assigns content to session$userData objects at app start
          Mod.Initialize(id = "Initialize",
                         CurationReport = CurationReport,
                         DSConnections = DSConnections)

          # If the option 'EndProcessWhenClosingApp' is TRUE, the following ensures that the background process is automatically ending when the app shuts down
          if (EndProcessWhenClosingApp == TRUE) { session$onSessionEnded(function() { stopApp() }) }
      }

      # Return Mini-App
      shiny::shinyApp(ui = UI,
                      server = Server)
  }

#-------------------------------------------------------------------------------

  # Either use CCPhosApp::RunAutonomousApp() to run the app in a separate background process or run it in the hosting session
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
