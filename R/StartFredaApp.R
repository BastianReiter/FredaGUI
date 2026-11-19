
#' StartFredaApp
#'
#' Launch Shiny app
#'
#' @param DSConnections \code{list} of \code{DSConnection} objects
#' @param ServerSpecifications \code{data.frame}
#' @param TestData \code{list} - Optional test data
#' @param SDSCheckData \code{list} - Optional SDSCheck data
#' @param RefinementReport \code{list} - Optional RefinementReport data
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
StartFredaApp <- function(#--- Arguments for app itself ---
                           TestData = NULL,
                           SDSCheckData = NULL,
                           RDSCheckData = NULL,
                           EDSCheckData = NULL,
                           RefinementReport = NULL,
                           DSConnections = NULL,
                           ServerSpecifications = NULL,
                           ServerWorkspaceInfo = NULL,
                           #--- Arguments for app wrapper ---
                           EndProcessWhenClosingApp = TRUE,
                           RunAutonomously = FALSE,
                           RunInViewer = FALSE,
                           ...)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # Create the app initiating function (UI and server component resulting in a ShinyApp object)
  InitFunction <- function(...)
  {
      # Set option to use themes for semantic CCS
      #options(semantic.themes = TRUE)

      #Worker <- shiny.worker::initialize_worker()

      # Since the app is deployed as a package, the folder for external resources (e.g. CSS files, static images) needs to be added manually
      shiny::addResourcePath('www', system.file("www", package = "FredaGUI"))

      # Start Freda app
      shiny::shinyApp(ui = MainUIComponent(),
                      server = MainServerComponent(TestData = TestData,
                                                   SDSCheckData = SDSCheckData,
                                                   RDSCheckData = RDSCheckData,
                                                   EDSCheckData = EDSCheckData,
                                                   RefinementReport = RefinementReport,
                                                   DSConnections = DSConnections,
                                                   ServerSpecifications = ServerSpecifications,
                                                   ServerWorkspaceInfo = ServerWorkspaceInfo))
  }

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
