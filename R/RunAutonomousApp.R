
#' RunAutonomousApp
#'
#' Set up a background process essentially running a separate R session. Use this session to run a Shiny app autonomously from hosting R session.
#'
#' @param ShinyAppInitFunction \code{function} initializing a Shiny app with \code{shiny::shinyApp}
#' @param AppArguments \code{list} containing optional arguments for app initializing function
#' @param Host \code{string} Default: '127.0.0.1'
#' @param Port \code{integer} If this argument is \code{NULL} the function will find a random free port and use it
#' @param RunInViewer \code{logical} If \code{TRUE}, display and run the app in the RStudio Viewer pane (requires \code{rstudioapi}) instead of default web browser
#'
#' @return A \code{processx::r_process} object. See also documentation for \code{callr::r_bg()}.
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
RunAutonomousApp <- function(ShinyAppInitFunction,
                             AppArguments = NULL,
                             Host = "127.0.0.1",
                             Port = NULL,
                             RunInViewer = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # ShinyAppInitFunction <- FredaGUI::StartFredaApp
  # AppArguments <- list(CCPTestData = TestData)
  # Host <- "127.0.0.1"
  # Port <- NULL

  # If no 'Port' is specifically passed, find a free port randomly with httpuv::randomPort()
  if (is.null(Port)) { Port <- httpuv::randomPort() }

#-------------------------------------------------------------------------------

  Process <- callr::r_bg(
                args = list(ShinyAppInitFunction.Bg = ShinyAppInitFunction,
                            AppArguments.Bg = AppArguments,
                            Host.Bg = Host,
                            Port.Bg = Port),
                func = function(ShinyAppInitFunction.Bg, AppArguments.Bg, Host.Bg, Port.Bg)
                       {
                          #TestData <- readRDS("../dsCCPhos/Development/Data/TestData/CCPTestData.rds")

                          # Load namespaces for new background R session
                          library(FredaGUI)
                          library(DSI)
                          library(shiny)

                          # Start FredaGUI app
                          shiny::runApp(do.call(ShinyAppInitFunction.Bg, AppArguments.Bg),
                                        port = Port.Bg,
                                        host = Host.Bg,
                                        launch.browser = FALSE)

                          # Temporary error output
                          writeLines(capture.output(DSI::datashield.errors()), "ds_errors.txt")
                       },
                supervise = TRUE)

  # Check if URL is responding and stall if it needs time loading (Credit to Will Landau)
  while(!pingr::is_up(destination = Host, port = Port))
  {
      if (!Process$is_alive()) stop(Process$read_all_error())   # If process was ended for some reason, print error messages.
      Sys.sleep(0.01)   # Stall - Effectively check every 0.01 seconds if URL is available yet
  }

  # Compile the URL that hosts the process which itself runs the app
  AppURL <- paste0("http://", Host, ":", Port)

  # Print a message with the 'AppURL'
  dsFredaClient::PrintSoloMessage(c(Info = paste0("App available at ", AppURL)))

  # If the option 'RunInViewer' is TRUE, display and run the app in the RStudio Viewer pane...
  if (RunInViewer == TRUE & require(rstudioapi) == TRUE)
  {
      rstudioapi::viewer(url = AppURL)
  } else {   # ... otherwise open in default web browser
      browseURL(AppURL)
  }

  # Note: The background process started by callr::r_bg() can be ended automatically for Shiny apps by calling 'session$onSessionEnded(function() { stopApp() })' in the server component

  # system2("firefox", args = c("--new-window", AppURL))

#-------------------------------------------------------------------------------
  return(Process)
}
