

# --- MODULE: ModWidget ---

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module UI component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
Mod.Widget.UI <- function(id,
                          Title = "Freda",
                          WidgetMainUI)
#-------------------------------------------------------------------------------
{
  ns <- NS(id)

  shiny.semantic::semanticPage(

      # Set margin 0 (default is 10 px)
      margin = "0",

      # Add custom CSS file
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "www/FredaStyle.css")),

      # Title shown in browser
      title = Title,

      # Initiate use of shinyjs functionality
      shinyjs::useShinyjs(),

      # Initiate use of waiter functionality
      waiter::useWaiter(),
      waiter::waiterShowOnLoad(html = waiter::spin_3(),
                               color = waiter::transparent(.5)),

      #textOutput(outputId = "TestMonitor"),

      #-----------------------------------------------------------------------

      shiny.semantic::grid(id = "MainGrid",

          # Provide grid template (including definition of area names)
          grid_template = shiny.semantic::grid_template(

                                # --- Main grid layout for desktop devices ---
                                default = list(areas = rbind("header",
                                                             "main"),

                                               rows_height = c("minmax(40px, 6vh)", "90vh")),

                                # --- Main grid layout for mobile devices ---
                                mobile = list(areas = rbind("header",
                                                            "main"),

                                              rows_height = c("100px", "auto"))),

          area_styles = list(header = "padding: 10px 1em;
                                       background: rgb(5,73,150);
                                       background: linear-gradient(90deg, rgba(5,73,150,1) 8%, rgba(255,255,255,1) 100%);
                                       color: #595959;",
                             main = "padding: 10px;"),

          #--- HEADER --------------------------------------------------------

          header = shiny.semantic::split_layout(style = "display: flex;      /* Set up flexbox to use 'justify-content: space-between' to enable free space between columns without specifying column width */
                                                         justify-content: space-between;
                                                         align-items: center;",

                                                img(src = "www/FredaLogo.png",
                                                    alt = "FREDA Logo",
                                                    height = "30px")),

          #--- MAIN PANEL ----------------------------------------------------
          main = shiny.semantic::segment(class = "ui raised scrolling segment",
                                         style = "height: 100%;
                                                  overflow: auto;",

                                         div(style = "position: relative;",

                                             div(id = ns("WaiterScreenContainer"),
                                                 style = "position: absolute;
                                                          height: 100%;
                                                          width: 100%;
                                                          top: 0.5em;
                                                          left: 0;"),

                                                 WidgetMainUI(ns)))))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module server component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
Mod.Widget.Server <- function(id,
                              WidgetServerLogic)
#-------------------------------------------------------------------------------
{
  moduleServer(id,
               function(input, output, session)
               {
                  # Setting up loading behavior with waiter package functionality
                  #-------------------------------------------------------------
                  ns <- session$ns
                  WaiterScreen <- CreateWaiterScreen(ID = ns("WaiterScreenContainer"))

                  LoadingOn <- function()
                  {
                      WaiterScreen$show()
                  }

                  LoadingOff <- function()
                  {
                      WaiterScreen$hide()
                  }

                  # Call widget-specific server logic passed as function
                  WidgetServerLogic(session)
               })
}
