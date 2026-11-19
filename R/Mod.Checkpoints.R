

# --- MODULE: Checkpoints ---

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module UI component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
Mod.Checkpoints.UI <- function(id)
#-------------------------------------------------------------------------------
{
  ns <- NS(id)

  div(id = ns("CheckpointsContainer"),
      #class = "ui scrollable segment",
      style = "position: relative;
               height: 100%;
               overflow: auto;
               margin: 0;",

      div(id = ns("WaiterScreenContainer"),
          style = "position: absolute;
                   height: 100%;
                   width: 100%;
                   top: 0.5em;
                   left: 0;"),

      uiOutput(outputId = ns("CheckpointsTable")))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module server component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
Mod.Checkpoints.Server <- function(id)
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
                        WaiterScreen$show()
                    }

                    LoadingOff <- function()
                    {
                        WaiterScreen$hide()
                    }
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                    output$CheckpointsTable <- renderUI({  req(session$userData$Checkpoints())

                                                           # Assign loading behavior
                                                           LoadingOn()
                                                           on.exit(LoadingOff())

                                                           if (!is.null(session$userData$Checkpoints()))
                                                           {
                                                               DataFrameToHtmlTable(DataFrame = session$userData$Checkpoints(),
                                                                                    ColContentHorizontalAlign = "center",
                                                                                    ColumnLabels = c(ServerName = "Server",
                                                                                                     dsCCPhosVersion = "dsCCPhos"),
                                                                                    ColumnIcons = c(CheckConnection = "wifi",
                                                                                                    CheckPackageAvailability = "box",
                                                                                                    CheckFunctionAvailability = "cogs",
                                                                                                    CheckOpalTableAvailability = "server",
                                                                                                    CheckRefinementCompletion = "wrench",
                                                                                                    CheckEnrichmentCompletion = "magic"),
                                                                                    SemanticTableCSSClass = "ui small compact celled structured table",
                                                                                    TurnColorValuesIntoDots = TRUE)
                                                           }
                                                        })
               })
}

