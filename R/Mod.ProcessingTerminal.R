

# --- MODULE: Processing Terminal ---

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module UI component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
Mod.ProcessingTerminal.UI <- function(id,
                                      ButtonLabel)
#-------------------------------------------------------------------------------
{
  ns <- NS(id)

  div(style = "display: grid;
               height: 100%;
               grid-template-rows: 4em 22.8em;",

      div(style = "padding: 10px;
                   text-align: center;",

          shiny.semantic::action_button(ns("ProcessingTrigger"),
                                        class = "ui blue button",
                                        style = "box-shadow: 0 0 10px 10px white;",
                                        label = ButtonLabel)),

          div(style = "position: relative;",

              div(id = ns("WaiterScreenContainer"),
                  style = "position: absolute;
                           height: 100%;
                           width: 100%;
                           top: 0.5em;
                           left: 0;"),

              Mod.MessageMonitor.UI(ns("Monitor"))))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module server component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
Mod.ProcessingTerminal.Server <- function(id)
#-------------------------------------------------------------------------------
{
  moduleServer(id,
               function(input, output, session)
               {
                  ReturnMessages <- reactiveVal(NULL)
                  Complete <- reactiveVal(FALSE)

                  Mod.MessageMonitor.Server("Monitor",
                                            MessagesList = ReturnMessages)

                  observe({ shinyjs::showElement(id = "Monitor", anim = TRUE, animType = "fade") }) %>%
                      bindEvent(ReturnMessages())


                  # Setting up loading behavior with waiter package functionality
                  #-------------------------------------------------------------
                  ns <- session$ns
                  WaiterScreen <- CreateWaiterScreen(ID = ns("WaiterScreenContainer"))

                  LoadingOn <- function()
                  {
                      shinyjs::disable("ProcessingTrigger")
                      WaiterScreen$show()
                  }

                  LoadingOff <- function()
                  {
                      shinyjs::enable("ProcessingTrigger")
                      WaiterScreen$hide()
                  }


                  if (id == "CheckServerRequirements")
                  {
                      observe({ # Assign loading behavior
                                LoadingOn()
                                on.exit(LoadingOff())

                                # Trigger function CheckServerRequirements() and save returned list
                                ServerCheck <- SafeDS(dsCCPhosClient::CheckServerRequirements(ServerSpecifications = session$userData$ServerSpecifications(),
                                                                                              DSConnections = session$userData$DSConnections()))

                                # In case of error (determined by class of 'SafeDS()' output), break reactive chain and show error notification
                                if (inherits(ServerCheck, "dsFail")) { ShowDSError(); return(NULL) }

                                # Assign 'Messages' to reactive value ReturnMessages
                                ReturnMessages(ServerCheck$Messages)

                                # Update 'Checkpoints' data frame ...
                                Checkpoints <- session$userData$Checkpoints() %>%
                                                    left_join(select(ServerCheck$PackageAvailability, c(ServerName, CheckPackageAvailability)), by = join_by(ServerName)) %>%
                                                    left_join(ServerCheck$VersionOfdsCCPhos, by = join_by(ServerName)) %>%
                                                    left_join(select(ServerCheck$FunctionAvailability, c(ServerName, CheckFunctionAvailability)), by = join_by(ServerName)) %>%
                                                    left_join(select(ServerCheck$OpalTableAvailability, c(ServerName, CheckOpalTableAvailability)), by = join_by(ServerName))

                                # ... and reassign it to session$userData object
                                session$userData$Checkpoints(Checkpoints)

                                # Trigger function GetServerOpalDBInfo() and assign return (data.frame) to reactive value ServerOpalDBInfo in session$userData
                                session$userData$ServerOpalDBInfo(ServerCheck$OpalTableAvailability)

                                # Set reactive value 'Complete' TRUE
                                Complete(TRUE)

                                }) %>%
                          bindEvent(input$ProcessingTrigger)
                  }


                  if (id == "LoadData")
                  {
                      observe({ # Assign loading behavior
                                LoadingOn()
                                on.exit(LoadingOff())

                                # Trigger function LoadSourceDataSet()
                                Messages <- SafeDS(dsCCPhosClient::CCP.LoadSourceDataSet(ServerSpecifications = session$userData$ServerSpecifications(),
                                                                                      RunAssignmentChecks = TRUE,
                                                                                      DSConnections = session$userData$DSConnections()))

                                # In case of error (determined by class of 'SafeDS()' output), break reactive chain and show error notification
                                if (inherits(Messages, "dsFail")) { ShowDSError(); return(NULL) }

                                # Assign messages to reactive value
                                ReturnMessages(Messages)

                                # Trigger function ds.CheckDataSet() for SDS and save returned list
                                SDSCheckData <- SafeDS(dsFredaClient::ds.GetDataSetCheck(DataSetName = "CCP.SourceDataSet",
                                                                                         Module = "CCP",
                                                                                         Stage = "Source",
                                                                                         DSConnections = session$userData$DSConnections()))

                                if (inherits(SDSCheckData, "dsFail")) { ShowDSError(); return(NULL) }

                                # Assign to session$userData object
                                session$userData$SDSCheckData(SDSCheckData)

                                # Trigger function GetServerWorkspaceInfo()...
                                InfoData <- SafeDS(dsFredaClient::GetServerWorkspaceInfo(DSConnections = session$userData$DSConnections()))
                                # ... handle possible error ...
                                if (inherits(InfoData, "dsFail")) { ShowDSError(); return(NULL) }
                                # ... and assign return (data.frame) to reactive value ServerWorkspaceInfo in session$userData
                                session$userData$ServerWorkspaceInfo(InfoData)

                                # Set reactive value Complete TRUE
                                Complete(TRUE)

                                }) %>%
                          bindEvent(input$ProcessingTrigger)
                  }


                  if (id == "RefineData")
                  {
                      observe({ # Assign loading behavior
                                LoadingOn()
                                on.exit(LoadingOff())

                                # Trigger function ds.RefineData() and save return
                                Refinement <- SafeDS(dsFredaClient::ds.RefineData(SourceDataSetName = "CCP.SourceDataSet",
                                                                                Module = "CCP",
                                                                                OutputName = "CCP.RefinementOutput",
                                                                                UnpackRefinedDataSet = TRUE,
                                                                                RunAssignmentChecks = FALSE,
                                                                                DSConnections = session$userData$DSConnections()))

                                if (inherits(Refinement, "dsFail")) { ShowDSError(); return(NULL) }

                                # Assign returned messages (concatenated lists) to reactive value ReturnMessages
                                ReturnMessages(Refinement$Messages)

                                # Trigger function ds.CheckDataSet() for RDS and save returned list
                                RDSCheckData <- SafeDS(dsFredaClient::ds.GetDataSetCheck(DataSetName = "CCP.RefinedDataSet",
                                                                                         Module = "CCP",
                                                                                         Stage = "Refined",
                                                                                         DSConnections = session$userData$DSConnections()))

                                if (inherits(RDSCheckData, "dsFail")) { ShowDSError(); return(NULL) }

                                # Assign to session$userData object
                                session$userData$RDSCheckData(RDSCheckData)

                                # Update 'Checkpoints' data frame ...
                                Checkpoints <- session$userData$Checkpoints() %>%
                                                    left_join(Refinement$Refinement.Completion, by = join_by(ServerName))

                                # # ... and reassign it to session$userData object
                                session$userData$Checkpoints(Checkpoints)

                                # Trigger function GetServerWorkspaceInfo() and assign return to reactive value ServerWorkspaceInfo in session$userData
                                InfoData <- SafeDS(dsFredaClient::GetServerWorkspaceInfo(DSConnections = session$userData$DSConnections()))
                                # ... handle possible error ...
                                if (inherits(InfoData, "dsFail")) { ShowDSError(); return(NULL) }
                                # ... and assign return (data.frame) to reactive value ServerWorkspaceInfo in session$userData
                                session$userData$ServerWorkspaceInfo(InfoData)

                                # Trigger function ds.GetRefinementReport() and assign return to reactive value 'RefinementReport' in session$userData
                                RefinementReport <- SafeDS(dsFredaClient::ds.GetRefinementReport(Module = "CCP",
                                                                                             DSConnections = session$userData$DSConnections()))
                                # ... handle possible errors ...
                                if (inherits(RefinementReport, "dsFail")) { ShowDSError(); return(NULL) }
                                # ... and assign return to reactive value in session$userData
                                session$userData$RefinementReport(RefinementReport)

                                # Set reactive value Complete TRUE
                                Complete(TRUE)

                                }) %>%
                          bindEvent(input$ProcessingTrigger)
                  }


                  if (id == "EnrichData")
                  {
                      observe({ # Assign loading behavior
                                LoadingOn()
                                on.exit(LoadingOff())

                                # Trigger function ds.EnrichData() and save return
                                Enrichment <- SafeDS(dsCCPhosClient::ds.CCP.EnrichData(RefinedDataSetName = "CCP.RefinedDataSet",
                                                                                          OutputName = "CCP.EnrichmentOutput",
                                                                                          UnpackEnrichedDataSet = TRUE,
                                                                                          RunAssignmentChecks = FALSE,
                                                                                          DSConnections = session$userData$DSConnections()))

                                if (inherits(Enrichment, "dsFail")) { ShowDSError(); return(NULL) }

                                # Assign returned messages (concatenated lists) to reactive value ReturnMessages
                                ReturnMessages(Enrichment$Messages)

                                # Trigger function ds.CheckDataSet() for EDS and save returned list
                                EDSCheckData <- SafeDS(dsFredaClient::ds.GetDataSetCheck(DataSetName = "CCP.EnrichedDataSet",
                                                                                         Module = "CCP",
                                                                                         Stage = "Enriched",
                                                                                         DSConnections = session$userData$DSConnections()))

                                if (inherits(EDSCheckData, "dsFail")) { ShowDSError(); return(NULL) }

                                # Assign to session$userData object
                                session$userData$EDSCheckData(EDSCheckData)

                                # Update 'Checkpoints' data frame ...
                                Checkpoints <- session$userData$Checkpoints() %>%
                                                    left_join(Enrichment$EnrichmentCompletionCheck, by = join_by(ServerName))

                                # # ... and reassign it to session$userData object
                                session$userData$Checkpoints(Checkpoints)

                                # Trigger function GetServerWorkspaceInfo() and assign return to reactive value ServerWorkspaceInfo in session$userData
                                InfoData <- SafeDS(dsFredaClient::GetServerWorkspaceInfo(DSConnections = session$userData$DSConnections()))
                                # ... handle possible error ...
                                if (inherits(InfoData, "dsFail")) { ShowDSError(); return(NULL) }
                                # ... and assign return (data.frame) to reactive value ServerWorkspaceInfo in session$userData
                                session$userData$ServerWorkspaceInfo(InfoData)

                                # Set reactive value Complete TRUE
                                Complete(TRUE)

                                }) %>%
                          bindEvent(input$ProcessingTrigger)
                  }

                  return(Complete)
               })
}


