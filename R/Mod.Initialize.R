
# --- Module Initialize ---

# Has no UI component, only server

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module server component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#-------------------------------------------------------------------------------
Mod.Initialize <- function(id,
                           EDSCheckData = NULL,
                           TestData = NULL,
                           RDSCheckData = NULL,
                           RefinementReport = NULL,
                           DSConnections = NULL,
                           ExplorationData = NULL,
                           SDSCheckData = NULL,
                           ServerSpecifications = NULL,
                           ServerWorkspaceInfo = NULL)
#-------------------------------------------------------------------------------
{
  moduleServer(id,
               function(input, output, session)
               {
                  if (!is.null(EDSCheckData)) { session$userData$EDSCheckData(EDSCheckData) }

                  if (!is.null(TestData)) { session$userData$TestData <- TestData }

                  if (!is.null(RDSCheckData)) { session$userData$RDSCheckData(RDSCheckData) }

                  if (!is.null(RefinementReport)) { session$userData$RefinementReport(RefinementReport) }

                  if (!is.null(DSConnections)) { session$userData$DSConnections(DSConnections) }

                  if (!is.null(ExplorationData)) { session$userData$ExplorationData <- ExplorationData }

                  if (!is.null(SDSCheckData)) { session$userData$SDSCheckData(SDSCheckData) }

                  if (!is.null(ServerSpecifications)) { session$userData$ServerSpecifications(ServerSpecifications)
                  } else if (!is.null(session$userData$ServerSpecifications)) { session$userData$ServerSpecifications(as.data.frame(dsCCPhosClient::ServerSpecifications)) }

                  if (!is.null(ServerWorkspaceInfo)) { session$userData$ServerWorkspaceInfo(ServerWorkspaceInfo) }
               })
}
