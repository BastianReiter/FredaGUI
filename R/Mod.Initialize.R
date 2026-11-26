
# --- Module Initialize ---

# Has no UI component, only server

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module server component
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @noRd
#' @export
#-------------------------------------------------------------------------------
Mod.Initialize <- function(id,
                           EDSCheckData = NULL,
                           RDSCheckData = NULL,
                           RefinementReport = NULL,
                           SDSCheckData = NULL)
#-------------------------------------------------------------------------------
{
  moduleServer(id,
               function(input, output, session)
               {
                  if (!is.null(EDSCheckData)) { session$userData$EDSCheckData(EDSCheckData) }

                  if (!is.null(RDSCheckData)) { session$userData$RDSCheckData(RDSCheckData) }

                  if (!is.null(RefinementReport)) { session$userData$RefinementReport(RefinementReport) }

                  if (!is.null(SDSCheckData)) { session$userData$SDSCheckData(SDSCheckData) }
               })
}
