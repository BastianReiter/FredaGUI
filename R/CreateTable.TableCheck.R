
#' CreateTable.TableCheck
#'
#' Compile table check data from \code{dsFredaClient::ds.CheckTable()} or \code{dsFredaClient::ds.CheckDataSet()} into a table suited for display
#'
#' @param TableData \code{list} - Contains...
#'
#' @return \code{list} - Contains two \code{tibbles}:
#'                        \itemize{ \item 'TableDetails' - Contains coherent table check data
#'                                  \item 'HeaderColspans' }
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CreateTable.TableCheck <- function(TableData)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # TableData <- RDSTableCheck[c("TableRecordCounts",
  #                              "FeatureExistence",
  #                              "FeatureTypes",
  #                              "NonMissingValueRates")] %>%
  #                 list_transpose(simplify = FALSE) %>%
  #                 pluck("Staging")

#-------------------------------------------------------------------------------

  # If 'TableData' is NULL or of length 0 (which occurs when there is no meta data for a NULL object) or any of it's elements are NULL, abort function and return NULL
  if (length(TableData) == 0 || any(sapply(TableData, is.null))) { return(NULL) }


# Process data.frames within list 'TableData' to consolidate info in one data.frame
#-------------------------------------------------------------------------------

  # Process info on feature types for sensible printing
  FeatureTypes <- TableData$FeatureTypes %>%
                      rename_with(~ paste0(.x, ".TYPE"),
                                  -SiteName) %>%
                      mutate(across(.cols = -SiteName,
                                    .fns = ~ str_replace_all(string = .x,
                                                             pattern = c("character" = "chr",
                                                                         "double" = "dbl",
                                                                         "integer" = "int",
                                                                         "logical" = "log"))))

  # Process non-missing value rates for sensible printing
  NonMissingValueRates <- TableData$NonMissingValueRates %>%
                              rename_with(~ paste0(.x, ".NMVR"),
                                          -SiteName) %>%
                              mutate(across(.cols = -SiteName,
                                            .fns = function(.x)
                                                   {
                                                      # If rate is between 0% and 1%, display as '<1%', otherwise round to full numbers and add '%'
                                                      if_else(.x > 0 & .x < 0.01,
                                                              "<1%",
                                                              paste0(round(100 * .x, digits = 0), "%"))
                                                   }))

  # To enable value-dependent cell styling, create data.frame with features containing CSS code
  NonMissingValueRates_CSS <- TableData$NonMissingValueRates %>%
                                  rename_with(~ paste0("CellCSSCode.", .x, ".NMVR"),
                                              -SiteName) %>%
                                  mutate(across(.cols = -SiteName,
                                                .fns = function(.x)
                                                       {
                                                          # Convert decimal numbers between 0 and 1 into hexadecimal color codes ranging on a defined color palette
                                                          ColorFunction <- grDevices::colorRampPalette(c("#B03060", "#FFD700", "#016936"))      # Use base-function grDevices::colorRampPalette to create function that allows mapping to hexadecimal codes on a palette of defined color points
                                                          Palette <- ColorFunction(101)      # Create a vector of 101 colors to assure valid indexing (s. below)
                                                          ValueColor <- Palette[round(100 * .x, digits = 0) + 1]      # '+1' is necessary to avoid invalid indexing (the term in the []-brackets can create 101 different integer numbers)

                                                          # Turn hexadecimal color codes into CSS function 'rgba()' call (as a string), using custom function 'ColorToRGBCSS()'
                                                          BGColorCSSCode <- ColorToRGBCSS(ValueColor, Alpha = 0.4, RenderNATransparent = TRUE)
                                                          #FGColorCSSCode <- ColorToRGBCSS(ValueColor, Alpha = 1, RenderNATransparent = TRUE)
                                                          FGColorCSSCode <- "white"

                                                          # Build and return final strings of CSS code
                                                          paste0("background-color: ", BGColorCSSCode, "; ",
                                                                 "color: ", FGColorCSSCode, ";")
                                                       }))

  # Get table feature names
  FeatureNames <- names(TableData$FeatureExistence)[-1]

  # Get table record counts
  RecordCounts <- TableData$TableRecordCounts

  # Before joining tables, create vector defining correct column order
  ColumnOrder <- c("SiteName",
                   names(RecordCounts)[-1],
                   rbind(FeatureNames,
                         names(FeatureTypes)[-1],
                         names(NonMissingValueRates)[-1],
                         names(NonMissingValueRates_CSS)[-1]))

  TableDetails <- TableData$FeatureExistence %>%
                      left_join(RecordCounts, by = join_by(SiteName)) %>%
                      left_join(FeatureTypes, by = join_by(SiteName)) %>%
                      left_join(NonMissingValueRates, by = join_by(SiteName)) %>%
                      left_join(NonMissingValueRates_CSS, by = join_by(SiteName)) %>%
                      select(all_of(ColumnOrder))

  HeaderColspans <- c(1, 1, rep.int(3, length(FeatureNames))) %>%
                        set_names(c("SiteName", "RecordCount", FeatureNames))

  return(list(TableDetails = TableDetails,
              HeaderColspans = HeaderColspans))
}



