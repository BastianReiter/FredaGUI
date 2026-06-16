
#' DataFrameToHtmlTable
#'
#' Turn a data.frame into HTML table code.
#'
#' To enable cell-specific CSS classes or style code, the passed data.frame should have columns named 'CellCSSClass.ABC' where 'ABC' is another column in the data.frame.
#'
#' @param DataFrame \code{data.frame} or \code{tibble}
#' @param TableID \code{string} - Used to identify the table object in the DOM
#' @param CategoryColumn \code{string}
#' @param ColContentHorizontalAlign Either a single string for all columns or a named character vector to determine horizontal content alignment for specific columns
#' @param ColumnCSSClass Either a single string for all columns or a named character vector to determine table cell classes for specific columns
#' @param ColumnIcons named \code{vector}
#' @param ColumnLabels named \code{vector} with names being original table column names and elements being displayed column labels
#' @param ColumnLabelsLineBreak \code{logical} Indicating whether column labels should span across two lines
#' @param ColumnMaxWidth \code{integer} - Maximum width of columns in character spaces
#' @param HeaderColspans \code{integer vector}
#' @param RotatedHeaderNames \code{vector}
#' @param RowColorColumn \code{string}
#' @param SemanticTableCSSClass \code{string}
#' @param TableStyle A \code{string} containing CSS style elements
#' @param TurnColorValuesIntoDots \code{logical}
#' @param TurnLogicalsIntoIcons \code{logical}
#' @param TurnNAsIntoBlanks \code{logical}
#'
#' @return HTML code
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DataFrameToHtmlTable <- function(DataFrame,
                                 TableID = NULL,
                                 CategoryColumn = NULL,
                                 ColContentHorizontalAlign = "left",
                                 ColumnCSSClass = NULL,
                                 ColumnIcons = NULL,
                                 ColumnLabels = NULL,
                                 ColumnLabelsLineBreak = FALSE,
                                 ColumnMaxWidth = 50,
                                 HeaderColspans = NULL,
                                 RotatedHeaderNames = character(),
                                 RowColorColumn = NULL,
                                 SemanticTableCSSClass = "ui celled table",
                                 TableStyle = "",
                                 TurnColorValuesIntoDots = FALSE,
                                 TurnLogicalsIntoIcons = FALSE,
                                 TurnNAsIntoBlanks = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # DataFrame <- readRDS("TestTable.rds")
  # TableID <- NULL
  # CategoryColumn <- "Feature"
  # ColContentHorizontalAlign <- "center"
  # ColumnCSSClass <- NULL
  # ColumnIcons <- NULL
  # ColumnLabels <- c(Value.Raw = "Raw",
  #                   Value.Remediated = "Remediated",
  #                   Value.Recoded = "Recoded",
  #                   Value.Final = "Final",
  #                   Count.Raw = "Count")
  # ColumnLabelsLineBreak <- FALSE
  # ColumnMaxWidth <- 1000
  # HeaderColspans <- NULL
  # RotatedHeaderNames <- character()
  # RowColorColumn <- NULL
  # SemanticTableCSSClass <- "ui small compact celled structured table"
  # TableStyle <- ""
  # TurnColorValuesIntoDots <- TRUE
  # TurnLogicalsIntoIcons <- FALSE
  # TurnNAsIntoBlanks <- FALSE

#-------------------------------------------------------------------------------

  # If DataFrame is empty return empty string
  if (is.null(DataFrame))
  {
      return("")
  }

  # Replace quote marks with '&quot' to avoid html syntax errors
  DataFrame <- DataFrame %>%
                    mutate(across(where(is.character),
                                  ~ str_replace_all(.x, "'", "&quot;")))

  # Convert object passed to 'DataFrame' argument (e.g. tibble) to data.frame
  DataFrame <- as.data.frame(DataFrame)

  # If no explicit TableID is passed, assign it a random sample of letters
  TableID <- ifelse(is.null(TableID),
                    paste0(sample(LETTERS, 9, TRUE), collapse = ""),
                    TableID)

  # If there are special purpose columns, don't render their content in the table
  HiddenColumns <- c(CategoryColumn,
                     RowColorColumn,
                     names(DataFrame)[str_starts(names(DataFrame), "CellCSSClass")],      # All column names that start with "CellCSSClass"
                     names(DataFrame)[str_starts(names(DataFrame), "CellCSSCode")])      # All column names that start with "CellCSSCode"

  # If there is a column informing about categories, get all unique category values
  CategoryValues <- "None"
  if (!is.null(CategoryColumn))
  {
      CategoryValues <- unique(DataFrame[[CategoryColumn]])
  }

  # Define set of colors that are available for colored icons (taken from Semantic UI)
  AvailableColors <- c("red", "orange", "yellow", "olive", "green", "teal", "blue", "violet", "purple", "pink", "brown", "grey", "black")


#===============================================================================
# Table Header: Get collection of th-elements as character-vector
#===============================================================================

  RelevantColumnHeaders <- colnames(DataFrame)
  if (!is.null(HeaderColspans)) { RelevantColumnHeaders <- names(HeaderColspans) }

  StringsTableHeadRow <- purrr::modify(.x = RelevantColumnHeaders,
                                       function(colname)
                                       {
                                          if (!(colname %in% HiddenColumns))      # Don't include headers from hidden columns
                                          {
                                              HeaderCellCSSClass <- ""
                                              HeaderColspan <- 1

                                              if (!is.null(HeaderColspans) & colname %in% names(HeaderColspans))
                                              {
                                                  HeaderColspan <- HeaderColspans[[colname]]
                                              }

                                              # If 'ColContentHorizontalAlign' is a single string
                                              if (length(ColContentHorizontalAlign) == 1 & is.null(names(ColContentHorizontalAlign)))
                                              {
                                                  HeaderCellCSSClass <- paste(HeaderCellCSSClass,
                                                                              case_when(ColContentHorizontalAlign == "right" ~ "right aligned",
                                                                                        ColContentHorizontalAlign == "center" ~ "center aligned",
                                                                                        .default = ""))
                                              }

                                              # If 'ColContentHorizontalAlign' is a named vector with current column name in its names
                                              if (!is.null(names(ColContentHorizontalAlign)) & colname %in% names(ColContentHorizontalAlign))
                                              {
                                                  HeaderCellCSSClass <- paste(HeaderCellCSSClass,
                                                                              case_when(ColContentHorizontalAlign[colname] == "right" ~ "right aligned",
                                                                                        ColContentHorizontalAlign[colname] == "center" ~ "center aligned",
                                                                                        .default = ""))
                                              }

                                              # If optional 'RotatedHeaderNames' is passed
                                              if (colname %in% RotatedHeaderNames) { HeaderCellCSSClass <- paste0(HeaderCellCSSClass, " rotate") }

                                              # Define ColLabel
                                              ColLabel <- ""

                                              # Replace column label with icon if passed in ColumnIcons
                                              if (colname %in% names(ColumnIcons))
                                              {
                                                  ColLabel <- paste0("shiny.semantic::icon(class = '", ColumnIcons[colname], "')")

                                              } else {

                                                  # Define ColLabelText
                                                  ColLabelText <- colname

                                                  # Replace column label text if passed in ColumnLabels
                                                  if (colname %in% names(ColumnLabels)) { ColLabelText <- ColumnLabels[colname] }

                                                  if (ColumnLabelsLineBreak == FALSE)
                                                  {
                                                      # Truncate ColLabelText if required
                                                      if (!is.null(ColumnMaxWidth) & str_length(ColLabelText) > ColumnMaxWidth) { ColLabelText <- str_trunc(ColLabelText, ColumnMaxWidth, "right") }

                                                  } else {

                                                      if (str_length(ColLabelText) <= ColumnMaxWidth * 2) { ColLabelText <- paste0(substr(ColLabelText, 1, ColumnMaxWidth), " ", substr(ColLabelText, ColumnMaxWidth + 1, ColumnMaxWidth * 2)) }
                                                      else { ColLabelText <- paste0(substr(ColLabelText, 1, ColumnMaxWidth), " ", str_trunc(substr(ColLabelText, ColumnMaxWidth + 1, ColumnMaxWidth * 2), ColumnMaxWidth, "right")) }
                                                  }

                                                  # Put ColLabelText in span tag
                                                  ColLabel <- paste0("span('", ColLabelText, "')")
                                              }

                                              paste0("tags$th(",
                                                     "class = '", HeaderCellCSSClass, "', ",      # Add th CSS class
                                                     "colspan = '", HeaderColspan, "', ",
                                                     "div(", ColLabel, "))")

                                          } else { return(NA) }
                                       })

  # Make single string for thead-element
  StringTableHead <- paste0("tags$thead(tags$tr(",
                            paste0(StringsTableHeadRow[!is.na(StringsTableHeadRow)], collapse = ", "),
                            "))")


#===============================================================================
# Table body rows: Get collection of tr- and td-elements as character vectors
#===============================================================================

  StringsTableRows <- character()
  Data <- DataFrame

  # --- Loop through CATEGORIES (optionally) -----------------------------------
  for (k in 1:length(CategoryValues))
  {
      if (!is.null(CategoryColumn))
      {
          # Add a subheader-row for current category
          StringsTableRows <- c(StringsTableRows,
                                paste0("tags$tr(tags$th(style = 'background-color: #767676;
                                                                 color: white;',
                                                        colspan = '",
                                       ncol(DataFrame) - length(HiddenColumns),   # Number of columns that subheader-row is spanning over
                                       "', '",
                                       CategoryValues[k],
                                       "'))"))

          # Select only rows in data that belong to current category
          Data <- DataFrame[DataFrame[[CategoryColumn]] == CategoryValues[k], ]
      }

      # --- Loop through ROWS ------------------------------------------------
      for (i in 1:nrow(Data))
      {
          StringsTableRowCells <- character()

          # --- Loop through COLUMNS -----------------------------------------
          for (j in 1:ncol(Data))
          {
              ColumnName <- names(Data)[j]

              if (!(ColumnName %in% HiddenColumns))      # Don't include cells from hidden columns
              {
                  CellValue <- Data[i, j]

                  # Turn 'CellValue' into character string
                  if (!is.na(CellValue)) { CellValue <- as.character(CellValue) }      # For all non-NA values
                  else { CellValue <- case_when(TurnNAsIntoBlanks == TRUE ~ "", .default = "NA") }      # If 'CellValue' is NA, turn it into 'NA' string (default) or blank '', depending on optional argument

                  CellCSSClass <- ""
                  CellCSSCode <- ""
                  CellIcon <- "None"

                  if (!is.null(ColumnMaxWidth) && !is.na(ColumnMaxWidth))
                  {
                      CellCSSCode <- paste0("max-width: ", ColumnMaxWidth, "ch; ")
                  }

                  # Set specific column-wide CSS class for cells, if option is passed
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                  # If 'ColumnCSSClass' is a single string
                  if (length(ColumnCSSClass) == 1 & is.null(names(ColumnCSSClass))) { CellCSSClass <- paste(CellCSSClass, ColumnCSSClass) }

                  # If 'ColumnCSSClass' is a named vector with current column name in its names
                  if (!is.null(names(ColumnCSSClass)) & ColumnName %in% names(ColumnCSSClass)) { CellCSSClass <- paste(CellCSSClass, ColumnCSSClass[ColumnName]) }


                  # Set column-wide CSS class determining horizontal alignment, if option is passed
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                  # If 'ColContentHorizontalAlign' is a single string
                  if (length(ColContentHorizontalAlign) == 1 & is.null(names(ColContentHorizontalAlign)))
                  {
                      CellCSSClass <- paste(CellCSSClass,
                                            case_when(ColContentHorizontalAlign == "right" ~ "right aligned",
                                                      ColContentHorizontalAlign == "center" ~ "center aligned",
                                                      .default = ""))
                  }

                  # If 'ColContentHorizontalAlign' is a named vector with current column name in its names
                  if (!is.null(names(ColContentHorizontalAlign)) & ColumnName %in% names(ColContentHorizontalAlign))
                  {
                      CellCSSClass <- paste(CellCSSClass,
                                            case_when(ColContentHorizontalAlign[ColumnName] == "right" ~ "right aligned",
                                                      ColContentHorizontalAlign[ColumnName] == "center" ~ "center aligned",
                                                      .default = ""))
                  }


                  # Set cell-specific CSS class for cells, if optional CSS class columns are passed
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                  # In case there is a column defining a cell-specific CSS class for current data column
                  if (paste0("CellCSSClass.", ColumnName) %in% names(Data))
                  {
                      CellCSSClass <- paste(CellCSSClass,
                                            as.character(Data[i, paste0("CellCSSClass.", ColumnName)]))

                      # Determine code for icon to be displayed in cell based on CSS class (grepl() call checks if the string in 'CellCSSClass' contains certain substrings)
                      CellIcon <- case_when(grepl("CellCSSClass_Success", CellCSSClass, fixed = TRUE) ~ "shiny.semantic::icon(class = 'small green check')",
                                            grepl("CellCSSClass_Failure", CellCSSClass, fixed = TRUE) ~ "shiny.semantic::icon(class = 'small red times')",
                                            TRUE ~ "None")
                  }

                  # Set cell-specific CSS code for cells, if optional CSS code columns are passed
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                  # In case there is a column defining cell-specific CSS code for current data column
                  if (paste0("CellCSSCode.", ColumnName) %in% names(Data))
                  {
                      CellCSSCode <- paste0(CellCSSCode,
                                            as.character(Data[i, paste0("CellCSSCode.", ColumnName)]))
                  }

                  # Build strings for all cells in the current table row
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                  StringsTableRowCells <- c(StringsTableRowCells,
                                            paste0("tags$td(",
                                                   #--- Add td CSS class, if defined ---
                                                   ifelse(CellCSSClass != "",
                                                          paste0("class = '", CellCSSClass, "', "),
                                                          ""),
                                                   #--- Add td CSS code, if defined ---
                                                   ifelse(CellCSSCode != "",
                                                          paste0("style = '", CellCSSCode, "', "),
                                                          ""),
                                                   {  #--- Turn logical values into icon if option is passed ---
                                                      if (TurnLogicalsIntoIcons == TRUE & any(str_detect(CellValue, c("TRUE", "FALSE"))))      # Does 'CellValue' contain at least one instance of 'TRUE' or 'FALSE' strings
                                                      {
                                                          ReplacementVector <- c("TRUE" = "§start§ shiny.semantic::icon(class = 'small green check') §end§",      # Use pseudo-code tags to mark start and end of icon code
                                                                                 "FALSE" = "§start§ shiny.semantic::icon(class = 'small red times') §end§")

                                                          String <- str_replace_all(CellValue, ReplacementVector)
                                                          if (str_starts(String, "§start§")) { String <- str_sub(String, start = 8) } else { String <- paste0("'", String) }
                                                          if (str_ends(String, "§end§")) { String <- str_sub(String, end = -6) } else { String <- paste0(String, "'") }

                                                          str_replace_all(String, c("§start§" = "', ",
                                                                                    "§end§" = ", '"))

                                                      } else {

                                                          #--- Turn strings coding for available colors in cell value into colored dot icons if option is passed ---
                                                          if (TurnColorValuesIntoDots == TRUE & any(str_detect(CellValue, AvailableColors)))      # Does 'CellValue' contain at least one string that is also listed in 'AvailableColors'?
                                                          {
                                                              DetectedColors <- AvailableColors[str_detect(CellValue, AvailableColors)]
                                                              ReplacementVector <- paste0("§start§ shiny.semantic::icon(class = 'small ", DetectedColors, " circle') §end§")      # Use pseudo-code tags to mark start and end of icon code
                                                              names(ReplacementVector) <- DetectedColors

                                                              String <- str_replace_all(CellValue, ReplacementVector)
                                                              if (str_starts(String, "§start§")) { String <- str_sub(String, start = 8) } else { String <- paste0("'", String) }
                                                              if (str_ends(String, "§end§")) { String <- str_sub(String, end = -6) } else { String <- paste0(String, "'") }

                                                              str_replace_all(String, c("§start§" = "', ",
                                                                                        "§end§" = ", '"))

                                                          } else {

                                                              paste0("'", CellValue, "'")
                                                      }}
                                                   },
                                                   #--- Add optional icon to value as determined by cell class ---
                                                   ifelse(CellIcon != "None",
                                                          paste0(", shiny::HTML('&ensp;'), ", CellIcon),      # Add two spaces before icon
                                                          ""),
                                                   ")"))
              }
          }

          # Optionally determine class attribute for tr-element to colorize row
          RowColor <- ifelse(is.null(RowColorColumn),
                             "",
                             paste0("class = '", Data[i, RowColorColumn], "', "))

          # Add current row to table
          StringsTableRows <- c(StringsTableRows,
                                paste0("tags$tr(",
                                       RowColor,
                                       paste0(StringsTableRowCells, collapse = ", "),
                                       ")"))
      }
  }

  # Make single string for tbody-element
  StringTableBody <- paste0("tags$tbody(",
                            paste0(StringsTableRows, collapse = ", "),
                            ")")

  # Concatenate all substrings into one string
  HtmlCallString <- paste0("tags$table(id = '", TableID,
                                   "', class = '", SemanticTableCSSClass,
                                   "', style = '", TableStyle, "', ",
                           StringTableHead,
                           ", ",
                           StringTableBody,
                           ")")

  # Evaluate string to return html code that can be processed by output-function in UI
  eval(parse(text = HtmlCallString))
}
