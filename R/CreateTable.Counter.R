
#' CreateTable.Counter
#'
#' Compile table check data from \code{dsFredaClient::ds.CheckTable()} or \code{dsFredaClient::ds.CheckDataSet()} into a table suited for display
#'
#' @param CounterData \code{list} - Contains...
#'
#' @return Table object of \code{reactable} class
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CreateTable.Counter <- function(CounterData)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # CounterData <- CurationReport$Counter

#-------------------------------------------------------------------------------

  # Return NULL If 'CounterData' is NULL
  if (length(CounterData) == 0) { return(NULL) }


#-------------------------------------------------------------------------------

  PrepareReportData <- function(ReportData)
  {
      ReportData %>%
          mutate(Final.CountRecords.Proportion = 1 + Final.CountRecords.Change.Proportion,
                 Final.CountRootSubjects.Proportion = 1 + Final.CountRootSubjects.Change.Proportion,
                 Final.CountSeedSubjects.Proportion = 1 + Final.CountSeedSubjects.Change.Proportion) %>%
          select(any_of(c("Server",
                          "Table")),
                 starts_with("Initial"),
                 starts_with("PrimaryTableCleaning") & contains("Change"),
                 starts_with("TableNormalization") & contains("Change"),
                 starts_with("SecondaryTableCleaning") & contains("Change"),
                 starts_with("RecordSubsumption") & contains("Change"),
                 Final.CountRecords,
                 Final.CountRecords.Proportion,
                 Final.CountRootSubjects,
                 Final.CountRootSubjects.Proportion,
                 Final.CountSeedSubjects,
                 Final.CountSeedSubjects.Proportion) %>%
          mutate(SpaceColumn.1 = NA, .after = Initial.CountSeedSubjects) %>%
          mutate(SpaceColumn.2 = NA, .after = PrimaryTableCleaning.CountSeedSubjects.Change.Proportion) %>%
          mutate(SpaceColumn.3 = NA, .after = TableNormalization.CountSeedSubjects.Change.Proportion) %>%
          mutate(SpaceColumn.4 = NA, .after = SecondaryTableCleaning.CountSeedSubjects.Change.Proportion) %>%
          mutate(SpaceColumn.5 = NA, .after = RecordSubsumption.CountSeedSubjects.Change.Proportion)
  }



  ReportData.DataSetLevel <- CounterData %>%
                                pluck("DataSetLevel") %>%
                                PrepareReportData()

  ReportData.TableLevel <- CounterData %>%
                              pluck("TableLevel") %>%
                              list_rbind(names_to = "Table") %>%
                              filter(Table != ".DataSetRoot") %>%
                              arrange(Server, Table) %>%
                              PrepareReportData()

  ReportData.TableLevel.ByTable <- ReportData.TableLevel %>%
                                        split(.$Table) %>%
                                        map(\(X) X %>% select(-Table))

  ReportData.TableLevel.ByServer <- ReportData.TableLevel %>%
                                        relocate(Server, .after = last_col()) %>%
                                        split(.$Server)

  ReportData.Details <- CounterData %>%
                            pluck("Details")



  # Define default theme values and enable modification through arguments
  GetTableTheme <- function(Mod.Style = NULL,
                            Mod.HeaderStyle = NULL,
                            Mod.RowStyle = NULL)
  {
      Style <- list(fontFamily = "Inter, sans-serif",
                    fontSize = "14px",
                    fontVariantNumeric = "tabular-nums")

      HeaderStyle <- list(border = "none")

      RowStyle <- list(height = "40px")

      if (!is.null(Mod.Style)) Style <- Style %>% modifyList(Mod.Style)
      if (!is.null(Mod.HeaderStyle)) HeaderStyle <- HeaderStyle %>% modifyList(Mod.HeaderStyle)
      if (!is.null(Mod.RowStyle)) RowStyle <- RowStyle %>% modifyList(Mod.RowStyle)

      reactableTheme(headerStyle = HeaderStyle,
                     style = Style,
                     rowStyle = RowStyle,
                     groupHeaderStyle = list(background = dsFredaClient::FredaColors$DarkGrey,
                                             color = "#FFFFFF",
                                             borderBottom = "none"))
  }


  GetIcon <- function(Title, Category = NA)
  {
      IconName <- case_when(Category == "Records" ~ "bars",
                            Category == "RootSubjects" ~ "disease",
                            Category == "SeedSubjects" ~ "user-group",
                            .default = "bars")

      tags$span(title = Title, shiny::icon(name = IconName, lib = "font-awesome"))
  }


  ColDef.InitialValues <- function(Title, Category = NA, Level)
  {
      BackgroundColor <- case_when(Level == "TableLevel" ~ unname(ColorToRGBCSS(dsFredaClient::FredaColors$LightGrey, Alpha = 0.5)),
                                   Level == "SubtableLevel" ~ unname(ColorToRGBCSS(dsFredaClient::FredaColors$LightGrey, Alpha = 0.2)),
                                   .default = dsFredaClient::FredaColors$LightGrey)

      colDef(header = GetIcon(Title = Title, Category = Category),
             headerStyle = list(background = dsFredaClient::FredaColors$MediumGrey),
             cell = function(value) formatC(value, format = "d", big.mark = ","),
             style = list(background = BackgroundColor,
                          color = "#000000"))
  }


  ColDef.ChangeValues <- function(Title, Category = NA, Level, ReportData, DriverColumn)
  {
      colDef(header = GetIcon(Title = Title, Category = Category),
             headerStyle = list(background = dsFredaClient::FredaColors$LightGrey,
                                border = "none"),
             cell = function(value) { if (is.na(value)) { return("") }
                                      if (value == 0) { return("-") }
                                      FormattedValue <- formatC(abs(value), digits = 2, format = "d", big.mark = ",")
                                      if (value > 0) paste0("+ ", FormattedValue) else paste0("- ", FormattedValue) },
             style = function(value, index)
                     {
                        # Default colors
                        BackgroundColor <- case_when(Level == "TableLevel" ~ unname(ColorToRGBCSS(dsFredaClient::FredaColors$LightGrey, Alpha = 0.5)),
                                                     Level == "SubtableLevel" ~ unname(ColorToRGBCSS(dsFredaClient::FredaColors$LightGrey, Alpha = 0.2)),
                                                     .default = dsFredaClient::FredaColors$LightGrey)
                        ForegroundColor <- dsFredaClient::FredaColors$DarkGrey
                        # Get driver value
                        DriverValue <- ReportData[[DriverColumn]][index]
                        if (!is.null(DriverValue) &&!is.na(DriverValue) && DriverValue != 0) { BackgroundColor <- DecimalToColor(Decimal = DriverValue, Type = "Change") }

                        return(list(background = BackgroundColor,
                                    color = ForegroundColor))
                     })
  }


  ColDef.Space <- function(Width = 16)
  {
      colDef(name = "",
             width = Width,
             sortable = FALSE,
             style = list(background = "transparent", borderLeft = "none", borderRight = "none"),
             headerStyle = list(background = "transparent", borderLeft = "none", borderRight = "none"))
  }


  ColDef.FinalValues <- function(Title, Category = NA, Level)
  {
      BackgroundColor <- case_when(Level == "TableLevel" ~ unname(ColorToRGBCSS(dsFredaClient::FredaColors$Green, Alpha = 0.3)),
                                   Level == "SubtableLevel" ~ unname(ColorToRGBCSS(dsFredaClient::FredaColors$Green, Alpha = 0.15)),
                                   .default = unname(ColorToRGBCSS(dsFredaClient::FredaColors$Green, Alpha = 0.5)))

      colDef(header = GetIcon(Title = Title, Category = Category),
             headerStyle = list(background = unname(ColorToRGBCSS(dsFredaClient::FredaColors$Green, Alpha = 0.7))),
             cell = function(value) formatC(value, format = "d", big.mark = ","),
             style = list(background = BackgroundColor,
                          color = "#000000"))
  }


  GetDataBarSettings <- function(Level)
  {
      BarColor <- dsFredaClient::FredaColors$Green
      BackgroundColor <- dsFredaClient::FredaColors$MediumGrey
      if (Level == "TableLevel") { BarColor <- "#67BA67"
                                   BackgroundColor <- dsFredaClient::FredaColors$LightGrey }
      if (Level == "SubtableLevel") { BarColor <- "#92CE93"
                                      BackgroundColor <- dsFredaClient::FredaColors$LightGrey }

      list(min_value = 0,
           max_value = 1,
           fill_color = BarColor,
           background = BackgroundColor,
           text_color = "white",
           text_position = "center",
           text_size = 10,
           number_fmt = scales::label_percent(suffix = "%"),
           tooltip = TRUE,
           box_shadow = TRUE)
  }


  RowStyle.AllServers <- function(index)
  {
      if (ReportData.DataSetLevel$Server[index] == "All")
      {
          list(fontWeight = "800",
               borderTop = "2px solid #595959",
               borderBottom = "2px solid #595959")
      }
  }


  ColumnGroups <- list(colGroup(name = "Initial Counts",
                                columns = c("Initial.CountRecords",
                                            "Initial.CountRootSubjects",
                                            "Initial.CountSeedSubjects")),
                       colGroup(name = "Primary Table Cleaning",
                                columns = c("PrimaryTableCleaning.CountRecords.Change",
                                            "PrimaryTableCleaning.CountRootSubjects.Change",
                                            "PrimaryTableCleaning.CountSeedSubjects.Change")),
                       colGroup(name = "Table Normalization",
                                columns = c("TableNormalization.CountRecords.Change",
                                            "TableNormalization.CountRootSubjects.Change",
                                            "TableNormalization.CountSeedSubjects.Change")),
                       colGroup(name = "Secondary Table Cleaning",
                                columns = c("SecondaryTableCleaning.CountRecords.Change",
                                            "SecondaryTableCleaning.CountRootSubjects.Change",
                                            "SecondaryTableCleaning.CountSeedSubjects.Change")),
                       colGroup(name = "Record Subsumption",
                                columns = c("RecordSubsumption.CountRecords.Change",
                                            "RecordSubsumption.CountRootSubjects.Change",
                                            "RecordSubsumption.CountSeedSubjects.Change")),
                       colGroup(name = "Final Counts",
                                columns = c("Final.CountRecords",
                                            "Final.CountRecords.Proportion",
                                            "Final.CountRootSubjects",
                                            "Final.CountRootSubjects.Proportion",
                                            "Final.CountSeedSubjects",
                                            "Final.CountSeedSubjects.Proportion")))


  GetColumnDefinitions <- function(ReportData, Level = "DataSetLevel")
  {
      list(Initial.CountRecords = ColDef.InitialValues(Title = "Records", Category = "Records", Level = Level),
           Initial.CountRootSubjects = ColDef.InitialValues(Title = "Diagnoses", Category = "RootSubjects", Level = Level),
           Initial.CountSeedSubjects = ColDef.InitialValues(Title = "Patients", Category = "SeedSubjects", Level = Level),
           SpaceColumn.1 = ColDef.Space(16),
           PrimaryTableCleaning.CountRecords.Change = ColDef.ChangeValues(Title = "Records", Category = "Records", Level = Level, ReportData = ReportData, DriverColumn = "PrimaryTableCleaning.CountRecords.Change.Proportion"),
           PrimaryTableCleaning.CountRecords.Change.Proportion = colDef(show = FALSE),
           PrimaryTableCleaning.CountRootSubjects.Change = ColDef.ChangeValues(Title = "Diagnoses", Category = "RootSubjects", Level = Level, ReportData = ReportData, DriverColumn = "PrimaryTableCleaning.CountRootSubjects.Change.Proportion"),
           PrimaryTableCleaning.CountRootSubjects.Change.Proportion = colDef(show = FALSE),
           PrimaryTableCleaning.CountSeedSubjects.Change = ColDef.ChangeValues(Title = "Patients", Category = "SeedSubjects", Level = Level, ReportData = ReportData, DriverColumn = "PrimaryTableCleaning.CountSeedSubjects.Change.Proportion"),
           PrimaryTableCleaning.CountSeedSubjects.Change.Proportion = colDef(show = FALSE),
           SpaceColumn.2 = ColDef.Space(16),
           TableNormalization.CountRecords.Change = ColDef.ChangeValues(Title = "Records", Category = "Records", Level = Level, ReportData = ReportData, DriverColumn = "TableNormalization.CountRecords.Change.Proportion"),
           TableNormalization.CountRecords.Change.Proportion = colDef(show = FALSE),
           TableNormalization.CountRootSubjects.Change = ColDef.ChangeValues(Title = "Diagnoses", Category = "RootSubjects", Level = Level, ReportData = ReportData, DriverColumn = "TableNormalization.CountRootSubjects.Change.Proportion"),
           TableNormalization.CountRootSubjects.Change.Proportion = colDef(show = FALSE),
           TableNormalization.CountSeedSubjects.Change = ColDef.ChangeValues(Title = "Patients", Category = "SeedSubjects", Level = Level, ReportData = ReportData, DriverColumn = "TableNormalization.CountSeedSubjects.Change.Proportion"),
           TableNormalization.CountSeedSubjects.Change.Proportion = colDef(show = FALSE),
           SpaceColumn.3 = ColDef.Space(16),
           SecondaryTableCleaning.CountRecords.Change = ColDef.ChangeValues(Title = "Records", Category = "Records", Level = Level, ReportData = ReportData, DriverColumn = "SecondaryTableCleaning.CountRecords.Change.Proportion"),
           SecondaryTableCleaning.CountRecords.Change.Proportion = colDef(show = FALSE),
           SecondaryTableCleaning.CountRootSubjects.Change = ColDef.ChangeValues(Title = "Diagnoses", Category = "RootSubjects", Level = Level, ReportData = ReportData, DriverColumn = "SecondaryTableCleaning.CountRootSubjects.Change.Proportion"),
           SecondaryTableCleaning.CountRootSubjects.Change.Proportion = colDef(show = FALSE),
           SecondaryTableCleaning.CountSeedSubjects.Change = ColDef.ChangeValues(Title = "Patients", Category = "SeedSubjects", Level = Level, ReportData = ReportData, DriverColumn = "SecondaryTableCleaning.CountSeedSubjects.Change.Proportion"),
           SecondaryTableCleaning.CountSeedSubjects.Change.Proportion = colDef(show = FALSE),
           SpaceColumn.4 = ColDef.Space(16),
           RecordSubsumption.CountRecords.Change = ColDef.ChangeValues(Title = "Records", Category = "Records", Level = Level, ReportData = ReportData, DriverColumn = "RecordSubsumption.CountRecords.Change.Proportion"),
           RecordSubsumption.CountRecords.Change.Proportion = colDef(show = FALSE),
           RecordSubsumption.CountRootSubjects.Change = ColDef.ChangeValues(Title = "Diagnoses", Category = "RootSubjects", Level = Level, ReportData = ReportData, DriverColumn = "RecordSubsumption.CountRootSubjects.Change.Proportion"),
           RecordSubsumption.CountRootSubjects.Change.Proportion = colDef(show = FALSE),
           RecordSubsumption.CountSeedSubjects.Change = ColDef.ChangeValues(Title = "Patients", Category = "SeedSubjects", Level = Level, ReportData = ReportData, DriverColumn = "RecordSubsumption.CountSeedSubjects.Change.Proportion"),
           RecordSubsumption.CountSeedSubjects.Change.Proportion = colDef(show = FALSE),
           SpaceColumn.5 = ColDef.Space(16),
           Final.CountRecords = ColDef.FinalValues(Title = "Records", Category = "Records", Level = Level),
           Final.CountRecords.Proportion = colDef(name = "", cell = do.call(reactablefmtr::data_bars, c(list(data = ReportData), GetDataBarSettings(Level = Level)))),
           Final.CountRootSubjects = ColDef.FinalValues(Title = "Diagnoses", Category = "RootSubjects", Level = Level),
           Final.CountRootSubjects.Proportion = colDef(name = "", cell = do.call(reactablefmtr::data_bars, c(list(data = ReportData), GetDataBarSettings(Level = Level)))),
           Final.CountSeedSubjects = ColDef.FinalValues(Title = "Patients", Category = "SeedSubjects", Level = Level),
           Final.CountSeedSubjects.Proportion = colDef(name = "", cell = do.call(reactablefmtr::data_bars, c(list(data = ReportData), GetDataBarSettings(Level = Level)))))
  }


  # Main table
  Table <- reactable::reactable(data = ReportData.DataSetLevel,
                                pagination = FALSE,
                                defaultColDef = colDef(align = "center", vAlign = "center"),
                                borderless = TRUE,
                                columnGroups = ColumnGroups,
                                rowStyle = RowStyle.AllServers,
                                theme = GetTableTheme(),
                                onClick = "expand",
                                columns = c(GetColumnDefinitions(ReportData = ReportData.DataSetLevel, Level = "DataSetLevel"),
                                            list(Server = colDef(name = "Server",
                                                                 headerStyle = list(background = dsFredaClient::FredaColors$MediumGrey),
                                                                 width = 160,
                                                                 style = list(background = dsFredaClient::FredaColors$MediumGrey),
                                                                 details = function(index.DataSetLevel)
                                                                           {
                                                                              SelectedServer <- ReportData.DataSetLevel$Server[index.DataSetLevel]

                                                                              ReportData.TableLevel.SelectedServer <- ReportData.TableLevel.ByServer %>% pluck(SelectedServer)

                                                                              # --- Table-Level Summary table ---
                                                                              reactable(data = ReportData.TableLevel.SelectedServer,
                                                                                        pagination = FALSE,
                                                                                        defaultColDef = colDef(align = "center", vAlign = "center"),
                                                                                        borderless = TRUE,
                                                                                        theme = GetTableTheme(Mod.Style = list(fontSize = "12px"),
                                                                                                              Mod.HeaderStyle = list(display = "none"),
                                                                                                              Mod.RowStyle = list(height = "30px")),
                                                                                        onClick = "expand",
                                                                                        columns = c(GetColumnDefinitions(ReportData = ReportData.TableLevel.SelectedServer, Level = "TableLevel"),
                                                                                                    list(Server = colDef(show = FALSE),
                                                                                                         Table = colDef(width = 160,
                                                                                                                        style = list(background = unname(ColorToRGBCSS(dsFredaClient::FredaColors$MediumGrey, Alpha = 0.7))),
                                                                                                                        align = "left",
                                                                                                                        details = function(index.TableLevel)
                                                                                                                                  {
                                                                                                                                    SelectedTable <- ReportData.TableLevel.SelectedServer$Table[index.TableLevel]

                                                                                                                                    # --- For cumulated table-level summaries: On expand show table-specific summaries of different servers ---
                                                                                                                                    if (SelectedServer == "All")
                                                                                                                                    {
                                                                                                                                        ReportData.TableLevel.SelectedTable <- ReportData.TableLevel.ByTable %>%
                                                                                                                                                                                    pluck(SelectedTable)

                                                                                                                                        if (is.null(ReportData.TableLevel.SelectedTable))
                                                                                                                                        { return(NULL) } else { ReportData.TableLevel.SelectedTable <- ReportData.TableLevel.SelectedTable %>% filter(Server != "All") }

                                                                                                                                        return(reactable(data = ReportData.TableLevel.SelectedTable,
                                                                                                                                                         pagination = FALSE,
                                                                                                                                                         defaultColDef = colDef(align = "center", vAlign = "center"),
                                                                                                                                                         borderless = TRUE,
                                                                                                                                                         theme = GetTableTheme(Mod.Style = list(fontSize = "12px"),
                                                                                                                                                                               Mod.HeaderStyle = list(display = "none"),
                                                                                                                                                                               Mod.RowStyle = list(height = "30px")),
                                                                                                                                                         columns = c(GetColumnDefinitions(ReportData = ReportData.TableLevel.SelectedTable, Level = "SubtableLevel"),
                                                                                                                                                                     list(Server = colDef(width = 160,
                                                                                                                                                                                          style = list(background = unname(ColorToRGBCSS(dsFredaClient::FredaColors$MediumGrey, Alpha = 0.4))))))))

                                                                                                                                    # --- For server-specific table-level summaries: On expand show Counter details for selected Server and selected Table ---
                                                                                                                                    } else {

                                                                                                                                        ReportData.Details.SelectedServer.SelectedTable <- ReportData.Details %>%
                                                                                                                                                                                                pluck(SelectedServer, SelectedTable)

                                                                                                                                        if (is.null(ReportData.Details.SelectedServer.SelectedTable))
                                                                                                                                        { return(NULL) } else { ReportData.Details.SelectedServer.SelectedTable <- ReportData.Details.SelectedServer.SelectedTable %>% mutate(SpaceColumn.End = NA) }

                                                                                                                                        return(reactable(data = ReportData.Details.SelectedServer.SelectedTable,
                                                                                                                                                         pagination = FALSE,
                                                                                                                                                         defaultColDef = colDef(align = "center", vAlign = "center"),
                                                                                                                                                         borderless = TRUE,
                                                                                                                                                         fullWidth = TRUE,
                                                                                                                                                         theme = GetTableTheme(Mod.Style = list(fontSize = "11px"),
                                                                                                                                                                               Mod.HeaderStyle = list(background = dsFredaClient::FredaColors$PrimaryLight,
                                                                                                                                                                                                      color = "#FFFFFF"),
                                                                                                                                                                               Mod.RowStyle = list(height = "auto",
                                                                                                                                                                                                   borderBottom = "1px solid #D0D0D0")),
                                                                                                                                                         columns = list(Timestamp = colDef(show = FALSE),
                                                                                                                                                                        ProcessingStage = colDef(name = "Processing Stage",
                                                                                                                                                                                                 width = 150),
                                                                                                                                                                        ProcessTopic = colDef(name = "Topic",
                                                                                                                                                                                              width = 150),
                                                                                                                                                                        ProcessTopic.Subgroup = colDef(name = "Subtopic",
                                                                                                                                                                                                       width = 200),
                                                                                                                                                                        MessageClass = colDef(show = FALSE),
                                                                                                                                                                        Message = colDef(show = FALSE),
                                                                                                                                                                        CountRecords.Removed = colDef(name = "Removed Records",
                                                                                                                                                                                                      width = 150),
                                                                                                                                                                        CountRecords.Added = colDef(name = "Added Records",
                                                                                                                                                                                                    width = 150),
                                                                                                                                                                        CountRootSubjects.Affected = colDef(name = "Affected Diagnoses",
                                                                                                                                                                                                            width = 150),
                                                                                                                                                                        CountSeedSubjects.Affected = colDef(name = "Affected Patients",
                                                                                                                                                                                                            width = 150),
                                                                                                                                                                        SpaceColumn.End = colDef(name = ""))))
                                                                                                                                    }

                                                                                                                                  }))))

                                                                           }))))

#-------------------------------------------------------------------------------
  return(Table)
}

