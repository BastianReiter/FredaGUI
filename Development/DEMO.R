
#===============================================================================
#   - Virtual DataSHIELD infrastructure for Demo purposes -
#===============================================================================

library(Freda)

#-------------------------------------------------------------------------------
# Load CCP test data as Source Data Set
#-------------------------------------------------------------------------------

CCP.SourceDataSet <- readRDS("../Data/CCP/CCPTestData2026.rds")

CCP.FREDAModule <- MakeFREDAModule(ModuleIdentifier = "CCP",
                                   ExcelFilePath = file.path("C:", "ARBEIT Lokal", "dsCCPhos", "Development", "Data", "ModuleData", "FREDAModule_CCP.xlsx"))

#-------------------------------------------------------------------------------
# Check Tables for existence and completeness
#-------------------------------------------------------------------------------

SDSTableCheck <- GetDataSetCheck(DataSet = CCP.SourceDataSet,
                                 FREDAModule = CCP.FREDAModule,
                                 Stage = "Source")


#-------------------------------------------------------------------------------
# Refine data
#-------------------------------------------------------------------------------

RefinementOutput <- RefineData(SourceDataSet = CCP.SourceDataSet,
                               FREDAModule = CCP.FREDAModule)

CCP.RefinedDataSet <- RefinementOutput$DataSet

FredaGUI::Widget.RefinementReport(RefinementReport = Freda::ProcessRefinementReports(list(Test = RefinementOutput$Report)))


RDSTableCheck <- GetDataSetCheck(DataSet = CCP.RefinedDataSet,
                                 FREDAModule = CCP.FREDAModule,
                                 Stage = "Refined")

FredaGUI::Widget.DataSetCheck(SDSCheckData = Freda::AggregateDataSetChecks(list(Test = SDSTableCheck)),
                              RDSCheckData = Freda::AggregateDataSetChecks(list(Test = RDSTableCheck)))

