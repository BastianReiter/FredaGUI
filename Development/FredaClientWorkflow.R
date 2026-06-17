
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   - Virtual DataSHIELD infrastructure for testing purposes -
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load required packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(dsBaseClient)
library(dsFredaClient)
library(dsCCPhosClient)
library(dsTidyverseClient)
# library(resourcer)

# Print DataSHIELD errors right away
options(datashield.errors.print = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Establish Connections to virtual servers using dsCCPhosClient::ConnectToVirtualCCP()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TestData <- readRDS("../Data/CCP/CCPTestData2024.rds")


CCPConnections <- ConnectToVirtualCCP(CCPData = TestData,
                                      NumberOfServers = 3,
                                      NumberOfPatientsPerServer = 1000,
                                      AddedDsPackages = "dsTidyverse")
                                      #Resources = list(TestResource = TestResource))

# Display app in Viewer pane
# options(shiny.launch.browser = .rs.invokeShinyPaneViewer)

# BgProcess <- StartCCPhosApp(RunAutonomously = TRUE)


# BgProcess$is_alive()
# BgProcess$read_error()
# BgProcess$kill()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Raw Data Set (RDS) from Opal data base to R sessions on servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CCP.LoadRawDataSet(ServerSpecifications = NULL)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check RDS tables for existence and completeness
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RDSTableCheck <- dsFredaClient::ds.GetDataSetCheck(DataSetName = "CCP.RawDataSet",
                                                   Module = "CCP",
                                                   Stage = "Raw")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Optionally: Draw random sample from Raw Data Set on servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ds.CCP.DrawSample(RawDataSetName = "CCP.RawDataSet",
#                   SampleSize = 1000,
#                   SampleName = "RDSSample")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transform Raw Data Set (RDS) into Curated Data Set (CDS)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Transform Raw Data Set (RDS) into Curated Data Set (CDS) (using default settings)
ds.CurateData(RawDataSetName = "CCP.RawDataSet",
              Module = "CCP",
              OutputName = "CCP.CurationOutput")

CDSTableCheck <- ds.GetDataSetCheck(DataSetName = "CCP.CuratedDataSet",
                                    Module = "CCP",
                                    Stage = "Curated")

Widget.DataSetCheck(RDSCheckData = RDSTableCheck,
                    CDSCheckData = CDSTableCheck)

# Get curation reports
CurationReport <- ds.GetCurationReport(Module = "CCP")

#CurationReport <- readRDS("../CCP.PanCancerDFCI/CCP/Reports/CurationReport_20260526.rds")

Widget.CurationReport(Module = "CCP",
                      CurationReport = CurationReport,
                      RunAutonomously = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transform Curated Data Set (CDS) into Augmented Data Set (ADS)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Run ds.AugmentData
ds.CCP.AugmentData(CuratedDataSetName = "CCP.CuratedDataSet")


ADSTableCheck <- ds.CheckDataSet(DataSetName = "AugmentedDataSet")




#Widget.ProcessingMonitor(UseVirtualConnections = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get overview of objects in server workspaces
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Using dsCCPhosClient::GetServerWorkspaceInfo() and dsCCPhosClient::ds.GetObjectMetaData()
#-------------------------------------------------------------------------------

# Collect comprehensive information about all workspace objects
ServerWorkspaceInfo <- GetServerWorkspaceInfo()


Exploration <-dsFredaClient::GetExplorationData(OrderList = list(CCP.ADS.Diagnosis = c("ICD10Code",
                                                                                       "ICDOTopographyCode",
                                                                                       "LocalizationSide",
                                                                                       "ICDOMorphologyCode",
                                                                                       "Grading",
                                                                                       "UICCStage",
                                                                                       "UICCStageCategory",
                                                                                       "TNM.T",
                                                                                       "TNM.N",
                                                                                       "TNM.M",
                                                                                       "PatientAgeAtDiagnosis",
                                                                                       "TimeDiagnosisToDeath",
                                                                                       "TimeFollowUp"),
                                                                 CCP.ADS.Patient = c("Sex",
                                                                                     "LastVitalStatus",
                                                                                     "CausesOfDeath",
                                                                                     "CountDiagnoses")))


Widget.ServerExplorer(ServerWorkspaceInfo = ServerWorkspaceInfo,
                              ExplorationData = Exploration,
                              EnableLiveConnection = FALSE,
                              RunAutonomously = TRUE,
                              UseVirtualConnections = FALSE)

#Proc$read_error()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Process ADS tables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data Exploration
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Perform exemplary analyses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Log out from all servers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DSI::datashield.logout(CCPConnections)



