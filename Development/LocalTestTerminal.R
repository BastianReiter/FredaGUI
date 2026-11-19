
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#      FredaGUI Test Terminal
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


TestData <- readRDS("../dsCCPhos/Development/Data/TestData/CCPTestData.rds")

RefinementReport <- readRDS(file = "C:/Users/Basti/OneDrive/ARBEIT/IDMKD/Projekte/CCP/CDSG Projekte/EvaSchaefer/RefinementReport_20250731.rds")
#SDSTableCheck <- readRDS(file = "C:/Users/Basti/OneDrive/ARBEIT/IDMKD/Projekte/CCP/CDSG Projekte/EvaSchaefer/SDSTableCheck_20250731.rds")
RDSTableCheck <- readRDS(file = "C:/Users/Basti/OneDrive/ARBEIT/IDMKD/Projekte/CCP/CDSG Projekte/EvaSchaefer/RDSTableCheck_20250731.rds")
EDSTableCheck <- readRDS(file = "C:/Users/Basti/OneDrive/ARBEIT/IDMKD/Projekte/CCP/CDSG Projekte/EvaSchaefer/EDSTableCheck_20250731.rds")




StartFredaApp(TestData = TestData,
              RunAutonomously = FALSE)
               # SDSTableCheckData = SDSTableCheck
               # EDSTableCheckData = EDSTableCheck,
               # RefinementReportData = RefinementReport)



ServerExplorer()



#TestData_Frankfurt <- readRDS("../dsCCPhos/Development/Data/RealData/CCPRealData_Frankfurt.rds")
#TestData_WithEmptyTables <- readRDS("../dsCCPhos/Development/Data/TestData/CCPTestData_WithEmptyTables.rds")

#SiteSpecifications <- dsCCPhosClient::CCPSiteSpecifications




library(dsCCPhosClient)

TestData <- readRDS("../dsCCPhos/Development/Data/RealData/CCPRealData_Frankfurt.rds")

CCPConnections <- ConnectToVirtualCCP(CCPTestData = TestData,
                                      NumberOfSites = 3,
                                      NumberOfPatientsPerSite = 1000)

Messages_ServerRequirements <- CheckServerRequirements(DataSources = CCPConnections)

LoadSourceDataSet(DataSources = CCPConnections)

WorkspaceInfo <- GetServerWorkspaceInfo(DataSources = CCPConnections)



