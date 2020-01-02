rm(list = ls())

require(data.table)
require(readxl)
require(glmnet)
require(stats)
require(randomForest)
require(nnet)
require(MASS)
require(ordinalForest)
# library(gsubfn)

setDTthreads(threads = 0)

source("C:/Users/baris.isik/Desktop/Master/IE 582/Project/Codes/parameters.r")

colnames <- names(as.data.table(read_excel(paste0(CleanDataPath,"Scenarios.xlsx"))))
ScenarioData <- as.data.table(read_excel(paste0(CleanDataPath,"Scenarios.xlsx"),skip = 2,col_names = F))

colnames <- colnames[1:ncol(ScenarioData)]
setnames(ScenarioData,colnames)

load(paste0(CleanDataPath,"TrainInput.rdata"))
source(paste0(CodePath,"runScenarios.R"))


runScenarios(ScenarioData = ScenarioData[ScenarioId %in% 55:max(ScenarioData[,ScenarioId])],
             FullData = Matches
)

# for(scn in 2:max(as.numeric(ScenarioData[-1,ScenarioId]))){
#   # for(scn in 215:274){
#   runScenarios(ScenarioData = ScenarioData[ScenarioId == scn],
#                TrainFeat = TrainFeatures,
#                TestFeat = TestFeatures
#   )
# }



