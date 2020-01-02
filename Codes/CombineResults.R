rm(list = ls())
require(data.table)
source("C:/Users/baris.isik/Desktop/Master/IE 582/Project/Codes/parameters.r")

Files <- setdiff(list.files(paste0(FitDataPath)),"ResultsSummary.rdata")

Files <- unlist(strsplit(x = unlist(strsplit(x = Files, split = "_")),split = "\\."))
Scenarios <- Files[grepl(x = unlist(strsplit(x = unlist(strsplit(x = Files, split = "_")),split = "\\.")), pattern = "\\d")]
# Scenarios <- as.integer(Scenarios)

if(file.exists(paste0(FitDataPath,"ResultSummary.rdata"))){
  load(paste0(FitDataPath,"ResultSummary.rdata"))
  OldScenarios <- unique(as.character(ResultSummary[,ScenarioId]))
  Scenarios <- setdiff(Scenarios,OldScenarios)
}else{
  ResultSummary <- NULL
}

Scenarios <- sort(as.integer(Scenarios))

i <- 1
RPS_matrix<- function(probs,outcomes){
  probs=as.matrix(probs)
  outcomes=as.matrix(outcomes)
  probs=t(apply(t(probs), 2, cumsum))
  outcomes=t(apply(t(outcomes), 2, cumsum))
  RPS = apply((probs-outcomes)^2,1,sum) / (ncol(probs)-1)
  return(RPS)
}

RPS_model<- function(home,draw,away,outcome){
  probs = cbind(home,draw,away)
  outcome = as.integer(outcome)
  outcome_h = as.integer(outcome == 1)
  outcome_d = as.integer(outcome == 2)
  outcome_a = as.integer(outcome == 3)
  outcomes = cbind(outcome_h, outcome_d,outcome_a)
  return(RPS_matrix(probs,outcomes))
}

for(i in Scenarios){
  
  load(paste0(FitDataPath,"Fit_",i,".rdata"))
  
  ScenarioResults[,Rps := RPS_model(home = HomeWin, draw = Draw, away = AwayWin,outcome = Outcome)]
  
  ScenarioResults <- ScenarioResults[,.(Rps = mean(Rps)), by = .(TrainTest)]
  ScenarioResults[, ScenarioId := i]
  
  ScenarioResults <- dcast(data = ScenarioResults, formula = ScenarioId ~ TrainTest, value.var = "Rps")
  ScenarioResults[, Model := Method]
  ScenarioResults[,Rps_Diff := Train - Test]
  setnames(ScenarioResults,c("Train","Test"),c("Rps_Train","Rps_Test"))
  setcolorder(ScenarioResults,c("ScenarioId","Model","Rps_Train","Rps_Test","Rps_Diff"))
  ResultSummary <- rbind(ResultSummary, ScenarioResults)
  print(paste0("Scenario ",i," is done."))
}

save(ResultSummary,file = paste0(FitDataPath,"ResultSummary.rdata"))

