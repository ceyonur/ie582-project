
# 
# TrainFeat = copy(TrainFeatures)
# TestFeat = copy(TestFeatures)
# ScenarioData <- ScenarioData[1:5]
# FullData <- copy(Matches)
# i <- 52

runScenarios <- function(ScenarioData, FullData, Seed = 5){
  
  stopifnot(!is.null(ScenarioData), !is.null(nrow(ScenarioData)), nrow(ScenarioData) > 0)
  print(paste0(nrow(ScenarioData)," scenarios will be running... Time: ",Sys.time()))
  
  for(i in 1:nrow(ScenarioData)){
    Data <- copy(FullData)
    ScenarioId <- ScenarioData[i, ScenarioId]
    
    print("--------------------")
    print(paste0("Scenario ",ScenarioId," is in progress... Time: ",Sys.time()))
    Method <- ScenarioData[i, Method]
    
    #glmnet
    Alpha <- ScenarioData[i, Alpha]
    Alpha <- ifelse(is.na(Alpha),1,Alpha)
    Lambda <- ScenarioData[i, Lambda]
    Lambda <- ifelse(is.na(Lambda),"lambda.min",Lambda)
    
    #random forest and ordfor
    nTrees <- ScenarioData[i, nTrees]
    NodeSize <- ScenarioData[i, NodeSize]
    VariableSelectionRatio <- ScenarioData[i, VariableSelectionRatio]
    
    Normalize <- ScenarioData[i, Normalize]
    
    Rv <- ScenarioData[i, RV]
    
    AllFeat <- ScenarioData[i, AllFeat]
    AllFeat <- ifelse(is.na(AllFeat),"0","1")
    
    FeatureNames <- grep(pattern = "^F\\d+", x = names(ScenarioData), value = T)
    FeatureNames <- as.character(ScenarioData[i, FeatureNames, with = F])
    FeatureNames <- FeatureNames[FeatureNames != "NA" & !is.na(FeatureNames)]
    FeatureNames <- unique(FeatureNames)
    
    if((AllFeat == "1") | (length(FeatureNames) == 0)){
      FeatureNames <- FeatureCols
    }
    
    setnames(Data,Rv,"RV")
    Data <- Data[,c("M_id","RV",FeatureNames), with = F]
    ScenarioResults <- NULL
    VarImp <- data.table(Feature = setdiff(names(Data),c("RV","M_id")))
    
    if(Method == "multinom"){
      VarImp <- NULL
    }
    # split <- 1
    
    for(split in 1:3){
      
      Train <- copy(Data[M_id %in% Train_M_ids_1])
      Test <- copy(Data[!(M_id %in% Train_M_ids_1)])
      
      TrainBase <- Train[,.(M_id,RV)]
      TestBase <- Test[,.(M_id,RV)]
      Train[,M_id := NULL]
      Test[,c("M_id","RV") := NULL]
      # scale data in range 0-1
      if(is.na(Normalize) == F){
        
        norm <- Normalize_Fit(Data = Train, cols = ColumnTypes(Train)[Type %in% c("integer","numeric"),Name])
        Normalize_Predict(Data = Train,Normalizer = norm)
        Normalize_Predict(Data = Test,Normalizer = norm)
        
      }
      
      #ordinal logistic regression
      #### polr ####
      if(Method == "polr"){
        Fit <- polr(RV ~ ., data = Train,method = "logistic")
        TempTrainPredict <- as.data.table(cbind(TrainBase,as.character(split),"Train",predict(object = Fit,newdata = Train, type = "probs")))
        setnames(TempTrainPredict,names(TempTrainPredict)[2:4],c(Rv,"Split","TrainTest"))
        
        TempTestPredict <- as.data.table(cbind(TestBase,as.character(split),"Test",predict(object = Fit,newdata = Test, type = "probs")))
        setnames(TempTestPredict,names(TempTestPredict)[2:4],c(Rv,"Split","TrainTest"))
        
        TempVarImp <- data.table(Feature = names(Fit$coefficients),Coefficients = Fit$coefficients)
        setnames(TempVarImp,"Coefficients",paste0("Coefficients_",split))
        
        VarImp <- merge(x = VarImp, y = TempVarImp,by = "Feature", all = T)
      }
      
      #### multinom ####
      if(Method == "multinom"){
        
        Fit <- multinom(formula = RV ~ ., data = Train)
        TempTrainPredict <- as.data.table(cbind(TrainBase,as.character(split),"Train",predict(object = Fit,newdata = Train, type = "probs")))
        setnames(TempTrainPredict,names(TempTrainPredict)[2:4],c(Rv,"Split","TrainTest"))
        
        TempTestPredict <- as.data.table(cbind(TestBase,as.character(split),"Test",predict(object = Fit,newdata = Test, type = "probs")))
        setnames(TempTestPredict,names(TempTestPredict)[2:4],c(Rv,"Split","TrainTest"))
        
        TempVarImp <- as.data.table(summary(Fit)$coefficients)
        TempVarImp[,`:=`(Split = split, Model = rownames(summary(Fit)$coefficients))]
        
        VarImp <- rbind(VarImp,TempVarImp, fill = T)
      }
      
      # glmnet VarImp missing
      #### glmnet ####
      if(Method == "glmnet"){
        
        x=model.matrix(RV~.,data=Train)
        y=Train[,RV]
        
        Fit <- cv.glmnet(x = x,y =  y, alpha = Alpha, family = "multinomial")
        
        TempTrainPredict <- as.data.table(cbind(TrainBase,as.character(split),"Train",
                                  matrix(data = predict(object = Fit, newx = x, s = Lambda,type="response"),ncol = 3)))
        setnames(TempTrainPredict,c("M_id",Rv,"Split","TrainTest",levels(TrainBase[,RV])))
        
        x <- model.matrix(~.,data=Test)
        TempTestPredict <- as.data.table(cbind(TestBase,as.character(split),"Test",
                                                matrix(data = predict(object = Fit, newx = x, s = Lambda,type="response"),ncol = 3)))
        setnames(TempTestPredict,c("M_id",Rv,"Split","TrainTest",levels(TrainBase[,RV])))
        
      }
      
      #### rforest ####
      if(Method == "rforest"){
        
        Mtry <- ceiling((ncol(Train) - 1) * VariableSelectionRatio)
        
        # if some special characters in train column names are present, randomForest doesnt like them
        FactorColNames <- ColumnTypes(Train)[Type == "factor",Name]
        FactorColNames_Old <- copy(FactorColNames)
        FactorColNames <- gsub(pattern = "\\.",replacement = "_",x = FactorColNames)
        FactorColNames <- gsub(pattern = "\\+",replacement = "",x = FactorColNames)
        FactorColNames <- gsub(pattern = "\\(",replacement = "",x = FactorColNames)
        FactorColNames <- gsub(pattern = "\\[",replacement = "",x = FactorColNames)
        FactorColNames <- gsub(pattern = "\\]",replacement = "",x = FactorColNames)
        FactorColNames <- gsub(pattern = ",",replacement = "_",x = FactorColNames)
        
        setnames(Train, FactorColNames_Old,FactorColNames)
        setnames(Test, FactorColNames_Old,FactorColNames)
        
        Fit <- randomForest(RV ~., data = Train,ntree =  nTrees, nodesize = NodeSize, 
                     mtry = Mtry,keep.forest = T,check.names = T)
        
        TempTrainPredict <- as.data.table(cbind(TrainBase,as.character(split),"Train",predict(object = Fit,newdata = Train, type = "prob")))
        setnames(TempTrainPredict,names(TempTrainPredict)[2:4],c(Rv,"Split","TrainTest"))
        
        TempTestPredict <- as.data.table(cbind(TestBase,as.character(split),"Test",predict(object = Fit,newdata = Test, type = "prob")))
        setnames(TempTestPredict,names(TempTestPredict)[2:4],c(Rv,"Split","TrainTest"))
        
        TempVarImp <- data.table(Feature = rownames(as.matrix(Fit$importance)), MeanDecreaseGini = as.matrix(Fit$importance)[,1] )
        setnames(TempVarImp,"MeanDecreaseGini",paste0("MeanDecreaseGini_",split))
        
        VarImp <- merge(x = VarImp, y = TempVarImp,by = "Feature", all = T)
      } 
      
      #### ordfor ####
      if(Method == "ordfor"){
        
        Mtry <- ceiling((ncol(Train) - 1) * VariableSelectionRatio)
        nTrees <- 1000
        Fit <- ordfor(depvar = "RV",data = Train,min.node.size = NodeSize,mtry = Mtry, ntreefinal = nTrees,
                      nsets = 500,ntreeperdiv = 50)
        
        TempTrainPredict <- as.data.table(cbind(TrainBase,as.character(split),"Train",predict(object = Fit,newdata = Train,type = "prob")$classfreqtree))
        setnames(TempTrainPredict,names(TempTrainPredict)[2:4],c(Rv,"Split","TrainTest"))
        
        TempTestPredict <- as.data.table(cbind(TestBase,as.character(split),"Test",predict(object = Fit,newdata = Test,type = "prob")$classfreqtree))
        setnames(TempTestPredict,names(TempTestPredict)[2:4],c(Rv,"Split","TrainTest"))
        
        TempVarImp <- data.table(Feature = names(Fit$varimp),Importance = Fit$varimp)
        setnames(TempVarImp,"Importance",paste0("Importance_",split))
        
        VarImp <- merge(x = VarImp, y = TempVarImp,by = "Feature", all = T)
        
      }
      
      ScenarioResults <- rbind(ScenarioResults,TempTrainPredict,TempTestPredict)
      
      print(paste0("Scenario ",ScenarioId,", split ",split," is done... Time: ",Sys.time()))
      # split for ends
    }
    save(ScenarioResults,VarImp,Fit,Method,file = paste0(FitDataPath,"Fit_",ScenarioId,".rdata"))
    print(paste0("Scenario ",ScenarioId," is done... Time: ",Sys.time()))
    # scenario for ends
  }
  # function ends
}
  
  