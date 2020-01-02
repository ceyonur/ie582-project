#testing comment.
# library("RODBC")
# require("data.table")
# library(zoo)
# library(cluster)
# library(readxl)


TimeToNum <- function(x){
  
  return(as.integer(paste0(sprintf("%02d",year(x)), sprintf("%02d", month(x)),sprintf("%02d",mday(x)),sprintf("%02d",hour(x)))))
  
}


num2date<-function(x) {
  y=floor(x/10000)
  m=round(x %% 10000 ,-2)/100
  d=x %% 100
  return(as.Date(ISOdate(y,m,d)))
}

date2num<-function(x) {
  x=as.character(x)
  y=as.integer(substr(x,1,4))*10000
  m=as.integer(substr(x,6,7))*100
  d=as.integer(substr(x,9,10))
  return(y+m+d)
}

string2num<-function(x){
  b<-which(strsplit(x, "")[[1]]=="/")
  y=as.integer(substring(x,b[2]+1,nchar(x)))*10000
  m=as.integer(substring(x,1,b[1]-1)) *100
  d=as.integer(substring(x,b[1]+1,b[2]-1))
  return(y+m+d)
}

# csv'den veri yuklendiginde ilk kolon ismindeki abukluk icin
fixFirstName<-function(x){
  fcn<-names(x)[1]
  setnames(x, c(fcn),c(substr(fcn,4,nchar(fcn))))
  return(x)
}




accuracy=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  AD=abs(actual-mean)
  R2=1-sum(error^2)/sum((actual-mean)^2)
  #AdjR2=1-(1-R2)*(n-1)/(n-k-1)
  DB=sum(diff(error)^2)/sum(error^2)
  #FE=sqrt(sum(error^2)/(n-k))
  FBias=sum(error)/sum(actual)
  MPE=sum(error/actual)/n
  MAPE=sum(abs(error/actual))/n
  RMSE=sqrt(sum(error^2)/n)
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  MASE=MAD/mean(abs(diff(actual)))
  RAE= sum(abs(error))/sum(abs(actual-mean))
  WMAPE=MAD/mean
  #l=data.frame(n,mean,sd,CV,AD,R2,DB,FBias,MPE,MAPE,RMSE,MAD,MADP,MASE,RAE,WMAPE,error)
  l=data.frame(n,mean,sd,CV,R2,DB,FBias,MPE,MAPE,RMSE,MAD,MADP,MASE,RAE,WMAPE)
  return(l)
}

CountNA<-function(x){
  x<-as.data.table(x)
  y<-0
  for(i in names(x)){
    y<-y+x[,sum(is.na(get(i))==T)]
  }
  return(y)
}


CountNAforeach<-function(x){
  x<-as.data.table(x)
  for(i in names(x)){
    y<-0
    y<-y+x[,sum(is.na(get(i))==T)]
    print(paste0(y," NAs in ",i))
  }
}

ChangeNA<-function(x,y){
  x<-as.data.table(x)
  y<-as.numeric(y)
  for(i in names(x)){
    x[is.na(get(i))==T,(i):=y]
  }
  return(x)
}

ChangeChar<-function(x,y,z){
  x<-as.data.table(x)
  y<-as.character(y)
  z<-as.character(z)
  for(i in names(x)){
    x[get(i)==y,(i):=z]
  }
}

RoundAll<-function(x,y){
  x<-as.data.table(x)
  y<-as.numeric(y)
  for(i in names(x)){
    x[,(i):=round(get(i),y)]
  }
}

ChangeAlltoNumeric<-function(x){
  x<-as.data.table(x)
  for(i in names(x)){
    x[,(i):=as.numeric(get(i))]
  }
}

clusterCiz<- function(series,clus){
  d=series
  nofcluster=max(clus$clustering)
  par(mfrow=c(2,ceiling(nofcluster/2)))
  ylim<-max(series)
  for(i in 1:nofcluster){
    ind=which(clus$clustering==i)
    if(length(ind)>0){
      lim=max(abs(d[ind,]),na.rm=T)
      # plot(d[ind[i],],type='l',col=1,ylim=c(0,1.1*lim),main=paste('Cluster ',i),ylab='Satis')
      plot(d[ind[i],],type='l',col=1,ylim=c(-3,ylim),main=paste('Cluster ',i),ylab='Deger')
      for(t in 2:length(ind)){
        points(d[ind[t],],type='l',col=1)
        
      }
      proto=apply(as.matrix(d[ind,]),2,mean,na.rm=T)
      points(proto,type='l',col=2,lwd=2,lty=2)
      c<-(d[ind[t]])
      
    }
  }
}



F_Cluster<-function(senaryo,nofcluster=4,Cizdir=T,Kaydet=F,HangiYil=2016){
  # senaryo=93
  # nofcluster=4
  # HangiYil=2016
  #
  load(paste0(DataPath,"FocusSatis_DB.rdata"))
  Senaryo<-as.data.table(read_excel(paste0(DataPath,"Senaryolar.xlsx")))
  
  AggrId<-Senaryo[SenaryoId==senaryo,.(AggrId)]
  
  #   if(file.exists(paste0(ClusterPath,"Clusters_AggrId_",AggrId,".csv"))){
  #     print(sprintf('%s senaryosu icin cluster mevcut...',senaryo))
  #     return()
  #   }
  
  AggVariables<-as.vector(as.matrix(Senaryo[SenaryoId==senaryo,.(V1,V2,V3,V4,V5,V6,V7,V8)]))
  AggVariables<-AggVariables[is.na(AggVariables)==F]
  # arabalarda degisik hiyerarsilerde olabilir ama her zaman ulke bazinda aggregation
  AggVariables<-AggVariables[AggVariables!="B_BayiKodu"]
  UrunVariables<-AggVariables
  #excelde T_YilAy yok
  AggVariables<-c(AggVariables,"T_YilAy")
  
  
  #
  UrunMaster<-fread(paste0(DataPath,"UrunMaster.csv"))
  UrunMaster<-UrunMaster[!(U_Paket=="RS" | U_Paket=="ST LINE") ,]
  UrunMaster[,`:=`(U_Kapi=as.factor(U_Kapi),U_Sanziman=as.factor(U_Sanziman),
                   U_Paket=as.factor(U_Paket),U_YakitTipi=as.factor(U_YakitTipi),U_MotorHacmi=as.factor(U_MotorHacmi))]
  
  setkey(Satis,U_SasiNo)
  setkey(UrunMaster,U_SasiNo)
  
  ##
  Satis<-UrunMaster[Satis,,nomatch=0]
  
  # Aya cikiliyor, urun ana veri hazirlaniyor
  AylikBazArac<-Satis[T_Yil==HangiYil,.(Satis=.N),by=AggVariables]
  Urun<-unique(AylikBazArac[,UrunVariables,with=F])
  Urun[,Key:=.I]
  #   eval(parse(text=paste("setkey(Urun,",paste(UrunVariables,collapse = ','),")",sep=' ')))
  #   eval(parse(text=paste("setkey(AylikBazArac,",paste(UrunVariables,collapse = ','),")",sep=' ')))
  setkeyv(Urun,UrunVariables)
  setkeyv(AylikBazArac,UrunVariables)
  
  AylikBazArac<-AylikBazArac[Urun][,.(Key,T_YilAy,Satis)]
  
  setorder(AylikBazArac,Key,T_YilAy)
  
  AylikBazArac<-reshape(AylikBazArac, idvar = "Key", timevar = "T_YilAy", direction = "wide")
  AylikBazArac<-ChangeNA(AylikBazArac,0)
  
  Key<-AylikBazArac$Key
  AylikBazArac[,Key:=NULL]
  AylikBazArac<-as.matrix(AylikBazArac)
  rownames(AylikBazArac)<-Key
  
  
  distMatrix=dist(AylikBazArac)
  clus=pam(distMatrix,nofcluster)
  
  if(Cizdir==T){
    # kac cluster olmali buradan incelenecek
    mydata <- AylikBazArac
    wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
    for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                         centers=i)$withinss)
    plot(1:15, wss, type="b", xlab="Number of Clusters",
         ylab="Within groups sum of squares")
    # cluster ciz
    clusterCiz(AylikBazArac,clus)
  }
  
  if(Kaydet==T){
    clus<-data.table(cbind(Key=Key,Cluster=as.integer(clus$clustering)))
    setkey(Urun,Key)
    setkey(clus,Key)
    clus<-clus[Urun]
    clus[,AggrId:=AggrId]
    
    write.csv2(x = clus, file = paste0(ClusterPath,"Clusters_AggrId_",AggrId,".csv"),row.names=F)
    save(clus, file = paste0(ClusterPath,"Clusters_AggrId_",AggrId,".rdata"))
  }
  
}

NormalizeColumns <- function(Data,Cols){
  
  TempData <- copy(Data)
  for(i in Cols){
    TempData[,(i):= (get(i)-mean(Data[,get(i)],na.rm = T))/sd(Data[,get(i)],na.rm = T) ]
  }
  
  return(TempData)
}

fieldMatch <- function(x, y){
  ux <- uniqueN(x)
  uy <- uniqueN(y)
  print(paste0("Unique X: ", ux, " - Unique Y: ", uy))
  print(paste0("Union (X+Y): ", uniqueN(union(x,y))))
  xy <- uniqueN(setdiff(x,y))
  print(paste0("X-Y: ", xy, " (", 100*xy/ux,"% of X)"))
  yx <- uniqueN(setdiff(y,x))
  print(paste0("Y-X: ", yx, " (", 100*yx/uy,"% of Y)"))
  print(paste0("Intersection: ", ux-xy, " (", 100*(ux-xy)/ux, "% of X - ", 100*(uy-yx)/uy, "% of Y)"))
}

CutIntervals <- function(x){
  
  x <- as.numeric(x)*10
  cuts <- seq(from=1.5,500.5,by=1)
  cuts <- c(0,cuts)
  cuts <- cuts*10
  
  return(as.numeric(cut(x = x,breaks = cuts)))
}

Entropy <- function(cat.vect){
  px  = table(cat.vect)/length(cat.vect)
  lpx = log(px, base=2)
  ent = -sum(px*lpx)
  return(ent)
}

ReverseEntropy <- function(cat.vect){
  px   = table(cat.vect)/length(cat.vect)
  spx2 = sum(px^2)
  return(spx2)
}

ColumnTypes <- function(Data = NULL){
  
  require(data.table)
  return(data.table(Name = names(Data),Type= unlist(sapply(Data,function(x){
    return(class(x)[1])
  }),use.names = F)))
}

TransformFactors <- function(InputData = NULL, RV = NULL){
  
  Data <- copy(InputData)
  ColTypes <- ColumnTypes(Data)[!(Name %in% RV) & Type %in% c("factor","ordered")]
  i <- ColTypes[,Name][1]
  for(i in ColTypes[,Name]){
    levels <- levels(Data[,get(i)])
    for(j in levels){
      Data[,(paste0(i,".",j))  := factor(ifelse(get(i) == j,"1","0"))]
    }
    Data[,(i):=NULL]
  }
  return(Data)
}

as.h2o_KeepColNames <- function(Data = NULL){
  ColNames <- copy(names(Data))
  Temp <- as.h2o(copy(Data))
  for(i in seq_along(ColNames)){
    names(Temp)[i] = ColNames[i]
  }
  return(Temp)
}

# string <- "ýiðgÐGüuÜUþsÞSÝIçcÇCöoÖO ABCDWEFWELRKGNUIQWNDÝASKÝP"
tr2en <- function(string){
  # letterDict <- fread(paste0(CommonScriptPath, "tr2en.csv"), encoding = "UTF-8")
  letterDict <- fread(paste0(DataPath, "tr2en.csv"), encoding = "UTF-8")
  
  temp <- string
  for(letter in letterDict$tr){
    # print(letter)
    temp <- gsub(x = temp, pattern = letter, replacement = letterDict[tr == letter, en])
  }
  return(temp)
}

# Cols are character vector of column names to FUN be applied. Every column
# FUN is function to be applied
# If verbose = T for loop counter over Cols will be written
ExecuteOverUniques <- function(Data,Cols,NewColName=NULL,FUN,verbose = F,RowBy = F){
  TempData <- copy(Data)
  TempData <- as.data.table(TempData)
  ColOrder <- names(TempData)
  for(i in Cols){
    UniqueInCol <- unique(TempData[,i,with = F])
    if(RowBy == T){
      UniqueInCol[,TempRowId := .I]
      UniqueInCol[,NewCol := FUN(get(i)),by = TempRowId]
      UniqueInCol[,TempRowId:=NULL]
    }else{
      UniqueInCol[,NewCol := FUN(get(i))]  
    }
    TempData <- merge(x = TempData, y = UniqueInCol, by = i, all = T)
    if(is.null(NewColName)){
      TempData[,(i) := NewCol]
      TempData[,NewCol:=NULL]
    }else{
      setnames(TempData,"NewCol",NewColName[which(Cols==i)] )
    }
    
    
    if(verbose == T){
      print(paste0("Column ",i," is done."))
    }
  }
  setcolorder(x = TempData,neworder = ColOrder)
  return(TempData)
}

# Find object with specific type in given environment
LsType <- function(pattern = ".",env = globalenv()){
  TypeList <- sapply(ls(envir = env),function(x){class(eval(parse(text = x),envir = env))})
  SelectedTypes <- names(TypeList[grepl(pattern = pattern,x = TypeList)])
  return(SelectedTypes)
}

RandomPointsInCountry <- function(NofPoints=100,Country="Turkey",MinLat =35.8215347357,MaxLat=42.1414848903,MinLon=26.0433512713,MaxLon=44.7939896991){
  
  library(maps)
  library(data.table)
  PointData <- data.table(Id = 1:NofPoints,Lat = runif(n = NofPoints, min = MinLat, max = MaxLat),Lon = runif(n = NofPoints, min = MinLon, max = MaxLon)) 
  PointData[,Where := map.where(database = "world",Lon,Lat)]
  
  while(nrow(PointData[grepl(pattern = Country,x = Where,ignore.case = T)==T]) != NofPoints){
    
    Rows <- nrow(PointData[grepl(pattern = Country,x = Where,ignore.case = T)==F | is.na(Where) == T])
    PointData[grepl(pattern = Country,x = Where,ignore.case = T)==F | is.na(Where) == T,`:=`(Lat = runif(n = Rows, min = MinLat, max = MaxLat),Lon = runif(n = Rows, min = MinLon, max = MaxLon))]
    PointData[grepl(pattern = Country,x = Where,ignore.case = T)==F | is.na(Where) == T,Where := map.where(database = "world",Lon,Lat)]
    # print(nrow(PointData[grepl(pattern = Country,x = Where,ignore.case = T)==F | is.na(Where) == T]))
    
  }
  
  PointData[,Where:=NULL]
  return(PointData)
  
}

CountLessOrGreaterforeach <- function(Data = x, Treshold = 0, LeftRight = "Left"){
  x<-as.data.table(Data)
  names <- ColumnTypes(x)[Type %in% c("integer","numeric"),Name]
  if(LeftRight == "Left"){
    for(i in names){
      ratio <- nrow(x[get(i) < Treshold]) / nrow(x)
      print(paste0(nrow(x[get(i) < Treshold])," less than ", Treshold ," in ",i, " ----- Ratio:",
                   round(ratio,digits = 3)))
    }
  }else{
    for(i in names){
      ratio <- nrow(x[get(i) > Treshold]) / nrow(x)
      print(paste0(nrow(x[get(i) > Treshold])," greater than ", Treshold ," in ",i, " ----- Ratio:",
                   round(ratio,digits = 3)))
    }
  }
}

Normalize_Fit <- function(Data = NULL, cols = NULL){
  mincols <- NULL
  rangecols <- NULL
  for(i in cols){
    
    mintemp <- min(Data[,get(i)],na.rm = T)
    rangetemp <- max(Data[,get(i)],na.rm = T) - mintemp
    
    mincols <- c(mincols,mintemp)
    rangecols <- c(rangecols,rangetemp)
    
  }
  
  return(list(cols = cols, min = mincols, range = rangecols))
  
}


Normalize_Predict <- function(Data = NULL, Normalizer = NULL){
  
  counter <- 1
  for(i in Normalizer$cols){
    Data[,(i) := (get(i) - Normalizer$min[counter]) / Normalizer$range[counter]]
    counter <- counter + 1
  }
}

FactorizeColumns <- function(MainData = NULL, 
                             ToBeFactorized = NULL, 
                             Breaks = NULL,
                             FactorizedColumnNames = NULL
                             # StartingFeatureCodes = NULL,
                             # FactorList = FactorColumnList, 
                             # NameOfFactorList = "FactorColumnList",
                             # Max = 10000
){
  
  # New column names for factors
  # FactorizedColumnNames <- paste0(StartingFeatureCodes,FactorizedColumnNames)
  
  # loop for new columns
  for(i in 1:length(ToBeFactorized)){
    
    # if minimum is higher than 0, take 0 as minimum
    Min <- MainData[,min(get(ToBeFactorized[i]),na.rm = T)]
    
    if(Min > 0){
      Min <- 0
    }
    
    # Max <- ceiling(MainData[,max(get(ToBeFactorized[i]))] + 0.0001)
    Max <- Inf
    # take 100 as maksimum
    # Max <- 100
    
    # change start and end points of breaks
    Breaks[[i]] <- c(Min,Breaks[[i]],Max)
    
    # factorize columns
    MainData[,(FactorizedColumnNames[i]):=cut(x = get(ToBeFactorized[i]), breaks = Breaks[[i]],include.lowest = T)]
    MainData[,(FactorizedColumnNames[i]) := ordered(x = get(FactorizedColumnNames[i]),levels = levels(get(FactorizedColumnNames[i])),labels = paste0(1:length(levels(get(FactorizedColumnNames[i]))),"_",levels(get(FactorizedColumnNames[i]))))]
    # levels(MainData$FactorizedColumnNames[i]) <- paste0(1:length(levels(get(FactorizedColumnNames[i]))),"_",levels(get(FactorizedColumnNames[i])))
  }
  
  # FactorList <- c(FactorList,FactorizedColumnNames)
  # assign(x = NameOfFactorList,value = FactorList,envir = globalenv())
  
  return(MainData)
}

