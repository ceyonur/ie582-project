rm(list = ls())
require(data.table)
require(zoo)
source("C:/Users/baris.isik/Desktop/Master/IE 582/Project/Codes/parameters.r")

Matches <- fread(paste0(RawDataPath,"Latest/matches.csv"))
Stats <- fread(paste0(RawDataPath,"Latest/stats.csv"))

Round_m_ids <- c(273302,273303,273306,273308,273309,273305,273310,273307,273301,273304)


Matches[, Outcome := (ifelse(match_hometeam_score == match_awayteam_score, 0 , 
                             ifelse(match_hometeam_score > match_awayteam_score, 1 , -1)))]

Matches[,Outcome := factor(Outcome,levels = c(1,0,-1),labels = c("HomeWin","Draw","AwayWin"),ordered = T )]
Matches <- Matches[match_status == "Finished" | match_id %in% Round_m_ids,.(M_id = match_id,Date = as.Date(as.POSIXct(epoch,origin = "1970-01-01")),
                      H_id = match_hometeam_id, A_id = match_awayteam_id, H_Score = match_hometeam_score,
                      A_Score = match_awayteam_score,Outcome)]

H_Matches <- Matches[,.(M_id,Date,HomeAway = "H",T_id = H_id,Score = H_Score,Opponent_Score = A_Score,Outcome)]
H_Matches[,Row := .N, by = .(Date,T_id)]

A_Matches <- Matches[,.(M_id,Date,HomeAway = "A",T_id = A_id,Score = A_Score,Opponent_Score = H_Score,Outcome)]
A_Matches[,Row := .N, by = .(Date,T_id)]

# if there is more than one game of team per day we considered it as duplicated game and removed
duplicated_matches <- unique(c(H_Matches[Row > 1, M_id],A_Matches[Row > 1, M_id]))

H_Stats <- Stats[,.(M_id = match_id,HomeAway = "H", BallPossesion = home_BallPossession,
                    CornerKicks = home_CornerKicks, GoalAttempts = home_GoalAttempts,
                    ShotsOnGoal = home_ShotsonGoal, GoalKeeperPerformance = (away_ShotsonGoal - home_GoalkeeperSaves) / away_ShotsonGoal)]

A_Stats <- Stats[,.(M_id = match_id,HomeAway = "A", BallPossesion = away_BallPossession,
                    CornerKicks = away_CornerKicks, GoalAttempts = away_GoalAttempts,
                    ShotsOnGoal = away_ShotsonGoal, GoalKeeperPerformance = (home_ShotsonGoal - away_GoalkeeperSaves) / home_ShotsonGoal)]

Features <- rbind(H_Matches,A_Matches)
Stats <- rbind(H_Stats,A_Stats)
Stats[,GoalKeeperPerformance := ifelse(is.na(GoalKeeperPerformance),1,
                                       ifelse(GoalKeeperPerformance < 0,0,
                                              ifelse(GoalKeeperPerformance > 1,1,GoalKeeperPerformance)))]

# remove duplicated matches
Features <- Features[!(M_id %in% duplicated_matches)]
Features[,Row := NULL]
Features <- merge(x = Features, y = Stats, by = c("M_id", "HomeAway"),all.x = T,all.y = F)

Features <- Features[is.na(Outcome) == T | is.na(BallPossesion) == F | is.na(CornerKicks) == F | is.na(GoalAttempts) == F |
                     is.na(ShotsOnGoal) == F | is.na(GoalKeeperPerformance) == F,]
# convert ball possession to numeric
Features[,BallPossesion := as.integer(gsub(x = BallPossesion, pattern = "%",replacement = ""))/100]

# to calculate attacking power we use
Features[,TempScore := ifelse(Score <5,Score,5)]
Features[,`:=`(TempScore = ifelse(Score < quantile(Score,probs = c(0.95),na.rm = T)+1,Score,quantile(Score,probs = c(0.95),na.rm = T)+1),
               CornerKicks = ifelse(CornerKicks < quantile(CornerKicks,probs = c(0.9),na.rm = T)+1,CornerKicks,quantile(CornerKicks,probs = c(0.9),na.rm = T)+1),
               GoalAttempts = ifelse(GoalAttempts < quantile(GoalAttempts,probs = c(0.9),na.rm = T)+1,GoalAttempts,quantile(GoalAttempts,probs = c(0.9),na.rm = T)+1),
               ShotsOnGoal = ifelse(ShotsOnGoal < quantile(ShotsOnGoal,probs = c(0.9),na.rm = T)+1,ShotsOnGoal,quantile(ShotsOnGoal,probs = c(0.9),na.rm = T)+1))]

Normalizer <- Normalize_Fit(Data = Features,cols = c("TempScore","CornerKicks","GoalAttempts","ShotsOnGoal","BallPossesion"))
Normalize_Predict(Data = Features, Normalizer = Normalizer)

AreaOfSpider <- function(Arrays = NULL){
  Area <- 0
  # Arrays  <- unlist(Arrays)
  len <- length(Arrays)
  Arrays <- sort(x = Arrays,decreasing = T)
  Arrays <- Arrays[1:len-1]
  len <- len-1
  angle <- 360/len
  Arrays[len + 1] <- Arrays[1]
  for(i in 1:len){
    Area <- Area + sin(pi*angle/180)*Arrays[i]*Arrays[i+1]/2
  }
  return(Area)
}

Features[,Row := .I]
Features[, AttackingPower := AreaOfSpider(Arrays = c(TempScore,CornerKicks,GoalAttempts,ShotsOnGoal,BallPossesion)),by = Row]

Features[,c("TempScore","CornerKicks","GoalAttempts","ShotsOnGoal","BallPossesion","Row") := NULL]

Features[,Season := ifelse(Date < as.Date("2018-06-15"),"2017-2018",
                           ifelse(Date < as.Date("2019-06-15"),"2018-2019","2019-2020"))]

Features[,Win := ifelse((Outcome == "AwayWin" & HomeAway == "A") | (Outcome == "HomeWin" & HomeAway == "H"),1,0)]
Features[,WinDraw := ifelse(Outcome == "Draw" | Win == 1,1,0)]

setorder(Features,T_id,Date)
Features[,Match_No := seq_len(.N), by = .(T_id,Season)]

# InputData = copy(Features[HomeAway == "H"])
# HomeAway = "H"
H_A_Features_Generator <- function(InputData = NULL, HomeAway = NULL){
  
  Data <- copy(InputData)
  
  last_x_games <- ifelse(HomeAway == "H" ,"Home","Away")
  # for rollmean
  shiftcols <- c("Score","Opponent_Score","GoalKeeperPerformance","AttackingPower","Win","WinDraw")
  
  Data[,(shiftcols) := lapply(.SD,FUN = function(x){
    shift(x = x,n = 1,fill = NA)
  }),.SDcols = shiftcols, by = .(T_id,Season)]
  
  for(i in shiftcols){
    newname <- paste0(HomeAway,"_Avg_",i,"_Last3",last_x_games,"Games")
    Data[,(newname):= rollmean(x = get(i),k = 3,fill = NA,align = "right")]
  }
  
  # for streaks
  streakcols <- c("Win","WinDraw")
  for(i in streakcols){
    newname <- paste0(HomeAway,"_",i,"Streak_Last3",last_x_games,"Games")
    Data[,(newname) := rollapply(data = get(i),FUN = function(x){
      latest_0 <- max(which(x == 0))
      latest_0 <- ifelse(latest_0 == -Inf,1,latest_0)
      len <- length(x)
      return(len - latest_0)
    },width = 100,fill = NA, align = "right",partial = T), by = T_id]
  }
  
  Data <- Data[,c("M_id","T_id",paste0(HomeAway,"_Avg_",shiftcols,"_Last3",last_x_games,"Games"),
                  paste0(HomeAway,"_",streakcols,"Streak_Last3",last_x_games,"Games")),with = F]
  return(Data)
}

H_Features <- H_A_Features_Generator(InputData = copy(Features[HomeAway == "H"]),HomeAway = "H")
A_Features <- H_A_Features_Generator(InputData = copy(Features[HomeAway == "A"]),HomeAway = "A")

# overall performance of each team
shiftcols <- c("GoalKeeperPerformance","AttackingPower","Win","WinDraw")

Features[,(shiftcols) := lapply(.SD,FUN = function(x){
  shift(x = x,n = 1,fill = NA)
}),.SDcols = shiftcols, by = .(T_id,Season)]

for(i in shiftcols){
  newname <- paste0("Avg_",i,"_Last5Games")
  Features[,(newname):= rollmean(x = get(i),k = 5,fill = NA,align = "right")]
}

Features <- Features[,c("M_id","HomeAway","T_id",shiftcols,paste0("Avg_",shiftcols,"_Last5Games")),with = F]

Matches <- Matches[,.(M_id,H_id,A_id,Date,Outcome)]
Backup <- copy(Matches)
Matches <- copy(Backup)

# join home features
Matches <- merge(x = Matches, y = H_Features, by.x = c("M_id","H_id"), by.y = c("M_id","T_id"),all = F)
Matches <- merge(x = Matches, y = Features[HomeAway == "H"], by.x = c("M_id","H_id"), by.y = c("M_id","T_id"),all = F)
Matches[,HomeAway := NULL]

oldnames <- names(Features)[4:ncol(Features)]
setnames(Matches,oldnames,paste0("H_",oldnames))

# join away features
Matches <- merge(x = Matches, y = A_Features, by.x = c("M_id","A_id"), by.y = c("M_id","T_id"),all = F)
Matches <- merge(x = Matches, y = Features[HomeAway == "A"], by.x = c("M_id","A_id"), by.y = c("M_id","T_id"),all = F)
Matches[,HomeAway := NULL]

setnames(Matches,oldnames,paste0("A_",oldnames))

#### Elo ####
Matches[,Season := ifelse(Date < as.Date("2018-06-15"),"2017-2018",
                          ifelse(Date < as.Date("2019-06-15"),"2018-2019","2019-2020"))]

setorder(Matches,Date)
Matches[,DateId := .GRP, by = Date]
Matches[,DateId_Season := .(seq_len(.N)), by = .(Season)]

Matches[,Outcome_forelo := ifelse(Outcome == "HomeWin",1,
                                  ifelse(Outcome == "AwayWin",0,0.5))]
robj <- elo(Matches[DateId==1,.(DateId,H_id,A_id,Outcome_forelo)])
Ratings <- as.data.table(robj$ratings)[,c(1,2)]
Ratings[,DateId := 1]
i <- 2
BaseRatings <- rbind(unique(Matches[,.(DateId, Player = A_id)]),unique(Matches[,.(DateId, Player = H_id)]))
for(i in 2:max(Matches[is.na(Outcome) == F,DateId],na.rm = T)){
  Status <- copy(Ratings)
  setorder(Status, -DateId)
  Status[,Row := seq_len(.N), by = Player]
  Status <- Status[Row == 1, .(Player,Rating)]
  SeaasonDateId <- min(Matches[DateId == i, DateId_Season])
  
  if(SeaasonDateId == 1){
    mean <- mean(Status[,Rating])
    Status[,Rating := ((Rating - mean)*0.5) + mean]
    print("Scaled")
  }
  
  Matches[DateId==i,.(DateId,H_id,A_id,Outcome_forelo)]
  robj <- elo(Matches[DateId==i,.(DateId,H_id,A_id,Outcome_forelo)],status = Status)
  temp <- as.data.table(robj$ratings)
  temp <- temp[Games == 1,c(1,2)]
  temp[,DateId := i]
  Ratings <- rbind(Ratings,temp)
  print(i)
}

Ratings <- merge(x = BaseRatings, y = Ratings, by = c("Player","DateId"),all.x = T, all.y = F)
setorder(Ratings,Player,DateId)
Ratings[,DateId := shift(x = DateId,n = 1,type = "lead"),by = Player]

HomeElo <- Ratings[,.(DateId,Player,H_Elo = Rating)]
AwayElo <- Ratings[,.(DateId,Player,A_Elo = Rating)]

Matches <- merge(x = Matches, y = HomeElo, by.x = c("H_id","DateId"), by.y = c("Player","DateId"),all.x = T, all.y = F)
Matches <- merge(x = Matches, y = AwayElo, by.x = c("A_id","DateId"), by.y = c("Player","DateId"),all.x = T, all.y = F)

Feat <- c("H_Avg_GoalKeeperPerformance_Last3HomeGames","A_Avg_GoalKeeperPerformance_Last3AwayGames","H_Avg_AttackingPower_Last3HomeGames",
"A_Avg_AttackingPower_Last3AwayGames","H_Avg_AttackingPower_Last5Games","A_Avg_AttackingPower_Last5Games","A_WinDrawStreak_Last3AwayGames",
"H_WinDrawStreak_Last3HomeGames","A_Avg_Win_Last5Games","H_Avg_Win_Last5Games","H_Elo","A_Elo")

Test <- copy(Matches[M_id %in% Round_m_ids,c("M_id",Feat),with = F])

Matches  <- na.omit(Matches)
Matches <- Matches[,c("Outcome",Feat),with = F]

norm <- Normalize_Fit(Data = Matches, cols = Feat)
Normalize_Predict(Data = Matches,Normalizer = norm)
Normalize_Predict(Data = Test,Normalizer = norm)

# Fit <- ordfor(depvar = "Outcome",data = Matches,min.node.size = 5,mtry = 0.5, ntreefinal = 1000,
#               nsets = 500,ntreeperdiv = 50)
# RoundResults <- as.data.table(cbind(Test[,M_id],predict(object = Fit,newdata = Test,type = "prob")$classfreqtree))
# setnames(RoundResults,"V1","M_id")


x=model.matrix(Outcome~.,data=Matches)
y=Matches[,Outcome]

Fit <- cv.glmnet(x = x,y =  y, alpha = 0.5, family = "multinomial")

setorder(Test,M_id)
M_ids <- Test[,M_id]
x <- model.matrix(~.,data=Test[,Feat,with = F])

RoundResults <- as.data.table(cbind(M_ids, matrix(data = predict(object = Fit, newx = x, s = "lambda.min",type="response"),ncol = 3)))

setnames(RoundResults,c("M_id","HomeWin","Draw","AwayWin"))
save(RoundResults,file = paste0(CleanDataPath,"RoundResults.rdata"))

Matches <- fread(paste0(RawDataPath,"Latest/matches.csv"))

RoundResults <- merge(x = RoundResults,y = Matches[,.(match_id,match_hometeam_name,match_awayteam_name)], by.x = "M_id",by.y = "match_id",all.x = T, all.y = F)
RoundResults <- RoundResults[,.(M_id,Home = match_hometeam_name, Away = match_awayteam_name,
                                HomeWin,Draw,AwayWin)]

CountNAforeach(Matches)
model_txt = paste(t(RoundResults[, c("M_id", "HomeWin", "Draw", "AwayWin")]), collapse=',')
