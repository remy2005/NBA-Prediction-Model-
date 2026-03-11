library(dplyr)
library(readxl)

df<-NBA_Data_
#Renaming Columns 
names(df)[names(df)=="Away"]<-"Opp"
names(df)[names(df)=="Away PTS"]<- "PTS_O"
names(df)[names(df)=="PTS"]<- "PTS_T"
#Duplicating Home and Team to keep Home and transform Team
df$Team<-df$Home
#Adding gamer ID for each game
df$gamer_id<- 1:nrow(df)


gamedf<-df[ ,c("gamer_id", "Team", "PTS_T", "Opp","PTS_O", "Home", "Date")]

#Duplicating each gamer ID but flipping teams and opponents
flipgamedf<- gamedf %>% mutate(teamnew=Opp, oppnew=Team, PTS_TT=PTS_O, PTS_OO=PTS_T) %>% 
            select(-Team,-Opp, -PTS_T,-PTS_O) %>% rename(Team=teamnew, Opp=oppnew, PTS_T=PTS_TT, PTS_O=PTS_OO)
Cgamedf<- bind_rows(gamedf,flipgamedf)
Cgamedf<-Cgamedf[order(Cgamedf$gamer_id), ]


#Logical Arguments
Cgamedf$ifHome<- ifelse(Cgamedf$Home==Cgamedf$Team, 1,0)
Cgamedf$Win<- ifelse(Cgamedf$PTS_T>Cgamedf$PTS_O,1,0)
Cgamedf$PTS_diff<-Cgamedf$PTS_T-Cgamedf$PTS_O

#Table to view Home and Win
prop.table(table(ifHome = Cgamedf$ifHome, Win = Cgamedf$Win),1)
table(Cgamedf$ifHome)
#Order data frame
Cgamedf$Date<- as.Date(Cgamedf$Date)
Cgamedf<-Cgamedf[order(Cgamedf$gamer_id), ]

# ------------------ Streak ------------------
teams<-unique(Cgamedf$Team)
Cgamedf$Streak<-0
for (team in teams){
  idx<- which(Cgamedf$Team==team) #Creating a vector for each team and every game they played
  idx<- idx[order(Cgamedf$gamer_id[idx])]
  streak<-0
  for (i in idx){ #Looping through each row the team played
    Cgamedf$Streak[i]<-streak
    if (Cgamedf$Win[i]==1)
      streak <- ifelse(streak>=0,streak+1,1)
    else 
      streak <- ifelse(streak<=0, streak-1,-1)
  }
}

# ------------------ H2H rolling ------------------
rolling<-10
opps<-unique(Cgamedf$Opp)
Cgamedf$headtohead<-0

for (team in teams){
  for (oppo in opps){
    idx <- which(Cgamedf$Team==team & Cgamedf$Opp == oppo)
    idx <- idx[order(Cgamedf$gamer_id[idx])]
    h2h<-0
    if (length(idx)>=rolling){#Triggers if statement when the length of the vector is greater than rolling
      for (i in rolling:length(idx)){
      
        Cgamedf$headtohead[idx[i]] <- h2h
        h2h <- mean(Cgamedf$Win[idx[(i-rolling+1):i]], na.rm = TRUE)
      }
    }
  }
}

 # ------------------ Opp Streak ------------------
Cgamedf$OppoStreak<-0
for (oppo in opps){
  idx<-which(Cgamedf$Opp==oppo)
  idx<-idx[order(Cgamedf$gamer_id[idx])] #Creating a vector for each team and every game they played
  streak<-0
  for (i in idx){ #Looping through each row the team played
    Cgamedf$OppoStreak[i]<-streak
    if (Cgamedf$Win[i]==0){
      streak<- ifelse(streak>=0,streak+1,1)
    }  else{
      streak<- ifelse(streak<=0,streak-1,-1)
    }
  }
}
Cgamedf$Streakdiff<- Cgamedf$Streak-Cgamedf$OppoStreak

# ------------------ Calculates MMR of every team using ELO formula ------------------
Cgamedf$MMR<-0
Cgamedf$MMRopp<-0
mmr<- setNames(rep(1500,length(teams)), teams) #Creates table that stores every teams MMR
for (i in seq(1,nrow(Cgamedf),by=2)){ #Loops through the not flipped games 
  team<-Cgamedf$Team[i]
  oppo<- Cgamedf$Opp[i]
  Cgamedf$MMR[i]<-mmr[team]
  Cgamedf$MMRopp[i]<-mmr[oppo]
  win<-Cgamedf$Win[i]
  #MMR formula
  p_win<-1/(1+10^((mmr[oppo]-mmr[team])/400))
  K<- 24
  mmr[team]<-mmr[team]+K*(win-p_win)
  mmr[oppo]<-mmr[oppo]+K*((1-win)-(1-p_win))
}
mmr1<- setNames(rep(1500,length(teams)), teams) #Creates table that stores every teams MMR
for (i in seq(2,nrow(Cgamedf),by=2)){ #Loops through the flipped games
  team<-Cgamedf$Team[i]
  oppo<- Cgamedf$Opp[i]
  Cgamedf$MMR[i]<-mmr1[team]
  Cgamedf$MMRopp[i]<-mmr1[oppo]
  win<-Cgamedf$Win[i]
  p_win<-1/(1+10^((mmr1[oppo]-mmr1[team])/400))
  K<- 24
  mmr1[team]<-mmr1[team]+K*(win-p_win)
  mmr1[oppo]<-mmr1[oppo]+K*((1-win)-(1-p_win))
}
Cgamedf$MMRdiff<-Cgamedf$MMR-Cgamedf$MMRopp

# ------------------ Games Rested ------------------

Cgamedf$gamerested<-0
Cgamedf$gamerestedO<-0
for (team in teams){
  idx<-which(Cgamedf$Team==team)
  idx<-idx[order(Cgamedf$gamer_id[idx])]
  for (i in 2:length(idx)){
    previous_row<-idx[i-1]
    Cgamedf$gamerested[idx[i]]<-Cgamedf$Date[idx[i]]-Cgamedf$Date[previous_row]
  }
}
for (oppo in opps){
  idx<-which(Cgamedf$Opp==oppo)
  idx<-idx[order(Cgamedf$gamer_id[idx])]
  for (i in 2:length(idx)){
    previous_row<-idx[i-1]
    Cgamedf$gamerestedO[idx[i]]<-Cgamedf$Date[idx[i]]-Cgamedf$Date[previous_row]
  }
}

Cgamedf$gameresteddiff<-Cgamedf$gamerested-Cgamedf$gamerestedO


#Splitting data frame between training and test data  

Cgamedf$Date<-as.Date(Cgamedf$Date)

test_start<-as.Date("2025-01-01")
test_end<-as.Date("2026-03-05")

test_data<- Cgamedf[Cgamedf$Date>=test_start & Cgamedf$Date<=test_end, ]
training_data<-Cgamedf[Cgamedf$Date<test_start, ]

test_data<-test_data[order(test_data$gamer_id), ]

#Weighting data based upon date

training_data$Weight <- as.numeric(difftime(training_data$Date, min(training_data$Date), units="days"))
training_data$Weight <- training_data$Weight / max(training_data$Weight)

Cgamedf$Weight <- as.numeric(difftime(Cgamedf$Date, min(Cgamedf$Date), units="days"))
Cgamedf$Weight <- Cgamedf$Weight / max(Cgamedf$Weight)

training_data$Team <- as.factor(training_data$Team)
training_data$Opp  <- as.factor(training_data$Opp)


#Logit regression model

creative_model<-glm(Win ~  + headtohead + MMRdiff + ifHome + Team + Opp
                    ,data=training_data, family = binomial, weights = Weight)

summary(creative_model)

prediction_c<- predict(creative_model, newdata = test_data, type = "response")
prediction_cw<-ifelse(prediction_c>0.50,1,0)

cat(mean(prediction_cw==test_data$Win))
table(Prediction=prediction_cw, Actual=test_data$Win)


#PTS_total
PTS_MOE<- 20

Cgamedf$totalpts<-Cgamedf$PTS_O+Cgamedf$PTS_T
test_data$totalpts<-test_data$PTS_O+test_data$PTS_T
training_data$totalpts<-training_data$PTS_O+training_data$PTS_T

ptstotal_model<- lm(totalpts ~ Team+Opp+MMRdiff*ifHome
                    , data = training_data, weights = Weight)

predictptstotal<- predict(ptstotal_model, newdata = test_data, type = "response")
predictptstotal_Te<- ifelse(predictptstotal+PTS_MOE >= test_data$totalpts & 
                              predictptstotal-PTS_MOE <= test_data$totalpts, 1, 0)
summary(ptstotal_model)
mean(predictptstotal_Te)

average_error<- sqrt((mean((predictptstotal-test_data$totalpts)^2)))
average_error
