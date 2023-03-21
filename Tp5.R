#exo1

Nba<-read.csv(file="P:/BUT STID/programmation stat/TP5/dataset/NBA.csv")
dim(Nba)
summary(Nba)
Nba$PERIOD<-as.factor(Nba$PERIOD)
Nba$PTS_TYPE<-as.factor(Nba$PTS_TYPE)
summary(Nba)

#Exo 2
quantile(Nba$CLOSE_DEF_DIST,probs = seq(0,1,0.1),na.rm = TRUE )
length(unique(Nba$GAME_ID))
length(unique(Nba$SHOOTER))
Nba$SHOT_DIST_METRE<-Nba$SHOT_DIST*0.3


#Exo3
#a
barplot(table(Nba$PERIOD))
#b
table1<-table(Nba$SHOT_RESULT)
table1<-table1/(table1[1]+table1[2])*100
table1[1]<-round(table1[1],2)
table1[2]<-round(table1[2],2)
pie(table(Nba$SHOT_RESULT),labels=paste(rownames(table1),table1))
#c
hist(Nba$SHOT_DIST)
#d
table2<-table(Nba$PTS_TYPE,Nba$PERIOD)
table2<-prop.table(table2)
#table2<-table2/(table2[1]+table2[2])*100
#table2[1]<-round(table2[1],2)
#table2[2]<-round(table2[2],2)
barplot(table2)
#e
boxplot(Nba$SHOT_DIST)
#f

boxplot(formula=Nba$SHOT_DIST ~ Nba$PERIOD)

#Ex4
#a
Top10<-aggregate()