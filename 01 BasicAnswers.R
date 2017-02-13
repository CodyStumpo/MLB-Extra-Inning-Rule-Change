setwd("~/Documents/Code/mlb-hackathon-2017/")
library(readr)
#rawData = read.csv("2016-WS.csv")
rawData2016 = read_csv("2016.csv")
rawData2015 = read_csv("2015.csv")
rawData2014 = read_csv("2014.csv")

rawData=rbind(rawData2014,rawData2015,rawData2016)
rm(rawData2014, rawData2015, rawData2016)
library("dplyr")
library(ggplot2)

#here's an idea, what if last out from previous inning starts on second base in extra innings.
#expectation of outcomes & time saved.  simulate it out. not just expectations.
# how does home team wpct compare with this rule to actual?

rawData = rawData %>% select(gameString,inning, side, balls, strikes, outs, manOnFirst, manOnSecond, manOnThird, 
                             runsHome, visitingTeamFinalRuns, homeTeamFinalRuns)

## what happened in extra inning games?
xinning= unique(rawData %>% 
                  filter(inning>9) %>%
                  select (gameString) 
)

totalGames=nrow(unique(rawData %>% select(gameString)))
howOftenExtra = nrow(xinning)/totalGames #8.7%

#get last record of each xinning game, record # of innings & winner
xinning$innings=9
xinning$winner='H'

for(i in 1:nrow(xinning)){
  game=xinning[i,]
  record=rawData %>% filter(gameString==game$gameString) %>% tail(n=1) 
  xinning$innings[i]=record$inning
  if(record$visitingTeamFinalRuns>record$homeTeamFinalRuns) xinning$winner[i]='V'
}


#### 1. are extra innings like other innings?  similar distribution of runs?
runTable=rawData %>% select(gameString, inning, side, runsHome) %>% 
  group_by(gameString, inning, side) %>% summarise(runs=sum(runsHome, na.rm=TRUE))


numRegInnings=nrow(subset(runTable, inning<10))
numExtraInnings=nrow(subset(runTable, inning>9))
whatPctInningsExtra=numExtraInnings/(numExtraInnings+numRegInnings) #2.2%

r=table(subset(runTable, inning<10)$runs)
x=table(subset(runTable, inning>9 & side=='T')$runs) #ignore B because walkoffs

r=r/sum(r)
x=x/sum(x)
runDist=data.frame(regulation=r[1:6]*100, extra=x[1:6]*100)  #yes, they are quite similar

#### 2. what happens now in extra innings games
empiricalXInningDist=100*table(xinning$innings)/nrow(xinning)


#### 1-alt. what is the distribution of runs in full innings (now)
fullRunDist=100*table(runTable$runs)/nrow(runTable)

### 3. try to simulate today's extra inning length
simlength=10000
indexT=sample(1:nrow(runTable), simlength, replace=TRUE)
indexB=sample(1:nrow(runTable), simlength, replace=TRUE)
t=runTable[indexT,"runs"]
b=runTable[indexB,"runs"]

v=sum(t>b)
h=sum(b>t)

i=1
inningsVector=as.numeric()
innings=1
while(i < simlength){
  
  if(b[i,]==t[i,]) innings=innings+1 
  else {inningsVector=append(inningsVector,innings); innings=1}
  
  i=i+1  
}

SimXInningDist=100*table(inningsVector+9)/length(inningsVector)

plot(cumsum(empiricalXInningDist), col="blue", type = "l", 
     xlab="Extra Innings", ylab="Chance Game Over After X Extra Innings", main="Extra Inning Game Length Under Current Rules")
lines(cumsum(SimXInningDist), col="red")
legend("bottomright", 
       c("Empirical","Simulated"), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("blue","red"))

### 4. what happens with man on second, no outs?

##pull similar innings to simulate from
situations = unique(rawData %>% 
              filter(manOnFirst=='false' & manOnSecond=='true' & manOnThird=='false' & 
                       balls==0 & strikes==0 & outs==0) %>% 
              select (gameString, inning, side) 
)


situations$runs=0


for (i in 1:nrow(situations)){
gameInning=situations[i,]
records=rawData %>% filter(gameString==gameInning$gameString & inning==gameInning$inning & side==gameInning$side )
# find first instance of situation in this inning, then sum runs scored from that pitch on
countFromHere = min(which(records$manOnFirst=='false' & records$manOnSecond=='true' & records$manOnThird=='false' & 
                            records$balls==0 & records$strikes==0 & records$outs==0 ))
recordsToCount=records[countFromHere:nrow(records),]
runsFromThere=sum(recordsToCount$runsHome, na.rm=TRUE)
situations$runs[i]=runsFromThere
}
#this takes ~8 minutes on my laptop, write to disk
write_csv(situations, "situations.csv")

manOn2RunDist=100*table(situations$runs)/nrow(situations)

simlength=10000


t=situations %>% filter(side=='T') %>% select(runs)
b=situations %>% filter(side=='B') %>% select(runs)
indexT=sample(1:nrow(t), simlength, replace=TRUE)
indexB=sample(1:nrow(b), simlength, replace=TRUE)
t=t[indexT,]
b=b[indexB,]

v=sum(t>b)
h=sum(b>t)

i=1
newInningsVector=as.numeric()
innings=1
while(i < simlength){

if(b[i,]==t[i,]) innings=innings+1 
  else {newInningsVector=append(newInningsVector,innings); innings=1}
  
i=i+1  
}

NewSimXInningDist=100*table(newInningsVector+9)/length(newInningsVector)


plot(cumsum(empiricalXInningDist), col="blue", type = "l", 
     xlab="Extra Innings", ylab="Chance Game Over After X Extra Innings", main="Impact of Rule Change on Extra Inning Game Length")
lines(cumsum(NewSimXInningDist), col="red")
legend("bottomright", 
       c("Current","Proposed"), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("blue","red"))

## expected extra innings?
expectedSimXtraInnings = crossprod(SimXInningDist[1:8]/100, seq(8))
expectedNewSimXtraInnings = crossprod(NewSimXInningDist[1:8]/100, seq(8))
expectedishXtraInnings = crossprod(empiricalXInningDist[1:8]/100, seq(8))
expectedXtraInnings = numExtraInnings / nrow(xinning) /2

#what percent of innings would now be extra?
numNewExtraInnings = nrow(xinning) * expectedNewSimXtraInnings * 2
whatPctInningsNowExtra=numNewExtraInnings/(numNewExtraInnings+numRegInnings) #1.4%
