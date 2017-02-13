###RUN "01 Basic Answers.R" first.

### Would home team win more often?

currentHomeWin=sum(xinning$winner=='H')/nrow(xinning)  #50.16%
proposedHomeWin=h/(h+v)  #51.84%


#what is overall home team advantage in all games?
allGames= unique(rawData %>% 
                   select (gameString, visitingTeamFinalRuns, homeTeamFinalRuns) 
)

nrow(subset(allGames, visitingTeamFinalRuns < homeTeamFinalRuns)) / nrow(allGames) #53.4%



##### is it a good idea to bunt the runner to third?

situations2 = unique(rawData %>% 
                       filter(manOnFirst=='false' & manOnSecond=='false' & manOnThird=='true' & 
                                balls==0 & strikes==0 & outs==1) %>% 
                       select (gameString, inning, side) 
)

situations2$runs=0

for (i in 1:nrow(situations2)){
  gameInning=situations2[i,]
  records=rawData %>% filter(gameString==gameInning$gameString & inning==gameInning$inning & side==gameInning$side )
  # find first instance of situation in this inning, then sum runs scored from that pitch on
  countFromHere = min(which(records$manOnFirst=='false' & records$manOnSecond=='false' & records$manOnThird=='true' & 
                              records$balls==0 & records$strikes==0 & records$outs==1 ))
  recordsToCount=records[countFromHere:nrow(records),]
  runsFromThere=sum(recordsToCount$runsHome, na.rm=TRUE)
  situations2$runs[i]=runsFromThere
}
write_csv(situations2, "situations2.csv")



manOn3RunDist=100*table(situations2$runs)/nrow(situations2)

crossprod(manOn3RunDist[1:8]/100, seq(0,7)) #0.95
crossprod(manOn2RunDist[1:8]/100, seq(0,7)) #1.09

simlength=10000000 #these calcs are super fast and we need this many to stabilize

#visitor bunts, home team does not
t=situations2 %>% filter(side=='T') %>% select(runs)
b=situations %>% filter(side=='B') %>% select(runs)
indexT=sample(1:nrow(t), simlength, replace=TRUE)
indexB=sample(1:nrow(b), simlength, replace=TRUE)
t=t[indexT,]
b=b[indexB,]

v=sum(t>b)
h=sum(b>t)
h/(v+h) #53.4%

# visitor bunts, home team bunts if visitor scores 0 or 1.
t=situations2 %>% filter(side=='T') %>% select(runs)
indexT=sample(1:nrow(t), simlength, replace=TRUE)
t=t[indexT,]

b=situations %>% filter(side=='B') %>% select(runs)
indexB=sample(1:nrow(b), simlength, replace=TRUE)
b=b[indexB,]

b2=situations2 %>% filter(side=='B') %>% select(runs)
indexB2=sample(1:nrow(b2), simlength, replace=TRUE)
b2=b2[indexB2,]

switchup=which(t<2)
b[switchup,]=b2[switchup,]

v=sum(t>b)
h=sum(b>t)
h/(v+h) #53.4%

# visitor does not bunt, home team bunts if visitor score 0 or 1

t=situations %>% filter(side=='T') %>% select(runs)
indexT=sample(1:nrow(t), simlength, replace=TRUE)
t=t[indexT,]

b=situations %>% filter(side=='B') %>% select(runs)
indexB=sample(1:nrow(b), simlength, replace=TRUE)
b=b[indexB,]

b2=situations2 %>% filter(side=='B') %>% select(runs)
indexB2=sample(1:nrow(b2), simlength, replace=TRUE)
b2=b2[indexB2,]

switchup=which(t<2)
b[switchup,]=b2[switchup,]

v=sum(t>b)
h=sum(b>t)
h/(v+h) #52.4%


# everybody bunts!

t=situations2 %>% filter(side=='T') %>% select(runs)
indexT=sample(1:nrow(t), simlength, replace=TRUE)
t=t[indexT,]

b2=situations2 %>% filter(side=='B') %>% select(runs)
indexB2=sample(1:nrow(b2), simlength, replace=TRUE)
b2=b2[indexB2,]


v=sum(t>b)
h=sum(b>t)
h/(v+h) #52.4%

# visitor never bunts, home team always does

t=situations %>% filter(side=='T') %>% select(runs)
indexT=sample(1:nrow(t), simlength, replace=TRUE)
t=t[indexT,]

b2=situations %>% filter(side=='B') %>% select(runs)
indexB2=sample(1:nrow(b2), simlength, replace=TRUE)
b2=b2[indexB2,]


v=sum(t>b)
h=sum(b>t)
h/(v+h) #50.7%
