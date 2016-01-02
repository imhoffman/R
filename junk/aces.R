runs <- 1024
histo <- 1:runs
for (j in 1:runs) {
  counter <- 0
  draws <- 4096
  for (i in 1:draws) {
    roll <- sample(1:6, 5, replace=TRUE)
    tf <- (roll==1 | roll==6)
    if (sum(tf)==5){
      #print(i)
      #print(roll)
      counter <- counter + 1
    }
  }
  histo[j] <- counter/draws
}
h <- hist(histo,freq=FALSE,col="grey")
h$density <- h$counts/sum(h$counts)*100
plot(h,freq=FALSE,ylab="Percentage of Runs",xlab="Fraction of Draws in One Run Yielding Five Sixes (Including Aces) in One Roll",main=paste("Aces: Five of Something on First Roll Once Every",ceiling(1/mean(histo)/5),"Rolls"),col="grey")
text(range(histo)[2],par("usr")[4]*0.95,paste("analytical expectation =",signif((1/3)^5,4)*100,"%"),pos=2)
text(range(histo)[2],par("usr")[4]*0.85,paste("empirical mean =",signif(mean(histo),4)*100,"%"),pos=2)
text(range(histo)[1],par("usr")[4]*0.95,"Nruns of Ndraws",pos=4,offset=-1.0)
text(range(histo)[1],par("usr")[4]*0.85,paste("Ndraws =",draws),pos=4,offset=-1.0)
text(range(histo)[1],par("usr")[4]*0.75,paste("Nruns =",runs),pos=4,offset=-1.0)