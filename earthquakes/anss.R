# GT4p5 means greater than magnitude 4.5 as selected at
# http://earthquake.usgs.gov/earthquakes/search/
#eq <- read.csv("ANSS-GT4p5.csv",header=TRUE,sep=",")
h <- hist(eq$depth,breaks=101,col="grey")
h$counts <- log10(h$counts)
plot(h,col="grey",ylab=expression(plain(log)[10](plain(events))),xlab="earthquake depth (km)",main=expression(paste("Earthquakes worldwide having ", m>=4.5, " since 1900")))