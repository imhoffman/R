# R script
# IMH 21 Dec 2015
# reads in NOAA archival weather data, parses time, and readies Pa
# http://www.ncdc.noaa.gov/qclcd/QCLCD
KCON <- read.csv("2010-KCON.txt",skip=7,header=TRUE,stringsAsFactors=FALSE,sep=",")
KCON$IMHDateTime <- as.POSIXct(strptime(paste(KCON$Date%/%10000,(KCON$Date%%10000)%/%100,KCON$Date%%100,KCON$Time%%100,KCON$Time%/%100,sep=" "),"%Y %m %d %M %H",tz="EST5EDT"))
KCON$IMHPressurePa <- as.numeric(KCON$StationPressure)*3386.39
#plot(KCON$IMHDateTime,KCON$IMHPressurePa,xlab="Day in 2010",ylab="Pressure (Pa)",col=2,pch=-1)
h <- hist(na.omit(KCON$IMHPressurePa),freq=FALSE,col='grey')
h$density <- h$counts/sum(h$counts)*100
plot(h,freq=FALSE,ylab="Percentage of Measurements (%)",xlab="Atmospheric Pressure (Pa)", main="Histogram of Hourly Pressure Measurements",sub="Concord, NH, USA, 2010 Feb--Nov",col='grey')
x <- na.omit(KCON$IMHPressurePa)
text(0.25*(max(x)-min(x))+min(x),y=25,paste("mean =",floor(mean(x)),"Pa"),pos=2,offset=0.0)
text(0.25*(max(x)-min(x))+min(x),y=23,paste("stdev =",floor(sd(x)),"Pa"),pos=2,offset=0.0)
#xfit <- seq(min(x),max(x),length=128)
#yfit <- dnorm(xfit,mean=mean(x),sd=sd(x))
#lines(xfit,yfit,col=3,lwd=2)
