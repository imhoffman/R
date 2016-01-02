histp <- function(x,freq=FALSE,col="grey",breaks="Sturges",right=TRUE,xlim=range(x)) {
  h <- hist(x,breaks=breaks,freq=freq,col=col,right=right,xlim=xlim)
  h$density <- h$counts/sum(h$counts)*100
  plot(h,freq=freq,col=col,ylab="Percentage")
}
# hist1 for one column given another column
# to do: make axis/title labels the same in histp call
hist1 <- function(events=NULL,condition=NULL,percent=FALSE) {
  if (percent) {
    histp(na.omit(ifelse(condition,events,NA)))
  } else {
    hist(na.omit(ifelse(condition,events,NA)),col="grey")
  }
}