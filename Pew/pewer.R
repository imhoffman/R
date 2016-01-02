pew <- read.csv("Feb_2014_Views_Future_CSV.csv",header=TRUE,sep=",")
trim <- function(x,level=99) {
  replace(x,which(x>level),NA)
}
pew$pial2aIMH <- trim(pew$pial2a,level=4)
pew$pial2bIMH <- trim(pew$pial2b,level=4)
pew$pial2cIMH <- trim(pew$pial2c,level=4)
pew$pial2dIMH <- trim(pew$pial2d,level=4)
pew$pial2eIMH <- trim(pew$pial2e,level=4)
pew$pial3aIMH <- trim(pew$pial3a,level=2)
pew$pial3bIMH <- trim(pew$pial3b,level=2)
pew$pial3cIMH <- trim(pew$pial3c,level=2)
pew$pial3dIMH <- trim(pew$pial3d,level=2)
pew$pial4aIMH <- trim(pew$pial4a,level=2)
pew$pial4bIMH <- trim(pew$pial4b,level=2)
pew$pial4cIMH <- trim(pew$pial4c,level=2)
pew$pial5IMH <- trim(pew$pial5,level=7)
pew$educIMH <- trim(pew$educ2,level=8)
pial5_1 <- ifelse(pew$educIMH == 1, pew$pial5IMH, NA)
pial5_2 <- ifelse(pew$educIMH == 2, pew$pial5IMH, NA)
pial5_3 <- ifelse(pew$educIMH == 3, pew$pial5IMH, NA)
pial5_4 <- ifelse(pew$educIMH == 4, pew$pial5IMH, NA)
pial5_5 <- ifelse(pew$educIMH == 5, pew$pial5IMH, NA)
pial5_6 <- ifelse(pew$educIMH == 6, pew$pial5IMH, NA)
pial5_7 <- ifelse(pew$educIMH == 7, pew$pial5IMH, NA)
pial5_8 <- ifelse(pew$educIMH == 8, pew$pial5IMH, NA)
# from here down, don't use, eg, pew$pial2e; rather, use
# pial2e,data=pew or else you can't feed fit_x to predict
plot(jitter(pial5IMH)~educIMH,data=pew)
fit <- lm(pial5IMH~poly(educIMH,2,raw=TRUE),data=pew)
fit_x <- seq(-1,101,length.out=8192)
lines(fit_x,predict(fit,data.frame(educIMH=fit_x)),col="blue")
peak <- -summary(fit)$coefficients[2,1]/summary(fit)$coefficients[3,1]/2
text(peak,(par("usr")[4]-par("usr")[3])*0.75,pos=1,paste(signif(peak,3)))
# require(plotrix)
# h_multi <- list(pial5_1,pial5_2,pial5_3,pial5_4,pial5_5,pial5_6,pial5_7,pial5_8)
# multhist(h_multi,freq=FALSE)
#
# below here for general science survey
pew <- read.csv("Pew-GP-Science-2014.csv",header=TRUE,sep=",")
nukes <- data.frame(1:max(na.omit(trim(pew$educ2,level=8))))
for (i in 1:dim(nukes)[1]) {
  nukes$educ[i] <- i
  nukes$means[i] <- mean(replace(trim(pew$q24b,level=2),which(pew$educ2==i),NA),na.rm=TRUE)
  nukes$stdevs[i] <- sd(replace(trim(pew$q24b,level=2),which(pew$educ2==i),NA),na.rm=TRUE)
}
nukes <- subset(nukes,select=-1)