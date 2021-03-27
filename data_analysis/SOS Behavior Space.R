# SOS behavior space data analysis
# 20121206
# Cheng-Jun Wang @ cmc office
setwd("/Users/chengjun/百度云同步盘/Writing/沉默的螺旋多主体模型/SOSData/")
r2<-read.csv("RQ2.CSV",  header = T, sep = ",", stringsAsFactor=TRUE)
r2<-read.csv("RQ3-2.CSV",  header = T, sep = ",", stringsAsFactor=TRUE)
r2<-read.csv("RQ3-3.CSV",  header = T, sep = ",", stringsAsFactor=TRUE)
r2c<-read.csv("RQ3C.CSV",  header = T, sep = ",", stringsAsFactor=TRUE)
r2d<-read.csv("RQ3D.CSV",  header = T, sep = ",", stringsAsFactor=TRUE)


r2c = reshape_data(r2c)
r2d = reshape_data(r2d)
r2c$Group = 'Strong Reference Groups'
r2d$Group = 'Weak Reference Groups'
dat = rbind(r2c, r2d)
library(ggplot2)

a = ggplot(dat, aes(x=as.factor(time), y=value, color= Group))+
  geom_boxplot(position = position_dodge(width = .9))+ 
  xlab("时间")+ylab("沉默的人数") +
  scale_x_discrete(breaks=c(1:6)*10, 
                   labels=c("10", "20", "30","40", "50", "60"))+
  theme_bw(base_family = 'STHeiti', base_size = 12)

a + facet_grid(Group ~ .) + theme(legend.position = "none")


summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~regression of RQ1~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
r2<-read.csv("RQ1.CSV",  header = T, sep = ",", stringsAsFactor=TRUE)


Spiral <- function (n) {
	m= r2[,n]
	pos = max(  which(  !is.na( m )  )  )
	num = m[  1:pos  ]
	time = c(1:pos)
	reg<-lm(num~time)
	b <- summary(reg)$coefficients[2,1]
	sBeta<- b * (sd(time) / sd(num))
	rSquare  <- summary(reg)$adj.r.squared
	return (c(sBeta , rSquare))
	}
list = lapply(c(1:100), Spiral)
data<-as.data.frame(do.call(rbind, list))

names(data)<-c("beta", "r_square")
mean(data$r_square)
mean(data$beta)

sd(data$r_square)
sd(data$beta)

library(reshape)

reshape_data = function(dt){
  dt$time = 1:nrow(dt)
  dt= melt(dt, id = c("time"))
  dt = subset(dt, is.na(dt$value)==FALSE)
  return(dt)
}

dat = reshape_data(r2)

library(ggplot2)
ggplot(dat, aes(x=as.factor(time), y=value))+
  geom_boxplot(position = position_dodge(width = .9))+ 
  xlab("时间")+ylab("沉默的人数") +
  scale_x_discrete(breaks=c(1:6)*10, 
                   labels=c("10", "20", "30","40", "50", "60"))+
  theme_bw(base_family = 'STHeiti', base_size = 12)


ggplot(dat, aes(x=time, y=value))+geom_point()+stat_smooth()+xlab("时间")+ylab("沉默的人数") +
  theme_bw(base_family = 'STHeiti', base_size = 14)

#################
#~~~~~~~~~~~~~~~~~~~~~~~f test of population size~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################
p1000<-read.csv("RQ5P1000.CSV",  header = T, sep = ",", stringsAsFactor=TRUE)
p1500<-read.csv("RQ5P1500.CSV",  header = T, sep = ",", stringsAsFactor=TRUE)
p2000<-read.csv("RQ5P2000.CSV",  header = T, sep = ",", stringsAsFactor=TRUE)

library(reshape)

reshape_data = function(dt){
  dt$time = 1:nrow(dt)
  dt= melt(dt, id = c("time"))
  dt = subset(dt, is.na(dt$value)==FALSE)
  return(dt)
}


p1000 = reshape_data(p1000)
p1500 = reshape_data(p1500)
p2000 = reshape_data(p2000)

p1000$Group = 'P = 1000'
p1500$Group = 'P = 1500'
p2000$Group = 'P = 2000'

dat = rbind(p1000, p1500, p2000)


library(ggplot2)

a = ggplot(dat)+ 
  geom_boxplot(aes(x=as.factor(time), y=value, color= Group),
               position = position_dodge(width = .9),
               outlier.shape=NA)+
  scale_x_discrete(breaks=c(50, 100, 150, 200), 
                   labels=c("50", "100", "150", "200"))+
  xlab("时间")+ylab("沉默的人数") +
  theme_bw(base_family = 'STHeiti', base_size = 14) #+facet_wrap(~Group)


c <- a + facet_grid(Group ~ .) + theme(legend.position = "none")
c



Spiral <- function (n) {
	m= r2[,n]
	pos = max(  which(  !is.na( m )  )  )
	lastValue = m[  pos  ]
	return (c(pos, lastValue))
	}
list = lapply(c(1:100), Spiral)

data<-as.data.frame(do.call(rbind, list))
names(data)<-c("time", "silentNum")

dp1000<-data
dp1500<-data
dp2000<-data

dp1000$group= 1000
dp1500$group= 1500
dp2000$group= 2000

mean(dp1000$time) #  161
sd(dp1000$time)
mean(dp1500$time) #  156
mean(dp2000$time) # 98

dat<-rbind(dp1000, dp1500, dp2000)
fit <- aov(dat$time ~ dat$group)
summary(fit)

fit1 <- lm(dat$time ~ dat$group)
summary(fit1)

######################
#~~~~~~~~~~~~~~~~~~~~~~~f test of group size~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
######################

g2<-read.csv("RQ4V2.CSV",  header = T, sep = ",", stringsAsFactor=TRUE)
g4<-read.csv("RQ4V4.CSV",  header = T, sep = ",", stringsAsFactor=TRUE)
g6<-read.csv("RQ4V6.CSV",  header = T, sep = ",", stringsAsFactor=TRUE)


g2 = reshape_data(g2)
g4 = reshape_data(g4)
g6 = reshape_data(g6)

g2$Group = 'Radius = 2'
g4$Group = 'Radius = 4'
g6$Group = 'Radius = 6'

dat = rbind(g2, g4, g6)


library(ggplot2)

a = ggplot(dat)+ 
  geom_boxplot(aes(x=as.factor(time), y=value, color= Group),
               position = position_dodge(width = .9),
               outlier.shape=NA)+
  scale_x_discrete(breaks=c(50, 100, 150, 200), 
                   labels=c("50", "100", "150", "200"))+
  xlab("时间")+ylab("沉默的人数") +
  theme_bw(base_family = 'STHeiti', base_size = 14) #+facet_wrap(~Group)


c <- a + facet_grid(Group ~ .) + theme(legend.position = "none")
c

g2$Group = 'g2'
g4$Group = 'g4'
g6$Group = 'g6'
r2 = rbind(g2[1:50, ], g4[1:50,], g6[1:50, ])
r2$id = as.numeric(rownames(r2))


r2=g2
r2=g4
r2=g6

Spiral <- function (n) {
	m= r2[,n]
	pos = max(  which(  !is.na( m )  )  )
	lastValue = m[  pos  ]
	return (c(pos, lastValue))
	}
list = lapply(c(1:100), Spiral)

data<-as.data.frame(do.call(rbind, list))
names(data)<-c("time", "silentNum")

dg2<-data
dg4<-data
dg6<-data

dg2$group=2
dg4$group=4
dg6$group=6

dat<-rbind(dg2, dg4, dg6)
fit <- aov(dat$time ~ dat$group)
summary(fit)
plot(data$silent~data$time)

rg1<-lm(dat$time ~ dat$group)
summary(rg1)
plot(dat$time ~ dat$group)

data<-subset(dat, dat$group==6)
mean(dg2$time) #  162.79
mean(dg4$time) #  126.19
mean(dg6$time) #  98.02


#~~~~~~~~~~~~~~~~~~t test of a stronger reference group~~~~~~~~~~~~~~~~~~#
d3c<-data
d3d<-data

d3c$media=5
d3d$media=1

dat<-rbind(d3c, d3d)
t.test(dat$time~dat$media)

data$silent <- ifelse(data$silentNum > 500, 1, 0) 

#~~~~~~~~~~~~~~~~~~~~~~~dynamic~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
p1000<-read.csv("RQ5P1000.CSV",  header = T, sep = ",", stringsAsFactor=TRUE)

r2<-p1000

Spiral <- function (n) {
	m= r2[,n]
	pos = max(  which(  !is.na( m )  )  )
	lastValue = m[  pos  ]
	return (c(pos, lastValue))
	}
list = lapply(c(1:100), Spiral)

data<-as.data.frame(do.call(rbind, list))
names(data)<-c("time", "silentNum")

plot( r2[,which( data$time==max(data$time) )]~c(1:max(data$time)), type = "n", 
	xlab="time", ylab= "Spiral of Silence", ylim= c(0, 25))

findGrowth<-function(n){
       time <- 1:(data$time[n]-1)   
	 Growth <- subset(r2[,n], is.na(r2[,n])==FALSE)	
	 difGrowth <- diff(Growth, lag=1)
	 points(   difGrowth~time, col=sample(c(1:1000),1),pch=sample(c(0:18), 1), cex=0.5, type='p' )
	 return( cbind(difGrowth, time))
	}

sapply( c(1:100),  findGrowth  )

## to save Tiff with dpi 2000
tiff("D:/Growth.tif", 
	width=5, height=5, pointsize = 10,
	units="in", res=700,
	compression = "lzw") #c("none", "rle", "lzw", "jpeg", "zip")

# stop saving figure
dev.off()

list = lapply(c(1:50), findGrowth)
dat<-as.data.frame(do.call(rbind, list)); dim(dat)
names(dat)<-c("time", "silentNum")
cor(dat)

########################################################################
plot( r2[,which( data$time==max(data$time) )]~c(1:max(data$time)), type = "n", 
	xlab="time", ylab= "Spiral of Silence", ylim= c(190, 1820))

findGrowth<-function(n){
       time <- 1:data$time[n]   
	 dailyGrowth <- subset(r2[,n], is.na(r2[,n])==FALSE)	
	 points(   dailyGrowth~time, col=sample(c(1:1000),1),pch=sample(c(0:18), 1), cex=0.5, type='p' )
	}

sapply(  list<-sample(   c(1:20), 20 ),  findGrowth  )

## to save Tiff with dpi 2000
tiff("D:/WangFig4.tif", 
	width=8, height=4, pointsize = 10,
	units="in", res=500,
	compression = "lzw") #c("none", "rle", "lzw", "jpeg", "zip")

# stop saving figure
dev.off()

