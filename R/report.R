#!/usr/bin/Rscript

require(ggplot2)
require(plyr)
require(xtable)
require(nlme)
require(lattice)
require(moments)
require(psych)
require(car)

setwd("~/Dropbox/R/binaural-beats")

stroop <- read.csv("items.csv", header=T)
conditions <- read.csv("conditions.csv", header=T)

stroop$sound<-as.factor(stroop$sound)
stroop$level<-as.factor(stroop$level)
stroop$subject<-as.factor(stroop$subject)

stroop$mean <- ave(stroop$time, stroop$subject, stroop$sound, stroop$level, FUN=mean)
stroop$median <- ave(stroop$time, stroop$subject, stroop$sound, stroop$level, FUN=median)
stroop$sd <- ave(stroop$time, stroop$subject, stroop$sound, stroop$level, FUN=sd)
stroop$mad <- ave(stroop$time, stroop$subject, stroop$sound, stroop$level, FUN=mad)
stroop$i<-as.factor((stroop$trial-1)*10+stroop$item)

plot(density(stroop$time))

stroop_without_outliers <- stroop[stroop$time < stroop$mean + 2*stroop$sd,]

bwtheme  <- canonical.theme(color = FALSE)
f.mean <- function(x,y,...) {
  panel.bwplot(x,y,..., pch='|');
  panel.points(tapply(y, factor(x), FUN = mean), pch=20)
}

rt = ddply(stroop, .(subject, age, gender, sound, level, sequence), summarise, value=mean(time), sd=sd(time), skewness=skewness(time), kurtosis=kurtosis(time))
rtwo = ddply(stroop_without_outliers, .(subject, age, gender, sound, level, sequence), summarise, value=mean(time), sd=sd(time), skewness=skewness(time), kurtosis=kurtosis(time))
logrtwo = ddply(stroop_without_outliers, .(subject, age, gender, sound, level, sequence), summarise, value=mean(log(time)), sd=sd(log(time)), skewness=skewness(log(time)), kurtosis=kurtosis(log(time)))

summary<-logrtwo

summary <- merge(summary, conditions, by=c("subject","sound","level","age","gender","sequence"))
summary$accuracy = 1-(summary$runs-5)/(summary$runs-5 + 50)
summary$level = factor(summary$level, labels=c("neutral", "incongruent"))
summary$sound = factor(summary$sound, labels=c("beta", "delta/theta"))
summary$subject = factor(summary$subject)
summary$sequence = factor(summary$sequence)

ddply(summary, .(level, sound), summarise, mean=mean(value), sd=sd(value))
ddply(summary, .(level, sound), summarise, mean=mean(accuracy), sd=sd(accuracy))
ddply(summary, .(level, sound), summarise, mean=mean(runs), sd=sd(runs))

ddply(summary, .(level), summarise, mean=mean(value), sd=sd(value))
ddply(summary, .(sound), summarise, mean=mean(value), sd=sd(value))

bwplot(value ~ sound | level, data=summary, panel=f.mean, par.settings=bwtheme, layout=c(2,1), xlab="Binaural beats frequencies and Stroop levels conditions", ylab="log RT")
bwplot(accuracy ~ sound | level, data=summary, panel=f.mean, par.settings=bwtheme, layout=c(2,1), xlab="Binaural beats frequencies and Stroop levels conditions", ylab="accuracy (%)")
#bwplot(accuracy ~ time | sound, data=summary, panel=f.mean, par.settings=bwtheme, layout=c(2,1))

qplot(accuracy, value, shape=factor(sound), data=summary) + geom_smooth()
#qplot(accuracy, time, data=summary, geom = "line")
qplot(accuracy, time, shape=factor(sound), data=summary,  geom=c("point", "smooth"), method="lm", formula=y ~ poly(x, 2))
boxplot(value~accuracy, data=summary)
ggplot(summary, aes(time, accuracy, shape=sound)) + geom_point()

interaction.plot(summary$sound, summary$subject, summary$value, data=summary, xlab="time", ylab="Tolerance", legend=F)
xyplot(value ~ level | sound, data = summary, groups = subject, type = "o", par.settings=bwtheme, panel = panel.superpose)
xyplot(value ~ sound | level, data = summary, groups = subject, type = "o", par.settings=bwtheme, panel = panel.superpose)

anova <- aov(accuracy~(sound*level*sequence+Error(subject/(sound*level))), data=summary)
anova.table <- xtable(anova, caption="Repeated measures ANOVA", label="Table:ANOVA")
summary(anova)

cor(summary$accuracy,summary$time)

anova <- aov(value~(sound*level+accuracy+Error(subject/(sound*level))), data=summary)
anova.table <- xtable(anova, caption="Repeated measures ANOVA", label="Table:ANOVA")
summary(anova)

effect = ddply(summary, .(subject, sound), summarise, value=mean(value))
test = t.test(value ~ sound, data=effect, paired=TRUE)
test
tapply(effect$value, effect$sound, FUN=mean)
tapply(effect$value, effect$sound, FUN=sd)

#accuracy
anova <- aov(accuracy~(sound*level*sequence+Error(subject/(sound*level))), data=summary)
anova.table <- xtable(anova, caption="Repeated measures ANOVA", label="Table:ANOVA")
summary(anova)