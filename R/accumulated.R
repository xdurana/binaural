#!/usr/bin/Rscript

require(ggplot2)
require(plyr)

setwd("~/Dropbox/R/binaural-beats")

accumulated <- read.csv("summary.csv", header=T)
accumulated$sound = factor(accumulated$sound)
accumulated$subject = factor(accumulated$subject)

accumulated$offerror = accumulated$offerror - 5
accumulated$onerror = accumulated$onerror - 5

accumulated$accuracy_off = 1 - accumulated$offerror/(50 + accumulated$offerror)
accumulated$accuracy_on = 1 - accumulated$onerror/(50 + accumulated$onerror)
accumulated$accuracy = 1 - (accumulated$offerror + accumulated$onerror)/(100 + accumulated$offerror + accumulated$onerror)

summary(accumulated)
boxplot(accumulated$offtime~stroop$sound)
boxplot(accumulated$ontime~stroop$sound)
boxplot(accumulated$totaltime~stroop$sound)
boxplot(accumulated$offerror~stroop$sound)
boxplot(accumulated$onerror~stroop$sound)
boxplot(accumulated$accuracy_off~stroop$sound)
boxplot(accumulated$accuracy_on~stroop$sound)
boxplot(accumulated$accuracy~stroop$sound)

boxplot(accumulated$totaltime~stroop$sound,
        main = "Total time distribution for each sound",
        names = c("beta","delta/theta"),
        xlab = "Type of sound",
        ylab = "Total Stroop task time (s)")

boxplot(accumulated$accuracy~stroop$sound,
        main = "Trial accuracy for each sound",
        names = c("beta","delta/theta"),
        xlab = "Type of sound",
        ylab = "Accuracy (%)")

#ANOVA single factor, repeated measures

anova.model = aov(totaltime~sound*sequence, data=accumulated)
anova.table = summary(anova.model)
anova.table

anova.model = aov(totaltime~(sound+Error(subject/sound)), data=accumulated)
anova.table = summary(anova.model)
anova.table

anova.model = aov(accuracy~(sound+Error(subject/sound)), data=accumulated)
anova.table = summary(anova.model)
anova.table




#require(lme4)
#m1 <- lmer(value~level*sound+i+(1|subject), data=stroop)
#m2 <- lmer(value~level*sound+i+(1|subject)+(1|i), data=stroop)
#m3 <- lmer(value~level*sound+(1|subject), data=stroop)
#m4 <- lmer(value~level*sound+(1|subject)+(1|i), data=stroop)
#m5 <- lmer(value~level*sound+(i|subject), data=stroop)
#m6 <- lmer(value~level*sound+i+(i|subject), data=stroop)

#anova(m1, m2, m3, m4, m6)

#model<-m6
#qqnorm(resid(model))

#summary(model)
#coef(model)
#Anova(model)
#anova(model)


############################

accumulated <- read.csv("summary.csv", header=T)
accumulated$sound = factor(accumulated$sound)
accumulated$subject = factor(accumulated$subject)

accumulated$offerror = accumulated$offerror - 5
accumulated$onerror = accumulated$onerror - 5

accumulated$accuracy_off = 1 - accumulated$offerror/(50 + accumulated$offerror)
accumulated$accuracy_on = 1 - accumulated$onerror/(50 + accumulated$onerror)
accumulated$accuracy = 1 - (accumulated$offerror + accumulated$onerror)/(100 + accumulated$offerror + accumulated$onerror)

summary(accumulated)
boxplot(accumulated$offtime~accumulated$sound)
boxplot(accumulated$ontime~accumulated$sound)
boxplot(accumulated$totaltime~accumulated$sound)
boxplot(accumulated$offerror~accumulated$sound)
boxplot(accumulated$onerror~accumulated$sound)
boxplot(accumulated$accuracy_off~accumulated$sound)
boxplot(accumulated$accuracy_on~accumulated$sound)
boxplot(accumulated$accuracy~accumulated$sound)


total <- merge(summary, accumulated, by=c("subject",""))]


boxplot(accumulated$totaltime~accumulated$sound,
        main = "Total time distribution for each sound",
        names = c("beta","delta/theta"),
        xlab = "Type of sound",
        ylab = "Total Stroop task time (s)")

boxplot(accumulated$accuracy~accumulated$sound,
        main = "Trial accuracy for each sound",
        names = c("beta","delta/theta"),
        xlab = "Type of sound",
        ylab = "Accuracy (%)")

#ANOVA single factor, repeated measures

anova.model = aov(totaltime~sound*sequence, data=accumulated)
anova.table = summary(anova.model)
anova.table

anova.model = aov(totaltime~(sound+Error(subject/sound)), data=accumulated)
anova.table = summary(anova.model)
anova.table

anova.model = aov(accuracy~(sound+Error(subject/sound)), data=accumulated)
anova.table = summary(anova.model)
anova.table