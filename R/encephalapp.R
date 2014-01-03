#!/usr/bin/Rscript

require(ggplot2)
require(plyr)
require(xtable)
library(nlme)
#library(lme4)
library(lattice)
library(moments)
library(psych)

# stroop
# return dataset with single-item reaction time measurements
enc_stroop <- function(file) {
  stroop <- read.csv(file, header=T)
  stroop$i<-(stroop$trial-1)*10+stroop$item
  stroop$mean <- ave(stroop$time, stroop$subject, stroop$sound, stroop$level, FUN=mean)
  stroop$median <- ave(stroop$time, stroop$subject, stroop$sound, stroop$level, FUN=median)
  stroop$sd <- ave(stroop$time, stroop$subject, stroop$sound, stroop$level, FUN=sd)
  stroop$mad <- ave(stroop$time, stroop$subject, stroop$sound, stroop$level, FUN=mad)
  
  return(stroop)
}

# remove_outliers
# eliminates outliers depending on the criteria 
enc_remove_outliers <- function(stroop, times) {
  stroop <- stroop[stroop$time < stroop$median + times*stroop$mad,]
  return(stroop)
}

# normality
# check for normality
enc_normality <- function(stroop, s) {
  data = stroop[stroop$subject == s,]$time
  skewness(data)
  kurtosis(data)
  qqnorm(data);
  qqline(data, col = 2)
}

# transform_inverse
# dependent variable transformation for normalization
enc_transform_inverse <- function(stroop) {
  stroop$time<-1/stroop$time
  return(stroop)
}


# transform_log
# dependent variable transformation for normalization
enc_transform_log <- function(stroop) {
  stroop$time<-log(stroop$time)
  return(stroop)
}

# hist
# different graphic plots
enc_hist <- function(stroop) {
  hist(stroop$time, breaks=200)
}

# describe
# different graphic plots
enc_describe <- function(stroop) {
  summary(stroop)
  boxplot(stroop$time~stroop$sound+stroop$level+stroop$subject)
  #plot(density(stroop$time))
}

# describe_subject
# different graphic plots for subject
enc_describe_subject <- function(stroop, s) {
  ds=stroop[stroop$subject == s,]
  qqmath(~time|subject+level+sound, data=ds)
  histogram(~time|subject+level+sound, data=ds, nint=50)
}

# summarise
# summarise groups per subject
enc_summarise <- function(stroop) {
  summary = ddply(stroop, .(subject, age, gender, sound, level, sequence), summarise, rt=mean(time),  sd=sd(time), shapiro=enc_shapiro(time))
  summary$time = summary$rt
  summary$level = factor(summary$level)
  summary$sound = factor(summary$sound)
  summary$subject = factor(summary$subject)
  summary$sequence = factor(summary$sequence)
  str(summary)
  boxplot(summary$rt~summary$sound+summary$level)
  boxplot(summary$rt~summary$sound, main = "Reaction time distribution for each sound", names = c("beta","delta/theta"), ylab ="Reaction time (s)", xlab ="Type of sound")
  boxplot(summary$rt~summary$level, main = "Reaction time distribution for each Stroop level", names = c("neutral","incongruent"), ylab ="Reaction time (s)", xlab ="Stroop level")
  boxplot(summary$rt~summary$sequence)
  return(summary)
}

# describe_interactions
# plot interactions
enc_describe_interactions = function(summary) {
  interaction.plot(summary$level, summary$sound, summary$time)
  with(summary, interaction.plot(x.factor=sound, trace.factor=level, response=time, fun=mean, type="b", legend=T, ylab="rt", main="Interaction Plot"))
  coplot(time ~ level | sound, data=summary, panel=panel.smooth, xlab="Reaction time vs stroop level, given a sound")
}

# anova
# two-tailed, two factor, repeated measures ANOVA 
enc_anova = function(summary) {
  anova.model = aov(time~(sound*level+Error(subject/(sound*level))), data=summary)
  anova.table = summary(anova.model)
  anova.table
}

# anova_stroop
# two-tailed, two factor, repeated measures ANOVA 
enc_anova_stroop = function(summary) {
  anova.model = aov(time~(sound+Error(subject/sound)), data=summary)
  anova.table = summary(anova.model)
  anova.table
}

# shapiro
# Shapiro-Wilk Test
enc_shapiro = function(dfr) {
  return (shapiro.test(dfr)$p.value)
}