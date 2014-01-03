#!/usr/bin/Rscript

setwd("~/Dropbox/R/binaural-beats")
source(file="encephalapp.R")

stroop = enc_stroop("items.csv")
enc_describe(stroop)

stroop_without_outliers = enc_remove_outliers(stroop, 2)
enc_describe(stroop_without_outliers)

stroop_transformed = enc_transform_inverse(stroop_without_outliers)
enc_describe(stroop_transformed)

enc_normality(stroop, 2)
enc_normality(stroop_without_outliers, 2)
enc_normality(stroop_transformed, 2)

enc_describe(stroop_transformed)
enc_describe_subject(stroop_transformed, 4)

summary = enc_summarise(stroop_without_outliers)


enc_describe_interactions(summary)
enc_anova(summary)
enc_describe(summary)
describeBy(summary, summary$sound, mat=TRUE)[,c("group1","n","mean","sd","min","median","max")][19,]
describeBy(summary, summary$sound, mat=TRUE)[,c("group1","n","mean","sd","min","median","max")][20,]

compare = enc_summarise(stroop_without_outliers)
compare$level = factor(compare$level)
compare[compare$level == ' off',]$rt = - compare[compare$level == ' off',]$rt
compare <- ddply(compare, .(subject, age, gender, sound, sequence), summarise, difference=sum(rt))
boxplot(compare$difference~compare$sound)
anova.model = aov(difference~(sound+Error(subject/sound)), data=compare)
anova.table = summary(anova.model)
anova.table