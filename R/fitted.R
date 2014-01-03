#!/usr/bin/Rscript

library(fBasics)

setwd("~/Dropbox/R/binaural-beats")
stroop <- read.csv("items.csv", header=T)

subject<-stroop[stroop$subject == 2,]
subject<-subject[51:100,]
subject<-subject[51:100,]

vector=subject$time
summary(vector)
plot(table(vector))

#Moment estimation (Gamma)
alpha = mean(vector)/var(vector)
beta = (mean(vector))**2/var(vector) 
x.gamma = rgamma(n=1000,scale=8.77, shape=8.63)
hist(x.gamma) 
qqplot(vector,x.gamma)

#Maximum likelihood estimation
library(MASS)
fitdistr(1/vector,"gamma")
ks.test(vector, "pgamma", scale=43.77, shape=43.33) 

truehist(1/vector, main="Histogram of sample")



x.norm<-rnorm(n=200,m=10,sd=2)
hist(x.norm,main="Histogram of observed data")
plot(density(x.norm),main="Density estimate of data")
plot(ecdf(x.norm),main="Empirical cumulative distribution function")

z.norm<-(x.norm-mean(x.norm))/sd(x.norm) ## standardized data
qqnorm(z.norm) ## drawing the QQplot
abline(0,1) ## drawing a 45-degree reference line

x.wei<-rweibull(n=200,shape=2.1,scale=1.1) ## sampling from a Weibull distribution with parameters shape=2.1 and scale=1.1
x.teo<-rweibull(n=200,shape=2, scale=1) ## theorical quantiles from a Weibull population with known paramters shape=2 e scale=1
qqplot(x.teo,x.wei,main="QQ-plot distr. Weibull") ## QQ-plot
abline(0,1) ## a 45-degree reference line is plotted

x.poi<-rpois(n=200,lambda=2.5)
hist(x.poi,main="Poisson distribution")

curve(dnorm(x,m=10,sd=2),from=0,to=20,main="Normal distribution")
curve(dgamma(x,scale=1.5, shape=2),from=0,to=15,main="Gamma distribution")

skewness(x.norm)
kurtosis(x.norm)


library(gamlss)
m1<-gamlss(y~1, family=exGAUS) 
plot(m1)



#library(lme4)
#lmer(time~level+(1|subject), data=stroop)




library(nlme)
#rank
#summary$rt <- rank(summary$rt)
model<-lme(time ~ factor(sound)*factor(level), data = stroop, random = ~1|subject/i)
summary(model)
anova(model)
plot(ranef(model))

library(gamlss.dist)
mGA <- histDist(stroop$time, "GA", density = TRUE)
gamlss(time ~ 1, data = stroop)
library(fitdistrplus)
fitdist(stroop[1:50,]$time, "exGAUS", method="mle", start=c(mu=15, sigma=1, nu=3))

fitdistr(stroop$time, "gamma")
library(vcd)
gf<-goodfit(stroop$time, type="binomial", method= "MinChisq") 
summary(gf)

library(lme4)
library(MCMCglmm)

model <- glmer(time ~sound + (sound| subject, summary ,family = Gamma)
               
fm2 <- lm(rt~sound*level, data=summary)

anova.model = aov(rt~sound*level, data=summary)
anova.table = summary(anova.model)
anova.table

plot(fitted(anova.model), resid(anova.model))

#Post-hoc tests
effect = ddply(summary, .(subject, level), summarise, rt=mean(rt))
t.test(rt ~ level, data = effect, paired=TRUE)

sound.mean <- aggregate(rt~sound, summary, mean)
sound.sd <- aggregate(rt~sound, summary, sd)

level.mean <- aggregate(rt~level, summary, mean)
level.sd <- aggregate(rt~level, summary, sd)

sequence.mean <- aggregate(rt~sequence, summary, mean)
sequence.sd <- aggregate(rt~sequence, summary, sd)

gender.mean <- aggregate(rt~gender, summary, mean)
gender.sd <- aggregate(rt~gender, summary, sd)

mean(summary$age)


model<-glm(time~sound*level+subject, data=stroop, family=Gamma)

library(nlme)
model<-lme(time~sound*level, random=~1|subject/sound/level/trial/item, data=stroop, method="ML")
model<-lme(time~sound*level, random=~1|subject/sound/level, data=stroop, method="ML")

library(stats4)
mle(minuslogl = stroop, start=list(lambda=2,alfa=1))

library(MASS)
fitdistr(stroop[stroop$subject==2,]$time,"gamma")

library(vcd)
gf<-goodfit(stroop$time, type="poisson",method= "MinChisq")
ks.test(stroop$time,"pweibull", shape=2, scale=1)
 