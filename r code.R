# q1b
#########
setwd("C:/Users/Max/Desktop/Double Degree notes/5B/Stat 431/A2")
df = read.csv("q1b.csv", as.is=TRUE,header= TRUE)
model_1 = glm(df$Pain~df$Treatment, family = binomial(link = logit))
summary(model_1)
# getting odds ratio
# note that beta_1 is log odds ratio of repsonse for subjects with treatment vs no treatment
# which is what we're looking for
OR = exp(as.numeric(model1$coefficients[2]))
OR
# for 95% wald based CI
# note that SE(beta_1) = 1.272 
se = 1.272
log_OR_lower = as.numeric((model1$coefficients[2]))-1.96*se
log_OR_upper = as.numeric((model1$coefficients[2]))+1.96*se
OR_CI = list(exp(log_OR_lower), exp(log_OR_upper))
OR_CI

#q1c
#############
model_age = glm(df$Pain~df$Age, family = binomial(link = logit))
model_sex = glm(df$Pain~df$Sex..1...Male., family = binomial(link = logit))
model_duration = glm(df$Pain~df$Duration, family = binomial(link = logit))
summary(model_age)
summary(model_sex)
summary(model_duration)

#q1d
###########
model_full = glm(df$Pain~df$Treatment+df$Age+df$Sex..1...Male.+df$Duration, family = binomial(link = logit))
summary(model_full)

#q2
##########
# let x be 1 to indicate right handed, 0 to indicate left handed
q2 = read.csv('q2.csv', as.is=TRUE,header= TRUE)
q2$resp_bio = cbind(q2$Bio...RH, q2$Bio...Total-q2$Bio...RH)
q2$resp_adopt = cbind(q2$Adopt...RH, q2$Adopt...Total-q2$Adopt...RH)

# biological parents
############
q2_model_bio = glm(resp_bio~Father+Mother, family = binomial(link = logit), data = q2)
summary(q2_model_bio)

# adoptive parents
############
q2_model_adopt = glm(resp_adopt~Father+Mother, family = binomial(link = logit), data = q2)
summary(q2_model_adopt)

#2c
###########

#add column to indicate whether or not both parents are right handed
# where factor = 0 when both parents are right handed and 1 otherwise
# => vector will be t(c(0,1,1)) since the first row is both right handed, and rest are not
q2$both_right = cbind(c(0,1,1))
q2$resp_total = cbind(q2$Bio...RH+q2$Adopt...RH, q2$Bio...Total-q2$Bio...RH+q2$Adopt...Total-q2$Adopt...RH)
q2_model_total = glm(resp_total~both_right, family = binomial(link=logit), data = q2)
summary(q2_model_total)
log_or_ratio = as.numeric(q2_model_total$coefficients[2])
log_or_ratio #-0.4670006

#q3
###########
# convert age groups into indicator variables
# x1 = 1, if person belongs to 25-34 age group, 0 o.w.
# x2 = 1, if person belongs to 35-44 age group, 0 o.w.
# so on
df3 = read.csv("q3.csv", as.is=TRUE,header= TRUE)
df3$resp = cbind(df$Cancer...Yes, df$Cancer...No)
model = glm(resp~x1+x2+x3+x4+x5+x6+Alcohol, family = binomial(link = logit), data = df3)


# q3b
###########

#q3c
###########

#q3d
###########
unsaturated_model = glm(resp~Alcohol, family = binomial(link = logit), data = df3)
