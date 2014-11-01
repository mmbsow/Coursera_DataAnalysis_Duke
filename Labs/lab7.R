# Lab 7

# load the data
load(url("http://www.openintro.org/stat/data/evals.RData"))

# plots
plot(x=evals$age, y=evals$bty_avg)
boxplot(age~gender,data=evals)
mosaicplot(~ rank + gender, data = evals)
# or: mosaicplot(table(evals$rank,evals$gender))

# build simple linear regression model with:
# - explanatory variable: bty_avg
# - response variable: score
m_bty = lm(score ~ bty_avg, data=evals)
summary(m_bty)

# build diagnostic plots to check if conditions for regression are verified
plot(m_bty$residuals ~ evals$bty_avg)
hist(m_bty$residuals)
qqnorm(m_bty$residuals)
qqline(m_bty$residuals)

# build pairwise scatterplots and correlations
#plot(evals$bty_avg ~ evals$bty_f1lower)
#cor(evals$bty_avg, evals$bty_f1lower)
plot(evals[,13:19])
cor(evals[,13:19])

# build multiple linear regression model with:
# - explanatory variables: bty_avg and gender
# - response variable: score
m_bty_gen <- lm(score ~ bty_avg + gender, data = evals)
summary(m_bty_gen)

# build diagnostic plots to check if conditions for regression are verified
plot(m_bty_gen$residuals ~ evals$bty_avg)
hist(m_bty_gen$residuals)
qqnorm(m_bty_gen$residuals)
qqline(m_bty_gen$residuals)

# plot regression lines
multiLines(m_bty_gen)

# build multiple linear regression model with:
# - explanatory variables: bty_avg and rank
# - response variable: score
m_bty_rank <- lm(score ~ bty_avg + rank, data = evals)
summary(m_bty_rank)

# build full model and display summary
m_full <- lm(score ~ rank + ethnicity + gender + language + age + 
                 cls_perc_eval + cls_students + cls_level + cls_profs +
                 cls_credits + bty_avg, 
             data = evals)
summary(m_full)

# build models removing one variable

# without rank
m1 <- lm(score ~ ethnicity + gender + language + age 
         + cls_perc_eval + cls_students + cls_level + 
             cls_profs + cls_credits + bty_avg, 
         data = evals)
# without ethnicity
m2 <- lm(score ~ rank + gender + language + age 
         + cls_perc_eval + cls_students + cls_level + 
             cls_profs + cls_credits + bty_avg, 
         data = evals)
# without gender
m3 <- lm(score ~ rank + ethnicity + language + age 
         + cls_perc_eval + cls_students + cls_level + 
             cls_profs + cls_credits + bty_avg, 
         data = evals)
# without language
m4 <- lm(score ~ rank + ethnicity + gender + age 
         + cls_perc_eval + cls_students + cls_level + 
             cls_profs + cls_credits + bty_avg, 
         data = evals)
# without age
m5 <- lm(score ~ rank + ethnicity + gender + language 
         + cls_perc_eval + cls_students + cls_level + 
             cls_profs + cls_credits + bty_avg, 
         data = evals)
# without cls_perc_eval
m6 <- lm(score ~ rank + ethnicity + gender + language 
         + age + cls_students + cls_level + 
             cls_profs + cls_credits + bty_avg, 
         data = evals)
# without cls_students
m7 <- lm(score ~ rank + ethnicity + gender + language + age +
             cls_perc_eval + cls_level + 
             cls_profs + cls_credits + bty_avg, 
         data = evals)
# without cls_level
m8 <- lm(score ~ rank + ethnicity + gender + language + age +
             cls_perc_eval + cls_students + 
             cls_profs + cls_credits + bty_avg, 
         data = evals)
# without cls_profs
m9 <- lm(score ~ rank + ethnicity + gender + language + age +
             cls_perc_eval + cls_students + cls_level +
             cls_credits + bty_avg, 
         data = evals)
# without cls_credits
m10 <- lm(score ~ rank + ethnicity + gender + language + age +
             cls_perc_eval + cls_students + cls_level +
             cls_profs + bty_avg, 
         data = evals)
# without bty_avg
m11 <- lm(score ~ rank + ethnicity + gender + language + age +
              cls_perc_eval + cls_students + cls_level +
              cls_profs + cls_credits, 
          data = evals)

# compare adjusted R2
summary(m_full)$adj.r.squared
summary(m1)$adj.r.squared
summary(m2)$adj.r.squared
summary(m3)$adj.r.squared
summary(m4)$adj.r.squared
summary(m5)$adj.r.squared
summary(m6)$adj.r.squared
summary(m7)$adj.r.squared
summary(m8)$adj.r.squared
summary(m9)$adj.r.squared
summary(m10)$adj.r.squared
summary(m11)$adj.r.squared
