setwd("/Users/Mariame/Repositories//Coursera_DataAnalysis_Duke/Labs/")

load(url("http://www.openintro.org/stat/data/kobe.RData"))
head(kobe)

# list of hits and misses for the first 9 shots
kobe$basket[1:9]

# calculate the lengths of all shooting streaks and display distribution
kobe_streak <- calc_streak(kobe$basket)
barplot(table(kobe_streak))

# simulation: coin flip
outcomes <- c("heads", "tails")
sim_fair_coin <- sample(outcomes, size = 100, replace = TRUE)
table(sim_fair_coin)
sim_unfair_coin <- sample(outcomes, size = 100, replace = TRUE, prob = c(0.2,0.8))
table(sim_unfair_coin)

# simultion: independent shooter (45% prob H, 55% prob M)
outcomes <- c("H", "M")
sim_basket <- sample(outcomes, size = 133, replace = TRUE, prob = c(0.45, 0.55))

# compare Kobe's result to independent shooter's
table(kobe$basket)
table(sim_basket)

# calculate the lengths of all shooting streaks and display distribution for independent
sim_streak <- calc_streak(sim_basket)
barplot(table(sim_streak))
summary(sim_streak)

