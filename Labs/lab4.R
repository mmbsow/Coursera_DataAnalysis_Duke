load(url("http://bit.ly/dasi_nc"))
colnames(nc)
nulls <- nc$gained[is.na(nc$gained)]
length(nulls)
summary(nc)
hist(nc$gained)
gained_clean = na.omit(nc$gained)
n = length(gained_clean)

boot_means = rep(NA, 100)
for(i in 1:100){
    boot_sample = sample(gained_clean, n, replace = TRUE)
    boot_means[i] = mean(boot_sample)
}
hist(boot_means)

> xbar <- mean(boot_means)
> sder <- sd(boot_means)
> xbar + c(-1,1) * qnorm(0.95) * sder

source("http://bit.ly/dasi_inference")
inference(nc$gained, type = "ci", method = "simulation", conflevel = 0.9, est = "mean", boot_method = "perc")
inference(nc$gained, type = "ci", method = "simulation", conflevel = 0.95, est = "mean", boot_method = "se")
inference(nc$gained, type = "ci", method = "simulation", conflevel = 0.95, est = "median", boot_method = "se")

boxplot(nc$weight ~ nc$habit)
by(nc$weight, nc$habit, mean)
by(nc$weight, nc$habit, median)
by(nc$weight, nc$habit, length)

inference(y = nc$weight, x = nc$habit, est = "mean", type = "ht", null = 0, alternative = "twosided", method = "theoretical")
inference(y = nc$weight, x = nc$habit, est = "mean", type = "ci", null = 0, alternative = "twosided", method = "theoretical")

by(nc$mage, nc$mature, summary)

# Part B: GSS
load(url("http://bit.ly/dasi_gss_ws_cl"))
hist(gss$wordsum)
summary(gss$wordsum)
boxplot(gss$wordsum ~ gss$class)
summary(gss$class)
inference(y = gss$wordsum, x = gss$class, est = "mean", type = "ht", alternative = "greater", method = "theoretical")

