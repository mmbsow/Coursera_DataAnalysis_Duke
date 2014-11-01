source("http://bit.ly/dasi_inference")
load(url("http://www.openintro.org/stat/data/atheism.RData"))
us12 <- subset(atheism, atheism$nationality == "United States" & atheism$year == "2012")
us12_atheist <- subset(us12, us12$response == "atheist")
pct_atheist <- dim(us12_atheist)[1] / dim(us12)[1]

inference(us12$response, est = "proportion", type = "ci", method = "theoretical", success = "atheist")


n <- 1000
p <- seq(0, 1, 0.01)
me <- 2 * sqrt(p * (1 - p)/n)
plot(me ~ p)

spain <- subset(atheism, atheism$nationality == "Spain")
inference(y=spain$response, x=spain$year, est = "proportion", type = "ht", method = "theoretical", success = "atheist", alternative = "twosided")

us <- subset(atheism, atheism$nationality == "United States")
inference(y=us$response, x=us$year, est = "proportion", type = "ht", method = "theoretical", success = "atheist", alternative = "twosided")

z <- qnorm(0.975)
p <- 0.5
me <- 0.01
n <- ((z * sqrt(p * (1-p))) / me)^2
