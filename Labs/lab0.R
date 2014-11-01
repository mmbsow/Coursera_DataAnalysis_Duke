source("http://www.openintro.org/stat/data/present.R")

# dimension of the dataframe
dim(present)

# column names in the dataframe
names(present)

# years are included in this dataframe
present$year

# counts of girls born each year
present$girls

plot(x=present$year, y=present$girls, type="l")

# compte total number of births for each year
plot(present$year, present$boys + present$girls, type = "l")
present$total <- present$boys + present$girls

# proportion of boys (ratio of boys over girls)
present$boys/present$girls

# proportion of newborns that are boys
present$boys/(present$boys + present$girls)
plot(present$year, present$boys/(present$boys + present$girls), type="l")

# do boys outnumber girls in each year?
present$boys > present$girls

# plot boy to girl ratio for every year
plot(present$year, present$boys/present$girls, type="l")

# absolute difference between boys and girls births
abs(present$boys-present$girls)

# year with max absolute difference
present[which.max(abs(present$boys-present$girls)),]
