setwd("/Users/Mariame/Repositories//Coursera_DataAnalysis_Duke/Labs/")

# Load the cdc data frame into the workspace
source("http://www.openintro.org/stat/data/cdc.R")
names(cdc)
dim(cdc)

class(cdc$genhlth)
class(cdc$weight)
class(cdc$smoke100)

head(cdc)
tail(cdc)

# summary for numerical variable
summary(cdc$weight)
mean(cdc$weight)
var(cdc$weight)
median(cdc$weight)

# summary for categorical variable
table(cdc$gender) # same as summary(cdc$gender) for categorical
table(cdc$gender)/20000 # as relative frequency
barplot(table(cdc$gender))

# tabulate gender and smoke variables
# with gender as rows, smoke as columns
gender_smokers = table(cdc$gender, cdc$smoke100)
mosaicplot(gender_smokers)

# access a single value (weight of the 567th person)
cdc[567, 6]
cdc$weight[567]

# weight for the first 10 respondents
cdc[1:10, 6]
cdc$weight[1:10]

# all the data for the first 10 respondents
cdc[1:10, ]

# all the weights
cdc[, 6]
cdc$weight

# subset using logical vectors
males <- subset(cdc, cdc$gender == "m")
head(males)
over30 <- subset(cdc, cdc$age >= 30)
head(over30)
males.over.30 <- subset(cdc, cdc$gender == "m" & cdc$age >= 30)
head(males.over.30)

# subset respondents below 25 or above 65
young.or.old <- subset(cdc, cdc$age < 25 | cdc$age > 65)
summary(young.or.old$age)

# respondents under 23 who smoke
under23.and.smoke <- subset(cdc, cdc$age < 23 & cdc$smoke100 == 1)

# boxplot of height
boxplot(cdc$height)
summary(cdc$height)

# boxplots by gender
boxplot(cdc$height ~ cdc$gender)

# compute bmi
bmi <- (cdc$weight/cdc$height^2) * 703
boxplot(bmi ~ cdc$genhlth)
boxplot(bmi ~ cdc$smoke100)
hist(bmi, breaks = 50)
plot(cdc$weight, cdc$wtdesire)

