library(Hmisc)
rm(list=ls()) #removes all variables stored previously

data <- read.csv("~/Documents/covid_R/COVID19_line_list_data.csv")
#from Hmisc
describe(data) 

#clean up death column as there are some dates into it 
data$death_dummy <- as.integer(data$death!= 0)

# death rate
sum(data$death_dummy) / nrow(data)

# AGE
# claim: people who die are older

dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)

#na.rm to remove all the lines where the age is unknown
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)

# is this statistically significant?
t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.99)
# normally, if p-value < 0.05, we reject null hypothesis/conjecture
# here, p-value ~ 0, so we reject the null hypothesis and 
# conclude that this is statistically significant 
#people who die are older: 99 percent confidence interval: -25.52122 -15.50661 (delta alive-death)
# claim is true 

# GENDER
# claim: gender has no effect
men = subset(data, gender == "male")
women = subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE) 
mean(women$death_dummy, na.rm = TRUE) 

# is this statistically significant?
t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.99)
# 99% confidence: men have from 0.8% to 8.8% higher chance
# of dying.
# p-value = 0.002 < 0.05, so this is statistically
# significant
# claim is true

