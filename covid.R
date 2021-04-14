library(Hmisc)
rm(list=ls()) #removes all variables stored previously

data <- read.csv("~/Documents/R/covid_R/COVID19_line_list_data.csv")
#from Hmisc
describe(data) 

#clean up death column as there are some dates into it 
data$death_dummy <- as.integer(data$death != 0)

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
# p-value < 0.05 => we reject null hypothesis/conjecture
# conclude that this is statistically significant 
# claim is true 

# GENDER
# claim: gender has no effect
men = subset(data, gender == "male")
women = subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE) 
mean(women$death_dummy, na.rm = TRUE) 

# is this statistically significant?
t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.99)
# p-value = 0.002 < 0.05, so this is statistically significant
# claim is true


#GRAPH date-deaths

#date format
dDate = as.Date(data$reporting.date, "%m/%d")

#regroup the date with aggregate() and FUN=sum to sum the values of the same date
a = aggregate(data$death_dummy, by=list(reporting.date=dDate), FUN=sum)

#Once I aggregate, I convert to a list and then a data.frame
new_df = as.data.frame(as.list(a))
new_df
plot(new_df$reporting.date, new_df$x, type="l", xlab="Date",ylab="Deaths", ylim = c(0,17)) 

