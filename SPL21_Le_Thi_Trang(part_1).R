################################################################################ 
#################### Statistical Programming Languages 2021  ##################
#################### Programming Tasks                       ##################
#################### Author: Trang Le Thi (616071)           ##################
################################################################################

### Exercise 1:

sink("my_session.txt")
sessionInfo()
sink()

# function sessionInfo() provides information about R version, attached base packages in R,
# processor and operating system of the computer, and other information. 

###############################################################

### Exercise 2:

# 2a)

bikes <- read.csv("bikes.csv")

bikes <- subset(bikes, select = c(dteday, season, hr, workingday, weathersit, temp, cnt))


head(bikes)

str(bikes)

# to see the order of variable in each column
names(bikes)

names (bikes)[1] <- "date"
names (bikes)[3]<- "hour"
names (bikes)[5]<- "weather"
names (bikes)[6]<- "temperature_norm"
names (bikes)[7]<- "bike_count"

str(bikes)


# transform class of date to POSIXct:
class(bikes$date)
bikes$date <- as.POSIXct(bikes$date)
class(bikes$date)


# tranform class of workingday to logical
table(bikes$workingday)
bikes$workingday <- as.logical(bikes$workingday)
class(bikes$workingday)
table(bikes$workingday)


# tranform season to factor
table(bikes$season)
bikes$season <- factor(bikes$season, labels = c("winter", "spring", "summer", "fall"))
table(bikes$season)
class(bikes$season)


# transform weather to factor

table(bikes$weather)
bikes$weather <- factor(bikes$weather, labels = c("very good", "good", "bad", "very bad"))
table(bikes$weather)
class(bikes$weather)

# What are the classes of the other variables? Are they all adequate? 
## class of hour: integer
class(bikes$hour)
## class of temperature_norm: numeric
class(bikes$temperature_norm)
## class of bike_count: integer
class(bikes$bike_count)

# Classes of the other variable are adequate


###############################################################

# 2b) Does the data set contain missing values? 
sum(is.na(bikes))
# There is no missing value in this data frame


###############################################################

# 2c) How many observations in the data set counted at least 150 in bike_count? 
# What's their share on the total number of observations?

high_count <- sum(bikes$bike_count >= 150)
# There are 8414 observations in the data set counted at least 150 in bike_count

total <- length(bikes$bike_count)
share <- high_count/total


# The percentage of their share is 48.41%

###############################################################

# 2d) Use a two-sample t-test to decide whether the average value of bike_count differs in the two hours
# 12 and 15 in the season winter (significance level: 5%). Briefly justify your decision in a comment.

attach(bikes)

two_group <- hour %o% c(12, 15)

t.test(bike_count[two_group], hour[two_group])

# p-value < 2.2e-16 < 0.05 => the null hypothesis of identical means can be rejected.
# Thus, we conclude that there is the difference between the average number of bikes rented in 12h and one in 15h.

###############################################################

# 2e) Compute the total number of bike_count for each value of weather and sort them in ascending order.
# Is the order surprising? Briefly justify your answer in a comment.

weather_scenarios <- tapply(bike_count, weather, sum, na.rm = TRUE)
sort(weather_scenarios, decreasing = FALSE)


# The number of rented bikes is higher when the weather is better. It is not a surprising result. 

###############################################################

# 2f)For each value of workingday compute the average bike_count per hour:

criteria <- list(workingday, hour)
count_hour <- table(criteria)

sum_bike <- tapply(bike_count, criteria, sum, na.rm = TRUE)
average_bike_per_hour <- sum_bike/count_hour
average_bike_per_hour

# Draw a plot
barplot(average_bike_per_hour, beside=TRUE, col=c("aquamarine3","coral"), main = " Average counts for workingdays versus non-workingdays over hour")
legend("topleft", c("non-working","working"), pch=15, col=c("aquamarine3","coral"))

detach(bikes)
# In general, bikes were rented mainly from 7 to 22h
# On the working days, bikes were rented mostly from 7 to 9h, or after 17h
# On the non-working days, most of bike were rent from 10 to 19h

###############################################################

# 2g)

# Add a new variable temperature to the data set, which contains the original temperature values t in Celsius.

tmin <- -6
tmax <- +39
temperature <- bikes$temperature_norm*(tmax - tmin) + tmin
bikes$temperature <- temperature
str(bikes)

# Range of temeperature is from -5.1 to 39
range(bikes$temperature)

# The date of the observation with the highest temperature is 07/07/2012

bikes$date[which(bikes$temperature == max(bikes$temperature))]

###############################################################

### Exercise 3: 
install.packages("quantreg")
library(quantreg)
data(engel)
str(engel)


# To creat pdf file for the plot:

pdf(file = "myplot_exercise3.pdf", width = 7, height = 7)

# Set plot layout
layout(mat = matrix(c(2, 1, 0, 3), nrow = 2, byrow = FALSE), 
       heights = c(1, 2), widths = c(2,1))

# Plot 1: Scatterplot
log_income <- log(engel$income, base = 10)
log_foodexp <- log(engel$foodexp, base = 10)
plot(log_income, log_foodexp, pch = 3, col = "cyan", xlab = "Log Income", ylab = "Log Food Expenditure")
reg <- lm(log_foodexp ~ log_income)
abline(reg)

# Plot 2:

hist(log_income, freq = FALSE, col = "grey", border = "black", xlab = "Log Income", xaxt = "n", yaxt = "n", main = NULL)

axis(1, at = c(2.5, 3, 3.5))
axis(2, seq(0, 2.5, 0.5), las = 2, gap.axis = 1)

# question: how to make a space from 2.5 to 0 in xaxis

# Add density to plot as line

m <- mean(log_income, na.rm = TRUE)
sd <- sd(log_income, na.rm = TRUE)
seq <- seq(min(log_income, na.rm = TRUE), max(log_income, na.rm = TRUE), length.out = 100)
lines(seq, dnorm(x = seq, mean = m, sd = sd), col = "chartreuse")

lines(density(log_income, kernel = "gaussian"), lty = 2, col = "red")


# Plot 3

boxplot(log_foodexp, col = "orange", ylab = "Log Food Expenditures")

# To finish creating the pdf file

dev.off()



