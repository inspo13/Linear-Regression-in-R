#CREATED BY - PRACHI BHATT - 19BAI10111


#STEP 1:
#install the packages you need for the analysis
install.packages("ggplot2")
#data visualization
install.packages("dplyr")
#make data manipulation easier
install.packages("broom")
#an attempt to bridge the gap from untidy outputs of predictions and estimations to the tidy data we want to work with.
install.packages("ggpubr")
#provides some easy-to-use functions for creating and customizing 'ggplot2'- based publication ready plots.

#load the packages into your R environment
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

#Loading the data set
df <- read.csv("/users/prachibhatt/Desktop/Regression in R/happyscore_income.csv",
               header = FALSE)
df
summary(happyscore_income)
#when we run this function we see a table in our console with a numeric summary of the data.
#This tells us the minimum, median, mean, and maximum values of the independent variable (income) and 
#dependent variable (happyScore):

#**********************************************************************************************************
#STEP 2:Make sure your data meet the assumptions
#Normality-
qplot(happyscore_income$happyScore, geom="histogram", bins =80) 
#The observations are roughly bell-shaped (more observations in the middle of the distribution, 
#fewer on the tails), so we can proceed with the linear regression.
#The bin width of a date variable is the number of days in each time; 
#the bin width of a time variable is the number of seconds. bins. Number of bins. Overridden by binwidth. Defaults to 30.

#Linearity
plot(x = happyscore_income$income,
     y = happyscore_income$happyScore,
     xlab = "income",
     ylab = "happyScore",
     main = "happyScore vs income")
#The relationship looks roughly linear, so we can proceed with the linear model.

#**************************************************************************************************************
#STEP 3- Simple regression: income and happiness
#happyScore=a+income∗b
#“a” and “b” are called the intercept and the slope respectively
#The slope measures the change of happyScore with respect to the income.
#lm([target variable] ~ [predictor variables], data = [data source])
#With the command summary(lmhappyscore_income) you can see detailed information on the model’s performance and coefficients.
lmhappyscore_income = lm(happyscore_income$income~happyscore_income$happyScore, data = df) #Create the linear regression
summary(lmhappyscore_income)

#The estimated effect of income on happiness is 4291.8
# the most important thing to note is the p-value (here it is 2.2e-16, or almost zero), 
#which will indicate whether the model fits the data well.
#From these results, we can say that there is a significant positive relationship between income and happiness 
#(p-value < 0.001),with a 4291.8-unit (+/- 0.01) increase in happiness for every unit increase in income.
#**************************************************************************************************************
#STEP 4- Check for homoscedasticity
par(mfrow=c(2,2))
plot(lmhappyscore_income)
par(mfrow=c(1,1))

#The most important thing to look for is that the red lines representing the mean of the residuals are all 
#basically horizontal but not a straight line and not properly centered around zero. This means there are some outliers or biases in the data that 
#would make a linear regression invalid in some cases.

#In the Normal Q-Qplot in the top right, we can see that the real residuals from our model form an almost 
#perfectly one-to-one line with the theoretical residuals from a perfect model.

#Based on these residuals, we can say that our model meets the assumption of homoscedasticity.

#******************************************************************************************************
#Step 5: Visualize the results with a graph
income.graph<-ggplot(happyscore_income, aes(x=income, y=happyScore))+
  geom_point()
income.graph
#Adding the linear regression line to the plotted data
income.graph <- income.graph + geom_smooth(method="lm", col="black")

income.graph

#Adding the equation for the regression line.

income.graph <- income.graph +
  stat_regline_equation(label.x = 3, label.y = 7)

income.graph

#Make the graph ready for publication
income.graph +
  theme_bw() +
  labs(title = "Reported happiness as a function of income",
       x = "Income (x$10,000)",
       y = "Happiness score (0 to 10)")
