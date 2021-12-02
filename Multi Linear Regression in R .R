#CREATED BY - PRACHI BHATT - 19BAI10111



#STEP 1:

#load the packages into your R environment
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

#Loading the data set
df <- read.csv("/users/prachibhatt/Desktop/Regression in R/StartupsC.csv",
               header = FALSE)
df
summary(StartupsC)
#because the variables are quantitative, running the code produces a numeric summary 
#of the data for the independent variables (MarketingSpend, Administration and RandDSpend)
#and the dependent variable (Profit):

#***********************************************************************************************
#STEP 2:Make sure your data meet the assumptions
#Independence of observations (aka no autocorrelation)

cor(StartupsC$RandDSpend, StartupsC$Administration)
cor(StartupsC$Administration, StartupsC$MarketingSpend)
#When we run this code, the output is 0.1960682, -0.08583099. The correlation between RandDSpend 
#and Administration is less that is only 19%,  so we can include both parameters in our model.
#And The correlation between Administration and MarketingSpend is less that is only -8%,  so we can include 
#both parameters in our model.

#Normality-
qplot(StartupsC$Profit, geom="histogram",bins = 40) 
#The observations are roughly bell-shaped (more observations in the middle of the distribution, 
#fewer on the tails), so we can proceed with the linear regression.

#The bin width of a date variable is the number of days in each time; 
#the bin width of a time variable is the number of seconds. bins. Number of bins. Overridden by binwidth. Defaults to 30.

#Linearity
plot(x = StartupsC$MarketingSpend,
     y = StartupsC$Profit,
     xlab = "MarketingSpend",
     ylab = "Profit",
     main = "Profit vs MarketingSpend")
#Although the relationship between Marketing spend and profit is a bit less clear, it still appears linear.
#We can proceed with linear regression.
plot(x = StartupsC$RandDSpend,
     y = StartupsC$Profit,
     xlab = "R&DSpend",
     ylab = "Profit",
     main = "Profit vs R&DSpend")
#appears linear so we can proceed with linear regression.
plot(x = StartupsC$Administration,
     y = StartupsC$Profit,
     xlab = "Administration",
     ylab = "Profit",
     main = "Profit vs Administration")
#Although the relationship between Marketing spend and profit is a bit less clear, it still appears linear.
#We can proceed with linear regression.
#***************************************************************************************************************
#STEP 3-Multiple Linear regression: RandDSpend, Administration, MarketingSpend, and  Profit

lmStartups <- lm(StartupsC$Profit~StartupsC$RandDSpend+ StartupsC$Administration+ StartupsC$MarketingSpend, data = df) #Create the multi linear regression
summary(lmStartups)
#The estimated effect of R and D spend on Profit is 7.771e-01, while the estimated effect of Marketing spend is 1.926e-02.
#This means that for every 1% increase in R and D spend, there is a correlated 7.771e-01% increase in the Profit.
#Meanwhile, for every 1% increase in Marketing spend, there is a 1.926e-02% increase in the Profit.
#and for every 1% increase in Administration, there is a 6.029e-02% decrease in the Profit.

#The standard errors for these regression coefficients are very small, and the t-statistics are very large.
#The p-values reflect these small errors and large t-statistics. For both parameters, 
#there is almost zero probability that this effect is due to chance.

#*****************************************************************************************************************
#STEP 4-Step 4: Check for homoscedasticity
par(mfrow=c(2,2))
plot(lmStartups)
par(mfrow=c(1,1))

#par(mfrow()) command will divide the Plots window into the number of rows and columns specified in the brackets.
#Residuals are the unexplained variance. They are not exactly the same as model error, but they are calculated from it, so seeing a bias in the residuals would also indicate a bias in the error.

#The most important thing to look for is that the red lines representing the mean of the residuals are all 
#basically horizontal but not a straight line and not properly centered around zero. This means there are some outliers or biases in the data that 
#would make a linear regression invalid in some cases.

#In the Normal Q-Qplot in the top right, we can see that the real residuals from our model form an almost 
#perfectly one-to-one line with the theoretical residuals from a perfect model.

#Based on these residuals, we can say that our model meets the assumption of homoscedasticity.

#****************************************************************************************************************
#STEP 5-  Visualize the results with a graph

#The visualization step for multiple regression is more difficult than for simple regression, 
#because we now have two predictors. One option is to plot a plane, but these are difficult to read and not often published.
#We will try a different method: plotting the relationship between Marketing spend and profit at different
#levels of Administration and RandDSpend. In this we will take like, Administration and RandDSpend will be treated as a factor with three levels, just for the 
#purposes of displaying the relationships in our data.

#1. Create a new dataframe with the information needed to plot the model

plotting<-expand.grid(
        MarketingSpend = seq(min(StartupsC$MarketingSpend), max(StartupsC$MarketingSpend), length.out=30),
        Administration=c(min(StartupsC$Administration), mean(StartupsC$Administration), max(StartupsC$Administration)),
        RandDSpend = c(min(StartupsC$RandDSpend), mean(StartupsC$RandDSpend), max(StartupsC$RandDSpend)))

plotting
#2. Predict the values of profit based on your linear model
newPlotting<-plotting[!(row.names(plotting) %in% c(47:270)), ]

newPlotting$predicted.y <- predict.lm(lmStartups, newdata=newPlotting)

#3. Round the Administration and RandDSpend numbers to two decimals
newPlotting$Administration<- round(newPlotting$Administration, digits = 2)
newPlotting$RandDSpend<- round(newPlotting$RandDSpend, digits = 2)

#4. Change the Administration and RandDSpend variables into factors

newPlotting$Administration <- as.factor(newPlotting$Administration)
newPlotting$RandDSpend <- as.factor(newPlotting$RandDSpend)

#5. Plot the original data
startups.plot <- ggplot(StartupsC, aes(x=MarketingSpend, y=Profit)) +
        geom_point()

startups.plot

#6. Add the regression lines
startups.plot <- startups.plot+
        geom_line(data=newPlotting, aes(x=MarketingSpend, y=predicted.y, color=Administration), size=1.25)

startups.plot


#7. Make the graph ready for publication
startups.plot <-
        startups.plot +
        theme_bw() +
        labs(title = "Profit as a function of MarketingSpend, \n Administration, and RandDSpend",
             x = "MarketingSpend",
             y = "Profit",
             color = "Administration and RandDSpend")

startups.plot

#Equation 
#startups.plot + annotate(geom="text", x=30, y=1.75, label="= 15 + (1.926e-02*MarketingSpend)+ (-6.029e-02*Administration) +(7.771e-01*RandDSpend)")


