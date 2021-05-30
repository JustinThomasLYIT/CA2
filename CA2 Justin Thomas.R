# CA2 - Justin Thomas - Predictive Modelling - World Happiness Dataset

# read in csv
happy <- read.csv("World-happiness.csv")
attach(happy)

# examine the first 20 rows
head(happy,20)

str(happy)

# I will rename some columns to make them easier to work with

colnames(happy)[colnames(happy) == "Log.GDP.per.capita"] <- "Log.GDP"
colnames(happy)[colnames(happy) == "Healthy.life.expectancy.at.birth"] <- "Life.Exp"
colnames(happy)[colnames(happy) == "Freedom.to.make.life.choices"] <- "Freedom"
colnames(happy)[colnames(happy) == "Perceptions.of.corruption"] <- "Corruption"
colnames(happy)[colnames(happy) == "Positive.affect"] <- "Pos"
colnames(happy)[colnames(happy) == "Negative.affect"] <- "Neg"
colnames(happy)[colnames(happy) == "ï..Country.name"] <- "Country"


# examine the first 20 rows Again
head(happy,20)


# Build the Predictive Model

# Take out Country as its a categorical variable
happy <- subset(happy, select = -c(Country))

# examine the first 10 rows
head(happy,10)

# remove the NA entries as we know 87% of data is fully there from CA1
# we would be difficult to retrieve the missing data in this scenario
# and for the purpose of fitting an multiple linear model this approach is fine
happy <- na.omit(happy)

attach(happy)

# Examine Correlations
round(cor(cbind(Life.Ladder, year, Log.GDP, Social.support, Life.Exp, Freedom,
                Generosity, Corruption, Pos, Neg)),2)

# Life expectancy and Log GDP show a strong relationship of 0.86 which might suggest 
# only one of the variables is needed in the regression analysis. However, knowing
# these variables measure totally different things and are probably related on healthcare
# grounds, i would prefer to keep them both in for now.



# Split the data into training and test data using a 70/30 % split
# We can train out model on the training data and test how good it is against the test data
# we set the seed here so the results are reproducable each time it is ran

set.seed(1)
no_rows_data <- nrow(happy)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

training_data <- happy[sample, ]
testing_data <-  happy[-sample, ]

# MLR Model
# The standard way of speciifying a linear regression model in R is using the lm()
# function with the model we want to build expressed as a formula and the data that 
# should be used, and save it into an object that we can explore the results in detail

attach(happy)
fit <- lm(Life.Ladder ~ year + Log.GDP + Social.support + Life.Exp + Freedom + Generosity
          + Corruption + Pos + Neg, data = happy)


# Model Validation
# Now, the models accuracy needs to be evaluated.
# The training and testing data is used to do this.

summary(fit)

# we can see all the variables apart from Neg are highly sigificant very low p values

library(car)
vif(fit)

# However, running the Variance Inflation Factors against the fit suggests all variables should remain
# as none are above 10. Many are close to 1 which is good.

# check for outliers
outlierTest(fit)

# Check for normality


paste("Skewness Life Ladder: ", round(e1071::skewness(happy$Life.Ladder),2))
paste("Skewness year: ", round(e1071::skewness(happy$year),2))
paste("Skewness GDP: ", round(e1071::skewness(happy$Log.GDP),2))
paste("Skewness Social Support: ", round(e1071::skewness(happy$Social.support),2))
paste("Skewness Life Exp: ", round(e1071::skewness(happy$Life.Exp),2))
paste("Skewness Freedom: ", round(e1071::skewness(happy$Freedom),2))
paste("Skewness Generosity: ", round(e1071::skewness(happy$Generosity),2))
paste("Skewness Corruption: ", round(e1071::skewness(happy$Corruption),2))
paste("Skewness Pos: ", round(e1071::skewness(happy$Pos),2))
paste("Skewness Neg: ", round(e1071::skewness(happy$Neg),2))



# Life Ladder
opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2)) # divide graph area in 2 columns
hist(happy$Life.Ladder, main = "Normality proportion of Life Ladder", xlab = "Life Ladder")

qqnorm(happy$Life.Ladder)
qqline(happy$Life.Ladder)

par <- opar

# Year

par(mfrow = c(1,2)) # divide graph area in 2 columns
hist(happy$year, main = "Normality proportion of Year", xlab = "Year")

qqnorm(happy$year)
qqline(happy$year)

par <- opar

# GDP

par(mfrow = c(1,2)) # divide graph area in 2 columns
hist(happy$Log.GDP, main = "Normality proportion of GDP", xlab = "GDP")

qqnorm(happy$Log.GDP)
qqline(happy$Log.GDP)
par <- opar

# Social Support

par(mfrow = c(1,2)) # divide graph area in 2 columns
hist(happy$Social.support, main = "Normality proportion of Social Support", xlab = "Social Support")

qqnorm(happy$Social.support)
qqline(happy$Social.support)
par <- opar

summary(powerTransform(happy$Social.support))
# lambda is 3.63

happy$Social.support2 <- '^'(happy$Social.support,3.63)

# Raise social support to power of 3.63 to transform it to improve models fit to normality

par(mfrow = c(1,2)) # divide graph area in 2 columns
hist(happy$Social.support2, main = "Normality proportion of Social Support2", xlab = "Social Support2")

qqnorm(happy$Social.support2)
qqline(happy$Social.support2)
par <- opar
paste("Skewness Support2: ", round(e1071::skewness(happy$Social.support2),2))

# Look at corruption as skewness was high
# Corruption

par(mfrow = c(1,2)) # divide graph area in 2 columns
hist(happy$Corruption, main = "Normality proportion of Corruption", xlab = "Corruption")

qqnorm(happy$Corruption)
qqline(happy$Corruption)
par <- opar

summary(powerTransform(happy$Corruption))
# Lambda is 2.83

happy$Corruption2 <- '^'(happy$Corruption,2.83)

# Transformed Corruption variable

par(mfrow = c(1,2)) # divide graph area in 2 columns
hist(happy$Corruption2, main = "Normality proportion of Corruption2", xlab = "Corruption2")

qqnorm(happy$Corruption2)
qqline(happy$Corruption2)
par <- opar
paste("Skewness Corruption2: ", round(e1071::skewness(happy$Corruption2),2))

# Use new transformed variables in model now
fit2 <- lm(Life.Ladder ~ year + Log.GDP + Social.support2 + Life.Exp + Freedom + Generosity
          + Corruption2 + Pos + Neg, data = happy)

# resplit training/test to add these columns in
set.seed(1)
no_rows_data <- nrow(happy)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

training_data <- happy[sample, ]
testing_data <-  happy[-sample, ]

# Check for Linearity

crPlots(fit2)
# All graphs look linear


# Influential Observations using Cook's Distance

cutoff <- 4/(nrow(training_data) - length(fit2$coefficients) - 2)
plot(fit, which=4, cook.levels = cutoff)
abline(h = cutoff, lty=2, col = "red")

# influential plot

influencePlot(fit2, main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")

# Homoscedasticity
ncvTest(fit2)
spreadLevelPlot(fit2)

# Data doesnt have constant variance in our residuals so transform Y
happy$Life.Ladder2 <- '^'(happy$Life.Ladder,1.73)

# Use new transformed variables in model now
fit3 <- lm(Life.Ladder2 ~ year + Log.GDP + Social.support2 + Life.Exp + Freedom + Generosity
           + Corruption2 + Pos + Neg, data = happy)

# resplit training/test to add these columns in
set.seed(1)
no_rows_data <- nrow(happy)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

training_data <- happy[sample, ]
testing_data <-  happy[-sample, ]

# Homoscedasticity
ncvTest(fit3)
spreadLevelPlot(fit3)

install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(fit3)
summary(gvmodel)

# MOdel is not correct but i will still proceed for CA2


# Comparing models using AIC
AIC(fit, fit3)

# my results suggest my original model was better before i transformed the variables.
# again, something has gone wrong here and this shouldnt be the case.

# Stepwise Regression (backwards)
library(MASS)
fit_test <- lm(Life.Ladder2 ~ year + Log.GDP + Social.support2 + Life.Exp + Freedom + Generosity
               + Corruption2 + Pos + Neg, data = training_data)
stepAIC(fit_test, direction = "backward")



# Model Forecasting
predicted_lifeladder <- predict(fit3, testing_data)

# Make acutals_predicted dataframe
actuals_predictions <- data.frame(cbind(actuals = testing_data$Life.Ladder2, predicted = predicted_lifeladder))
head(actuals_predictions, 20)
# the actuals v predicted actually seem more closely matched than expected

# plot of actual v predictions
plot(actuals_predictions, predicted_lifeladder, main = "Actual v Predicted")

correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy
# The model show us that it has 89% colleration accuracy

#Min Max accuracy and MAPE
min_max_accuracy <- mean(apply(actuals_predictions, 1, min) / apply(actuals_predictions, 1, max))
min_max_accuracy
# Gives us a prediction accuracy of 87%


# Residual Standard Error (RSE)

sigma(fit3) / mean(testing_data$Life.Ladder2)
# This estimates an error rate of 16.5% with the data we have at hand

# Input some sample tests to evaluate model

summary(happy) # we can quickly see the ranges in input data

df1 <- data.frame(year = c(2020), Log.GDP = c(6), Social.support2 = c(0.5), Life.Exp = c(63), 
                  Freedom = c(0.75), Generosity = c(0.0), Corruption2 = c(0.75), Pos = c(0.7), Neg = c(0.25) )
predicted_lifeladder1 <- predict(fit3, df1)
predicted_lifeladder1 <- log(predicted_lifeladder1,1.73) #transform life ladder back to 1-10 scale
predicted_lifeladder1
# we get a predicted life ladder score of 4.3 with the chosen inputs
# This is used as a baseline

# change the year
df2 <- data.frame(year = c(2015), Log.GDP = c(6), Social.support2 = c(0.5), Life.Exp = c(63), 
                  Freedom = c(0.75), Generosity = c(0.0), Corruption2 = c(0.75), Pos = c(0.7), Neg = c(0.25) )
predicted_lifeladder2 <- predict(fit3, df2)
predicted_lifeladder2 <- log(predicted_lifeladder2,1.73) #transform life ladder back to 1-10 scale
predicted_lifeladder2
# Changing just the year to 2015 changed the predicted life ladder score to 4.36 which is a tiny increase.

# change the GDP
df3 <- data.frame(year = c(2020), Log.GDP = c(11), Social.support2 = c(0.5), Life.Exp = c(63), 
                  Freedom = c(0.75), Generosity = c(0.0), Corruption2 = c(0.75), Pos = c(0.7), Neg = c(0.25) )
predicted_lifeladder3 <- predict(fit3, df3)
predicted_lifeladder3 <- log(predicted_lifeladder3,1.73) #transform life ladder back to 1-10 scale
predicted_lifeladder3
# Changing just the GDP to 11 changed the predicted life ladder score to 5.55 which is significant increase.

# change the social support 2 variable
df4 <- data.frame(year = c(2020), Log.GDP = c(6), Social.support2 = c(0.9), Life.Exp = c(63), 
                  Freedom = c(0.75), Generosity = c(0.0), Corruption2 = c(0.75), Pos = c(0.7), Neg = c(0.25) )
predicted_lifeladder4 <- predict(fit3, df4)
predicted_lifeladder4 <- log(predicted_lifeladder4,1.73) #transform life ladder back to 1-10 scale
predicted_lifeladder4
# Changing just the social support 2 variable to 0.9 changed the predicted life ladder score to 4.77 which is a small increase.


# change the life expectancy
df5 <- data.frame(year = c(2020), Log.GDP = c(6), Social.support2 = c(0.5), Life.Exp = c(75), 
                  Freedom = c(0.75), Generosity = c(0.0), Corruption2 = c(0.75), Pos = c(0.7), Neg = c(0.25) )
predicted_lifeladder5 <- predict(fit3, df5)
predicted_lifeladder5 <- log(predicted_lifeladder5,1.73) #transform life ladder back to 1-10 scale
predicted_lifeladder5
# Changing just the life expectancy to 75 changed the predicted life ladder score to 4.6 which is a small increase.

# change the freedom
df6 <- data.frame(year = c(2020), Log.GDP = c(6), Social.support2 = c(0.5), Life.Exp = c(63), 
                  Freedom = c(0.95), Generosity = c(0.0), Corruption2 = c(0.75), Pos = c(0.7), Neg = c(0.25) )
predicted_lifeladder6 <- predict(fit3, df6)
predicted_lifeladder6 <- log(predicted_lifeladder6,1.73) #transform life ladder back to 1-10 scale
predicted_lifeladder6
# Changing just the freedom to 0.95 changed the predicted life ladder score to 4.38 which is a tiny increase.

# change the genorisity
df7 <- data.frame(year = c(2020), Log.GDP = c(6), Social.support2 = c(0.5), Life.Exp = c(63), 
                  Freedom = c(0.75), Generosity = c(0.65), Corruption2 = c(0.75), Pos = c(0.7), Neg = c(0.25) )
predicted_lifeladder7 <- predict(fit3, df7)
predicted_lifeladder7 <- log(predicted_lifeladder7,1.73) #transform life ladder back to 1-10 scale
predicted_lifeladder7
# Changing just the genorisity to 0.95 changed the predicted life ladder score to 4.59 which is a small increase.

# change the corruption
df8 <- data.frame(year = c(2020), Log.GDP = c(6), Social.support2 = c(0.5), Life.Exp = c(63), 
                  Freedom = c(0.75), Generosity = c(0.00), Corruption2 = c(0.95), Pos = c(0.7), Neg = c(0.25) )
predicted_lifeladder8 <- predict(fit3, df8)
predicted_lifeladder8 <- log(predicted_lifeladder8,1.73) #transform life ladder back to 1-10 scale
predicted_lifeladder8
# Changing just the corruption to 0.95 changed the predicted life ladder score to 4.15 which is a small decrease

# change the positive affect
df9 <- data.frame(year = c(2020), Log.GDP = c(6), Social.support2 = c(0.5), Life.Exp = c(63), 
                  Freedom = c(0.75), Generosity = c(0.00), Corruption2 = c(0.75), Pos = c(0.9), Neg = c(0.25) )
predicted_lifeladder9 <- predict(fit3, df9)
predicted_lifeladder9 <- log(predicted_lifeladder9,1.73) #transform life ladder back to 1-10 scale
predicted_lifeladder9
# Changing just the positive affect to 0.95 changed the predicted life ladder score to 4.65 which is a small increase

# change the negative affect
df10 <- data.frame(year = c(2020), Log.GDP = c(6), Social.support2 = c(0.5), Life.Exp = c(63), 
                  Freedom = c(0.75), Generosity = c(0.00), Corruption2 = c(0.75), Pos = c(0.9), Neg = c(0.7) )
predicted_lifeladder10 <- predict(fit3, df10)
predicted_lifeladder10 <- log(predicted_lifeladder10,1.73) #transform life ladder back to 1-10 scale
predicted_lifeladder10
# Changing just the negative affect to 0.95 changed the predicted life ladder score to 4.81 which is a small increase


summary(fit3)


# workings

happy2 <- read.csv("World-happiness.csv")
summary(happy2)
ucountry <- unique(happy2$ï..Country.name)
length(ucountry)
