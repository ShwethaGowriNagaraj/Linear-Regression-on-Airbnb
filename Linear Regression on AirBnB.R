library(ggplot2)
library(tidyverse)
setwd()
airbnb <- read.csv("airbnb.csv")
# Manipulate
airbnb <- airbnb %>% 
  select (-bathrooms, -minstay, -location, -last_modified)
# use head() to print only the first few values of a vector, to avoid getting a really long list
# tail() prints only the last few values of a vector
head(airbnb$overall_satisfaction) 
airbnb <- airbnb %>% 
  mutate(overall_satisfaction = replace(overall_satisfaction, overall_satisfaction == 0, NA)) 
# create a "new" variable overall_satisfaction that is equal to overall_satisfaction with NA values where overall_satisfaction is equal to zero. 

# Say we wanted to replace NA with 0, then the command becomes: replace(overall_satisfaction, is.na(overall_satisfaction), 0)
# overall_satisfaction == NA won't work

head(airbnb$overall_satisfaction)
# Linear regression
## Simple linear regression
ggplot(data = airbnb, mapping = aes(x = overall_satisfaction, y = price)) +
  geom_jitter() # jitter instead of points, otherwise many dots get drawn over each other

ggplot(data = airbnb, mapping = aes(x = overall_satisfaction, y = log(price, base = exp(1)))) +
  geom_jitter() + # jitter instead of points, otherwise many dots get drawn over each other
  stat_summary(fun.y=mean, colour="green", size = 4, geom="point", shape = 23, fill = "green") + # means
  stat_smooth(method = "lm", se=FALSE) 
# regression line
linearmodel <- lm(price ~ overall_satisfaction, data = airbnb) # we create a linear model. The first argument is the model which takes the form of dependent variable ~ independent variable(s). The second argument is the data we should consider.
plot(linearmodel)
summary(linearmodel) # ask for a summary of this linear model

#  Correlations


# Make sure to include the use argument, otherwise the result will be NA due to the missing values on overall_satisfaction. 
# The use argument instructs R to calculate the correlation based only on the observations for which we have data on price and on overall_satisfaction.

cor(airbnb$price, airbnb$overall_satisfaction, use = "pairwise.complete.obs")

airbnb.corr <- airbnb %>% 
  filter(!is.na(overall_satisfaction)) %>% # otherwise you'll see NA's in the result
  select(price, overall_satisfaction, reviews, accommodates)

cor(airbnb.corr) # get the correlation matrix

install.packages("corrplot") # install and load the corrplot package
library(corrplot)

corrplot(cor(airbnb.corr), method = "number", type = "lower", bg = "grey") # put this in a nice table

# The command for the p-values is cor.mtest(airbnb.corr)
# But we want only the p-values, hence $p
# And we round them to five digits, hence round( , 5)

round(cor.mtest(airbnb.corr)$p, 5) 

# Multiple linear regression, without interaction
linearmodel <- lm(price ~ overall_satisfaction + reviews, data = airbnb)
summary(linearmodel)

# Multiple linear regression, with interaction

# overall_satisfaction + reviews: the interaction is not included as a predictor
# overall_satisfaction * reviews: the interaction between the two predictors is included as a predictor

linearmodel <- lm(price ~ overall_satisfaction * reviews, data = airbnb)
summary(linearmodel)


# Assumptions

# Normality of residuals
linearmodel <- lm(price ~ overall_satisfaction * reviews, data = airbnb)
residuals <- as_tibble(resid(linearmodel)) 

# ask for the residuals of the linear model with resid(linearmodel) 
# and turn this in a data frame with as_tibble()

ggplot(data = residuals, mapping = aes(x = value)) + 
  geom_histogram()

# Homoscedasticity of residuals
linearmodel <- lm(price ~ overall_satisfaction * reviews, data = airbnb)

# create a data frame (a tibble)
residuals_predicted <- tibble(residuals = resid(linearmodel), # the first variable is residuals which are the residuals of our linear model
                              predicted = predict(linearmodel)) # the second variable is predicted which are the predicted values of our linear model

ggplot(data = residuals_predicted, mapping = aes(x = predicted, y = residuals)) + 
  geom_point()

# Multicollinearity
# Make sure to include the use argument, otherwise the result will be NA due to the missing values on overall_satisfaction. 
# The use argument instructs R to calculate the correlation based only on the observations for which we have data on price and on overall_satisfaction.

cor.test(airbnb$overall_satisfaction,airbnb$reviews, use = "pairwise.complete.obs") # test for correlation
linearmodel <- lm(overall_satisfaction ~  accommodates * price, data = airbnb)
summary(linearmodel)
library(car) # the vif function is part of the car package

vif(linearmodel)

#######################################################
























































