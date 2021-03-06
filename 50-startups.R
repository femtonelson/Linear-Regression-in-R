#Importing the data
startup_data <- read.csv("50-startups.csv")

#Visualizing the data
str(startup_data)

#Given the relative small size of the dataset, cross-validation will not be performed.

# Evaluation of correlation matrix between numerical variables
startup_numeric <- startup_data %>% select(R.D.Spend, Administration, Marketing.Spend, Profit)
cor(startup_numeric)

# Analysis of variance between Profit and State (Single factor)
startup_factor <- startup_data %>% select(State, Profit)
test_aov <- aov(Profit~State, data=startup_factor)
summary(test_aov)

#Model fitting with all explanatory variables
my_model = lm(Profit~., data=startup_data)
summary(my_model)

#Variable selection - Considering only numerical explanatory variables in the model
my_model = lm(Profit~., data=startup_numeric)
summary(my_model)

#Considering only Marketing.Spend & R.D.Spend in the model
my_model = lm(Profit~R.D.Spend+Marketing.Spend, data=startup_numeric)
summary(my_model)

par(mfrow=c(2,2))
plot(my_model)

# Pedicting Profitability
newdata <- data.frame(R.D.Spend=300000, Marketing.Spend=200000)
result1 <- predict(my_model, newdata, interval="prediction",level=0.95)
result2 <- predict(my_model, newdata, interval="confidence",level=0.95)
