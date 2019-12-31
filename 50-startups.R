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

#Model fitting
my_model = lm(Profit~R.D.Spend, Administration, Marketing.Spend, data=startup_data)
summary(my_model)
