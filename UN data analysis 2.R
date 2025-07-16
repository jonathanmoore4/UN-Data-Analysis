



library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(rpart)


# Here I load the three data sets into R.
world.bank.data <- data.frame(read.csv("C:\\Users\\jonny\\Documents\\UN data analysis\\world bank constant gdp.csv"))
fatality.data <- data.frame(read.csv("C:\\Users\\jonny\\Documents\\UN data analysis\\UN fatalities.csv"))
mission_data <- data.frame(read.csv("C:\\Users\\jonny\\Documents\\UN data analysis\\UNPMM_V2.2.csv"))

# Here I remove uneeded variables from the world bank data.
world.bank.data2 <- world.bank.data[,-c(1,3,4)]

# Here I manipulate the police fatality data frame.
fatality.data2 <- fatality.data |>
  # First, I separate the year from the date which will be needed for combining with other data.
  mutate(year = substr(incident_date, start = 1, stop = 4)) |>
  # Then I rename isocode3 as Country.Code for clarity and constituency with other data.
  mutate(Country.Code = isocode3)|>
  # Lastly, I remove unneeded variables.
  subset(select = -c(1,3,5,7,10,11,12))

# Here I verify the year and date variables are numeric and Dates respectively.
fatality.data2$year <- as.numeric(fatality.data2$year)
fatality.data2$incident_date <- as.Date(fatality.data2$incident_date)

# Here I replace the ongoing missions with a definitive end date of 2024. Although this decreases the accuracy
# of the data, in order to create a simple model, this change would be beneficial. Then I calculate the length
# of the mission and add this as a new variable.
mission_data2 <- mission_data |>
  mutate(
    mission_yearend = ifelse(mission_yearend == "ACTIVE", 2024, as.numeric(mission_yearend)),
    mission_length = as.numeric(mission_yearend) - as.numeric(mission_yearest)
  ) |>
  # Here I remove unneeded variables.
  subset(select = c(2,4,6,12,14,24,68,70))|>
  # Here I simplify the country of the mission to just belong to a single country.
  mutate(gw_country = substr(gw_country,start = 1,stop =3))

# Here I verify the year of the mission is a numeric variable.
mission_data2$task_yearest <- as.numeric(mission_data2$task_yearest)

# Here I join the fatality data set with further information corresponding to the missions.
fatality.data3 <- left_join(fatality.data2,mission_data2,
                            by = c("year" = "task_yearest", "mission_acronym" = "mission_abbrev"))

# Here I join GDP data from the world bank. This does not ultimately prove very useful because there is too much
# missing data - most likely from GDP data often not being recorded in zones which require UN missions (i.e. war zones).
fatality.data4 <- left_join(fatality.data3,world.bank.data2,by = c("gw_country" = "Country.Code"))


# Here I remove all GDP data other than that of the relevant year of the mission.
fatality.data5 <- fatality.data4 |>
  pivot_longer(
    cols = starts_with("X"),
    names_to = "gdp_year",
    values_to = "gdp"
  )|>
  # The world bank data had year columns inconsistent with the exact numeric year, which needed to be altered.
  mutate(gdp_year = as.integer(sub("X", "", gdp_year)))|>
  # This is the final setep in filtering out all other years but the year of the mission.
  filter(as.numeric(year) == as.numeric(gdp_year)) |>
  subset(select = -c(4,6,14))





# Here I create a basic linear regression model to evaluate the importance of each predictor.
# Since the fatalities data set only includes 314 observations and contains factor variables with a large
# number of levels (such as the country of the personnel member), R's inbuilt linear regression and summary 
# function were very useful in analysing the importance of these variables as there were too few observations
# to make meaningful predictions using these categorical predictors.
lm_model <- lm(formula = mission_length ~ incident_date + gender + type_of_incident
               + mission_regionclass + mission_class + mission_length + gdp,
               data = fatality.data5)


summary(lm_model)

# The results of the summary suggested that the mission region, class of mission and mission date were
# the most important predictors for the model. Moving forward, for simplicity and for the ability to make
# predictions, only these predictors were used.

################   CROSS VALIDATION  ################### 

mse <- list()
mse.trees <- list()


# Here we repeat the cross validation 1000 times for both the linear regression model and decision tree.
for (x in 1:1000){

# Here we remove all all predictors other than aforementioned important variables.
cv_data <- fatality.data5 |>
  subset(select = c(1,8,10,11))

# Here all NAs are removed.
cv_data <- na.omit(cv_data)
  
# Here we sample indices from the data so that the data can be split apart into different folds.
a <- sample(nrow(cv_data),nrow(cv_data))

# Here 5 index folds are constructed for cross validation
folds <- split(a, cut(seq_along(a), 5, labels = FALSE))

# Here I create a list of 5 data frames corresponding to each of the 5 folds.
cv_data_folds <- lapply(folds, function(idx) cv_data[idx, , drop = FALSE])

# The following is the cross validation algorithm which trains on 4 of the 5 folds and makes predictions
# on the left out fold.
basic_lms <- list()
predictions <- list()
sum_mse <- 0

for (j in seq_along(folds)) {
  test_set <- cv_data_folds[[j]]
  train_set <- do.call(rbind, cv_data_folds[-j])
  
  current_lm <- lm(mission_length ~ incident_date +  mission_class + mission_regionclass, data = train_set, na.action = na.omit)
  basic_lms[[j]] <- current_lm
  
  current_predictions <- predict(current_lm, newdata = test_set)
  predictions[[j]] <- current_predictions
  
  sum_mse <- sum_mse + mean((current_predictions - test_set$mission_length)^2)
}

# This is the final mean squared error of the model. 
mse[x] <- sum_mse/5


######### Fitting a Decision tree using the same cross validation method ############


basic_trees <- list()
predictions.trees <- list()
sum_mse.trees <- 0

for (j in seq_along(folds)) {
  test_set <- cv_data_folds[[j]]
  train_set <- do.call(rbind, cv_data_folds[-j])
  
  current_tree <- rpart(mission_length ~ incident_date +  mission_class + mission_regionclass, data = train_set, na.action = na.omit)
  basic_trees[[j]] <- current_tree
  
  current_predictions.tree <- predict(current_tree, newdata = test_set)
  predictions.trees[[j]] <- current_predictions.tree
  
  sum_mse.trees <- sum_mse.trees + mean((current_predictions.tree - test_set$mission_length)^2)
}


mse.trees[x] <- sum_mse.trees/5

}


mean(unlist(mse))
mean(unlist(mse.trees))


# Here We have the 
mean(unlist(mse))/var(cv_data$mission_length)

mean(unlist(mse.trees))/var(cv_data$mission_length)

