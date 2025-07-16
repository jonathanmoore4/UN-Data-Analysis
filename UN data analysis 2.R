library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(rpart)
library(caret)

world.bank.data <- data.frame(read.csv("C:\\Users\\jonny\\Documents\\UN data analysis\\world bank constant gdp.csv"))
fatality.data <- data.frame(read.csv("C:\\Users\\jonny\\Documents\\UN data analysis\\UN fatalities.csv"))
mission_data <- data.frame(read.csv("C:\\Users\\jonny\\Documents\\UN data analysis\\UNPMM_V2.2.csv"))

world.bank.data2 <- world.bank.data[,-c(1,3,4)]

fatality.data2 <- fatality.data |>
  mutate(year = substr(incident_date, start = 1, stop = 4)) |>
  mutate(Country.Code = isocode3)|>
  subset(select = -c(1,3,5,7,10,11,12))

fatality.data2$year <- as.numeric(fatality.data2$year)
fatality.data2$incident_date <- as.Date(fatality.data2$incident_date)


mission_data2 <- mission_data |>
  mutate(
    mission_yearend = ifelse(mission_yearend == "ACTIVE", 2024, as.numeric(mission_yearend)),
    mission_length = as.numeric(mission_yearend) - as.numeric(mission_yearest)
  ) |>
  subset(select = c(2,4,6,12,14,24,68,70))|>
  mutate(gw_country = substr(gw_country,start = 1,stop =3))

mission_data2$task_yearest <- as.numeric(mission_data2$task_yearest)

fatality.data3 <- left_join(fatality.data2,mission_data2,
                            by = c("year" = "task_yearest", "mission_acronym" = "mission_abbrev"))

fatality.data4 <- left_join(fatality.data3,world.bank.data2,by = c("gw_country" = "Country.Code"))

fatality.data5 <- fatality.data4 |>
  pivot_longer(
    cols = starts_with("X"),
    names_to = "gdp_year",
    values_to = "gdp"
  )|>
  mutate(gdp_year = as.integer(sub("X", "", gdp_year)))|>
  filter(as.numeric(year) == as.numeric(gdp_year)) |>
  subset(select = -c(4,6,14))






lm_model <- lm(formula = mission_length ~ incident_date + gender + type_of_incident
               + mission_regionclass + mission_class + mission_length + gdp,
               data = fatality.data5)


#fatality.data6 <- fatality.data5 |>
 # subset(select = c(1,8,10,11))
  
#fatality.data6 <- fatality.data6[complete.cases(fatality.data6),]  

#train_index <- sample(1:314,314*0.8)
#train_data <- fatality.data6[train_index, ]
#test_data <- fatality.data6[-train_index, ]



#lm_basic <- lm(mission_length ~ type_of_incident + incident_date + mission_class + mission_regionclass, data = train_data)

#predicions_basic <- predict(lm_basic,newdata = test_data)

#MSE <- mean((test_data$mission_length - as.vector(predicions_basic))^2)

#MSE

cv_data <- fatality.data5 |>
  subset(select = -c(1))


cv_model <- train(formula = mission_length ~ mission_class + mission_regionclass,
                  data = cv_data,
                  method = "lm",
                  trControl = trainControl(method = "cv",
                                           number = 5),
                  na.action = na.omit)



