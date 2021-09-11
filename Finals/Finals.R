if (!require("ISLR")) {install.packages("ISLR")
  library("ISLR")
}

# Question 1
data = Auto
head(data)
str(data)

model = lm(mpg~horsepower, data)
summary(model)

# Question 2
data = mtcars
data$vs = as.factor(data$vs)
data$am = as.factor(data$am)
head(data)
str(data)

model = lm(mpg~hp, data)
summary(model)
model = lm(mpg~hp+am+vs, data)
summary(model)
model = lm(mpg~hp+am+vs+(vs*hp), data)
summary(model)

# Question 3
library(MASS)
library(dplyr)
data = Boston
data$result = as.factor(ifelse(data$medv>30, 1, 0))
data = select(data, -medv)
str(data)
head(data)

model = glm(result~., data, family='binomial')
summary(model)

# Question 4
library(caret)
data$prob = model$fitted.values
data$pred = as.factor(ifelse(data$prob>=0.5, 1, 0))
table(data$result, data$pred)

# question 5
data = read.csv("C:/Users/Admin/Desktop/MM/SU21 Data Analytics for Business/Finals/abalone.csv",
                stringsAsFactors = TRUE)
str(data)
head(data)

model = lm(Rings~Diameter+Height, data)
summary(model)

# Question 6
data1 = subset(data, data$Type == "M" | data$Type == "I")
data1$Type = relevel(data1$Type, ref = "I")
model1 = lm(Diameter~Type, data1)
summary(model1)

data2 = subset(data, data$Type == "F" | data$Type == "I")
data2$Type = relevel(data2$Type, ref = "I")
model2 = lm(Diameter~Type, data2)
summary(model2)

# Question 7
data = read.csv("C:/Users/Admin/Desktop/MM/SU21 Data Analytics for Business/Finals/Admissions.csv",
                stringsAsFactors = TRUE)
data$Admitted = as.factor(data$Admitted)
str(data)
head(data)

model = glm(Admitted~., data, family = 'binomial')
summary(model)
data$prob = model$fitted.values
data$pred = as.factor(ifelse(data$prob > 0.75, 1, 0))
confusionMatrix(data = data$pred, reference = data$Admitted)

# Question 8
library('PerformanceAnalytics')
library('lubridate')
data = managers
str(data)
head(data)

which.max(c(sd(data$HAM1), sd(data$HAM3), sd(data$HAM4), sd(data$`SP500 TR`)))

# Question 9
model = lm((HAM1-`US 10Y TR`)~(`SP500 TR`-`US 10Y TR`), data)
summary(model)
model = lm((HAM2-`US 10Y TR`)~(`SP500 TR`-`US 10Y TR`), data)
summary(model)
model = lm((HAM3-`US 10Y TR`)~(`SP500 TR`-`US 10Y TR`), data)
summary(model)
model = lm((HAM4-`US 10Y TR`)~(`SP500 TR`-`US 10Y TR`), data)
summary(model)

# Question 10
(Return.cumulative(data$HAM1, geometric = TRUE)+1)*8000

# Question 11
model3 = lm((HAM3-`US 10Y TR`)~(`SP500 TR`-`US 10Y TR`), data)
model4 = lm((HAM4-`US 10Y TR`)~(`SP500 TR`-`US 10Y TR`), data)
paste0("HAM3: ", mean(data$HAM3-data$`US 10Y TR`)/model3$coefficients[2])
paste0("HAM4: ", mean(data$HAM4-data$`US 10Y TR`)/model4$coefficients[2])

# Question 12 & 13
data = read.csv("C:/Users/Admin/Desktop/MM/SU21 Data Analytics for Business/Finals/Final_Exam_Factors.csv",
                stringsAsFactors = TRUE)
data$Date = mdy(data$Date)
str(data)
head(data)

model = lm((INTC-RF)~SMB+HML+QMJ+BAB+MOM+MKT_RF, data)
summary(model)
model = lm((NVDA-RF)~SMB+HML+QMJ+BAB+MOM+MKT_RF, data)
summary(model)

# Question 14
data = read.csv("C:/Users/Admin/Desktop/MM/SU21 Data Analytics for Business/Finals/KAG_wrangled_dataset.csv",
                stringsAsFactors = TRUE)
str(data)
head(data)

table(subset(data, data$Impressions > 10000)$gender)
383/(383+406)

# Question 15
data$conversion_rate = data$Approved_Conversion/data$Total_Conversion
mean(subset(data, data$xyz_campaign_id == 916)$conversion_rate)

# Question 16
data %>% group_by(gender, age) %>%
  summarize(cpc = sum(Spent)/sum(Clicks))

# Question 17
# (9/60/60)*(72x)-(9/60/60)*(1000) = 1
# 0.0025*1000 + 1
# 3.5/0.18

# Question 18
(1-(1000/1512))*(1000/1512)^7

# Question 19
data = read.csv("C:/Users/Admin/Desktop/MM/SU21 Data Analytics for Business/Finals/sample_data.csv",
                stringsAsFactors = TRUE)
str(data)
data

# Question 20
library(lubridate)
library(PerformanceAnalytics)
library(tidyverse)
library(fpp2)
data = read.csv("C:/Users/Admin/Desktop/MM/SU21 Data Analytics for Business/Finals/Store_Demand_Final.csv",
                stringsAsFactors = TRUE)
data$Date = mdy(data$Date)
str(data)
head(data)

data_ordered = data[order(data$Date),]
data_xts = xts(x = data_ordered[,-1], order.by = data_ordered[,1])
names(data_xts) = "total_demand"
str(data_xts)
coredata(data_xts)

model = ses(y = data_xts, h = 5, alpha = 0.25)

# Extra Question 2
library(dplyr)
data = read.csv("C:/Users/Admin/Desktop/MM/SU21 Data Analytics for Business/Finals/KAG_conversion_data_wrangled.csv",
                stringsAsFactors = FALSE)
str(data)
head(data)

data$organic_conversion = ifelse(data$Spent == 0, 1, 0)
data %>% group_by(campaign_id) %>%
  summarize(sum(organic_conversion), n())

# Extra Question 3
data %>% filter(organic_conversion == 1) %>% 
  group_by(ad_id) %>%
  summarize(summ = sum(Impressions)) %>%
  filter(summ == max(summ))
