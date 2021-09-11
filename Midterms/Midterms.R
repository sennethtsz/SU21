# Question 1
if (!require("ISLR")) {
  install.packages("ISLR")
  library("ISLR")}

data = College
data = cbind(School = rownames(data), data)
rownames(data) = 1:nrow(data)
head(data)
str(data)

model = lm(Personal~Room.Board, data)
summary(model)

# Question 2
model_loli = lm(log(Personal)~Room.Board, data)
model_lolo = lm(log(Personal)~log(Room.Board), data)
model_lilo = lm(Personal~log(Room.Board), data)

summary(model)
summary(model_loli)
summary(model_lolo)
summary(model_lilo)

# Question 3
summary(model_lilo)

# Question 4
summary(model_loli)

# Question 5
summary(model_lolo)

# Question 6, 7, 8, 9
data = read.csv("C:/Users/Admin/Desktop/MM/SU21 Data Analytics for Business/Midterms/binary.csv",
                header = TRUE,
                fileEncoding ="UTF-8-BOM")
# data$admit = as.factor(data$admit)
# data$admit = relevel(data$admit, ref="0")
str(data)

model = glm(admit~., data, family = "binomial")
summary(model)

# Question 10
if (!require("ggExtra")){
  install.packages("ggExtra")
  library("ggExtra")}
if (!require("ROCR")){
  install.packages("ROCR")
  library("ROCR")}
if (!require("pROC")){
  install.packages("pROC")
  library("pROC")}

data$prob = predict(model, data[,2:3], type = "response")
# data$pred = ifelse(data$prob > 0.5, 1, 0)
pred = prediction(data$prob, data$admit)
perf = performance(pred, measure = "auc")
perf@y.values

# Question 11
if (!require("lubridate")){
  install.packages("lubridate")
  library("lubridate")}
data = read.csv("C:/Users/Admin/Desktop/MM/SU21 Data Analytics for Business/Midterms/Berkshire.csv",
                header = TRUE,
                fileEncoding ="UTF-8-BOM")
data$Date = mdy(data$Date)
data = subset(data, data$Date >= as.Date("1976-11-30") & data$Date <= as.Date("2005-12-31"))
str(data)
summary(data$Date)

sd(data$BrkRet)

# Question 12
mean(data$BrkRet)*100

# Question 13
model = lm(BrkRet~MKT, data)
summary(model)

# Question 14
if(!require("PerformanceAnalytics")){
  install.packages("PerformanceAnalytics")
  library("PerformanceAnalytics")}
if(!require("xts")){
  install.packages("xts")
  library("xts")}

data_ordered = data[order(data$Date),]
data_xts = xts(data_ordered[,-1], order.by = data_ordered$Date)
format((as.numeric(Return.cumulative(data_xts$BrkRet))+0)*10000, nsmall = 2, big.mark = ",")

# Question 15
SharpeRatio(data_xts$BrkRet, data_xts$RF)

# Question 16
SharpeRatio(data_xts$MKT, data_xts$RF)

# Question 17, 18
data = read.csv("C:/Users/Admin/Desktop/MM/SU21 Data Analytics for Business/Midterms/Factor_HiTec.csv",
                header = TRUE,
                fileEncoding ="UTF-8-BOM")
str(data)
head(data)

model = lm(HiTec_rf~Mkt_rf+SMB+HML+Mom+BAB+QMJ, data)
summary(model)

# Question 19
if(!require("zoo")){
  install.packages("zoo")
  library("zoo")}
data = read.csv("C:/Users/Admin/Desktop/MM/SU21 Data Analytics for Business/Midterms/UPS_KO.csv",
                header = TRUE,
                fileEncoding ="UTF-8-BOM")
data$Date = ym(data$Date)
data = subset(data, data$Date >= as.Date("2015-04-01") & data$Date <= as.Date("2018-11-01"))
str(data)
head(data)
summary(data$Date)

model_ups = lm(UPS~Mkt_RF+SMB+HML, data)
summary(model_ups)

# Question 20
model_ko = lm(KO~Mkt_RF+SMB+HML, data)
summary(model_ko)
