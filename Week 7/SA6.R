if (!require("lubridate")){install.packages("lubridate")
  library("lubridate")}

data = read.csv("C:/Users/Admin/Desktop/MM/SU21 Data Analytics for Business/Week 7/Assignment_3_-_factors.csv",
                header = TRUE)
data$Date = mdy(data$Date)
str(data)

# Question 1-2
model = lm(Brk_exret~Mkt_rf, data)
summary(model)

# Question 3-6
model = lm(Brk_exret~Mkt_rf+SMB+HML, data)
summary(model)

# Question 7-10
model = lm(Brk_exret~Mkt_rf+SMB+HML+Mom+BAB+QMJ, data)
summary(model)
