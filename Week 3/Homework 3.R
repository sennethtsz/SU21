library("ISLR")
data = Carseats
str(data)

# question 1-4
model = lm(Sales~Price, data)
summary(model)

# question 5-6
data = within(data, ShelveLoc <- relevel(ShelveLoc, ref="Medium"))
model = lm(Sales~Price+ShelveLoc, data)
summary(model)

data_pred = data.frame(Price = c(0),
                       ShelveLoc = c("Medium"))
predict(model, data_pred)

# question 7
data_pred = data.frame(Price = c(0),
                       ShelveLoc = c("Bad"))
predict(model, data_pred)


# question 9
data = read.csv("C:/Users/Admin/Desktop/MM/SU21 Data Analytics for Business/Homework 3/PriceDemand.csv",
                header = TRUE)
str(data)
head(data)

model = lm(Qty~Price, data)
summary(model)

# question 10
model = lm(Qty~log(Price), data)
summary(model)

# question 11
model = lm(log(Qty)~Price, data)
summary(model)

# question 12 
model = lm(log(Qty)~log(Price), data)
summary(model)