data = read.csv("C:/Users/Admin/Desktop/MM/SU21 Data Analytics for Business/Week 9/Advertising_Updated.csv",
                header = TRUE)
str(data)

model = lm(Sales~., data)
summary(model)

pred_data = data.frame(TV = c(200),
                       Radio = c(10),
                       Newspaper = c(20))
predict(model, pred_data, interval = 'prediction')
?lm
