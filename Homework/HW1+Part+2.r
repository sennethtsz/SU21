
# SOLUTION BEGINS HERE
data = read.csv("../resource/asnlib/publicdata/airbnb_data.csv")
head(data)

drops = c("room_id", "survey_id", "host_id", "city")
data = data[,!names(data) %in% drops]
head(data)
str(data)

model = lm(price~., data=data)
summary(model)
# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE
data_pred = data.frame(bedrooms = c(1), 
                       accommodates = c(2), 
                       reviews = c(70), 
                       overall_satisfaction = c(4), 
                       room_type = c("Private room"))

data_pred$bedrooms = as.integer(data_pred$bedrooms)
data_pred$accommodates = as.integer(data_pred$accommodates)
data_pred$reviews = as.integer(data_pred$reviews)
data_pred$overall_satisfaction = as.double(data_pred$overall_satisfaction)
str(data_pred)
head(data_pred)

predict(model, data_pred)
# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE
# visualize outliers
lower.panel <- function(x, y){
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- round(cor(x, y), digits=2)
    txt <- paste0("R = ", r)
    cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(data, 
      lower.panel = lower.panel)

# install.packages("tidyverse", lib="../work/")
# library("tidyverse", lib.loc="../work/")

# identifying outliers using cooks
cooks = cooks.distance(model)
plot(cooks, pch="*")
abline(h=1,col="red")
text(x=1:length(cooks)+20, y=cooks, label=ifelse(cooks>1, names(cooks), ""), col="red")

as.numeric(names(cooks[cooks>1]))

# removing outlier
outlier = as.numeric(names(cooks[cooks>1]))
data_clean = data[-outlier,]

# model without outlier
model_clean = lm(price~., data_clean)
summary(model_clean)
predict(model_clean, data_pred)
# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE
data = read.csv("../resource/asnlib/publicdata/direct_marketing.csv")
str(data)
head(data)

data = within(data, History <- relevel(History, ref = 4))
str(data)

model = lm(AmountSpent~Salary + History + Salary*History, data)
summary(model)
# AmountSpent=1.96+0.002Salary+25.45Low+79.30Medium+72.67High-0.002LowSalary-0.002MediumSalary-0.0006HighSalary
# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE
data_none = data.frame(Salary = c(10000),
                       History = c("None"))
data_low = data.frame(Salary = c(10000),
                      History = c("Low"))
data_medium = data.frame(Salary = c(10000),
                         History = c("Medium"))
data_high = data.frame(Salary = c(10000),
                       History = c("High"))
print(paste0("None: ", predict(model, data_none)))
print(paste0("Low: ", predict(model, data_low)))
print(paste0("Medium: ", predict(model, data_medium)))
print(paste0("High: ", predict(model, data_high)))
# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE
data = read.csv("../resource/asnlib/publicdata/airbnb_data.csv")
str(data)

# linear-linear
model_linear_linear = lm(price~overall_satisfaction, data)
summary(model_linear_linear)

# linear-log
model_linear_log = lm(price~log(overall_satisfaction+1), data)
summary(model_linear_log)

# log-linear
model_log_linear = lm(log(price)~overall_satisfaction, data)
summary(model_log_linear)

# log-log
model_log_log = lm(log(price+1)~log(overall_satisfaction+1), data)
summary(model_log_log)

# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE
data = read.csv("../resource/asnlib/publicdata/titanic_data.csv")
str(data)
head(data)

model = glm(Survived~Sex, data, family = "binomial")
summary(model)
# SOLUTION ENDS HERE
