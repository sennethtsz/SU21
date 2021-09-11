
# SOLUTION BEGINS HERE

if (!require("dplyr")) {install.packages("dplyr")
                       library("dplyr")}

data = read.csv("../resource/asnlib/publicdata/KAG.csv", stringsAsFactors = FALSE)
str(data)
head(data)
unique(data$campaign_id)

summarize_at(data[data$Clicks == 0 & data$Spent == 0,], vars(CTR, CPC), list(mean))

data %>% filter(CPC == min(CPC)) %>% filter(Impressions == max(Impressions))

# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE

data %>% mutate(CPM = Spent/(Impressions/1000)) %>%
group_by(campaign_id) %>%
summarize(mean_CPM = mean(CPM), n(), .groups = 'drop') %>% 
filter(mean_CPM == max(mean_CPM))

# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE

data = data %>% mutate(ROAS = round((5*Total_Conversion + 50*Approved_Conversion)/Spent, 2))
data_box = data %>% filter(Spent != 0) %>%
    filter(interest == 15|interest == 21|interest == 101)
str(data_box)

if (!require("ggplot2")) {install.packages("ggplot2")
                       library("ggplot2")}

ggplot(data_box, 
       aes(x = as.factor(interest), y = ROAS, fill = as.factor(gender))) +
geom_boxplot() +
scale_y_log10() +
xlab("Interest ID") +
ylab("ROAS") +
ggtitle("ROAS by Gender") +
theme(plot.title = element_text(hjust = 0.5))

# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE

data %>% filter(campaign_id == 1178) %>%
    filter(Spent != 0) %>%
    group_by(gender) %>%
    summarize("Mean ROAS" = round(mean(ROAS), 2), "Median ROAS" = median(ROAS), count = n(), .groups = 'drop')

# SOLUTION ENDS HERE

library(pROC) 
library(caret)
library(dplyr) 
library(ggplot2)

data <- read.csv("../resource/asnlib/publicdata/Advertising.csv", header = TRUE, stringsAsFactors = FALSE)
data$Clicked.on.Ad <- as.factor(data$Clicked.on.Ad)
head(data)

str(data)

# SOLUTION BEGINS HERE

ggplot(data, 
       aes(x = Daily.Internet.Usage, y = Age)) +
geom_point(aes(color = Clicked.on.Ad)) +
xlab("Daily Internet Usage") +
ylab("Age") +
ggtitle("Daily Internet Usage by Age") +
theme(plot.title = element_text(hjust = 0.5))

# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE

model = glm(Clicked.on.Ad~Daily.Time.Spent.on.Site+Age+Area.Income, data, family = binomial)
summary(model)

data$predicted = predict(model, data, type="response")
data$Clicked.on.Ad.Predicted = as.factor(ifelse(data$predicted >= 0.8, 1, 0))
str(data)

confusionMatrix(data = data$Clicked.on.Ad.Predicted, reference = data$Clicked.on.Ad)

# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE

rc = roc(data$Clicked.on.Ad, data$predicted)
plot(smooth(rc), col="red")
rc$auc

# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE

arrival = 62
service = 70
paste0("Average waiting time in queue: ", round((arrival/(service*(service-arrival)))*60), " minutes")

# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE

new_service = 84
paste0("Average number of customers in the queue: ", round((arrival^2)/(new_service*(new_service-arrival))), " Customers")

# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE

data = data.frame(servers = c(seq(7,20)))
data = data %>% mutate(service_rate = servers*14,
                      avg_waiting_time_mins = round((82/(service_rate*(service_rate-82)))*60, 2))
plot(data$servers,
     data$avg_waiting_time_mins, 
     type="l", 
     col="red",
     xlab="Servers", 
     ylab = "Average Waiting Time (Mins)")
abline(h=3, col="blue")

# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE

data

# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE

# SOLUTION ENDS HERE
