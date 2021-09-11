
# SOLUTION BEGINS HERE
if(!require("datasets")) {install.packages("datasets")
                          library("datasets")}
if(!require("dplyr")) {install.packages("dplyr")
                       library("dplyr")}
data(PlantGrowth)

data = PlantGrowth
str(data)
unique(data$group)
head(data)

data1 = subset(data, group == "trt1" | group == "ctrl")
str(data1)
unique(data1$group)

data2 = subset(data, group == "trt2" | group == "ctrl")
str(data2)
unique(data2$group)

model1 = lm(weight~group, data1)
summary(model1)

model2 = lm(weight~group, data2)
summary(model2)
# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE
avg_weight_ctrl = mean(data[which(data$group == "ctrl"),]$weight)
avg_weight_trt1 = mean(data[which(data$group == "trt1"),]$weight)
avg_weight_trt2 = mean(data[which(data$group == "trt2"),]$weight)

print("Average weight for:")
print(paste0("Control: ", avg_weight_ctrl))
print(paste0("Treatment Group 1: ", avg_weight_trt1))
print(paste0("Treatment Group 2: ", avg_weight_trt2))
# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE
data = read.csv("../resource/asnlib/publicdata/Min_Wage.csv")
str(data)
head(data)
unique(data$State)
unique(data$d)

data$State = relevel(as.factor(data$State), ref = "Philadelphia")
str(data)

data = data %>% mutate(group = case_when((State == "Philadelphia" & d == 0) ~ "A",
                                        (State == "New Jersey" & d == 0) ~ "B",
                                        (State == "Philadelphia" & d == 1) ~ "C",
                                        (State == "New Jersey" & d == 1) ~ "D"))
table(data$group)
# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE
mean_A = mean(data[which(data$group == "A"),]$fte)
mean_B = mean(data[which(data$group == "B"),]$fte)
mean_C = mean(data[which(data$group == "C"),]$fte)
mean_D = mean(data[which(data$group == "D"),]$fte)

print(paste0("Mean fte for Group A: ", mean_A))
print(paste0("Mean fte for Group B: ", mean_B))
print(paste0("Mean fte for Group C: ", mean_C))
print(paste0("Mean fte for Group D: ", mean_D))
# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE
diff_ctrl = mean_C - mean_A
diff_trt = mean_D - mean_B
diff_in_diff = diff_trt - diff_ctrl
print(paste0("Difference in difference = ", diff_in_diff))
# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE
model = lm(fte ~ State + d + State*d, data)
summary(model)
# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE
if(!require("lubridate")) {install.packages("lubridate")
                           library("lubridate")}
if(!require("PerformanceAnalytics")) {install.packages("PerformanceAnalytics")
                                      library("PerformanceAnalytics")}
if(!require("xts")) {install.packages("xts")
                     library("xts")}

data = read.csv("../resource/asnlib/publicdata/Berkshire.csv")
str(data)
head(data)

data$Date = mdy(data$Date)
str(data)
head(data)

print(paste0("Standard deviation of Berkshire Hathaway: ", round(sd(data$BrkRet)*100, 2), "%"))

# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE
print(paste0("Average return of Berkshire Hathaway: ", round(mean(data$BrkRet)*100, 2), "%"))
# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE
data = data %>% mutate(outperform = ifelse(BrkRet > MKT, 1, 0))
print(paste0("Berkshire Hathaway has on average outperformed the market ", sum(data$outperform)/nrow(data)*100, "% of the time"))
# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE
data_ordered = data[order(data$Date),]
data_xts = xts(data_ordered[,-1], order.by=data_ordered$Date)
print(paste0("$10,000 investment from the start of the sample period would have grown to $", 
             format(as.numeric((Return.cumulative(data_xts$BrkRet, geometric = TRUE)+1)*10000), nsmall = 2, big.mark = ",")))
# SOLUTION ENDS HERE

# SOLUTION BEGINS HERE
chart.CumReturns(data_xts, 
                 main = "Cumulative Returns", 
                 wealth.index = FALSE,
                 legend.loc = TRUE)

# SOLUTION ENDS HERE
