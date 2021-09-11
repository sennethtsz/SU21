library(ggplot2)
library(dplyr)
library(readr)
library(Ecdat)

filter(Housing, price >80000, lotsize < 4000)
filter(Housing, price >80000, lotsize < 4000, recroom=='yes')

slice(Housing, 10:25)

#re-order 
arrange(Housing, price, lotsize)

#select
head(select(Housing, price, bedrooms))

#rename
head(rename(Housing, bath_rooms = bathrms))

#unique values
distinct(select(Housing, gashw))

#mutate -add columns from other columns
head(mutate(Housing, total_rms = bedrooms + bathrms))

#transmute - only want new column back
head(transmute(Housing, total_rms = bedrooms + bathrms))

#pipe operator  %>% allows for multiple operation
#Data %>% op1 %>% op2
Housing2 <-Housing %>% filter(price > 70000) %>% mutate(total_rms = bedrooms + bathrms) %>% arrange(desc(lotsize))

Housing %>% filter(bathrms==2) %>% summarise(avg_price = mean(price))
