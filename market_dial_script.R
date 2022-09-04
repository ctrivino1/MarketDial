library(mosaic)
library(tidyverse)
library(DT)
library(pander)
library(dplyr)




dat <- read_csv("C:/Users/Admin/Downloads/marketdial_ds_takehome (1)/marketdial_ds_takehome/transactions.csv")

products <- read_csv("C:/Users/Admin/Downloads/marketdial_ds_takehome (1)/marketdial_ds_takehome/products_of_interest.csv",col_names = F)


control <- read_csv("C:/Users/Admin/Downloads/marketdial_ds_takehome (1)/marketdial_ds_takehome/q3_control_stores.csv",col_names = F)

control$Treatment <- "Control"

glimpse(control)
  
treatment <- read_csv("C:/Users/Admin/Downloads/marketdial_ds_takehome (1)/marketdial_ds_takehome/q3_treatment_stores.csv",col_names = F)

treatment$Treatment <- "Display"

treatment %>% glimpse()

ct <- rbind(treatment,control)
names(ct)[1] = "store_id"
glimpse(ct)


t <- dat %>% filter(store_id %in% treatment$X1) 

c <- dat %>% filter(store_id %in% control$X1)

data <- rbind(t,c)
nrow(data)

glimpse(data)

names(data)[1] = "store_id"
glimpse(data)

view(data)

final_dat <- dat %>%  filter(store_id %in% data$store_id)
nrow(final_dat)
final_dat <- final_dat %>% filter(product_id %in% products$X1)
nrow(final_dat)
final_dat <- final_dat %>% left_join(ct,by="store_id")
final_dat <- final_dat %>% drop_na(Treatment)
nrow(final_dat)
view(final_dat)



final_dat['weeks']=cut(as.Date(final_dat$date_week),breaks=as.Date(c('2016-07-17','2016-10-15','2016-10-16','2016-11-12','2017-02-12')),labels = c('Weeks 1-4','none','Weeks 5-9','Weeks 10-22'))

final_dat <- final_dat %>% drop_na(weeks)

nrow(final_dat)

view(final_dat)

final_dat <- 

display <- final_dat %>% 
  group_by(Treatment) %>% 
  filter(Treatment=="Display")

glimpse(display)

control <- final_dat %>% 
  group_by(Treatment) %>% 
  filter(Treatment=="Control")

glimpse(control)


# testing anova
myaov <- aov( revenue ~ Treatment+weeks+Treatment:weeks, data=final_dat)

warp.aov <- aov( log1p(revenue) ~ Treatment+weeks+Treatment:weeks, data=final_dat)
summary(warp.aov)






par(mfrow=c(1,2))
plot(warp.aov, which = 1:2, pch=16)

final_dat$date_week <- as.Date(final_dat$date_week)
xyplot(log1p(revenue) ~ as.factor(Treatment), data = final_dat, type=c("p","a"), main="Average mean of revenue", col="green")

xyplot(log1p(revenue) ~ weeks, data = final_dat, type=c("p","a"), main="The average mean of revenue for each trial period", col="purple")

xyplot(log1p(revenue) ~ weeks, data=final_dat, groups=Treatment, type=c("p","a"), main="Significance of the Interaction", auto.key=list(corner=c(1,1)))

