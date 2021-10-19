#Woratham Bubpha 640632057
setwd("D:/Data sci CMU/Sem 1/Statistics for Data Science/project")
data <- read.csv("AB_NYC_2019.csv",stringsAsFactors = T)
#data preparation
-----------------------------------
# 1. clean data
str(data)
data <- data[-c(1:4,13)]
sum(is.na(data))
sum(is.na(data$reviews_per_month))
#review_per_month has 25% of na but if see the data number of review is 0 review_per_month =NA
#replace Na with 0
data[is.na(data)]=0
data
# 2.Exploration data
str(data)
boxplot(data$price)
boxplot(data$minimum_nights)
boxplot(data$calculated_host_listings_count)
# there are a lot of outlier data

library(forcats)
ggplot(data, aes(x = fct_infreq(data$room_type), fill = data$room_type)) +
  geom_bar() +
  theme(legend.position = "bottom")

ggplot(data, aes(x = fct_infreq(data$neighbourhood_group), fill = data$neighbourhood_group)) +
  geom_bar() +
  theme(legend.position = "bottom")

ggplot(data, aes(x = room_type, y = price)) +
  geom_violin() +
  scale_y_log10()

ggplot(data, aes(x = neighbourhood_group, y = price)) +
  geom_violin() +
  scale_y_log10()

# 3 check correlation 
data1=data[-c(1:9,13)]
data1.cor = cor(data1, method = c("spearman"))
library("Hmisc")
data1.rcorr = rcorr(as.matrix(data1.cor))
data1.rcorr
library(corrplot)
corrplot(data1.cor)
# each variables have weak relationship with price
library(GGally)
data1.pairs = pairs(data1[,c(1:6)],main="scatterplot matrix")
# there are exponential relationship

#use dummy
library(fastDummies)
data.dum = dummy_cols(data, select_columns = c('neighbourhood_group','room_type'))
names(data.dum) %<>% stringr::str_replace_all("\\s","_") %>% tolower

# split data 80:20
samplesize = 0.8*nrow(data.dum)
set.seed(123)
index = sample(seq_len(nrow(data.dum)), size=samplesize)
data_train = data.dum[index,]
data_test = data.dum[-index,]

# Build model by linear regression
data_train =  data_train %>% filter(price < quantile(data_train$price, 0.9) & price > quantile(data_train$price, 0.1))
model = lm(price ~ neighbourhood_group_brooklyn+neighbourhood_group_manhattan+neighbourhood_group_staten_island
           +latitude+longitude+room_type_private_room+room_type_shared_room
           +reviews_per_month+number_of_reviews+calculated_host_listings_count+availability_365,
           data=data_train)
summary(model)
AIC(model)
predModel1 = predict(model, newdata = data_test)
predModel1= exp(predModel1)
plot(model)

# Build model by logarithmic transformation
dataTrainLog =  dataTrain %>% filter(price < quantile(dataTrain$price, 0.9) & price > quantile(dataTrain$price, 0.1))
modelLog = lm(log1p(price) ~ neighbourhood_group_brooklyn+neighbourhood_group_manhattan+neighbourhood_group_staten_island
              +latitude+longitude+room_type_private_room+room_type_shared_room
              +log1p(reviews_per_month)+log1p(number_of_reviews)+log1p(calculated_host_listings_count)+log1p(availability_365),
              data=data_train)
summary(modelLog) 
AIC(modelLog)
dataTest = data_test %>% filter(price <= quantile(data_test$price, 0.8) & price >= quantile(data_test$price, 0.2)) %>% tidyr::drop_na()
regressionPred = predict(modelLog, newdata = dataTest)
regressionPred = exp(regressionPred)

