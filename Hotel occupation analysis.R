library(ggplot2)
library(outliers)
library(readr)
library(dplyr)
library(tidyverse)
library(tidyr)
library(caret)
library(dendextend)
library(psych)
library(factoextra)
library(gridExtra)
library(cluster)
library(flexclust)
library(knitr)
library(kableExtra)
library(rpart) 
library(rpart.plot)


hotel_bookings <- read_csv("hotel_bookings.csv")

str(hotel_bookings)
#Factor: 14
Int: 18

sum(is.na(hotel_bookings))
#there are 4 missing value in dataset

hotel_bookings[!complete.cases(hotel_bookings),]
#use to locate the missing value (row no. 40604, 40668, 40680, 41161)

data <- na.omit(hotel_bookings)
#remove row no. 40604, 40668, 40680, 41161 to elimante NA and also insert dataset into new data

summary(data)
#to check overall stat of variables

data <- data[,-24:-25]
#remove variables that not useful in prediction (24: agent, 25:company these two variables contain to many NULL and will not use in prediction)

data$is_canceled = as.factor(data$is_canceled)
#change integer variable to dummy variable because it contain only 2 type of answer whether yes or no

data$arrival_date_year = as.factor(data$arrival_date_year)
data$arrival_date_day_of_month = as.factor(data$arrival_date_day_of_month)
data$arrival_date_week_number = as.factor(data$arrival_date_week_number)
#change integer variable to factor due to no. date/week/year

levels(data$meal)[levels(data$meal) == "Undefined"] = "SC"
#packages: Undefined/SC – no meal, therefore undefined equal SC

data = filter(data, distribution_channel!= "Undefined")
levels(data$distribution_channel)
levels(data$distribution_channel)[5] = levels(data$distribution_channel)[4]
summary(data$market_segment)
levels(data$market_segment)
levels(data$market_segment)[8] = levels(data$market_segment)[7]
#remove Undefined from the distribution_channel variable and market_segment    

data %>% group_by(country) %>% count() %>% filter(n > 1000)
#check how many country contain data more 1000 obsevation

data = filter(data, country == "AUT"| country == "BEL"| country == "BRA"| country == "CHE"| country == "CN"| country == "DEU"| country == "ESP"| country == "FRA"| country == "GBR"| country == "IRL"| country == "ITA"| country == "NLD"| country == "PRT"| country == "SWE"| country == "USA")
#remove country that contain observation less than 1000

data$country = droplevels(data$country)

#remove outliers 
ggplot(data=data,aes(x="",y=total_of_special_requests))+
  geom_boxplot(outlier.color='red')+
  
  geom_text(data=data[data$total_of_special_requests>quantile(data$total_of_special_requests,0.99),], aes(label=total_of_special_requests),nudge_x = 0.2)
outlier(data$total_of_special_requests,opposite = FALSE, logical = FALSE)
data$total_of_special_requests = rm.outlier(data$total_of_special_requests, fill = TRUE, median = TRUE, opposite = FALSE)

ggplot(data=data,aes(x="",y=adr))+
  geom_boxplot(outlier.color='red')+
  geom_text(data=data[data$adr>quantile(hotel_bookings$adr,0.99),], aes(label=adr),nudge_x = 0.2)
outlier(data$adr,opposite = FALSE, logical = FALSE)
data$adr= rm.outlier(data$adr, fill = TRUE, median = TRUE, opposite = FALSE)

ggplot(data=data,aes(x="",y=lead_time))+
  geom_boxplot(outlier.color='red')+
  geom_text(data=data[data$lead_time>quantile(data$lead_time,0.99),], aes(label=lead_time),nudge_x = 0.2)
outlier(data$lead_time,opposite = FALSE, logical = FALSE)
data$lead_time= rm.outlier(data$lead_time, fill = TRUE, median = TRUE, opposite = FALSE)

ggplot(data=data,aes(x="",y=stays_in_weekend_nights))+
  geom_boxplot(outlier.color='red')+
  geom_text(data=data[data$stays_in_weekend_nights>quantile(data$stays_in_weekend_nights,0.99),], aes(label=stays_in_weekend_nights),nudge_x = 0.2)
outlier(data$stays_in_weekend_nights,opposite = FALSE, logical = FALSE)
data$stays_in_weekend_nights= rm.outlier(data$stays_in_weekend_nights, fill = TRUE, median = TRUE, opposite = FALSE)

ggplot(data=data,aes(x="",y=stays_in_week_nights))+
  geom_boxplot(outlier.color='red')+
  geom_text(data=data[hotel_bookings$stays_in_week_nights>quantile(data$stays_in_week_nights,0.99),], aes(label=stays_in_week_nights),nudge_x = 0.2)
outlier(data$stays_in_week_nights,opposite = FALSE, logical = FALSE)
data$stays_in_week_nights= rm.outlier(data$stays_in_week_nights, fill = TRUE, median = TRUE, opposite = FALSE)

ggplot(data=data,aes(x="",y=adults))+
  geom_boxplot(outlier.color='red')+
  geom_text(data=data[data$adults>quantile(data$adults,0.99),], aes(label=adults),nudge_x = 0.2)
outlier(data$adults,opposite = FALSE, logical = FALSE)
data$adults= rm.outlier(data$adults, fill = TRUE, median = TRUE, opposite = FALSE)

ggplot(data=data,aes(x="",y=booking_changes))+
  geom_boxplot(outlier.color='red')+
  geom_text(data=data[data$booking_changes>quantile(data$booking_changes,0.99),], aes(label=booking_changes),nudge_x = 0.2)
outlier(data$booking_changes,opposite = FALSE, logical = FALSE)
data$booking_changes= rm.outlier(data$booking_changes, fill = TRUE, median = TRUE, opposite = FALSE)

ggplot(data=data,aes(x="",y=babies))+
  geom_boxplot(outlier.color='red')+
  geom_text(data=data[data$babies>quantile(data$babies,0.99),], aes(label=babies),nudge_x = 0.2)
outlier(data$babies,opposite = FALSE, logical = FALSE)
data$babies= rm.outlier(data$babies, fill = TRUE, median = TRUE, opposite = FALSE)

ggplot(data=data,aes(x="",y=children))+
  geom_boxplot(outlier.color='red')+
  geom_text(data=data[data$children>quantile(data$children,0.99),], aes(label=children),nudge_x = 0.2)
outlier(data$children,opposite = FALSE, logical = FALSE)
data$children= rm.outlier(data$children, fill = TRUE, median = TRUE, opposite = FALSE)



#formatting arrival_date_month
mapping <- c("January" = 1, "February" = 2, "March" = 3, "April" = 4, "May" = 5, "June" = 6,
             "July" = 7, "August" = 8, "September" = 9, "October" = 10, "November" = 11,
             "December" = 12)
data$arrival_date_month <- mapping[data$arrival_date_month]

#formatting hotel
mapping1 <- c("Resort Hotel" = 1, "City Hotel" = 2)
data$hotel <- mapping1[data$hotel]

#formatting market_segment
mapping2 <- c("Aviation" = 1, "Complementary" = 2, "Corporate" = 3, "Direct" =4,
              "Groups" = 5, "Offline TA/TO" = 6, "Online TA" = 7, "Undefined" = 8)
data$market_segment <- mapping2[data$market_segment]

#formatting customer_type
mapping3 <- c("Contract" = 1, "Group" = 2, "Transient" = 3, "Transient-Party" =4)
data$customer_type <- mapping3[data$customer_type]

#formatting reservation_status
mapping4 <- c("Canceled" = 1, "Check-Out" = 2, "No-Show" = 3)
data$reservation_status <- mapping4[data$reservation_status]


#removing variables not useful for prediciton
data_clean <- subset(data, select=-c(meal,country,distribution_channel,reserved_room_type,assigned_room_type,deposit_type,reservation_status_date,
                                     lead_time, arrival_date_week_number, arrival_date_day_of_month, days_in_waiting_list, adr,
                                     required_car_parking_spaces,booking_changes,previous_bookings_not_canceled))


data_clean_scale = data.frame(scale(data_clean))

hotel.pca <- prcomp(data_clean_scale[1:15])
hotel.pca.df <- data.frame(hotel.pca$rotation)
kable(hotel.pca.df, format = 'html') %>%
  kable_styling(bootstrap_options = c('hover', 'striped'))

  
#change factor variable to numeric and removing variables based on PCA analysis
data_clean$is_canceled = as.numeric(data$is_canceled)
data_clean$arrival_date_year = as.numeric(data$arrival_date_year)
data_clean <- na.omit(data_clean)
data_clean <- subset(data_clean, select = -c(arrival_date_year, stays_in_week_nights, adults, children,
                                             customer_type, reservation_status))


###k-means clustering

#Total within sum of squares Plot
within_ss = sapply(1:10,FUN = function(x){
  set.seed(617)
  kmeans(x = data_cluster,centers = x,iter.max = 100,nstart = 25)$tot.withinss})
ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

#Ratio Plot
ratio_ss = sapply(1:10,FUN = function(x) {
  set.seed(617)
  km = kmeans(x = data_cluster,centers = x,iter.max = 100,nstart = 25)
  km$betweenss/km$totss} )
ggplot(data=data.frame(cluster = 1:10,ratio_ss),aes(x=cluster,y=ratio_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))


# base on ratio_ss and within_ss, we use kmean clustering with 3 cluster
km = kmeans(x = data_cluster,centers = 3,iter.max = 1000,nstart = 25)

k_segment = km$cluster
table(k_segment)

data_combine = cbind(data_clean, k_segment)

seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x))) }

seg.summ (data_clean,km$cluster)

clusplot(data_clean, km$cluster, color=TRUE, shade=TRUE,
         labels=3, lines=0, main="K-means cluster plot")

temp = data.frame(cluster = factor(k_segment),
                  factor1 = fa(data_cluster,nfactors = 3,rotate = 'varimax')$scores[,1],
                  factor2 = fa(data_cluster,nfactors = 3,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()


#Cluster sizes
sort(table(km$cluster))
clust <- names(sort(table(km$cluster)))


#Compare clusters
data_cluster[km$cluster==clust[1],1:9]
data_cluster[km$cluster==clust[2],1:9]
data_cluster[km$cluster==clust[3],1:9]




######## 1) PREDICT NUMBER OF SPECIAL REQUESTS


#Split data for prediction
split = createDataPartition(y=data_cluster$total_of_special_requests,p = 0.6,list = F,groups = 100)
train = data_cluster[split,]
test = data_cluster[-split,]

trainMinusSP <- subset(train,select=-c(total_of_special_requests))
testMinusSP <- subset(test,select=-c(total_of_special_requests))
preproc = preProcess(trainMinusSP)
trainNorm = predict(preproc,trainMinusSP)
testNorm = predict(preproc,testMinusSP)


###k-means clustering

#Total within sum of squares Plot
within_ss = sapply(1:10,FUN = function(x){
  set.seed(617)
  kmeans(x = trainNorm,centers = x,iter.max = 1000,nstart = 25)$tot.withinss})
ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

#Ratio Plot
ratio_ss = sapply(1:10,FUN = function(x) {
  set.seed(617)
  km = kmeans(x = trainNorm,centers = x,iter.max = 1000,nstart = 25)
  km$betweenss/km$totss} )
ggplot(data=data.frame(cluster = 1:10,ratio_ss),aes(x=cluster,y=ratio_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))


km_split = kmeans(x = trainNorm,centers = 3,iter.max=10000,nstart=100)


k_segment_split = km_split$cluster
table(k_segment_split)

data_combine_split = cbind(trainNorm, k_segment_split)


seg.summ (trainNorm,km_split$cluster)

#Apply clustering solutions from train to test
km_kcca = as.kcca(km_split,trainNorm) 
clusterTrain = predict(km_kcca)
clusterTest = predict(km_kcca,newdata=testNorm)


table(clusterTrain)

table(clusterTest)


#Split train and test based on cluster membership
train1 = subset(train,clusterTrain==1)
train2 = subset(train,clusterTrain==2)
train3 = subset(train,clusterTrain==3)
test1 = subset(test,clusterTest==1)
test2 = subset(test,clusterTest==2)
test3 = subset(test,clusterTest==3)


#Predict for each cluster with trees
tree1 = rpart(total_of_special_requests~.,train1,minbucket=10)
tree2 = rpart(total_of_special_requests~.,train2,minbucket=10)
tree3 = rpart(total_of_special_requests~.,train3,minbucket=10)
predt1 = predict(tree1,newdata=test1)
predt2 = predict(tree2,newdata=test2)
predt3 = predict(tree3,newdata=test3)

sset1 = sum((test1$total_of_special_requests-predt1)^2)
sset2 = sum((test2$total_of_special_requests-predt2)^2)
sset3 = sum((test3$total_of_special_requests-predt3)^2)
pred_tree = (c(predt1,predt2,predt3))
total_of_special_requests_Overall_tree = c(test1$total_of_special_requests, test2$total_of_special_requests, test3$total_of_special_requests)
sseOverall_tree = sum((pred_tree - total_of_special_requests_Overall_tree)^2); sseOverall_tree


#Predict using regression with a focus on numeric variables
linear_data = lm(total_of_special_requests~hotel+is_canceled+lead_time+arrival_date_year+
            arrival_date_month+arrival_date_week_number+ arrival_date_day_of_month+
            stays_in_week_nights+ stays_in_week_nights+ adults+ children+ babies+
            market_segment+ is_repeated_guest+ previous_cancellations+ previous_bookings_not_canceled+
            booking_changes+ days_in_waiting_list+ customer_type+ adr+ required_car_parking_spaces+
            reservation_status,data)
sseLinear_data = sum(linear_data$residuals^2); sseLinear_data

paste('SSE for model on entire data',sseLinear_data)

paste('SSE for model on clusters',sseOverall_tree)


Group_1_requests <- data.frame(Prediction = predt1,  Hotel = test1$hotel, Month = test1$arrival_date_month, Segment = test1$market_segment) %>% 
  group_by(test1$hotel,test1$market_segment,test1$arrival_date_month) 
Group_1_requests[order(Group_1_requests$Prediction, decreasing = TRUE), ] %>%
  filter(Hotel == '1')
Group_1_requests[order(Group_1_requests$Prediction, decreasing = TRUE), ] %>%
  filter(Hotel == '2')

Group_2_requests <- data.frame(Prediction = predt2,  Hotel = test2$hotel, Month = test2$arrival_date_month, Segment = test2$market_segment) %>% 
  group_by(test2$hotel,test2$market_segment,test2$arrival_date_month) 
Group_2_requests[order(Group_2_requests$Prediction, decreasing = TRUE), ] %>%
  filter(Hotel == '1')
Group_2_requests[order(Group_2_requests$Prediction, decreasing = TRUE), ] %>%
  filter(Hotel == '2')

Group_3_requests <- data.frame(Prediction = predt3,  Hotel = test3$hotel, Month = test3$arrival_date_month, Segment = test3$market_segment) %>% 
  group_by(test3$hotel,test3$market_segment,test3$arrival_date_month) 
Group_3_requests[order(Group_3_requests$Prediction, decreasing = TRUE), ] %>%
  filter(Hotel == '1')
Group_3_requests[order(Group_3_requests$Prediction, decreasing = TRUE), ] %>%
  filter(Hotel == '2')



###### 2) BEST TIME OF YEAR TO BOOK A HOTEL/RESORT ROOM


#removing variables not useful for prediciton
data_clean_2 <- subset(data, select=-c(meal,country,distribution_channel,reserved_room_type,assigned_room_type,deposit_type,reservation_status_date,
                                     lead_time, arrival_date_week_number, arrival_date_day_of_month, days_in_waiting_list,
                                     required_car_parking_spaces,booking_changes,previous_bookings_not_canceled))
data_cluster_2 = data_clean_2
str(data_cluster_2)

#Split data for prediction
split_2 = createDataPartition(y=data_cluster_2$total_of_special_requests,p = 0.6,list = F,groups = 100)
train_2 = data_cluster_2[split,]
test_2 = data_cluster_2[-split,]

trainMinusADR <- subset(train_2,select=-c(adr))
testMinusADR <- subset(test_2,select=-c(adr))
preproc_2 = preProcess(trainMinusADR)
trainNorm_2 = predict(preproc_2,trainMinusADR)
testNorm_2 = predict(preproc_2,testMinusADR)


###k-means clustering

#Total within sum of squares Plot
within_ss = sapply(1:10,FUN = function(x){
  set.seed(617)
  kmeans(x = trainNorm_2,centers = x,iter.max = 1000,nstart = 25)$tot.withinss})
ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

#Ratio Plot
ratio_ss = sapply(1:10,FUN = function(x) {
  set.seed(617)
  km = kmeans(x = trainNorm_2,centers = x,iter.max = 1000,nstart = 25)
  km$betweenss/km$totss} )
ggplot(data=data.frame(cluster = 1:10,ratio_ss),aes(x=cluster,y=ratio_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))


km_split_2 = kmeans(x = trainNorm,centers = 3,iter.max=10000,nstart=100)


k_segment_split_2 = km_split_2$cluster
table(k_segment_split_2)

data_combine_split_2 = cbind(trainNorm_2, k_segment_split_2)

seg.summ (trainNorm_2,km_split_2$cluster)

#Apply clustering solutions from train to test
km_kcca_2 = as.kcca(km_split_2,trainNorm_2) 
clusterTrain_2 = predict(km_kcca_2)
clusterTest_2 = predict(km_kcca_2,newdata=testNorm_2)


table(clusterTrain_2)

table(clusterTest_2)


#Split train and test based on cluster membership
train4 = subset(train_2,clusterTrain==1)
train5 = subset(train_2,clusterTrain==2)
train6 = subset(train_2,clusterTrain==3)
test4 = subset(test_2,clusterTest==1)
test5 = subset(test_2,clusterTest==2)
test6 = subset(test_2,clusterTest==3)


#Predict for each cluster with trees
tree4 = rpart(adr~.,train4,minbucket=10)
tree5 = rpart(adr~.,train5,minbucket=10)
tree6 = rpart(adr~.,train6,minbucket=10)
predt4 = predict(tree4,newdata=test4)
predt5 = predict(tree5,newdata=test5)
predt6 = predict(tree6,newdata=test6)
sset4 = sum((test4$adr-predt4)^2)
sset5 = sum((test5$adr-predt5)^2)
sset6 = sum((test6$adr-predt6)^2)
pred_tree_adr = (c(predt4,predt5,predt6))
total_of_special_requests_Overall_tree_adr = c(test4$adr, test5$adr, test6$adr)
sseOverall_tree_adr = sum((pred_tree_adr - total_of_special_requests_Overall_tree_adr)^2); sseOverall_tree_adr


Group_1_month <- data.frame(Prediction = predt4,  Hotel = test4$hotel, Month = test4$arrival_date_month) %>% 
  group_by(test4$hotel) 
Group_1_month[order(Group_1_month$Prediction, decreasing = FALSE), ] %>%
  filter(Hotel == '1')
Group_1_month[order(Group_1_month$Prediction, decreasing = FALSE), ] %>%
  filter(Hotel == '2')

Group_2_month <- data.frame(Prediction = predt5,  Hotel = test5$hotel, Month = test5$arrival_date_month) %>% 
  group_by(test5$hotel) 
Group_2_month[order(Group_2_month$Prediction, decreasing = FALSE), ] %>%
  filter(Hotel == '1', Prediction > '2')
Group_2_month[order(Group_2_month$Prediction, decreasing = FALSE), ] %>%
  filter(Hotel == '2', Prediction > '2')

Group_3_month <- data.frame(Prediction = predt6,  Hotel = test6$hotel, Month = test6$arrival_date_month) %>% 
  group_by(test6$hotel) 
Group_3_month[order(Group_3_month$Prediction, decreasing = FALSE), ] %>%
  filter(Hotel == '1')
Group_3_month[order(Group_3_month$Prediction, decreasing = FALSE), ] %>%
  filter(Hotel == '2')




# 3) Optimal length of stay for best adr

median(test4$stays_in_week_nights)
median(test4$stays_in_weekend_nights)

Group_1 <- data.frame(Prediction = predt4, ALS = test4$stays_in_week_nights, Hotel = test4$hotel) %>% 
  group_by(test4$hotel) 
Group_1[order(Group_1$Prediction, decreasing = FALSE), ] %>%
  filter(Hotel == '1', ALS == '3')
Group_1[order(Group_1$Prediction, decreasing = FALSE), ] %>%
  filter(Hotel == '2', ALS == '3')

Group_1_wk <- data.frame(Prediction = predt4, ALS = test4$stays_in_weekend_nights, Hotel = test4$hotel) %>% 
  group_by(test4$hotel) 
Group_1_wk[order(Group_1_wk$Prediction, decreasing = FALSE), ] %>%
  filter(Hotel == '1', ALS == '1')
Group_1_wk[order(Group_1_wk$Prediction, decreasing = FALSE), ] %>%
  filter(Hotel == '2', ALS == '1')



median(test5$stays_in_week_nights)
median(test5$stays_in_weekend_nights)

Group_2 <- data.frame(Prediction = predt5, ALS = test5$stays_in_week_nights, Hotel = test5$hotel) %>% 
  group_by(test5$hotel) 
Group_2[order(Group_2$Prediction, decreasing = FALSE), ] %>%
  filter(Hotel == '1', ALS == '1', Prediction > '2')
Group_2[order(Group_2$Prediction, decreasing = FALSE), ] %>%
  filter(Hotel == '2', ALS == '1', Prediction > '2')

Group_2_wk <- data.frame(Prediction = predt5, ALS = test5$stays_in_weekend_nights, Hotel = test5$hotel) %>% 
  group_by(test5$hotel) 
Group_2_wk[order(Group_2_wk$Prediction, decreasing = FALSE), ] %>%
  filter(Hotel == '1', ALS == '1')
Group_2_wk[order(Group_2_wk$Prediction, decreasing = FALSE), ] %>%
  filter(Hotel == '2', ALS == '1')



median(test6$stays_in_week_nights)
median(test6$stays_in_weekend_nights)

Group_3 <- data.frame(Prediction = predt6, ALS = test6$stays_in_week_nights, Hotel = test6$hotel) %>% 
  group_by(test6$hotel) 
Group_3[order(Group_3$Prediction, decreasing = FALSE), ] %>%
  filter(Hotel == '1', ALS == '2')
Group_3[order(Group_3$Prediction, decreasing = FALSE), ] %>%
  filter(Hotel == '2', ALS == '2')

Group_3_wk <- data.frame(Prediction = predt6, ALS = test6$stays_in_weekend_nights, Hotel = test6$hotel) %>% 
  group_by(test6$hotel) 
Group_3_wk[order(Group_3_wk$Prediction, decreasing = FALSE), ] %>%
  filter(Hotel == '1', ALS == '1')
Group_3_wk[order(Group_3_wk$Prediction, decreasing = FALSE), ] %>%
  filter(Hotel == '2', ALS == '1')


