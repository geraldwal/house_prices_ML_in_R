source('/Users/Gerald/Personal Drive/IE MBD/Term III/Advanced R/load_libraries.R')
source('/Users/Gerald/Personal Drive/IE MBD/Term III/Advanced R/f_partition.R')
source('/Users/Gerald/Personal Drive/IE MBD/Term III/Advanced R/Session 4/classification_metrics.R')
source("/Users/Gerald/Personal Drive/IE MBD/Term III/Advanced R/regression_metrics.R")
library(GGally)
library(MLmetrics)
# Import data
house_train <- fread("https://gist.githubusercontent.com/geraldwal/c15f40a258a15f6417e70705d57e9a21/raw/800519c3f4d633b8985cf9fc4b60ebccc89494eb/house_price_train")
house_test <- fread("https://gist.githubusercontent.com/geraldwal/58f48ae5a44f7061ced0c3486e2f07aa/raw/5a77070d797ffa9598c0eb6f37facb6a2a20c78b/house_price_test")

head(house_train)
head(house_test)

#VISUALIZATIONS
plot_ly(y=~price,x=~bedrooms,data = house_train,
        hoverinfo = "text",text= ~paste("Price",price,"\n","Bedrooms:",bedrooms,"\n","Grade",
                                        grade ,"\n","Condition",condition,"\n","Year Built :",yr_built))%>%
  add_markers()%>%
  layout(title="Price with No of Bedrooms")

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(house_train$price, main="Price", sub=paste("Outlier rows: ", boxplot.stats(house_train$price)$out))  # box plot for 'price'
# --> take log of price
boxplot(house_train$lat, main="lat", sub=paste("Outlier rows: ", boxplot.stats(house_train$lat)$out))  # box plot for 'lat'

# Stack train and test into one dataset
house_test$split <- "test"
house_test$price <- 0
house_train$split <- "train"

house_data <- rbind(house_train, house_test)
str(house_data)

# Copy to continue working on
hd <- house_data

#Example changing datatype: house_data[, "floors"] <- sapply(house_data[, "floors"], as.integer)

#date transformations and variable extractions
hd <- cbind(hd, hd$date) #copy of date column
hd$date <- mdy(hd$date)
hd$date <-as.numeric(as.Date(hd$date, origin = "1900-01-01"))
colnames(hd)[colnames(hd)=="V2"] <- "copy_date"
hd$copy_date <- as.Date(hd$copy_date, format = '%m/%d/%Y')

hd$month <- month(hd$copy_date)
hd$day <- day(hd$copy_date)
hd$year <- year(hd$copy_date)

unique(hd$year) #check years
str(hd)

sum(duplicated(hd$id))
hd <- hd[!duplicated(hd$id), ]
str(hd)


# Split data table again
hdTrain <- hd[which(hd$split == "train"),]
hdTest <- hd[which(hd$split == "test"),]

# Correlation matrix
library("PerformanceAnalytics")
my_data <- hdTrain[, c(3:21)]
chart.Correlation(my_data, histogram=TRUE, pch=19)

# EDA on train dataset
unique(house_data$bedrooms)
plot(table(house_train$bedrooms))

unique(house_data$long)
unique(house_data$long)
unique(house_data$long)
unique(house_data$long)

plot(table(house_train$price))
summary(house_train)
plot1<-ggpairs(data=hdTrain, columns=3:7,
               #mapping = aes(color = "dark green"),
               axisLabels="show")
plot1

# ideas --> some of the vars could be converted into factors - the ones that do not have linear relationship, you see this from box/scatterplot
# e.g. BED,FLOOR,WATERFRONT,VIEW
# sqft basement into 0 or 1

library(MASS)
#The better the fit, the lower the AIC, the more complex the model, the higher the AIC
#The lower the better: -4 is better than -2

library(caret)


#Baseline lm
lmodfit<-lm(price~.,data=trainNumeric)
summary(lmodfit)

test_lm<-predict(lmodfit, newdata = trainNumeric)

mape<-mape(real=trainNumeric$price, predicted = test_lm)
mape

#Stepwise lm
Blmodfit<-step(lmodfit,steps = 10000,trace = F) #Step is used to  select best model by #Minimizing AIC value
summary(Blmodfit)

test_lm<-predict(Blmodfit, newdata = trainNumeric)

mape_baseline<-mape(real=trainNumeric$price, predicted = test_lm)
mape_baseline

#Stepwise lm
Blmodfit<-step(lmodfit,steps = 10000,trace = F) #Step is used to  select best model by #Minimizing AIC value
summary(Blmodfit)

test_lm<-predict(Blmodfit, newdata = trainNumeric)

mape<-mape(real=trainNumeric$price, predicted = test_lm)
mape

#### 1.1 Base R Partitioning Tree 
library(rpart)
library(partykit)
library(rpart.plot)
tree_0<-rpart(formula = formula, data = hdTrain, method = 'anova', model=TRUE)

print(as.party(tree_0))

rpart.plot(tree_0, digits = 4,type = 5,box.palette = 'Gn') # lat and sqft_living seem most important variables



trainNumeric <- hdTrain[,c(2:21, 24:26)]
testNumeric <- hdTest[,c(2:21, 24:26)]

mape_glmnet<-mape(real=trainNumeric$price, predicted = test_glmnet)
mape_glmnet
#XGBoost
library(xgboost)
xgb_reg_0<-xgboost(booster='gblinear',
                   data=as.matrix(trainNumeric[, !'price', with=F]),
                   label=trainNumeric$price,
                   nrounds = 100,
                   objective='reg:linear')
print(xgb_reg_0)

MAPE()
#Feature engineering
#1. Date
#2. Eliminate ID
#3. Keep bedrooms only <10
# Log of the price
# Bathroom - factor
# Basement yes or no
# Renovation yes or no
# Binning the lat

# model evaluation

result<-data.table(method=c('tree','rf','xgb','lm','glmnet','xgb_reg'),
                   rmse=sapply(df_pred[,!c('price','id')],function(x) return(rmse(real=df_pred$price, predicted=x))),
                   mae=sapply(df_pred[,!c('price','id')],function(x) return(mae(real=df_pred$price, predicted=x))),
                   mape=sapply(df_pred[,!c('price','id')],function(x) return(mape(real=df_pred$price, predicted=x))))


result


result[which.min(result$mape)]

# plotting results metrics

ggplot(result, aes(x=method, y=mape))+geom_bar(stat='identity')
ggplot(result, aes(x=method, y=rmse))+geom_bar(stat='identity')
ggplot(result, aes(x=method, y=mae))+geom_bar(stat='identity')