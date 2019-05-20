source('/Users/Gerald/Personal Drive/IE MBD/Term III/Advanced R/load_libraries.R')
source('/Users/Gerald/Personal Drive/IE MBD/Term III/Advanced R/f_partition.R')
source('/Users/Gerald/Personal Drive/IE MBD/Term III/Advanced R/Session 4/classification_metrics.R')
library(GGally)
# Import data
house_train <- fread("https://gist.githubusercontent.com/geraldwal/c15f40a258a15f6417e70705d57e9a21/raw/800519c3f4d633b8985cf9fc4b60ebccc89494eb/house_price_train")
house_test <- fread("https://gist.githubusercontent.com/geraldwal/58f48ae5a44f7061ced0c3486e2f07aa/raw/5a77070d797ffa9598c0eb6f37facb6a2a20c78b/house_price_test")

head(house_train)
head(house_test)

# Stack train and test into one dataset
house_test$split <- "test"
house_train$split <- "train"

house_data <- rbind(house_train, house_test)
str(house_data)

#copy to continue working on
hd <- house_data

#Example changing datatype: house_data[, "floors"] <- sapply(house_data[, "floors"], as.integer)
unique(hd$month)
#date transformations and variable extractions
hd <- cbind(hd, hd$date)
hd$date <- mdy(hd$date)
hd$date <-as.numeric(as.Date(hd$date, origin = "1900-01-01"))
colnames(hd)[colnames(hd)=="V2"] <- "copy_date"
hd$copy_date <- as.Date(hd$copy_date, format = '%m/%d/%Y')
#str(hd)

hd$month <- month(hd$copy_date)
hd$day <- day(hd$copy_date)
hd$year <- year(hd$copy_date)

# Correlation

# EDA on train dataset
unique(house_data$long)

plot(table(house_train$price))
summary(house_train)
plot1<-ggpairs(data=house_train, columns=3:7,
               #mapping = aes(color = "dark green"),
               axisLabels="show")
plot1

# ideas --> some of the vars could be converted into factors - the ones that do not have linear relationship, you see this from box/scatterplot
# sqft basement into 0 or 1



#### 1.1 Base R Partitioning Tree 
library(rpart)
library(partykit)
library(rpart.plot)
tree_0<-rpart(formula = formula, data = house_train$split, method = 'anova', model=TRUE)

print(as.party(tree_0))

rpart.plot(tree_0, digits = 4,type = 2,box.palette = 'Gn')
