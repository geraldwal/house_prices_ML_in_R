source('/Users/Gerald/Personal Drive/IE MBD/Term III/Advanced R/load_libraries.R')
source('/Users/Gerald/Personal Drive/IE MBD/Term III/Advanced R/f_partition.R')
source('/Users/Gerald/Personal Drive/IE MBD/Term III/Advanced R/Session 4/classification_metrics.R')

# Import data
house_train <- fread("https://gist.githubusercontent.com/geraldwal/c15f40a258a15f6417e70705d57e9a21/raw/800519c3f4d633b8985cf9fc4b60ebccc89494eb/house_price_train")
house_test <- fread("https://gist.githubusercontent.com/geraldwal/58f48ae5a44f7061ced0c3486e2f07aa/raw/5a77070d797ffa9598c0eb6f37facb6a2a20c78b/house_price_test")

head(house_train)
head(house_test)

# Stack train and test into one dataset
house_test$price <- 0
house_data <- rbind(house_train, house_test)

# EDA on train dataset
plot(table(house_train$price))

