if(!"randomForest" %in% installed.packages()) {
  install.packages("randomForest")}
if(!"caret" %in% installed.packages()) {
  install.packages("caret")}
if(!"GGally" %in% installed.packages()) {
  install.packages("GGally")}
if(!"MLmetrics" %in% installed.packages()) {
  install.packages("MLmetrics")}
if(!"gbm" %in% installed.packages()) {
  install.packages("gbm")}
library(randomForest)
library(caret)
library(gbm)
library(GGally)
library(MLmetrics)

# data munging libraries
library(data.table)
library(lubridate)

# plotting libraries
library(ggplot2)
options(scipen = 999)
theme_set(theme_minimal(base_size = 16))
library(ggrepel)
library(plotly)