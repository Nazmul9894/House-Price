#read data
train <- read.csv(file = file.choose(), header = T, sep = ",")
test <- read.csv(file = file.choose(), header = T, sep = ",")

#library
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
install.packages("caret")
library(caret)
library(gridExtra)
library(scales)
install.packages("Rmisc")
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
install.packages("xgboost")
library(xgboost)

#size & structure
dim(train)
str(train)

#getting rid of IDs
test_labels <- test$Id
test$Id <- NULL
train$Id <- NULL
test$SalePrice <- NA
all <- rbind(train, test)
dim(all)

#exploring improtant feature
ggplot(all[!is.na(all$SalePrice), ], aes(SalePrice)) + geom_histogram(fill = "red", binwidth = 10000) + 
  scale_x_continuous(breaks = seq(0, 800000, by = 100000), labels = comma)

#saleprice summary
summary(all$SalePrice)

#variables having correlation with saleprice
numericVars <- which(sapply(all, is.numeric))
NumericVarNames <- names(numericVars)
cat('There are', length(numericVars), 'numeric variables')
all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))

#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")
