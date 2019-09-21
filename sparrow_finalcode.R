#set working directory
setwd("/Users/jill_jewelry/Desktop/BAN600DataSet")

#read the desired file
sparrow <- readRDS("sparrow.rds")
sparrow.original <-readRDS("sparrow.original.rds")

library(dplyr)
glimpse(sparrow)
library(DataExplorer)
library(ggplot2)
library(sqldf)
library(broom)
library(stargazer)
library(caret)
library(car)

#selecting desired columns (continuous variables) from dataset (sparrow)
data <- data.frame(sparrow$total_length, sparrow$wingspan, sparrow$weight, sparrow$beak_head, sparrow$humerus, sparrow$femur, sparrow$legbone, sparrow$skull, sparrow$sternum)
remove(data)

#removing desired columns from dataset
sparrow$status <- NULL
sparrow$age <- NULL

#exploratory analysis
dim(sparrow)
summary(sparrow)
object.size(sparrow)

#looking for missing values in a dataset
is.na(sparrow)
plot_missing(sparrow)

#scatter plot
attach(sparrow)
plot(total_length, wingspan, main = "Scatterplot for Sparrow", col = "blue")

#histogram
plot_histogram(sparrow)

#correlation
length.weight <- subset(sparrow, select = c("total_length", "weight"))
cor(length.weight)
length.wingspan <- subset(sparrow, select = c("total_length", "wingspan"))
cor(length.wingspan)
plot_correlation(sparrow)
plot_correlation(humerus.femur)

#finding Mode
temp <- table(as.vector(total_length))
names(temp)[temp == max(temp)]
temp <- table(as.vector(wingspan))
names(temp)[temp == max(temp)]
temp <- table(as.vector(weight))
names(temp)[temp == max(temp)]
temp <- table(as.vector(beak_head))
names(temp)[temp == max(temp)]
temp <- table(as.vector(humerus))
names(temp)[temp == max(temp)]
temp <- table(as.vector(femur))
names(temp)[temp == max(temp)]
temp <- table(as.vector(legbone))
names(temp)[temp == max(temp)]
temp <- table(as.vector(skull))
names(temp)[temp == max(temp)]
temp <- table(as.vector(sternum))
names(temp)[temp == max(temp)]


#finding Coeffient of Skewness and Kurtosis for each variables
library(e1071)
skewness(total_length)
skewness(wingspan)
skewness(weight)
skewness(beak_head)
skewness(humerus)
skewness(femur)
skewness(legbone)
skewness(skull)
skewness(sternum)
kurtosis(total_length)
kurtosis(wingspan)
kurtosis(weight)
kurtosis(beak_head)
kurtosis(humerus)
kurtosis(femur)
kurtosis(legbone)
kurtosis(skull)
kurtosis(sternum)


# Filled Density Plot
d <- density(sparrow$total_length)
plot(density(sparrow$total_length), main="Bell Cure of Total Length")
d <- density(sparrow$wingspan)
plot(density(sparrow$wingspan), main="Bell Curve for Wingspan")
d <- density(sparrow$weight)
plot(density(sparrow$weight), main="Bell Curve for Weight")
d <- density(sparrow$beak_head)
plot(density(sparrow$beak_head), main="Bell Curve for Beak Head")
d <- density(sparrow$humerus)
plot(density(sparrow$humerus), main="Bell Curve for humerus")
d <- density(sparrow$femur)
plot(density(sparrow$femur), main="Bell Curve for femur")
d <- density(sparrow$legbone)
plot(density(sparrow$legbone), main="Bell Curve for Leg Bone")
d <- density(sparrow$skull)
plot(density(sparrow$skull), main="Bell Curve for Skull")
d <- density(sparrow$sternum)
plot(density(sparrow$Sternum), main="Bell Curve for Sternum")
plot_density(sparrow)

#variance
var(sparrow$total_length)
var(sparrow$wingspan)
var(sparrow$weight)
var(sparrow$beak_head)
var(sparrow$humerus)
var(sparrow$femur)
var(sparrow$legbone)
var(sparrow$skull)
var(sparrow$sternum)

#standard deviation
sd(sparrow$total_length)
sd(sparrow$wingspan)
sd(sparrow$weight)
sd(sparrow$beak_head)
sd(sparrow$humerus)
sd(sparrow$femur)
sd(sparrow$legbone)
sd(sparrow$skull)
sd(sparrow$sternum)

#scatterplot
library(car)
scatterplot(sparrow$humerus ~ sparrow$wingspan, xlab = "humerus", ylab = "femur", 
            main ="Scatter Plot for Humerus and Wingspan", labels = row.names(sparrow))
scatterplot(sparrow$femur ~ sparrow$legbone, xlab = "femur", ylab = "legbone", 
            main ="Scatter Plot for Femur and Leg bone", labels = row.names(sparrow))
scatterplot(sparrow$humerus ~ sparrow$legbone, xlab = "humerus", ylab = "legbone", 
            main ="Scatter Plot for Humerus and Leg bone", labels = row.names(sparrow))
scatterplot(sparrow$sternum ~ sparrow$skull, xlab = "sternum", ylab = "skull", 
            main ="Scatter Plot for Sternum and Skull", labels = row.names(sparrow))
scatterplot(sparrow$total_length ~ sparrow$beak_head, xlab = "total_length", ylab = "beak_head", 
            main ="Scatter Plot for Total Length and Beak Head", labels = row.names(sparrow))
scatterplot(sparrow$sternum ~ sparrow$total_length, xlab = "sternum", ylab = "total_lenght", 
            main ="Scatter Plot for Sternum and Total Length", labels = row.names(sparrow))

#scatterplot matrices for all variables
pairs(~total_length+wingspan+weight+beak_head+humerus+femur+legbone+skull+sternum, 
      data=sparrow, main="Scatterplot Matrices", col="blue")

#categorical variables
sparrow.original$age = factor(sparrow.original$age,
                              levels = c('adult', 'juvenile'),
                              labels = c(1,0))

sparrow.original$status = factor(sparrow.original$status,
                                 levels = c('Perished','Survived'),
                                 labels = c(FALSE,TRUE))

glm <- glm(status ~ age + total_length + wingspan + weight + beak_head + humerus + femur + 
             legbone + skull + sternum,
           family = 'binomial', data = sparrow.original)
summary(glm)

#regression analysis using only continuous variables 
reg <- lm(wingspan ~ total_length + weight + beak_head + humerus+ femur + legbone + skull + sternum, data = sparrow)
summary(reg)
