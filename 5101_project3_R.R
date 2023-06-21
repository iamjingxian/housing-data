# DSA5101 Introduction to Big Data for Industry
#

# optional: clear all variables
# rm(list=ls()) 

################################
### section 0: preliminaries ###
################################

## 0.1: importing libraries ##

# df manulation
library(dplyr)

# for plotting
library(ggplot2)
library(gridExtra)
library(ggpubr)

# for regression and statistics
library(rpart)
library(rstatix)

# for vif and stepVIF
library(car)
library(pedometrics)

# for cv
library(caret)

# for pca
library(factoextra)

# for correlation plots
library(corrplot)
library(data.table)



## 0.2: load data ##
data <- read.csv('project_residential_price_data_optional.csv')
                 # stringsAsFactors = FALSE)

# check data types in order to convert categorical variables to factors
sapply(data, class)
summary(data)

# convert categorical variables to factors
data$V.1 <- factor(data$V.1)
data$V.10 <- factor(data$V.10)
data$V.30 <- factor(data$V.30)

# check V.1, V10, and V30 are indeed factors/categorical
summary(data) 

###################################
### section 1: EDA of variables ###
###################################

## 1.1 ggplot of all features against V.9, price 

# 1:9. 9 plots in a page is a good size
p <- list()
for (i in 1:9) { # loop over range
  p[[i]] <- ggplot(data, aes_string(x=data[,i], y='V.9')) + 
    geom_point(size = 0.3) +
    geom_smooth(method = 'lm') +
    labs(x = colnames(data)[i])
}
do.call(grid.arrange,p)

# 10:18
p <- list()
for (i in 1:9) { # loop over range
  p[[i]] <- ggplot(data, aes_string(x=data[,i+9], y='V.9')) + 
    geom_point(size = 0.3) +
    geom_smooth(method = 'lm') +
    labs(x = colnames(data)[i+9])
}
do.call(grid.arrange,p)

# 19:27
p <- list()
for (i in 1:9) { # loop over range
  p[[i]] <- ggplot(data, aes_string(x=data[,i+18], y='V.9')) + 
    geom_point(size = 0.3) +
    geom_smooth(method = 'lm') +
    labs(x = colnames(data)[i+18])
}
do.call(grid.arrange,p)

# 28:30
p <- list()
for (i in 1:3) { # loop over range
  p[[i]] <- ggplot(data, aes_string(x=data[,i+27], y='V.9')) + 
    geom_point(size = 0.3) +
    geom_smooth(method = 'lm') +
    labs(x = colnames(data)[i+27])
}
do.call(grid.arrange,p)


## 1.2. box plots of variables against V.30, margin of project

par(mfrow = c(5, 1))
# 5 rows in a page is a good size
for (i in 1:30) {
  boxplot(data[,i] ~ V.30,
          xlab = colnames(data)[i],
          data =data, horizontal = TRUE)
}

## 1.3: histograms of residential type

total_floor_area_plot <- qplot(V.2,data=data,geom="histogram",bins=20,fill=factor(V.10)) + guides(fill = guide_legend(title="Residential Type")) + theme(legend.position=c(.7,.7)) + labs(x="Floor area (m^2)",y="Frequency",title="Histogram of total floor area of building")

lot_area_plot <- qplot(V.3,data=data,geom="histogram",bins=20,fill=factor(V.10)) + guides(fill = guide_legend(title="Residential Type")) + theme(legend.position=c(.7,.7)) + labs(x="Lot area (m^2)",y="Frequency",title="Histogram of lot area")

price_unit <- ggplot(data, aes(fill=factor(V.10),y=V.8)) + geom_boxplot() + labs(x="Residential building type",y="Price per m^2 (10000 IRR)",title="Price at project start") + guides(fill = guide_legend(title="Residential Type")) + theme(legend.position=c(.3,.7))

price_breakdown <- ggplot(data) + aes(x=V.8,fill=factor(V.10)) + geom_histogram(binwidth=100) + facet_grid(.~V.10) + guides(fill = guide_legend(title="Residential Type")) + theme(legend.position=c(.7,.7))

price_vs_floor_area <- ggplot(data=data) + geom_point(mapping = aes(x=V.2,y=V.8,color=factor(V.10))) + guides(color = guide_legend(title="Residential Type")) + theme(legend.position=c(.7,.7)) + labs(x="Floor area (m^2)",y="Price per m^2 (10000 IRR)",title="Scatter plot of price vs floor area")

plot_area_breakdown <- ggplot(data, aes(fill=factor(V.10),y=V.2)) + geom_boxplot() + labs(x="Residential building type",y="Floor area (m^2)",title="Floor area by residential type") + guides(fill = guide_legend(title="Residential Type")) + theme(legend.position=c(.3,.7))

grid.arrange(total_floor_area_plot, lot_area_plot, nrow=2)
grid.arrange(price_unit, price_breakdown, nrow=2)
grid.arrange(price_vs_floor_area, plot_area_breakdown, nrow=2)









#################################################
### Section 2. Model selection and prediction ###
#################################################

## 2.1 lm model 

# reload data
data <- read.csv('project_residential_price_data_optional.csv')

set.seed(37)  # seed to generate random number
train = sample(nrow(data), 0.7*nrow(data))  # sample() takes a sample of the specified size from the elements
test = setdiff(seq_len(nrow(data)), train) 
data_train<-data[train,]  #training data set
data_test<-data[test,]    #testing

summary(data_train) # check

# setting up for 5-fold cv 
train_control = trainControl(method = "cv", number = 5)
#regression model using all the variables
lm_all = train(V.9~., data = data_train, 
               method = "lm", trControl = train_control)
summary(lm_all)

# predict on test data using lm_all
estimate_all <- predict(lm_all,type='raw',newdata=data_test) # use 'raw' for regression
observed <- data_test$V.9 #observed outcome
R2_all <- format(cor(estimate_all,observed)^2,digits=4) #R2

# predict on test data using lm_step, where stepVIF does a backward model selection 
# need to do this again using only train data)
if (require(car)) {
  lm_step <- lm(V.9~., data=select(data_train,-c(V.30)))
  lm_step <- stepVIF(lm_step, threshold = 5, verbose = TRUE)
}

# variables remaining = V.1 V.2 V.4 V.7 V.8 V.14 V.25 V.10

estimate_step <- predict(lm_step,type='response',newdata=data_test)
observed <- data_test$V.9 #observed outcome
R2_step = format(cor(estimate_step,observed)^2,digits=4) #R2


# plot predicted vs actual
par(mfrow = c(1, 2))

plot(x=estimate_all, y=observed,
     xlab='Predicted Values',
     ylab='Actual Values',
     main=paste('R2 score (all variables) =', R2_all))
abline(a=0, b=1) #add diagonal line for estimated regression line

plot(x=estimate_step, y=observed,
     xlab='Predicted Values',
     ylab='Actual Values',
     main=paste('R2 score (8 variables) =', R2_step))
abline(a=0, b=1) #add diagonal line for estimated regression line

# predict on test data using lm_step 
# need to do this again using only train data)
if (require(car)) {
  lm_step <- lm(V.9~., data=select(data_train,-c(V.30)))
  lm_step <- stepVIF(lm_step, threshold = 5, verbose = TRUE)
}

summary(lm_step)


## 2.2 pca 

# from lm_step, the set of uncorrelated variables are 
# V.1 V.2 V.4 V.7 V.8 V.14 V.25 V.10

data_train_step = select(data_train,
                         c(V.1, V.2, V.4, V.7, V.8, 
                           V.9, V.14, V.25, V.10))

# pca with standardization
pca<-prcomp(data_train,center=T,scale=T)
pca_step<-prcomp(data_train_step,center=T,scale=T)
summary(pca)
summary(pca_step)


# Eigenvalues extraction
eig.val <- get_eigenvalue(pca)
eig.val_step <- get_eigenvalue(pca_step)
eig.val # pc1-pc10 to explain >95%
eig.val_step # pc1-pc6 to explain >95%

# Scree Plot
par(mfrow = c(1, 2))
fviz_eig(pca_step,addlabels = TRUE)  
fviz_eig(pca,addlabels = TRUE)  

# pcs contribution plot
fviz_contrib(pca, choice = "var", axes = 1:10, head = 6)
# require ALL 30 variablse to explain >95%
fviz_contrib(pca_step, choice = "var", axes = 1:6, head = 6)
# all 9 variables in data_train_step contribute equally to explain > 95%
# pca results consistent with the 9 "variables" as sufficient predictors

res.ind <- get_pca_ind(pca)
res.ind


#####################################
### Section 3. Hypotheses testing ###
#####################################


## 3.1 Hypothesis: The price means of the 4 residential types are different as they are drawn from distinct populations

data$V.10 <- as.factor(data$V.10)

# All-pairs t test on population mean
pairwise.test <- data %>% t_test(V.9 ~ V.10)

ggboxplot(data,x="V.10",y="V.9",color="V.10",ylab="Actual sales price (10000 IRR)",xlab="Residential type") + labs(title="All-pairs t-test on actual sales vs residential type") + stat_pvalue_manual(pairwise.test,label="p.adj",y.position=c(6000,6500,7000,7500,8000,8500))

# ANOVA test to see if means of the various residential types are same/different

anova_actual_price <- aov(V.9 ~ V.10,data=data)
anova_actual_price_tukey <- TukeyHSD(anova_actual_price)
par(mfrow = c(1, 1))
plot(anova_actual_price_tukey,las=1)

# There is a chance where the means of residential types 1 and 2 are the same


## 3.2 correlation and contingency tables for categorical factors

# contingency tables for V1 and V10
V1_table <- table(data$V.1, data$V.30)
V10_table <- table(data$V.10, data$V.30)
par(mfrow = c(2, 1))
# mosiac plots
mosaicplot(V1_table)
mosaicplot(V10_table)
# odds ratio for V1 (NA for V10)
fisher.test(V1_table)
# fisher.test(V10_table)

# chi-sq contingency analysis
chisq.test(V1_table)
chisq.test(V10_table)

## 3.3 ANOVA tests for models

# step up lm with ALL or selected variables
lm_all <- lm(V.9~., data=select(data,-c(V.30)))
summary(lm_all)
lm_step <- lm(V.9~., data=select(data,
                                 c(V.1, V.2, V.4, V.7, V.8, 
                                   V.9, V.14, V.25, V.10)))
summary(lm_step)

anova(lm_all, lm_step)
