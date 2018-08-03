# Predictive-Modeling-Analysis-Examples

#####################
### Book Problems ###
#####################


## Chapter 2: #10
#a
library(MASS)
Boston

#b
#Check to see which variables are non-numeric, and convert them to numeric
str(Boston)
Boston$chas <- as.numeric(Boston$chas)
Boston$rad <- as.numeric(Boston$rad)
pairs(Boston)
pairs(Boston$medv ~ Boston$lstat)

#c
cor(Boston, Boston$crim)

#d
summary(Boston$crim)
hist(Boston$crim)
summary(Boston$tax)
hist(Boston$tax)
summary(Boston$ptratio)
hist(Boston$ptratio)

#e
nrow(subset(Boston, Boston$chas==1))

#f
median(Boston$ptratio)

#g
lowest_medv <- Boston[order(Boston$medv), ]
lowest_medv[1, ]

#f
nrow(subset(Boston, Boston$rm > 7))
nrow(subset(Boston, Boston$rm > 8))
more_than_8rms <- subset(Boston, Boston$rm > 8)
summary(more_than_8rms)

######################################################

## Chapter 3: #15
#a
plot.default(Boston$crim, Boston$zn)
abline(lm(Boston$crim ~ Boston$zn))
summary(lm(Boston$crim ~ Boston$zn))
lm_zn <- lm(Boston$crim ~ Boston$zn)

plot.default(Boston$crim, Boston$indus)
abline(lm(Boston$crim ~ Boston$indus))
summary(lm(Boston$crim ~ Boston$indus))
lm_indus <- lm(Boston$crim ~ Boston$indus)

plot.default(Boston$crim, Boston$chas)
abline(lm(Boston$crim ~ Boston$chas))
summary(lm(Boston$crim ~ Boston$chas))
lm_chas <- lm(Boston$crim ~ Boston$chas)

plot.default(Boston$crim, Boston$nox)
abline(lm(Boston$crim ~ Boston$nox))
summary(lm(Boston$crim ~ Boston$nox))
lm_nox <- lm(Boston$crim ~ Boston$nox)

plot.default(Boston$crim, Boston$rm)
abline(lm(Boston$crim ~ Boston$rm))
summary(lm(Boston$crim ~ Boston$rm))
lm_rm <- lm(Boston$crim ~ Boston$rm)

plot.default(Boston$crim, Boston$age)
abline(lm(Boston$crim ~ Boston$age))
summary(lm(Boston$crim ~ Boston$age))
lm_age <- lm(Boston$crim ~ Boston$age)

plot.default(Boston$crim, Boston$dis)
abline(lm(Boston$crim ~ Boston$dis))
summary(lm(Boston$crim ~ Boston$dis))
lm_dis <- lm(Boston$crim ~ Boston$dis)

plot.default(Boston$crim, Boston$rad)
abline(lm(Boston$crim ~ Boston$rad))
summary(lm(Boston$crim ~ Boston$rad))
lm_rad <- lm(Boston$crim ~ Boston$rad)

plot.default(Boston$crim, Boston$tax)
abline(lm(Boston$crim ~ Boston$tax))
summary(lm(Boston$crim ~ Boston$tax))
lm_tax <- lm(Boston$crim ~ Boston$tax)

plot.default(Boston$crim, Boston$ptratio)
abline(lm(Boston$crim ~ Boston$ptratio))
summary(lm(Boston$crim ~ Boston$ptratio))
lm_ptratio <- lm(Boston$crim ~ Boston$ptratio)

plot.default(Boston$crim, Boston$black)
abline(lm(Boston$crim ~ Boston$black))
summary(lm(Boston$crim ~ Boston$black))
lm_black <- lm(Boston$crim ~ Boston$black)

plot.default(Boston$crim, Boston$lstat)
abline(lm(Boston$crim ~ Boston$lstat))
summary(lm(Boston$crim ~ Boston$lstat))
lm_lstat <- lm(Boston$crim ~ Boston$lstat)

plot.default(Boston$crim, Boston$medv)
abline(lm(Boston$crim ~ Boston$medv))
summary(lm(Boston$crim ~ Boston$medv))
lm_medv <- lm(Boston$crim ~ Boston$medv)

#b
summary(lm(Boston$crim ~ .,data = Boston))

#c
coefficient_plot <- c((coefficients(lm_zn)[2]), coefficients(lm_indus)[2], coefficients(lm_chas)[2], coefficients(lm_nox)[2], coefficients(lm_rm)[2], coefficients(lm_age)[2], coefficients(lm_dis)[2], coefficients(lm_rad)[2], coefficients(lm_tax)[2], coefficients(lm_ptratio)[2], coefficients(lm_black)[2], coefficients(lm_lstat)[2], coefficients(lm_medv)[2])
remove_intercept <- coefficients(lm(Boston$crim ~ .,data = Boston))[-1]
plot(coefficient_plot, remove_intercept)

#d
summary(lm(Boston$crim ~ poly(Boston$zn, 3)))
summary(lm(Boston$crim ~ poly(Boston$indus, 3)))
summary(lm(Boston$crim ~ poly(Boston$nox, 3)))
summary(lm(Boston$crim ~ poly(Boston$rm, 3)))
summary(lm(Boston$crim ~ poly(Boston$age, 3)))
summary(lm(Boston$crim ~ poly(Boston$dis, 3)))
summary(lm(Boston$crim ~ poly(Boston$rad, 3)))
summary(lm(Boston$crim ~ poly(Boston$tax, 3)))
summary(lm(Boston$crim ~ poly(Boston$ptratio, 3)))
summary(lm(Boston$crim ~ poly(Boston$black, 3)))
summary(lm(Boston$crim ~ poly(Boston$lstat, 3)))
summary(lm(Boston$crim ~ poly(Boston$medv, 3)))

######################################################

## Chapter 6: #9

library(ISLR)
college_data <- College

#a
set.seed(13)
tr <- sample(1:nrow(college_data), size = 0.2 * nrow(college_data))
train <- college_data[-tr, ]
test <- college_data[tr, ]
rm(college_data)

#b
# number of applications is the y variable
linear_model <- lm(train$Apps ~ ., data = train)
summary(linear_model)

prediction <- predict(linear_model, newdata = test)
MSE_linear <- mean((test$Apps - prediction)^2)
print(MSE_linear)

#c
library(glmnet)
xtrain <- model.matrix(train$Apps ~ ., data = train)[, -1]
ytrain <- train$Apps
xtest <- model.matrix(test$Apps ~ ., data = test)[, -1]
ytest <- test$Apps

set.seed(13)
cross_valid <- cv.glmnet(xtrain, ytrain, alpha = 0)
plot(cross_valid)

best_lambda <- cross_valid$lambda.min
ridge_model <- glmnet(xtrain, ytrain, alpha = 0, lambda = best_lambda)
ridge_model$beta

ridge_prediction <- predict(ridge_model, s = best_lambda, newx = xtest)
MSE_ridge <- mean((ridge_prediction - ytest)^2)
print(MSE_ridge)

#d
set.seed(13)
cross_valid <- cv.glmnet(xtrain, ytrain, alpha = 1)
plot(cross_valid)

best_lambda <- cross_valid$lambda.min
lasso_model <- glmnet(xtrain, ytrain, alpha = 1, lambda = best_lambda)
lasso_model$beta

lasso_prediction <- predict(lasso_model, s = best_lambda, newx = xtest)
MSE_lasso <- mean((lasso_prediction - ytest)^2)
print(MSE_lasso)

#e
library(pcr)
library(pls)
library(plyr)
set.seed(13)
pcr_model <- pcr(train$Apps ~ ., data = train, scale = T, validation = "CV")
validationplot(pcr_model, val.type = "MSEP")

pcr_prediction <- predict(pcr_model, test, ncomp = 17)
MSE_pcr <- mean((pcr_prediction - ytest)^2)
print(MSE_pcr)

#f
set.seed(13)
pls_model <- plsr(train$Apps ~ ., data = train, scale = T, validation = "CV")
validationplot(pls_model, val.type = "MSEP")

pls_prediction <- predict(pls_model, test, ncomp = 7)
MSE_pls <- mean((pls_prediction - ytest)^2)
print(MSE_pls)

#g
#these RMSE's were generated for the purposes of creating a comparison table
sqrt(MSE_linear)
sqrt(MSE_ridge)
sqrt(MSE_lasso)
sqrt(MSE_pcr)
sqrt(MSE_pls)

######################################################

## Chapter 6: #11

#a
library(MASS)
Boston
set.seed(13)

#train and test samples
trBoston <- sample(1:nrow(Boston), size = 0.2 * nrow(Boston))
trainBoston <- Boston[-trBoston, ]
testBoston <- Boston[trBoston, ]

#best subset selection
require(leaps)

predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

k = 10
folds <- sample(1:k, nrow(Boston), replace = TRUE)
cv.errors <- matrix(NA, k, 13, dimnames = list(NULL, paste(1:13)))
for (j in 1:k) {
  best.fit <- regsubsets(crim ~ ., data = Boston[folds != j, ], nvmax = 13)
  for (i in 1:13) {
    pred <- predict(best.fit, Boston[folds == j, ], id = i)
    cv.errors[j, i] <- mean((Boston$crim[folds == j] - pred)^2)
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
plot(mean.cv.errors, type = "b", xlab = "Number of Predictor Variables", ylab = "MSE")

#ridge
library(glmnet)
xtrainBoston <- model.matrix(trainBoston$crim ~ ., data = trainBoston)[, -1]
ytrainBoston <- trainBoston$crim
xtestBoston <- model.matrix(testBoston$crim ~ ., data = testBoston)[, -1]
ytestBoston <- testBoston$crim

set.seed(13)
cross_valid <- cv.glmnet(xtrainBoston, ytrainBoston, alpha = 0)
plot(cross_valid)

best_lambda <- cross_valid$lambda.min
ridge_model <- glmnet(xtrainBoston, ytrainBoston, alpha = 0, lambda = best_lambda)
ridge_model$beta

ridge_prediction <- predict(ridge_model, s = best_lambda, newx = xtestBoston)
MSE_ridge <- mean((ridge_prediction - ytestBoston)^2)
print(MSE_ridge)

#lasso
set.seed(13)
cross_valid <- cv.glmnet(xtrainBoston, ytrainBoston, alpha = 1)
plot(cross_valid)
print(cross_valid)

best_lambda <- cross_valid$lambda.min
lasso_model <- glmnet(xtrainBoston, ytrainBoston, alpha = 1, lambda = best_lambda)
lasso_model$beta

lasso_prediction <- predict(lasso_model, s = best_lambda, newx = xtestBoston)
MSE_lasso <- mean((lasso_prediction - ytestBoston)^2)
print(MSE_lasso)

#PCR
set.seed(13)
pcr_model <- pcr(trainBoston$crim ~ ., data = trainBoston, scale = T, validation = "CV")
validationplot(pcr_model, val.type = "MSEP")

pcr_prediction <- predict(pcr_model, testBoston, ncomp = 13)
MSE_pcr <- mean((pcr_prediction - ytestBoston)^2)
print(MSE_pcr)

#b
print(sqrt(mean(mean.cv.errors)))
print(MSE_ridge)
print(MSE_lasso)
print(MSE_pcr)

#c
plot(mean.cv.errors, type = "b", xlab = "Number of Predictor Variables", ylab = "MSE")
print(mean.cv.errors)

######################################################

## Chapter 4: #10
#a
library(ISLR)
Weekly
summary(Weekly)
pairs(Weekly)
cor(Weekly[, -9])
plot(Weekly$Volume)

#b
lin_reg <- glm(Weekly$Direction ~ Weekly$Lag1 + Weekly$Lag2 + Weekly$Lag3 + Weekly$Lag4 + Weekly$Lag5 + Weekly$Volume, family = binomial)
summary(lin_reg)

#c
lin_reg.probs <- predict(lin_reg, type="response")
lin_reg.preds <- ifelse(lin_reg.probs>.5, "Up", "Down")
confusion_matrix <- table(Weekly$Direction, lin_reg.preds)
confusion_matrix

correct_pred <- (557 + 54) / (54 + 430 + 48 + 557)
error_pred <- 1 - correct_pred
marketUP <- 557 / (48 + 557)
marketDOWN <- 54 / (54 + 430)

#d
trainWeekly <- (Weekly$Year <= 2008)
testWeekly <- Weekly[!trainWeekly, ]

testDirection <- Weekly$Direction[!trainWeekly]
confusion_matrix_d <- glm(testWeekly$Direction ~ testWeekly$Lag2, data = testWeekly, family = "binomial", subset = trainWeekly)
summary(confusion_matrix_d)

probsd <- predict(confusion_matrix_d, data = testWeekly, type="response")
predsd <- rep("Down", length(probsd))
predsd[probsd > 0.5] <- "Up"
table(predsd, testWeekly$Direction)

correct_predD <- (8 + 57) / (8 + 4 + 35 + 57)
1 - correct_predD
57 / (57 + 4) 
8 / (8 + 35)

#skips letters e & f

#g
library(class)
set.seed(13)

trainG <- Weekly[trainWeekly, c("Lag2", "Direction")]
knn.predG <- knn(train = data.frame(trainG$Lag2), test = data.frame(testWeekly$Lag2), cl = trainG$Direction, k = 1)
confusion_matrix_g <- table(testWeekly$Direction, knn.predG)
confusion_matrix_g

(21 + 32) / (29 + 22 + 21 + 32)
1 - ((21 + 32) / (29 + 22 + 21 + 32))

#h
#to answer this question, look at the confusion matrices to determine which method has the lowest error rate

#i
# first try different values for k
library(class)
set.seed(13)

trainG <- Weekly[trainWeekly, c("Lag2", "Direction")]
knn.predG <- knn(train = data.frame(trainG$Lag2), test = data.frame(testWeekly$Lag2), cl = trainG$Direction, k = 10)
confusion_matrix_g <- table(testWeekly$Direction, knn.predG)
confusion_matrix_g

library(class)
set.seed(13)

trainG <- Weekly[trainWeekly, c("Lag2", "Direction")]
knn.predG <- knn(train = data.frame(trainG$Lag2), test = data.frame(testWeekly$Lag2), cl = trainG$Direction, k = 20)
confusion_matrix_g <- table(testWeekly$Direction, knn.predG)
confusion_matrix_g

library(class)
set.seed(13)

trainG <- Weekly[trainWeekly, c("Lag2", "Direction")]
knn.predG <- knn(train = data.frame(trainG$Lag2), test = data.frame(testWeekly$Lag2), cl = trainG$Direction, k = 40)
confusion_matrix_g <- table(testWeekly$Direction, knn.predG)
confusion_matrix_g

library(class)
set.seed(13)

trainG <- Weekly[trainWeekly, c("Lag2", "Direction")]
knn.predG <- knn(train = data.frame(trainG$Lag2), test = data.frame(testWeekly$Lag2), cl = trainG$Direction, k = 25)
confusion_matrix_g <- table(testWeekly$Direction, knn.predG)
confusion_matrix_g

#now just using Log1 and Log2 for the lm model
lin_reg <- glm(Weekly$Direction ~ Weekly$Lag1 + Weekly$Lag2, family = binomial)
summary(lin_reg)

#c
lin_reg.probs <- predict(lin_reg, type="response")
lin_reg.preds <- ifelse(lin_reg.probs>.5, "Up", "Down")
confusion_matrix <- table(Weekly$Direction, lin_reg.preds)
confusion_matrix

######################################################

## Chapter 8: #8
#a
library(ISLR)
set.seed(13)

#train and test samples
trCarseats <- sample(1:nrow(Carseats), size = 0.2 * nrow(Carseats))
trainCarseats <- Carseats[-trCarseats, ]
testCarseats <- Carseats[trCarseats, ]

#b
library(tree)
tree_carseats <- tree(trainCarseats$Sales ~ ., data = trainCarseats)
summary(tree_carseats)

plot(tree_carseats)
text(tree_carseats, pretty = 0)

tree_pred <- predict(tree_carseats, testCarseats)
MSE_carseats <- mean((tree_pred - testCarseats$Sales)^2)
print(MSE_carseats)

#c
cross_valid_carseats <- cv.tree(tree_carseats, FUN = prune.tree)
plot(cross_valid_carseats, type = "b")
min_tree <- cross_valid_carseats$size[which.min(cross_valid_carseats$dev)]
paste(min_tree)

prune_carseats <- prune.tree(tree_carseats, best = 14)
plot(prune_carseats)
text(prune_carseats, pretty = 0)

predict_prune <- predict(prune_carseats, newdata = testCarseats)
mean((predict_prune - testCarseats$Sales)^2)

#d
library(randomForest)
set.seed(13)

bagging <- randomForest(trainCarseats$Sales ~ ., data = trainCarseats, mtry = 10, ntree = 500, importance = TRUE)
pred_bagging <- predict(bagging, newdata = testCarseats)
mean((pred_bagging - testCarseats$Sales)^2)
importance(bagging)

#e
set.seed(13)
rand_forest <- randomForest(trainCarseats$Sales ~ ., data = trainCarseats, mtry = 5, importance = TRUE)
rand_forest_pred <- predict(rand_forest, newdata = testCarseats)
mean((rand_forest_pred - testCarseats$Sales)^2)
importance(rand_forest)

set.seed(13)
rand_forest <- randomForest(trainCarseats$Sales ~ ., data = trainCarseats, mtry = 1, importance = TRUE)
rand_forest_pred <- predict(rand_forest, newdata = testCarseats)
mean((rand_forest_pred - testCarseats$Sales)^2)
importance(rand_forest)

set.seed(13)
rand_forest <- randomForest(trainCarseats$Sales ~ ., data = trainCarseats, mtry = 7, importance = TRUE)
rand_forest_pred <- predict(rand_forest, newdata = testCarseats)
mean((rand_forest_pred - testCarseats$Sales)^2)
importance(rand_forest)

set.seed(13)
rand_forest <- randomForest(trainCarseats$Sales ~ ., data = trainCarseats, mtry = 2, importance = TRUE)
rand_forest_pred <- predict(rand_forest, newdata = testCarseats)
mean((rand_forest_pred - testCarseats$Sales)^2)
importance(rand_forest)

######################################################

## Chapter 8: #11
#a
library(ISLR)
set.seed(13)
Caravan$Purchase <- ifelse(Caravan$Purchase == "Yes", 1, 0)

#train and test samples
trCaravan <- sample(1:nrow(Caravan), 1000)
trainCaravan <- Caravan[trCaravan, ]
testCaravan <- Caravan[-trCaravan, ]

#b
library(gbm)
set.seed(13)

boost_Caravan <- gbm(trainCaravan$Purchase ~ ., data = trainCaravan, distribution = "gaussian", n.trees = 1000, shrinkage = .01)
summary(boost_Caravan)

#c
pred_boosting <- predict(boost_Caravan, testCaravan, type = "response", n.trees = 1000)
boost_pred <- ifelse(pred_boosting > 0.2, "Yes", "No")
table(boost_pred, testCaravan$Purchase)
54 / (216 + 54)

# logistic regression
log_regression <- glm(trainCaravan$Purchase ~ ., data = trainCaravan, family = "binomial")
pred_boosting_2 <- predict(log_regression, testCaravan, type = "response")
boost_pred_2 <- ifelse(pred_boosting > 0.2, 1, 0)
table(testCaravan$Purchase, boost_pred_2)

#####################
## Carlos Problems ##
#####################


## #1
#a
getwd()
setwd("C:\\Users\\vsm397\\Desktop\\Vir's Personal documents\\Pred Modeling (MSBA)")
df_beauty = read.csv('BeautyData.csv', header = T)

beauty_regression <- lm(df_beauty$CourseEvals ~ ., data = df_beauty)
summary(beauty_regression)

#2
#answered in the Word Doc

######################################################

## #2
getwd()
setwd("C:\\Users\\vsm397\\Desktop\\Vir's Personal documents\\Pred Modeling (MSBA)")
df_midcity = read.csv('MidCity.csv', header = T)

#new vector for brick
temporary_vector_brick <- as.character(df_midcity$Brick)
temporary_vector_brick[ temporary_vector_brick== "Yes"] <- 1
temporary_vector_brick[ temporary_vector_brick== "No"] <- 0
df_midcity$Brick = temporary_vector_brick

#dummy variables for nbhd 1
dummy_nbhd1 <- as.numeric(df_midcity$Nbhd)
dummy_nbhd1[dummy_nbhd1 == 1] <- 1
dummy_nbhd1[dummy_nbhd1 == 2] <- 0
dummy_nbhd1[dummy_nbhd1 == 3] <- 0
df_midcity$Nbhd1.Dummy <- dummy_nbhd1
dummy_nbhd1 <- type.convert(df_midcity$Nbhd)

#dummy variables for nbhd 2
dummy_nbhd2 <- as.numeric(df_midcity$Nbhd)
dummy_nbhd2[dummy_nbhd2 == 1] <- 0
dummy_nbhd2[dummy_nbhd2 == 2] <- 1
dummy_nbhd2[dummy_nbhd2 == 3] <- 0
df_midcity$Nbhd2.Dummy <- dummy_nbhd2
dummy_nbhd2 <- type.convert(df_midcity$Nbhd)

#dummy variables for nbhd 3
dummy_nbhd3 <- as.numeric(df_midcity$Nbhd)
dummy_nbhd3[dummy_nbhd3 == 1] <- 0
dummy_nbhd3[dummy_nbhd3 == 2] <- 0
dummy_nbhd3[dummy_nbhd3 == 3] <- 1
df_midcity$Nbhd3.Dummy <- dummy_nbhd3
dummy_nbhd3 <- type.convert(df_midcity$Nbhd)

#premium for brick houses
midcity_regression <- lm(df_midcity$Price ~ df_midcity$SqFt + df_midcity$Offers + df_midcity$Bedrooms + df_midcity$Bathrooms + df_midcity$Brick + df_midcity$Nbhd1.Dummy + df_midcity$Nbhd2.Dummy + df_midcity$Nbhd3.Dummy, data = df_midcity)
summary(midcity_regression)
17297 + (2 * 1981.62)
17297 - (2 * 1981.62)

#premium for Nbhd 3 Houses
midcity_regression <- lm(df_midcity$Price ~ df_midcity$SqFt + df_midcity$Offers + df_midcity$Bedrooms + df_midcity$Bathrooms + df_midcity$Brick + df_midcity$Nbhd2.Dummy + df_midcity$Nbhd3.Dummy, data = df_midcity)
summary(midcity_regression)
20681.04 + (2 * 3148.95)
20681.04 - (2 * 3148.95)

#premium for Brick & Nbhd 3 Houses
df_midcity$Brick <- as.numeric(df_midcity$Brick)
df_midcity$Nbhd3.Dummy <- as.numeric(df_midcity$Nbhd3.Dummy)
df_midcity$Dummy.Nbhd3.Dummy <- df_midcity$Brick + df_midcity$Nbhd3.Dummy

#remove all values != 2
df_midcity$Dummy.Nbhd3.Dummy <- as.character(df_midcity$Dummy.Nbhd3.Dummy)
df_midcity$Dummy.Nbhd3.Dummy[df_midcity$Dummy.Nbhd3.Dummy == "0"] <- 0
df_midcity$Dummy.Nbhd3.Dummy[df_midcity$Dummy.Nbhd3.Dummy == "1"] <- 0
df_midcity$Dummy.Nbhd3.Dummy[df_midcity$Dummy.Nbhd3.Dummy == "2"] <- 1

#run regression using this new variable
midcity_regression <- lm(df_midcity$Price ~ df_midcity$SqFt + df_midcity$Offers + df_midcity$Bedrooms + df_midcity$Bathrooms + df_midcity$Brick + df_midcity$Nbhd2.Dummy + df_midcity$Nbhd3.Dummy + df_midcity$Dummy.Nbhd3.Dummy, data = df_midcity)
summary(midcity_regression)

10181.58 + (2 * 4165.27)
10181.58 - (2 * 4165.27)

# importance of nbhd 2
-673.03 + (2 * 2376.48)
-673.03 - (2 * 2376.48)

######################################################

## #3
#1 answered in Word Doc

#2 answered in Word Doc

#3 answered in Word Doc

#4 answered in Word Doc

######################################################

## #4
#Run BART on California Dataset

getwd()
setwd("C:\\Users\\vsm397\\Desktop\\Vir's Personal documents\\Pred Modeling (MSBA)")
ca = read.csv('CAhousing.csv', header = T)

logMedVal <- log(ca$medianHouseValue)
ca$logMedVal = logMedVal
ca <- ca[,-c(9)]

x = ca[,1:8] 
y = ca$logMedVal # median value
head(cbind(x,y))

library(BART) #BART package
set.seed(99) #MCMC, so set the seed
nd=200 # number of kept draws
burn=50 # number of burn in draws
bf = wbart(x,y,nskip=burn,ndpost=nd)

n=length(y) #total sample size
set.seed(99) #
ii = sample(1:n,floor(.75*n)) # indices for train data, 75% of data
xtrain=x[ii,]; ytrain=y[ii] # training data
xtest=x[-ii,]; ytest=y[-ii] # test data
cat("train sample size is ",length(ytrain)," and test sample size is ",length(ytest),"\n")

set.seed(99)
bf_train = wbart(xtrain,ytrain)
yhat = predict(bf_train,as.matrix(xtest))

yhat.mean = apply(yhat,2,mean)

plot(ytest,yhat.mean)
abline(0,1,col=2)
yhatrmse = sqrt(sum((ytest-yhat.mean)^2)/nrow(xtest))


######################################################

## #5
#Run Neural Nets on Boston Dataset

library(MASS)
Boston

#Check to see which variables are non-numeric, and convert them to numeric
str(Boston)
Boston$chas <- as.numeric(Boston$chas)
Boston$rad <- as.numeric(Boston$rad)
pairs(Boston)
pairs(Boston$medv ~ Boston$lstat)

library(nnet)

###fit nn with just one x=lstat
set.seed(99)
znn = nnet(Boston$medv~.,Boston,size=3,decay=.1,linout=T)

###get fits, print summary,  and plot fit
fznn = predict(znn,Boston)
plot(Boston$lstat,Boston$medv)
oo = order(Boston$lstat)
lines(Boston$lstat[oo],fznn[oo],col="red",lwd=2)
abline(lm(Boston$medv~Boston$lstat,Boston)$coef)

summary(Boston)

### all three x's
znn = nnet(Boston$medv~.,Boston,size=5,decay=.1,linout=T)
fznn = predict(znn,Boston)
zlm = lm(Boston$medv~.,Boston)
fzlm = predict(zlm,Boston)
temp = data.frame(y=Boston$medv,fnn=fznn,flm=fzlm)
pairs(temp)
print(cor(temp))

# try different parameters
set.seed(13)
znn1 = nnet(Boston$medv~Boston$lstat,Boston,size=3,decay=.5,linout=T)
znn2 = nnet(Boston$medv~Boston$lstat,Boston,size=3,decay=.00001,linout=T)
znn3 = nnet(Boston$medv~Boston$lstat,Boston,size=50,decay=.5,linout=T)
znn4 = nnet(Boston$medv~Boston$lstat,Boston,size=50,decay=.00001,linout=T)
temp = data.frame(medv = Boston$medv, lstat = Boston$lstat)
znnf1 = predict(znn1,temp)
znnf2 = predict(znn2,temp)
znnf3 = predict(znn3,temp)
znnf4 = predict(znn4,temp)

### plot the fits

par(mfrow=c(2,2))
plot(Boston$lstat,Boston$medv)
lines(Boston$lstat[oo],znnf1[oo],lwd=2)
title("size=3, decay=.5")
plot(Boston$lstat,Boston$medv)
lines(Boston$lstat[oo],znnf2[oo],lwd=2)
title("size=3, decay=.00001")
plot(Boston$lstat,Boston$medv)
lines(Boston$lstat[oo],znnf3[oo],lwd=2)
title("size = 50, decay = .5")
plot(Boston$lstat,Boston$medv)
lines(Boston$lstat[oo],znnf4[oo],lwd=2)
title("size = 50, decay = .00001")

################################################################

### COMPLETED Examples- Predictive Modeling- Vir Mehta ###

################################################################
