##Results

###Data for Regression
data.clean <- data #Original Data Cleaned (Nothing Added) (With Id)
trans.numdat <- num.dat #cleaned, converted cat to num (with Id)
mdata <- trans.numdat
  mdata <- mdata[-c(which(mdata$GrLivArea > 4000)),] #cleaned, converted cat to num, no outliers.

#################################################################################################  
  
###Data for Decision Trees
tdata <- data
tdata <- tdata[-c(which(tdata$GrLivArea > 4000)),]

#bin SalePrice into bins; low,med,high,very high
price.check <- function(x) {
  if(tdata$SalePrice[x] <= 140000) {
    print('Low')
  } else {
    if(tdata$SalePrice[x] <= 200000) {
      print('Medium')
    } else {
      print("High")
    }
  }
}


x = 1
for(i in 1:nrow(tdata)) {
  tdata$Price.Level[x] <- price.check(x)
  x = x + 1
}

tdata <- as.data.frame(unclass(tdata)) # Final dataset with all char turned to factors
##################################################################################################

###Decision Tree - spliting data into train/testing (70/30)
dectrain <- tdata[train.idx,]
dectest <- tdata[-train.idx,]
dectrain.2 <- dectrain
dectest.2 <- dectest
dectrain <- subset(dectrain, select = -c(Id, SalePrice))
dectest <- subset(dectest, select = -c(Id, SalePrice, Price.Level))



###Decision Tree

#### Model 1 - All variables - with Char turned to factors
dec.mod1 <- ctree(Price.Level ~ ., data = dectrain)
plot(dec.mod1)
print(dec.mod1)

dec.predict1 <- predict(dec.mod1, dectest)
head(dec.predict1)

confMat <- table(dec.predict1, dectest.2$Price.Level)
confusionMatrix(confMat) #Sensitivity = TP, Specificity = TN


decmod1.acc <- sum(diag(confMat))/sum(confMat)
decmod1.acc ##0.7574371



### Model 2 - Model 1 with rpart (Decision Tree)

dec.mod2 <- rpart(Price.Level ~ Neighborhood + OverallQual + YearBuilt + GarageYrBlt + GrLivArea +
                  ExterQual + KitchenQual + X1stFlrSF + TotalBsmtSF + MSZoning + GarageType +
                  LotFrontage + TotRmsAbvGrd + GarageArea, data = dectrain)
dec.predict2 <- predict(dec.mod2, type = "class", dectest)

confMat.r <- table(dec.predict2, dectest.2$Price.Level)
decmod2.acc <- sum(diag(confMat.r))/sum(confMat.r) ##Accuracy 0.7437071

printcp(dec.mod2)
plotcp(dec.mod2)
plot(dec.mod2, uniform = TRUE)
rpart.plot(dec.mod2)
fancyRpartPlot(dec.mod2)

text(dec.mod2, use.n=TRUE, all=TRUE, cex=0.8)
post(dec.mod2, file = "dec_tree.ps")

pdec.mod2 <- prune(dec.mod2, cp = 0.01)
rpart.plot(pdec.mod2)
plot(pdec.mod2, uniform = TRUE)
text(pdec.mod2, use.n=TRUE, all=TRUE,cex=.8)
#ROC Curve needed?

###Model 3 with all variables
dec.mod3 <- rpart(Price.Level ~ ., data = dectrain)
dec.predict3 <- predict(dec.mod3, type = "class", dectest)

confMat.r3 <- table(dec.predict3, dectest.2$Price.Level)
decmod3.acc <- sum(diag(confMat.r2))/sum(confMat.r2)
confusionMatrix(confMat.r3)

plot(dec.mod2, unifrom = TRUE)
text(pdec.mod2, use.n=TRUE, all=TRUE,cex=.8)

###Plotting tree

library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)

prp(dec.mod2)

################################################################################################


###Regression - Spliting the Data into train/test sets (70/30)
#index.trainBU <- train.idx #backup
train.idx <- sample(nrow(mdata), 0.7*nrow(mdata))
train <- mdata[train.idx,]
test <- mdata[-train.idx,]
train.2 <- train
test.2 <- test
train <- subset(train, select = -c(Id))
test <- subset(test, select = -c(Id, SalePrice))

################################################################################################
  
###Linear Regression

####Model 1 - 10 feat - Highest Correlation Attributes


lr.mod1 <- lm(SalePrice ~ OverallQual + GrLivArea + GarageCars + GarageArea + 
                TotalBsmtSF + X1stFlrSF + FullBath + TotRmsAbvGrd + YearBuilt + 
                YearRemodAdd, data = train)
summary(lr.mod1) #r2 @ 0.8158

lr.predict1 <- predict(lr.mod1, type = "response", newdata = test)
lr.eval1 <- as.data.frame(cbind(SalePrice = test.2$SalePrice, PredictPrice = lr.predict1))
eval1.rmse <- rmse(lr.eval1$SalePrice, lr.eval1$PredictPrice)
eval1.rmse #$28781.54
head(lr.eval1,25)

plot(PredictPrice~SalePrice, lr.eval1, cex=.8)
abline(lr.mod1)
plot(lr.mod1)

tx <- lr.mod1$fitted.values

#residual plots
res.plot1 <- plot(lr.eval1$SalePrice, (lr.eval1$SalePrice-lr.eval1$PredictPrice), ylab="Residuals", xlab="SalePrice",main="Residual Plot - Regression Model 1")
abline(0,0)

#################################################################################################

####Model 2 - Include all 89 Variables incl cat to num.
lr.mod2 <- lm(SalePrice ~ ., data = train)
summary(lr.mod2) #r2 @ 0.8964

lr.predict2 <- predict(lr.mod2, type = "response", newdata = test)
lr.eval2 <- as.data.frame(cbind(SalePrice = test.2$SalePrice, PredictPrice = lr.predict2))
eval2.rmse <- rmse(lr.eval2$SalePrice, lr.eval2$PredictPrice)
eval2.rmse #$23,155.12
head(lr.eval2,25)

#Residual Plot
res.plot2 <- plot(lr.eval2$SalePrice, (lr.eval2$SalePrice-lr.eval2$PredictPrice), ylab="Residuals", xlab="SalePrice",main="Residual Plot - Regression Model 2")
abline(0,0)

####Model 3 - Stepwise regression to select variables
fit <- lm(SalePrice ~ ., data = train)
stepwise <- stepAIC(fit, direction = "both")
stepwise$anova #shows the variables selected

lr.mod3 <- lm(SalePrice ~ MSSubClass + LotFrontage + LotArea + OverallQual + 
                OverallCond + YearBuilt + YearRemodAdd + MasVnrArea + BsmtFinSF1 + 
                BsmtFinSF2 + BsmtUnfSF + X1stFlrSF + X2ndFlrSF + BsmtHalfBath + 
                BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + GarageYrBlt + 
                GarageCars + WoodDeckSF + OpenPorchSF + ScreenPorch + ExterQual + 
                ExterCond + BsmtCond + KitchenQual + GarageQual + BsmtExposure + 
                Functional + HipShedRoof + nbh.price.lvl + Ext1st.price.lvl + 
                SaleType.price.lvl + SaleCond.price.lvl + CdsFr3LotConfig + 
                PubUtilities + AttchdGarage + HasMasVnr + Has2ndFlr + HasRemodeled + 
                NewHouse + HasOpenPorch + HasScreenPorch + PositiveFeat1 + 
                GdMasVnr, data = train)
summary(lr.mod3) #r2 @ 0.8988

lr.predict3 <- predict(lr.mod3, type = "response", newdata = test)
lr.eval3 <- as.data.frame(cbind(SalePrice = test.2$SalePrice, PredictPrice = lr.predict3))
eval3.rmse <- rmse(lr.eval3$SalePrice, lr.eval3$PredictPrice)
eval3.rmse #$22697.29
head(lr.eval3,25)

plot(lr.eval3$PredictPrice ~ lr.eval3$SalePrice)


#Residual Plots
res.plot3 <- plot(lr.eval3$SalePrice, (lr.eval3$SalePrice-lr.eval3$PredictPrice), ylab="Residuals", xlab="SalePrice",main="Residual Plot - Regression Model 3")
abline(0,0)

###################################################################################################


####Model 4 - Numeric + 1 hot encoding #not included in report
h.data <- data.clean
h.data <- h.data[-c(which(h.data$GrLivArea > 4000)),]
#numeric - 38 variables
ohnum.col <- names(which(sapply(h.data, is.numeric)))
ohnum.dat <- h.data[,ohnum.col]
#catogorical - 43 variables
ohcat.col <-names(which(sapply(h.data, is.character)))
ohcat.dat <- h.data[,ohcat.col]

####One-hot Encoding 
dummy <- dummyVars("~ .", data = h.data[,ohcat.col])
ohcat.dat1h <- data.frame(predict(dummy, newdata = h.data[,ohcat.col])) #266 variables

oh.df <- cbind(ohnum.dat, ohcat.dat1h)
oh.df <- oh.df[-c(which(oh.df$GrLivArea > 4000)),] # removed outliers

ohtrain.idx <- sample(nrow(oh.df), 0.7*nrow(oh.df))
oh.train <- oh.df[ohtrain.idx,]
oh.test <- oh.df[-ohtrain.idx,]
oh.train.2 <- oh.train
oh.test.2 <- oh.test
oh.train <- subset(oh.train, select = -c(Id))
oh.test <- subset(oh.test, select = -c(Id, SalePrice))


model.1h <- lm(SalePrice ~ ., data = oh.train)
summary(model.1h)

predict.1h <- predict(model.1h, type = "response", newdata = oh.test)
lr.eval4 <- as.data.frame(cbind(SalePrice = oh.test.2$SalePrice, PredictPrice = predict.1h))
eval4.rmse <- rmse(lr.eval4$SalePrice, lr.eval4$PredictPrice)
rmse(eval.test1h$V1, eval.test1h$predict.1h) #39058.19




##################################################################################################


###Randon Forest

####Model 1 - RF with all variables
rf.mod1 <- randomForest(SalePrice ~ ., data = train)
summary(rf.mod1)

rf.predict1 <- predict(rf.mod1, type = "response", newdata = test)
rf.eval1 <- as.data.frame(cbind(SalePrice = test.2$SalePrice, PredictPrice = rf.predict1))
eval1.rmse <- rmse(rf.eval1$SalePrice, rf.eval1$PredictPrice)
eval1.rmse 
#RMSE $20365.42

importance(rf.mod1)
print(rf.mod1)

rf.residuals <- resid(rf.mod1)
rf.plot1 <- plot(rf.eval1$SalePrice, (rf.eval1$SalePrice-rf.eval1$PredictPrice), ylab="Residuals", xlab="SalePrice",main="Residual Plot - Random Forest")
abline(0,0)

rf.plot1 <- plot(rf.eval1$SalePrice, (rf.eval1$SalePrice-rf.eval1$PredictPrice), ylab="Residuals", xlab="SalePrice",main="Residual Plot - Random Forest")
