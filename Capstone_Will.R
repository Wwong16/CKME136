#Load data
data <- read.csv("C:\\Users\\William\\Documents\\Ryerson\\Summer 2017\\Datasets\\Housing Prices\\AmesPrices.csv", header = TRUE)

#load package
require(caret)
require(corrplot)
require(e1071) #skew
require(Metrics) #rmse
require(MASS)
require(randomForest)
require(party)
require(ggplot2)
require(glmnet)
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
require(dplyr)


#find the number of NAs in the dataset
na.cols <- which(colSums(is.na(data)) > 0)
mi <-colSums(is.na(data[na.cols]))
mi

############################################################################################

#Data Exploration Plots
###Neighborhood Vs SalePrice
boxplot(SalePrice~Neighborhood, data=data, las=2, main = "SalePrice VS Neighborhood", ylab = "SalePrice", xlab = "Neighborhood")

##
hist(data$SalePrice)
qplot(data$SalePrice, geom = 'histogram', main = 'Ames Housing Sale Price', xlab = 'Saleprice ($)') + geom_histogram(fill='lightblue', color = 'white') + scale_x_continuous(breaks = seq(0,800000,100000))


#function to help plot categoric data for easier data visualization
#removes the NAs and plots the counts for each catagory

plot.cat <- function(cols, df){
  for(col in cols){
    order.cols <- names(sort(table(data[,col]),decreasing = FALSE))
    
    num.plot<-qplot(data[,col]) +
      geom_bar(fill = 'cornflowerblue') +
      geom_text(aes(label=..count..), stat='count', vjust=-0.5) +
      theme_minimal() +
      scale_y_continuous(limits = c(0,max(table(df[,col]))*1.1)) +
      scale_x_discrete(limits = order.cols) +
      xlab(col) +
      theme(axis.text.x = element_text(angle = 30, size =12))
    
    print(num.plot)
  }
}

####PoolQC 1453 NAs; Maybe cause they don't have Pools

pool <- data[data$PoolArea != 0, c("PoolArea", "PoolQC")]
pool <- data[data$PoolArea == 0, c("PoolArea", "PoolQC")]
sum(pool$PoolArea)
  #sum = 0; all NAs in PoolQC does not have a pool
data[is.na(data$PoolQC), "PoolQC"] <- "NoPool"

####Garage has 5 different attributes that have NAs
colSums(is.na(data[na.cols])) # all 5 Garage columns with NAs have 81 missing values
garage.data <- c("GarageYrBlt", "GarageArea", "GarageCars", "GarageQual", "GarageFinish", "GarageCond", "GarageType")
garage <- data[is.na(data$GarageCond), garage.data]
sum(garage$GarageCars) #All NAs have 0 for GarageArea and GarageCars. Which means they have no garage

for (a in garage.data) {
  if(sapply(data[a], is.numeric) == TRUE) {
    data[sapply(data[a], is.na), a] <- 0
  }
  else {
    data[sapply(data[a], is.na),a] <- "None"
  }
}
  
####Electrical - Only 1 NA; we can fill with the most common variable
plot.cat("Electrical", data) #this shows SBrkr is the most common variable (1334)
barplot(data$Electrical, main = 'Electrical Variables', xlab = 'Electrical', ylab = 'Count')
data[is.na(data$Electrical), "Electrical"] <- "SBrkr"

####MasVnr Type & MasVnrArea - both have 8 NAs (row 689 [0,BrkFace], row 1242 [0,Stone])
mason <- data[is.na(data$MasVnrArea), c("MasVnrArea", "MasVnrType")]
data[is.na(data$MasVnrArea), "MasVnrArea"] <- 0
data[is.na(data$MasVnrType), "MasVnrType"] <- "None"

####Bsmt has 5 different col with NAs
bsmt <- data[is.na(data$BsmtExposure) | is.na(data$BsmtFinType2),c(31:39,48,49)] # Get all bsmt columns subset into a different frame

  #Only 1 BsmtExposure variable is NA and actually have a basementSF value. We can fill with most common value (No)
data[949,"BsmtExposure"] <- "No"
  #All other NAs are because the house does not have a basement total SF = 0. Fill these with NoBsmt
data[is.na(data$BsmtQual), "BsmtQual"] <- "None"
data[is.na(data$BsmtCond), "BsmtCond"] <- "None"
data[is.na(data$BsmtExposure), "BsmtExposure"] <- "None"
data[is.na(data$BsmtFinType1), "BsmtFinType1"] <- "None"

  #Row 333 has a NA in BsmtFinType2 and it has a value for BsmtFinSF2. We will fill with the most common value which is "Unf"
data[333, "BsmtFinType2"] <- "Unf"
data[is.na(data$BsmtFinType2), "BsmtFinType2"] <- "None"

####LotFrontage has 259 NAs
  #We will assume similar neighborhoods will have similar frontage
  #get the mean value of frontage for each neighborhood 
lotfront <- data[is.na(data$LotFrontage), c("LotFrontage", "Neighborhood")]
frontagevalues <- data[!is.na(data$LotFrontage), c("LotFrontage", "Neighborhood")]
frontagevalues$Neighborhood<- as.factor(frontagevalues$Neighborhood)
fillfrontage <- frontagevalues %>% group_by(Neighborhood) %>% summarise(mean = mean(LotFrontage))
  #fill in the mean lot frontage value based on neighborhood
x <- which(is.na(data$LotFrontage)) #returns row numbers of NA values

for (i in x) {
  frontmean <- as.numeric(fillfrontage[fillfrontage == data$Neighborhood[i], "mean"])
  data$LotFrontage[i] <- frontmean
}

####Alley has 1369 NA values
  #we will fill these with None as we can assume they don't have access to alley
data[is.na(data$Alley), "Alley"] <- "None"

####Fence has 1179 NA Values
  #We will fill these with None since there is probably no Fence
data[is.na(data$Fence), "Fence"] <- "None"

####FireplaceQu has 690 NAs
  #Find out if NAs are a result of 0 Fireplaces
firepl <- data[is.na(data$FireplaceQu), c("FireplaceQu", "Fireplaces")]
data[is.na(data$FireplaceQu), "FireplaceQu"] <- "None"

#### MiscFeature has 1406 NA values
  #we can assume these houses don't have any MiscFeatures. Fill with none.
data[is.na(data$MiscFeature), "MiscFeature"] <- "None"


     ####CLEAN####

########Split data into Numeric and categorical data
  #LotFrontage was showing as list. Convert it into Numeric
data$LotFrontage <- as.numeric(data$LotFrontage)

  #numeric - 38 variables
num.col <- names(which(sapply(data, is.numeric)))
num.dat <- data[,num.col]
  #catogorical - 43 variables
cat.col <-names(which(sapply(data, is.character)))
cat.dat <- data[,cat.col]

  #benchmark by 2 variables OverallQual and SalePrice
    #OverallQual = Rates overall material and finish of house
    #SalePrice = Sale price of house
plot(SalePrice ~ GrLivArea, data = data, main = "SalePrice Vs GrLivArea", xlab = "GrLivArea (SF)", ylab = "SalePrice ($)")
plot(SalePrice ~ TotalBsmtSF, data = data)
plot(SalePrice ~ X1stFlrSF, data = data)

data.rm <- data
which(data.rm$GrLivArea > 4000)
outli <- data.rm[data.rm$GrLivArea>4000, c('GrLivArea', 'SalePrice')]

##Converting ordinal data to numeric
  #Fireplacequ
fireprice <- data[,c("FireplaceQu", "SalePrice", "OverallQual")]
f1 <- fireprice %>% group_by(FireplaceQu) %>% summarise(mean.qual = mean(OverallQual), mean.price = mean(SalePrice), cnt = n()) %>% arrange(mean.qual)
qplot(x = reorder(f1$FireplaceQu, f1$mean.price), y = f1$mean.price, xlab = "FireplaceQu", ylab = "Average SalePrice") + geom_bar(stat = 'identity', fill = 'red') + scale_y_continuous()

###function to see how the different qualities affect saleprice and quality
show.qd <- function(col){
  qd.tbl <- data[,c(col,'SalePrice','OverallQual')] %>% group_by_(col) %>% summarise(mean.qual = mean(OverallQual), mean.price = mean(SalePrice), cnt = n()) %>% arrange(mean.price)
  
  print(qplot(x=reorder(qd.tbl[[col]],qd.tbl[["Mean.Price"]]), y = qd.tbl$mean.price, xlab = col, ylab = "Average Saleprice") + geom_bar(stat = 'identity', fill = 'red') + scale_y_continuous())
  return(data.frame(qd.tbl))
}

##fucntion to see price table.
price.tbl <- function(col) {
  col.price <- data[,c(col, 'SalePrice')] %>% group_by_(col) %>% summarise(Mean.Price = mean(SalePrice), Count = n()) %>% arrange(Mean.Price)
  return(col.price)
}

####

group.prices <- function(col) {
  group.table <- data[,c(col, 'SalePrice', 'OverallQual')] %>%
    group_by_(col) %>%
    summarise(mean.Quality = round(mean(OverallQual),2),
              mean.Price = mean(SalePrice), n = n()) %>%
    arrange(mean.Quality)
  
  print(qplot(x=reorder(group.table[[col]], -group.table[['mean.Price']]), y=group.table[['mean.Price']]) +
          geom_bar(stat='identity', fill='cornflowerblue') +
          theme_minimal() +
          scale_y_continuous() +
          labs(x=col, y='Mean SalePrice') +
          theme(axis.text.x = element_text(angle = 45)))
  
  return(data.frame(group.table))
}

####

###function to convert ordinal values to numbers
map.fcn <- function(cols, map.list, df){
  for (col in cols){
    df[col] <- as.numeric(map.list[data[,col]])
  }
  return(df)
}


###Columns with ordinal data
  ###
qa.col <- c('ExterQual', 'ExterCond', 'BsmtQual', 'BsmtCond', 'HeatingQC', 'KitchenQual', 'FireplaceQu', 'GarageQual', 'GarageCond')
ord.values <- c("None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)

      ###sidenote - understanding the functions####
      df.numeric <- map.fcn(qual.cols, ord.values, df.numeric)
      ord.values <- c("None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)

      num.dat2 <- map.fcn(qa.col,qa.list,num.dat2)
      num.dat2 <- num.dat2[FireplaceQu] <- as.numeric(c("None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)[data2[,"FireplaceQu"]])

###Convert the qa.col columns from ordinal data to numeric data
qa.col <- c('ExterQual', 'ExterCond', 'BsmtQual', 'BsmtCond', 'HeatingQC', 'KitchenQual', 'FireplaceQu', 'GarageQual', 'GarageCond')
num.dat <- map.fcn(qa.col, ord.values, num.dat)

###BsmtExposure from ord to num
bsmt.ord.values <- c("None" = 0, "No" = 1, "Mn" = 2, "Av" = 3, "Gd" = 4)
num.dat <- map.fcn(c('BsmtExposure'), bsmt.ord.values, num.dat)

###GarageFinish from ord to num #####
gfin.ord.values <- c("None" = 0, "Unf" = 1, "RFn" = 2, "Fin" = 3)
num.dat <- map.fcn(c('GarageFinish'), gfin.ord.values, num.dat)

###Fence from ord to num
fen.ord.values <- c("None" = 0, "MnWw" = 1, "GdWo" = 2, "MnPrv" = 3,"GdPrv" = 4)
num.dat <- map.fcn(c('Fence'), fen.ord.values, num.dat)

###Functional - Home Functionality 
  #'Typ' has 1360 obs out of 1460. Maybe just code it as 0 & 1?
fun.ord.values <- c("None" = 0, "Sal" = 1, "Sev" = 2, "Maj2" = 3, "Maj1" = 4, "Mod" = 5, "Min2" = 6, "Min1" = 7, "Typ" = 8)
num.dat <- map.fcn(c('Functional'), fun.ord.values, num.dat)

###Bsmt Fin Type 1 from ord to num

show.qd('BsmtFinType1')

#bsmt1&2 Ord to Values
AvgBsmtArea1 <- data[,c('BsmtFinType1', 'BsmtFinSF1')] %>% group_by_('BsmtFinType1') %>% summarise(meanArea = mean(BsmtFinSF1), count = n())

bsmt1.ord.values <- c('None' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)
num.dat <- map.fcn(c('BsmtFinType1'), bsmt1.ord.values, num.dat)

bsmt1.ord.values <- c('None' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)
num.dat <- map.fcn(c('BsmtFinType2'), bsmt1.ord.values, num.dat)

#Determine Correlation of Numeric Variables
num.dat <- subset(num.dat, select = -Id)
Correlation <- cor(num.dat)
Cor.Saleprice <- as.matrix(sort(Correlation[,'SalePrice'], decreasing = TRUE))

  #select the values that are >0.5 or < -0.5
cor.colnames <- names(which(apply(Cor.Saleprice,1,function(x) (x>0.5 | x< -0.5))))
corrplot(as.matrix(Correlation[cor.colnames,cor.colnames]), type = 'upper', method='color', addCoef.col = 'black', t1.cex = 0.5, cl.cex = 0.5, number.cex = 0.5)

######
#Pairs Plots

panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 

  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}
#########################


pairs(SalePrice ~ OverallQual + GrLivArea + GarageCars + GarageArea + TotalBsmtSF, data = num.dat, lower.panel = panel.smooth, upper.panel = panel.cor)
pairs(SalePrice ~ X1stFlrSF + FullBath + TotRmsAbvGrd + YearBuilt + YearRemodAdd, data = num.dat, lower.panel = panel.smooth, upper.panel = panel.cor)
#the one we used below
pairs(SalePrice ~ OverallQual+ GrLivArea + TotalBsmtSF + X1stFlrSF + FullBath +TotRmsAbvGrd, data = num.dat, lower.panel = panel.smooth, upper.panel = panel.cor)

#Building Type Prices: Grouping townhouse end units with single family houses since they have high saleprices.
bldgType.price <- data[,c("BldgType", 'SalePrice')] %>% group_by_('BldgType') %>% summarise(mean.price = mean(SalePrice), cnt = n()) %>% arrange(mean.price)
bldgType.price
num.dat['TwnhsE.1Fam'] <- (data$BldgType == 'TwnhsE' | data$BldgType == '1Fam') *1

#RoofStyle: Grp by highest mean saleprice; Hip & Shed
roofstyle.price <- data[,c("RoofStyle", 'SalePrice')] %>% group_by_('RoofStyle') %>% summarise(mean.price = mean(SalePrice), cnt = n()) %>% arrange(mean.price)
roofstyle.price

num.dat['HipShedRoof'] <- (data$RoofStyle == 'Hip' | data$RoofStyle == 'Shed') *1

#RoofMat: Gro Membran, WdShake and WdShngl together. They have over $200K saleprice
roofmat.price <- data[,c("RoofMatl", 'SalePrice')] %>% group_by_('RoofMatl') %>% summarise(mean.price = mean(SalePrice), cnt = n()) %>% arrange(mean.price)
num.dat['GdRoofMatl'] <- (data$RoofMatl == 'Membran' | data$RoofMatl == 'WdShake' | data$RoofMatl == 'WdShngl') *1

#HouseStyle
housestyle.price <- data[,c("HouseStyle", 'SalePrice')] %>% group_by_('HouseStyle') %>% summarise(mean.price = mean(SalePrice), cnt = n()) %>% arrange(mean.price)
#Price levels: mean prices of neighborhoods: <140k = 1, >=140k<200k = 2, >200k = 3
housestyle.lvl <- c('1.5Unf' = 1, 'SFoyer' = 1, '1.5Fin' = 2, '2.5Unf' = 2, 'SLvl' = 2, '1Story' = 2, '2Story' = 3, '2.5Fin' = 3)
num.dat$HouStyle.Price.lvl <- as.numeric(housestyle.lvl[data[,'HouseStyle']])

###Neighborhood Pricing#
nbh_price <- data[,c("Neighborhood", 'SalePrice')] %>% group_by_('Neighborhood') %>% summarise(mean.price = mean(SalePrice), cnt = n()) %>% arrange(mean.price)
##nbh_lvls: mean prices of neighborhoods: <140k = 1, >=140k<200k = 2, >200k = 3
nbh.lvl <- c('MeadowV' = 1, 'IDOTRR' = 1, 'BrDale' = 1, 'BrkSide' = 1, 'Edwards' = 1, 'OldTown' = 1, 'Sawyer' = 1, 'Blueste' = 1, 'SWISU' = 2, 'NPkVill' = 2, 'NAmes' = 2, 'Mitchel' = 2, 'SawyerW' = 2, 'NWAmes' = 2, 'Gilbert' = 2, 'Blmngtn' = 2, 'CollgCr' = 2, 'Crawfor' = 3, 'ClearCr' = 3, 'Somerst' = 3, 'Veenker' = 3, 'Timber' = 3, 'StoneBr' = 3, 'NridgHt' = 3, 'NoRidge' = 3)
num.dat$nbh.price.lvl <- as.numeric(nbh.lvl[data[,'Neighborhood']])

##Exterior1st pricing
price.tbl('Exterior1st')
ext1st.lvl <- c('BrkComm' = 1, 'AsphShn' = 1, 'CBlock' = 1, 'AsbShng' = 1, 'MetalSd' = 2, 'Wd Sdng' = 2, 'WdShing' = 2, 'Stucco' = 2, 'HdBoard' = 2, 'Plywood' = 2, 'BrkFace' = 2, 'VinylSd' = 3, 'CemntBd' = 3, 'Stone' = 3, 'ImStucc' = 3)
num.dat$Ext1st.price.lvl <- as.numeric(ext1st.lvl[data[,'Exterior1st']])

##Exterior2nd Pricing
price.tbl('Exterior2nd')
ext2nd.lvl <- c('CBlock' = 1, 'AsbShng' = 1, 'Brk Cmn' = 1, 'AsphShn' = 1, 'Wd Sdng' = 2, 'MetalSd' = 2, 'Stucco' = 2, 'Stone' = 2, 'Wd Shng' = 2, 'HdBoard' = 2, 'Plywood' = 2, 'BrkFace' = 2, 'VinylSd' = 3, 'CmentBd' = 3, 'ImStucc' = 3, 'Other' = 3)
num.dat$Ext2nd.price.lvl <- as.numeric(ext2nd.lvl[data[,'Exterior2nd']])

##SaleType Pricing; Group var with similar mean saleprice together
price.tbl('SaleType')
saletype.lvl <- c('Oth' = 1, 'ConLD' = 1, 'ConLw' = 2, 'COD' = 2, 'WD' = 3, 'ConLI' = 4, 'CWD' = 4, 'Con' = 5, 'New' = 5)
num.dat$SaleType.price.lvl <- as.numeric(saletype.lvl[data[,'SaleType']])

##SaleCondition
price.tbl('SaleCondition')
salecond.lvl <- c('AdjLand' = 1, 'Abnorml' = 2, 'Family' = 2, 'Alloca' = 3, 'Normal' = 3, 'Partial' = 4)
num.dat$SaleCond.price.lvl <- as.numeric(salecond.lvl[data[,'SaleCondition']])

##MSZoning
price.tbl('MSZoning')
mszoning.lvl <- c('C (all)' = 1, 'RM' = 2, 'RH' = 2, 'RL' = 3, 'FV' = 4)
num.dat$MSZone.price.lvl <- as.numeric(mszoning.lvl[data[,'MSZoning']])


###Creating Binary Features 

  #LotShape - Regular & Irregular
  price.tbl('LotShape')

num.dat['LotShapeReg'] <- (data$LotShape == 'Reg') *1
num.dat['LandContourLvl'] <- (data$LandContour == 'Lvl') *1
num.dat['LandSlopeGtl'] <- (data$LandSlope == 'Gtl') *1
num.dat['CdsFr3LotConfig'] <- (data$LotConfig == 'CulDSac' | data$LotConfig == 'FR3') *1
num.dat['PubUtilities'] <- (data$Utilities == 'AllPub') *1 #all but 1 obs is not Pub
num.dat['ElectricalStd'] <- (data$Electrical == 'SBrkr') *1
num.dat['AttchdGarage'] <- (data$GarageType == 'Attchd' | data$GarageType == 'BuiltIn') *1 #Attch & BuiltIn have higher saleprices.
num.dat['PavedDriveY'] <- (data$PavedDrive == 'Y') *1
num.dat['ShedMcFeat'] <- (data$MiscFeature == 'Shed') *1
num.dat['HasMasVnr'] <- (data$MasVnrArea > 0) *1
num.dat['Has2ndFlr'] <- (data$X2ndFlrSF > 0) *1
num.dat['HasWoodDeck'] <- (data$WoodDeckSF > 0) *1
num.dat['HasRemodeled'] <- (data$YearBuilt != data$YearRemodAdd) *1
num.dat['RecentRemoded'] <- (data$YearRemodAdd == data$YrSold) *1
num.dat['NewHouse'] <- (data$YearBuilt == data$YrSold) *1
num.dat['HasOpenPorch'] <- (data$OpenPorchSF > 0) *1
num.dat['HasEnclosedPorch'] <- (data$EnclosedPorch > 0) *1
num.dat['Has3ssPorch'] <- (data$X3SsnPorch > 0) *1
num.dat['HasScreenPorch'] <-(data$ScreenPorch > 0) *1
num.dat['PositiveFeat1'] <- (data$Condition1 == 'PosA' | data$Condition1 == 'PosN') *1 #Group together houses close to positive features like parks...etc
num.dat['PositiveFeat2'] <- (data$Condition2 == 'PosA' | data$Condition2 == 'PosN') *1
num.dat['GdMasVnr'] <- (data$MasVnrType == 'BrkFace' | data$MasVnrType == 'Stone') *1 #Grouped BrkFace & Stone together. Over $200k saleprice.
num.dat['GasHeating'] <- (data$Heating == 'GasW' | data$Heating == 'GasA') *1 #Gas heating has significant higher saleprice.
num.dat['HasCentralAir'] <- (data$CentralAir == 'Y') *1
num.dat['NoAlley'] <- (data$Alley == 'None') *1 #No alley way results in higher saleprice
num.dat['Age'] <- as.numeric(2010 - data$YearBuilt)

####One-hot Encoding 
cat.to.num.idx <- dummyVars("~ .", data = data[,cat.col])
cat.to.num <- data.frame(predict(cat.to.num.idx, newdata = data[,cat.col]))

###Check for normality for SalePrice
n.hist <- hist(data$SalePrice, xlab = 'SalePrice', ylab = 'Count', main = 'House Saleprice')
  #Distribution is skewed to the right

skewed <- apply(num.dat,2,skewness) #SalePrice has a value of 1.879; right skewed.

###Perform Log transformation to saleprice to normalize
num.dat.BUlog <- num.dat #backup
num.dat$Log.SalePrice <- log(num.dat$SalePrice)
num.dat <- subset(num.dat, select = -c(Id, SalePrice)) #

###Divide data (num.dat) into training and testing set. 70/30 split
train.idx <- sample(nrow(num.dat), 0.7*nrow(num.dat))
train <- num.dat[train.idx,]
test <- num.dat[-train.idx,]
#train.2 <- train
#test.2 <- test
train <- subset(train, select = -c(Id))
test <- subset(test, select = -c(Id, SalePrice))

##Simple Linear Regression - with all variables
model.1 <- lm(Log.SalePrice ~ ., data = train)
summary(model.1)

predict.1 <- predict(model.1, interval = "prediction", newdata = test)
predict.1

p.log.price <- as.numeric(predict.1[,"fit"])
eval.test <- data.frame(cbind(RealPrice = test.2$SalePrice, RealLogPrice = test.2$Log.SalePrice, p.log.price))
eval.test$p.price <- exp(p.log.price)


rmse(eval.test$RealLogPrice, eval.test$p.log.price) #0.1454363

#same model - 
predict.2 <- predict(model.1, type = "response", newdata = test)
predict.2
eval.test2 <- cbind(test.2, predict.2)
eval.test2$p.price <- exp(predict.2)
rmse(eval.test2$SalePrice, eval.test2$p.price) #33908.64

##Linear Regression with Stepwise
fit <- lm(SalePrice ~ ., data = train)
stepwise <- stepAIC(fit, direction = "both")
stepwise$anova #shows the variables selected

model.2 <- lm(Log.SalePrice ~ MSSubClass + LotFrontage + LotArea + OverallQual + 
                OverallCond + YearBuilt + BsmtFinSF2 + X1stFlrSF + X2ndFlrSF + 
                BsmtFullBath + FullBath + HalfBath + TotRmsAbvGrd + GarageYrBlt + 
                GarageCars + WoodDeckSF + EnclosedPorch + ScreenPorch + PoolArea + 
                MoSold + YrSold + FireplaceQu + ExterQual + ExterCond + BsmtQual + 
                HeatingQC + KitchenQual + GarageQual + BsmtExposure + GarageFinish + 
                Functional + BsmtFinType1 + BsmtFinType2 + PavedDriveY + 
                HasMasVnr + HasWoodDeck + HasEnclosedPorch + Has3ssPorch + 
                PubUtilities + CdsFr3LotConfig + nbh.price.lvl + RecentRemoded + 
                GdMasVnr + HasCentralAir + HouStyle.Price.lvl + Ext2nd.price.lvl + 
                SaleCond.price.lvl, data = train)
summary(model.2)

predict.3 <- predict(model.2, type = "response", newdata = test)
eval.test3 <- cbind(test.2, predict.3)
eval.test3$p.price <- exp(predict.3)
rmse(eval.test3$Log.SalePrice, eval.test3$predict.3) #0.14387
rmse(eval.test3$SalePrice, eval.test3$p.price) #33752.18

###Linear Model No Log
#data
NLtrain<-train.2
#Train data - remove Log
NLtrain <- subset(NLtrain, select = -Log.SalePrice)
NLfit <- lm(SalePrice ~ ., data = NLtrain)
NLstepwise <- stepAIC(NLfit, direction = "both")
NLstepwise$anova

NLtest <- test.2
NLtest <- subset(NLtest, select = -Log.SalePrice)

NLmodel <- lm(SalePrice ~ MSSubClass + LotFrontage + LotArea + OverallQual + 
                OverallCond + YearRemodAdd + MasVnrArea + X1stFlrSF + X2ndFlrSF + 
                BsmtFullBath + FullBath + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + 
                GarageYrBlt + GarageCars + WoodDeckSF + ScreenPorch + FireplaceQu + 
                ExterQual + ExterCond + BsmtQual + BsmtCond + KitchenQual + 
                GarageQual + BsmtExposure + Functional + BsmtFinType1 + HasMasVnr + 
                Has2ndFlr + HasWoodDeck + NewHouse + PubUtilities + CdsFr3LotConfig + 
                nbh.price.lvl + PositiveFeat1 + PositiveFeat2 + TwnhsE.1Fam + 
                HipShedRoof + RecentRemoded + GdMasVnr + GdRoofMatl + Ext1st.price.lvl + 
                Ext2nd.price.lvl + SaleCond.price.lvl + MSZone.price.lvl, data = NLtrain)

NLpredict <- predict(NLmodel, type = "response", newdata = NLtest)
NLeval <- as.data.frame(cbind(Rprice = test.2$SalePrice, Pprice = NLpredict))
rmse(NLeval$Rprice, NLeval$Pprice) #29742.36

###Random Forest Model
RF_Model1 <- randomForest(Log.SalePrice ~ ., data = train)
RF_Model1

predict.rf <- predict(RF_Model1, test)
eval.rf <- cbind(test.2, predict.rf)
rmse(eval.rf$Log.SalePrice, eval.rf$predict.rf) #0.14637


###linear model with Stepwise for report/ NO LOG
fit <- lm(SalePrice ~ ., data = train)
stepwise <- stepAIC(fit, direction = "both")
stepwise$anova #shows the variables selected

model.3 <- lm(SalePrice ~ MSSubClass + LotFrontage + LotArea + OverallQual + 
                OverallCond + MasVnrArea + BsmtUnfSF + X1stFlrSF + X2ndFlrSF + 
                BsmtFullBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + 
                TotRmsAbvGrd + GarageYrBlt + GarageCars + WoodDeckSF + OpenPorchSF + 
                EnclosedPorch + ScreenPorch + ExterQual + BsmtQual + KitchenQual + 
                FireplaceQu + GarageQual + BsmtExposure + Functional + BsmtFinType1 + 
                TwnhsE.1Fam + HipShedRoof + HouStyle.Price.lvl + nbh.price.lvl + 
                Ext1st.price.lvl + Ext2nd.price.lvl + SaleType.price.lvl + 
                MSZone.price.lvl + LandContourLvl + LandSlopeGtl + CdsFr3LotConfig + 
                PubUtilities + ElectricalStd + HasMasVnr + Has2ndFlr + RecentRemoded + 
                NewHouse + PositiveFeat2, data = train)
summary(model.3)

predict.4 <- predict(model.3, type = "response", newdata = test)
eval.test4 <- cbind(test.2, predict.4)
eval.test4$p.price <- exp(predict.3)
rmse(eval.test4$SalePrice, eval.test4$predict.4) 
rmse(eval.test3$SalePrice, eval.test3$p.price) #32093.45

###Random Forest Model
RF_Model2 <- randomForest(SalePrice ~ MSSubClass + LotFrontage + LotArea + OverallQual + 
                            OverallCond + MasVnrArea + BsmtUnfSF + X1stFlrSF + X2ndFlrSF + 
                            BsmtFullBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + 
                            TotRmsAbvGrd + GarageYrBlt + GarageCars + WoodDeckSF + OpenPorchSF + 
                            EnclosedPorch + ScreenPorch + ExterQual + BsmtQual + KitchenQual + 
                            FireplaceQu + GarageQual + BsmtExposure + Functional + BsmtFinType1 + 
                            TwnhsE.1Fam + HipShedRoof + HouStyle.Price.lvl + nbh.price.lvl + 
                            Ext1st.price.lvl + Ext2nd.price.lvl + SaleType.price.lvl + 
                            MSZone.price.lvl + LandContourLvl + LandSlopeGtl + CdsFr3LotConfig + 
                            PubUtilities + ElectricalStd + HasMasVnr + Has2ndFlr + RecentRemoded + 
                            NewHouse + PositiveFeat2, data = train)
RF_Model2

predict.rf2 <- predict(RF_Model2, test)
eval.rf2 <- cbind(test.2, predict.rf2)
rmse(eval.rf2$SalePrice, eval.rf2$predict.rf2) #29619.51

