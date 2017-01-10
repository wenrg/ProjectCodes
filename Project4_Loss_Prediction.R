# plot(allstate.train[, 118], allstate.train[, 130]) # cont117 and cont129, aes(fill = cat_), will find out the key categorical var.

library(ggplot2)
library(vioplot)
library(car)

# setwd()
allstate.train = read.csv('train.csv')
allstate.test = read.csv('test.csv')

cont = 118:132
cat = 2:117
summary(allstate.train[, cont])
sapply(allstate.train[, cont], sd)

# correlation between cont vars
cors = cor(allstate.train[, cont])

pairs(allstate.train[, cont]) #### never run this line...


plot(allstate.train[, 128], allstate.train[, 129])
train_cont = paste(paste('allstate.train[,', 118:132), ']', collapse = ',')
vioplot(allstate.train[, 128], allstate.train[, 129])
vioplot(allstate.train[, 118 ],allstate.train[, 119 ],allstate.train[, 120 ],allstate.train[, 121 ])
vioplot(allstate.train[, 122 ],allstate.train[, 123 ],allstate.train[, 124 ],allstate.train[, 125 ])
vioplot(allstate.train[, 126 ],allstate.train[, 127 ],allstate.train[, 130 ],allstate.train[, 131 ])
vioplot(allstate.train[, 132 ])
 

# transform loss
plot(density(allstate.train[, 132]))
model.saturated = lm(loss ~., data = allstate.train[, -1])
ms.summary = summary(model.saturated)
ms.coef = ms.summary$coefficients
bc = boxCox(model.saturated) # lambda = 0, log(y)
loss.log = log(allstate.train$loss + 1)


ggplot(allstate.train, aes(x = cat112, fill = cat112)) + geom_bar() + guides(fill = F)
levels(allstate.train$cat112) # states


plot(allstate.train[, 118], allstate.train[, 123]) # possible linear
plot(allstate.train[, 118], allstate.train[, 126]) # possible linear
plot(allstate.train[, 118], allstate.train[, 127]) # possible linear
plot(allstate.train[, 118], allstate.train[, 130]) # two lines ???

## differences of the levels in cat vars between train.csv and test.csv
lst = c()
for (i in 2:117){
  lst[i-1] = identical(levels(allstate.train[, i]), levels(allstate.test[, i]))
}
cat.different = which(lst == F) + 1

for (i in cat.different){
  cat("cat", i-1,"train set has", setdiff(allstate.train[, i], allstate.test[, i]), ';')
  cat("test set has", setdiff(allstate.test[, i], allstate.train[, i]),"\n")
}
# cat 89 train set has I ;test set has F 
# cat 90 train set has G ;test set has  
# cat 92 train set has F ;test set has G E 
# cat 96 train set has  ;test set has H 
# cat 99 train set has  ;test set has U 
# cat 101 train set has N U ;test set has  
# cat 102 train set has H J ;test set has  
# cat 103 train set has  ;test set has M 
# cat 105 train set has R S ;test set has  
# cat 106 train set has  ;test set has Q 
# cat 109 train set has BM CJ BV BY BT B BF BP J AG AK ;test set has AD 
# cat 110 train set has BK H BN DV EI BD BI AN AF CB EH ;test set has BH CA EN 
# cat 111 train set has D ;test set has L 
# cat 113 train set has BE T AC ;test set has AA R 
# cat 114 train set has X ;test set has  
# cat 116 train set has BI V BL X FS P GQ AY MF JD AH EV CC AB W AM IK AT JO AS JN BF DY IB EQ JT AP MB C IO DQ HO MT FO JI FN HU IX ;
#          test set has AQ EM FY AI N ET KO BJ IW DB LP MX BR BH JS ER A BN BE IS LS HS EX 

# dummy variables: cat vars over 53 levels
dm.astrain = model.matrix(loss ~., data = allstate.train[, -1])
head(dm.astrain)
dm.astest = model.matrix(~., data = allstate.test[, -1])

# drop near zero variables
prep = preProcess(dm.astrain, method = 'nzv')
dm.astrain = predict(prep, dm.astrain)
dim(dm.astrain) # [1] 188318   153
dm.astest = predict(prep, dm.astest)
dim(dm.astest)

# create train and test sets
set.seed(0)
train.index = createDataPartition(loss.log, times = 1, p = 0.8, list = F)

sub.dmtrain = dm.astrain[train.index, ]
sub.dmtest = dm.astrain[-train.index, ]
loss.train = loss.log[train.index]
loss.test = loss.log[-train.index]

# library(doMC)
# library(parallel)
# number_of_cores = detectCores()
# registerDoMC(cores = number_of_cores/2)

##### gradient boosting machines #####
# n.trees = 200
# interaction.depth = 6
# shrinkage = 0.5
tCtrl = trainControl(method = 'cv', number = 10, verboseIter = T, summaryFunction = defaultSummary)
gbmGrid = expand.grid(n.trees = seq(100, 300, 20), 
                      interaction.depth = 6, 
                      shrinkage = 0.1,
                      n.minobsinnode = 20)
gbmFit = train(x = sub.dmtrain, 
               y = loss.train,
               method = "gbm", 
               trControl = tCtrl,
               tuneGrid = gbmGrid,
               metric = 'RMSE',
               maximize = F)

plot(gbmFit)
plot(gbmFit, plotType = "level")
gbmImp = varImp(gbmFit, scale = F)
plot(gbmImp, top = 20)
mean(gbmFit$resample$RMSE)
predicted = predict(gbmFit, sub.dmtest)
postResample(pred = predicted, obs = loss.test)[1]
plot(x = loss.test, y = predict(gbmFit, sub.dmtest))


# modeling with all data
tCtrl = trainControl(method = 'cv', number = 10, verboseIter = T, summaryFunction = defaultSummary)
gbmGrid = expand.grid(n.trees = 140, 
                      interaction.depth = 6, 
                      shrinkage = 0.5,
                      n.minobsinnode = 20)
gbmFit = train(x = dm.astrain, 
               y = loss.log,
               method = "gbm", 
               trControl = tCtrl,
               tuneGrid = gbmGrid,
               metric = 'RMSE',
               maximize = F)


predicted.loss = predict(gbmFit, dm.astest)
predicted.eloss = exp(predicted.loss)
sample_submission$loss = predicted.eloss
write.csv(sample_submission, file = 'sample_submission.csv', row.names = F)


#########################################################################################################
##############################best tuning parameters#####################################################
#########################################################################################################

tCtrl = trainControl(method = 'cv', number = 5, verboseIter = T, summaryFunction = defaultSummary)
gbmGrid = expand.grid(n.trees = 1700,
                      interaction.depth = 6, # this is the best tree depth, no need to change
                      shrinkage = 0.1,
                      n.minobsinnode = 50)
gbmFit = train(x = sub.dmtrain, 
               y = loss.train,
               method = 'gbm', 
               trControl = tCtrl,
               tuneGrid = gbmGrid,
               metric = 'RMSE',
               maximize = F)
