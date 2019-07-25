getwd()  

setwd("\\\\WIL-HOMEDRIVE01\\D17911$\\Desktop\\Case Study")
library(xlsx)
commercial = read.csv('commercial.csv', header = TRUE, sep=',')
## Correlation
library(dplyr)
commercial_all = cbind(commercial, dd)
commercial_cor = select(commercial_all, dmsold, dmprecond, dmpostcond,  smiles,  MMR, Floor_price_to_MMR, Vol, Vel, Date)
corrr = cor(commercial_cor, use = "complete.obs")
corrr
library('Hmisc')
mydata.rcorr = rcorr(as.matrix(commercial_cor))



## Test set
commercial_test = sample_n(commercial, 6000)
commercial_test2 = select(commercial_test, dmsold, Floor_price_to_MMR, auctioncity, Date, dmpostcond, smiles, color, Vol, Vel )
table(commercial_test2$dmsold)
## Training set
commercial_train = commercial[!(commercial$sser17 %in% commercial_test$sser17), ]
commercial_sold = filter(commercial_train, dmsold == 1)
nrow(commercial_sold)
commercial_unsold = filter(commercial_train, dmsold == 0)
nrow(commercial_unsold)
commercial_train2 = select(commercial_train, dmsold, Floor_price_to_MMR, auctioncity, Date, dmpostcond, smiles, color, Vol, Vel )

library(rpart)
library(ROSE)
data_balanced_over <- ovun.sample(dmsold ~ ., data = commercial_train2, method = "over",N = 40000)$data
table(data_balanced_over$dmsold)

tree.rose <- rpart(dmsold ~ ., data = commercial_train2)
tree.over <- rpart(dmsold ~ ., data = data_balanced_over)
pred.tree.rose <- predict(tree.rose, newdata = commercial_test2)
pred.tree.over <- predict(tree.over, newdata = commercial_test2)


roc.curve(commercial_test2$dmsold, pred.tree.rose)

evaluate = cbind(commercial_test2, pred.tree.over)
evaluate = mutate(evaluate, predict = if_else(pred.tree.over >= 0.4, 1, 0))
table(evaluate$predict, evaluate$dmsold)


library(ggplot2)
library(rlang)
library(recipes)
library(caret)
set.seed(42)
model_rf <- caret::train(dmsold ~ .,
                         data = commercial_train2,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  number = 10, 
                                                  repeats = 10, 
                                                  verboseIter = FALSE))
