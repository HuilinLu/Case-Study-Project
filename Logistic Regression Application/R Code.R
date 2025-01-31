## Question 1~2
test = read.csv(file = 'test.csv')
train = read.csv(file = 'train.csv')
final = read.csv(file = 'final.csv')
head(test)
head(train)
head(final)
##  logistic regression model that includes all the attributes
library(dplyr)
train2 = mutate(train, wine = if_else(v12 >= 6, 1, 0)) %>% select(-v12)
test2 = mutate(test, wine = if_else(v12 >= 6, 1, 0)) %>% select(-v12)
glm.logit = glm(wine~., data=train2, family = binomial)
summary(glm.logit)
glm.probs = predict(glm.logit, newdata = test2, type = 'response')
glm.pred = ifelse(glm.probs >= 0.5, 1, 0)
table(glm.pred, test2$wine)   ## Confusion table

## Question 3
glm.logit2 = glm(wine~v1 + v2 + v3 + v4 + v5, data=train2, family = binomial)
summary(glm.logit2)
glm.probs2 = predict(glm.logit2, newdata = test2, type = 'response')
glm.pred2 = ifelse(glm.probs2 >= 0.5, 1, 0)
table(glm.pred2, test2$wine) 

## Question 4: ROC Curve
library(pROC)
library(ggplot2)
library(e1071)
roc1 = roc(test2$wine~glm.pred)
roc1
roc2 = roc(test2$wine~glm.pred2)
roc2
plot(roc1)
plot(roc2)

## Question 6 Linear regression model
reg1 = lm(v12~., data=train)
summary(reg1)
mse1 = mean(reg1$residuals^2)

## Question 7: Confusion table
pred.lm = predict(reg1, test)
pred.lm2 = if_else(pred.lm>=6, 1, 0)
table(pred.lm2, test2$wine)


### Extra point
final.logit = glm(v10~., data=final, family = binomial)
summary(final.logit)
