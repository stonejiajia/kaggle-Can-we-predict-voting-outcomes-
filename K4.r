library(Matrix)
library(xgboost)
getwd()

setwd("/Users/stone20091652/GitHub/Data Science/kaggle Can we predict voting outcomes")
train = read.csv("train2.csv")

test = read.csv("test2.csv")


library(caTools)

set.seed(88)

spl = sample.split(train, SplitRatio = 0.7)

Train = subset(train, spl==TRUE)

Test = subset(train, spl==FALSE)

train.mx <- sparse.model.matrix(Party ~ .-USER_ID, data = Train)

test.mx <- sparse.model.matrix(Party ~ .-USER_ID, data = Test)

str(sparse.model.matrix)

dtrain = xgb.DMatrix(train.mx, label=Train$Party)

dtest<-xgb.DMatrix(test.mx, label=Test$Party)

train.gdbt = xgb.train(params=list(objective="multi:softmax", num_class=10, eval_metric="mlogloss", eta=0.2, max_depth=5, subsample=1, colsample_bytree=0.5), data=dtrain, nrounds=150, watchlist=list(eval=dtest, train=dtrain))

pred = predict(train.gdbt,newdata=dtest)

sum(diag(table(Test$Party,pred)))/nrow(Test)

pred

PredTest = predict(train.gdbt, newdata=test)


test2.mx <- sparse.model.matrix(USER_ID~., data = test)

pred = predict(train.gdbt,newdata=dtest)
