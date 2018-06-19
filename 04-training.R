library("data.table")
library("dplyr")

#df <- fread("tweets-processed.csv")
df <- fread("tweets.csv")
df$text <- NULL
df$account_lang <- as.factor(df$account_lang)
df$source <- as.factor(df$source)
df$popular <- as.factor(df$popular)

glimpse(df)

set.seed(unclass(Sys.time()))
idsTrain <- sample(nrow(df), round(nrow(df) * .8), replace = FALSE)
df.train <- df[idsTrain, ]
df.test <- df[-idsTrain, ]


library(mlbench)
library(caret)
#library(e1071)


#The example below loads the Pima Indians Diabetes dataset and constructs an Learning Vector Quantization (LVQ) model
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(popular ~ ., data=df.train, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

dfVariables <- importance$importance
dfVariables$variable <- rownames(dfVariables)
dfVariables$no <- NULL
dfVariables$importance <- dfVariables$si
dfVariables$si <- NULL
rownames(dfVariables) <- NULL
dfVariables <- dfVariables[order(-dfVariables$importance),] #ordeno por mayor importancia



################

control <- trainControl(method="cv")
indepVars <- ""
bestModel <- NA
bestAuc <- 0

for(i in 1:nrow(dfVariables)) {
  row <- dfVariables[i,]
  indepVars <- paste(indepVars, row$variable , sep = "+")
  print(indepVars)
  
  model <- train(as.formula(paste0("popular ~ ", indepVars)), data=df.train, method="rpart", trControl=control)
  print(model)
  
  predicts <- predict(model, df.test)
  auc <- mean(df.test$popular == predicts)
  print(auc)
  
  if(bestAuc == 0){
    bestAuc <- auc
    bestModel <- model
  } else if(auc > bestAuc){
    bestAuc <- auc
    bestModel <- model
    break
  }
}

library(rpart.plot)
rpart.plot(bestModel$finalModel)
