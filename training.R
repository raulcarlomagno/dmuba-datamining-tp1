library("data.table")
library("dplyr")

df <- fread("tweets-processed.csv")
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

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)
