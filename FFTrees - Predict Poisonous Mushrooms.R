library('datasets')
library('rpart')
library('rpart.plot')
library('randomForest')
library('dplyr')
library('ggplot2')
library('FFTrees')

mushrooms

rows <- sample(nrow(mushrooms), nrow(mushrooms) * 0.7, replace=FALSE)
train <- mushrooms[rows, ]
test <- mushrooms[-rows, ]

model <- FFTrees(poisonous ~ . , train)
summary(model)
model

plot(model)
plot(model, what = 'cues')

pred <- predict(model, test)
table(pred, test$poisonous)
