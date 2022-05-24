library('neuralnet')
library('tidyverse')
library('caret')
library('rpart')
library('rpart.plot')

?diamonds
# diamonds dataset is the price, carat, clarity and other features of about 54,000 diamonds.

data('diamonds')
View(diamonds)

# filter only the diamonds whose cut is good or ideal
df <- diamonds %>%
  filter(cut %in%c ('Good', 'Ideal'))

dim(df)
# df has 26,447 rows. Let's take only a sample of 5,000 diamonds

df <- sample_n(df, 5000)

# create a new variable that labels the diamonds whose cut is ideal as 1 and others as 0
df$binary <- ifelse(df$cut == 'Ideal', 1, 0)

# note that df has also additional binary column now. binary will be our classification variable.
df

# change the type of binary to factor
df$binary <- as.factor(df$binary)

# drop the cut columns as we would not want out algorithm to take cut column into consideration
# for a sound random forest model
df <- df[, -2]

# check for null values
sum(is.na(df))       

# create train and test set
rows <- createDataPartition(y=df$binary, p= .7, list=F, times=1)

train <- df[rows, ]
test <- df[-rows, ]

# display the number of rows for the train and the test datasets
nrow(train)
nrow(test)

# build a k-fold cross-validation 
control <- trainControl(method='repeatedcv', number=2, repeats=3) # number here is the number of folds

# build the model
model <- train(binary ~ ., data=train, method='ranger', trControl=control) # ranger is the random forest method

# we can see that the model has 7 mtry and 98% accuracy and kappa value 0.94
model

# implement our model on a sample from the test set
random_rows <- sample(x=1:1499, size=10)

predict(model, newdata = test[random_rows, ])
test[random_rows, 8]
# the model accurately predicted the cut of all 10 diamonds from the sample set!

# predict the cut values of the test set
predict <- predict(model, newdata=test)

# display the confusion matrix
confusionMatrix(predict, test$binary)
# we have seen that the model has 256 true negative and 1227 true positive estimates

plot(model)
