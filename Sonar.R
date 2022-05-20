library('tree')
library('caret')
library('rpart')
library('rpart.plot')
library('mlbench')


# Sonar is the dataset used by Gorman and Sejnowski to discriminate rocks and mines, respectively denoted by R and M in the dataset, 
# by the sonar signals bounced off the corresponding elements. Each row consists of 60 numbers ranging from 0 to 1 that represents the
# energy within a particular frequency band.

data('Sonar')
View(Sonar)

head(Sonar)


# How frequently each class appears in the dataset?
table(Sonar$Class)

# Check for null values
sum(is.na(Sonar))
# Thankfully there is no missing values in the dataset.

# Get the summary of the dataset
summary(Sonar)

# Check the number of rows and columns
nrow(Sonar)
ncol(Sonar)
# the dataset has 208 rows and 61 columns, as expected

# Set the seed to 5 to randomly select some features from the dataset
set.seed(5)

# Randomly choose 16 features from the dataset
columns <- sample(x=1:60, size=16)
columns

pre_var <- Sonar[, columns]
pre_var


# display the boxplot of the selected features
par(mfrow = c(4,4))
for (i in 1:ncol(pre_var)) {
  boxplot(pre_var[, i], xlab=names(pre_var[i]),
          main=paste('Boxplot of', names(pre_var[i])),
          horizontal=TRUE, col='steelblue')
}


# Create train and test sets

set.seed(31)

index = createDataPartition(y=Sonar$Class, p=0.8, list=FALSE)

train_set <- Sonar[index, ]
test_set <- Sonar[-index, ]

# check the dimension of the train and the test set
dim(train_set)
dim(test_set)

# build the model
model <- tree(
  formula= Class ~., 
  data=train_set
 )

summary(model)

# plot the model

plot(model)
text(model)

# fit the model using the rpart function

model_2 <- rpart(Class ~ .,  data=Sonar, method='class')

model_2
summary(model_2)

# visualize the model_2
rpart.plot(model_2)


# Evaluating our model

random_rows = sample(x= 1:41, size=10) # Since the test set has 41 rows in total

test_set[random_rows, ]

predict(model_2, newdata=test_set[random_rows, ])
test_set[random_rows, 61]


# predict the whole test for the rpart model

prediction_2 <- predict(model_2, newdata=test_set, type='class')

# display the confusion matrix
table(x=prediction_2, y=test_set$Class)

# the confusion matrix tells us that the tree model predicted 34 of 41 classes correctly

# plot the table of predictions

confusionMatrix(data=prediction_2, reference=test_set$Class)

# We have seen that our model has 90% accuracy with Kappa value equal to 0.8 which can be considered as substantial.
# We have sensitivity value 0.81 meaning the 80% of True class (M) predictly corrected, and 1.0 Specificity means all negative classes correctly predicted

# Calculate the F-1 score
# F-1 = 2* (recall * precision) / (recall * precision)
F_1 <- 2 * (0.8182 * 1) / ( 0.8182 + 1)
F_1

# Calculate the prediction error rate
error <- round(mean(prediction_2 != test_set$Class), 2)
# we have 0.1 error value

## -- END --
