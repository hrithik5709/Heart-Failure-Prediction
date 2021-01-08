if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
data <- read.csv("https://storage.googleapis.com/kagglesdsdata/datasets/727551/1263738/heart_failure_clinical_records_dataset.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20210108%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20210108T050902Z&X-Goog-Expires=259199&X-Goog-SignedHeaders=host&X-Goog-Signature=9ec3e505d4e54c638b27a01e64c98c3b6557c6cce3c4f62c52f8b60d8e131ef18e3f82b49909453d406ed7e4efcb3093e81aead5e89b3438135f15f3e6bd69f18f1136b1153c88137e855f026f739b9feb372589e71014746013d4bb4c8686f3ae457f32523f2f885a3a1d287e4b5e0ffe6844b5c882953a3fa74d16047790470a1112dbd70924d7d1f1c3e193808cf1ad0f82c67f0c175ba6111b2eb1e79aea973be629e6971db649674610db8344f39a6ddd14a2a91b1203ca771fd55b5723deca63a811f387b462eb7a9c58e82dfd86a30326a93f549e9cb8c68ceea89457761c5fa80473901aeea074ccf4c7dcfabe6a5390ce0369cda4a1eb7594c2d16d")
# Changing datatype mutating some columns into factors for future usage
features = c("anaemia", "diabetes", "high_blood_pressure", "sex", "smoking", "DEATH_EVENT")
data <- data %>%
  mutate_at(features, as.factor)
#Data Preprocessing
# split into test and train
set.seed(1,sample.kind = "Rounding")
test_index <- createDataPartition(y = data$DEATH_EVENT, times = 1, p = 0.5, list = FALSE) #making partition
test <- data[test_index,]
train <- data[-test_index,]
head(train) # lookup of the test data



#using knn for prediction
suppressWarnings(set.seed(1,sample.kind = "Rounding")) # use set.seed(1) for R versions prior R 3.5
knn_fit <- knn3(DEATH_EVENT ~ ., data = test)
x <- as.matrix(train[,0:12])
y=train$DEATH_EVENT
knn_fit <- knn3(x, y)
knn_fit <- knn3(DEATH_EVENT~ ., data = train, k=5)
y_hat_knn <- predict(knn_fit,test, type = "class")
result1<-confusionMatrix(data = y_hat_knn, reference = test$DEATH_EVENT)$overall["Accuracy"] # storing accuracy

#using logistic regresssion for prediction

lr <- glm(DEATH_EVENT ~ .,
          family=binomial(logit), data=train)
pred <- as.factor(predict(lr, newdata=test, type="response") >= 0.5) %>%
  fct_recode("0" = "FALSE", "1" = "TRUE") # making the prediction fit
result2<-confusionMatrix(pred, test$DEATH_EVENT, positive = "1")$overall["Accuracy"] # storing accuracy

# comparing accuracy
print("KNN:")
print(result1)

print("Logistic Regression")
print(result2)

print("Hence we conclude to use the logistic regression model for prediction")
