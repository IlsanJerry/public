## iris data
library(class)
library(e1071)
library(gmodels)

data("iris")

## 자료 탐색
tail(iris)
summary(iris)
pairs(iris[1:4],
      pch=21,
      bg = c("red","green","blue")
      [unclass(iris$Species)])

## 표준화
iris <- cbind(as.data.frame(scale(iris[1:4])), iris$Species)

## train vs test (5:5)
set.seed(1)
indexes = sample(1:nrow(iris), size = 0.5*nrow(iris))
train = iris[indexes,]
test = iris[-indexes,]

## label
train_labels <- train[,5]
test_labels <- test[,5]
train <- train[,-5]
test <- test[,-5]

## test sample 확인
table(test_labels)

## 교차검증으로 best k 찾기
set.seed(1)
knn.cross <- tune.knn(x = train, y = train_labels, 
                      k = 1:20,
                      tunecontrol = tune.control(sampling =
                                                   "cross"),
                      cross=10)
summary(knn.cross)

## best k plot
plot(knn.cross)

## k-NN (k = 1, 3)
kNNiris1 = knn(train, test, train_labels, k=1)
kNNiris3 = knn(train, test, train_labels, k=3)

## 결과
table(test_labels, kNNiris1)
table(test_labels, kNNiris3)

########################################################

## Pitch Type data
PitchSample <- read.csv(file.choose(),
                        header = TRUE,
                        stringsAsFactors = FALSE)
X <- PitchSample
X$pitch_type <- factor(X$pitch_type,
                       levels = c("FF", "SL", "CH"))

str(X)
## 자료 탐색
tail(X[,1:5])
summary(X[,1:4])
pairs(X[6:9],
      pch = 21,
      cex = 0.7,
      bg = c("red","green","blue")
      [unclass(X$pitch_type)])

## 표준화
X <- cbind(X$pitch_type, as.data.frame(scale(X[2:21])))

## train vs test (7:3)
set.seed(1)
indexes = sample(1:nrow(X), size = 0.7*nrow(X))
train = X[indexes,]
test = X[-indexes,]

## label
train_labels <- train[,1]
test_labels <- test[,1]
train <- train [,-1]
test <- test[,-1]

## test sample 확인
table(test_labels)

## 교차검증으로 best k 찾기
set.seed(1)
knn.cross <- tune.knn(x = train, y = train_labels, 
                      k = 1:20,
                      tunecontrol = tune.control(sampling =
                                                   "cross"),
                      cross=10)
summary(knn.cross)

## best k plot
plot(knn.cross)

## k-NN (k = 1, 12)
set.seed(1)
kNNpitch1 = knn(train, test, train_labels, k=1)
set.seed(1)
kNNpitch12 = knn(train, test, train_labels, k=12)

## 결과
table(test_labels, kNNpitch1)
table(test_labels, kNNpitch12)

