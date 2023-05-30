train <- read.csv("/Users/sonmingi/Desktop/강남대학교/데이터분석응용통계/open/train.csv")
library(lattice)
library(ggplot2)
library(caret) #createDataPartition train 0.7 / test 0.3

set.seed(1234)
index <- createDataPartition(train$TARGET, p=0.7,list=FALSE)
train_set <- train[index, ]
test_set <- train[-index, ]
dim(train_set)
dim(test_set)
head(train_set)
head(test_set)
str(train_set) #train df 를 훈련과 test를 위해 70:30 비율로 나눔

# 범죄 발생지 
sort(unique(train$범죄발생지)) # 14개의 nunique 값 존재

#데이터 전처리
summary(train) # 데이터 크기, 단위 다 다름. scale 할 꺼임.
train_set <- train_set[, -1] #target범주만 제외 하고 ID 제거
test_set <- test_set[, -1]
head(train_set)
head(test_set)

# 월 : 1 ~ 일 : 7 해석의 편의성을 위해 변경
# "공원" "백화점" "병원" "식당" "약국" "은행" "인도" "주거지" "주유소" "주차장" "차도" "편의점" "학교" "호텔/모텔"
train_set$요일<- as.numeric(factor(train_set$요일, levels = c('월요일','화요일','수요일','목요일','금요일','토요일','일요일')))
train_set$범죄발생지<- as.numeric(factor(train_set$범죄발생지, levels = sort(unique(train_set$범죄발생지))))
str(train_set)
test_set$요일<- as.numeric(factor(test_set$요일, levels = c('월요일','화요일','수요일','목요일','금요일','토요일','일요일')))
test_set$범죄발생지<- as.numeric(factor(test_set$범죄발생지, levels = sort(unique(test_set$범죄발생지))))
str(test_set)

train_set[1:18] <- scale(train_set[, 1:18], center = TRUE, scale = TRUE)
test_set[1:18] <- scale(test_set[,1:18],center =TRUE, scale = TRUE)
head(train_set)
head(test_set)
library(class)
knn_pre <- knn(train_set[,-19],test_set[,-19],train_set[,19],k=15,prob = TRUE)
knn_pre

#unique(train_set$TARGET)
#unique(test_set$TARGET)
# unique는 맞지만 level의 길이가 달라 level 길이 맞춰줌

# Find unique levels in both datasets
all_levels <- unique(c(train_set[, 19], test_set[, 19]))

# Update factor levels in both datasets
train_set[, 19] <- factor(train_set[, 19], levels = all_levels)
test_set[, 19] <- factor(test_set[, 19], levels = all_levels)
unique(train_set$TARGET)
unique(test_set$TARGET)

set.seed(1234)
res_acc <- numeric(30)
for(k in (1:30)){
  knn_pre <- knn(train_set[,-19],test_set[,-19],train_set[,19],k=k,prob = TRUE)
  res<- confusionMatrix(knn_pre, test_set[,19])
  res_acc[k] <- 1 - res$overall[1]
}

x<- 1:length(res_acc)
plot(x, res_acc, pch=10, col = 'blue', type = 'o', lwd = 2, 
     main = "Find the Optimal k") 
grid() #28번째 k가 가장 좋음.

knn_pre <- knn(train_set[,-19],test_set[,-19],train_set[,19],k=2,prob = TRUE)
res <- confusionMatrix(knn_pre, test_set[,19])
res
