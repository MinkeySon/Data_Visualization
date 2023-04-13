data<-read.csv('/Users/sonmingi/Desktop/강남대학교/데이터분석응용통계/data/과목시험결과.csv')
head(data)
attach(data)
library(tidyverse)
restest2<-lm(final~t1+t3+t4+t5)
summary(restest2)
vif(restest2)

restest3<-lm(final~t1+t2)
summary(restest3)

#AIC 산출
extractAIC(restest2)
extractAIC(restest3)
# -> AIC 관점으로 봤을 때 restest2 수치가 더 낮아서 더 적합함.

#cor() 함수 사용하여 편회귀계수 분석
res3<-restest3$coefficients
res3
test_data <- data.frame(t1,t2,t3,t4,t5)
cor(test_data)
#상관관계가 다 존재함
#중복되는 부분 4군데를 찾아야함.

custom_num1 <- res3[1]+res3[2]*1+res3[3]*1
custom_num1

custom_num2<-res3[1]+ res3[2]*2 + res3[3]*1
custom_num2

custom_num3<-res3[1]+ res3[2]*1 + res3[3]*2
custom_num3

custom_num3-custom_num2
