data<-read.csv('/Users/sonmingi/Desktop/강남대학교/데이터분석응용통계/data/과목시험결과.csv')
head(data)
attach(data)

# 1.다중회귀분석실시
restest<-lm(final~t1+t2+t3+t4)
summary(restest)
# 변수별 추정값 : t1, t3, t4는 0.05 미만으로 회귀계수가 유의하지만 t2의 경우 유의하지 않다.
# Adjusted R-squared:  0.7785 로 77.85%의 설명력을 가짐.
# p-value: < 2.2e-16 : p 값이 0.05 미만으로 모델이 유의함.

vif(restest)
# 다중공선성이 t1:1.435608 / t2:57.641912 / t3:1.722265 / t4 : 57.965530 
# t2와 t4가 독립성이 없음
cor(t2,t4)
#실제로 강한 상관관계가 있음

restest2<-lm(final~t1+t3+t4)
summary(restest2)
# 변수별 추정값 : 모든 회귀계수가 0.05미만으로 유의하다.
# Adjusted R-squared:  0.7785 로 77.94%의 설명력을 가짐.
# p-value: < 2.2e-16 : p 값이 0.05 미만으로 모델이 유의함.
vif(restest2)
#다중공선성이 t1:1.433091 / t2:1.720069 / t4:1.418363 로 독립변수간에 독립성이 높음