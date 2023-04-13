aks<-read.csv('/Users/sonmingi/Desktop/강남대학교/데이터분석응용통계/data/빈집털이조사.csv',
                 header=TRUE,stringsAsFactor = TRUE,sep=",",fileEncoding = 'euc-kr')
head(aks)

#범주형변수 변환
aks$빈집털이01 <- ifelse(aks$빈집털이 == "있음", 1, 0)
aks$보안01 <- ifelse(aks$보안 == "가입",1,0)
aks$경비견01<-ifelse(aks$경비견=="있음",1,0)
head(aks)


aks2<-read.csv('/Users/sonmingi/Desktop/강남대학교/데이터분석응용통계/data/빈집털이조사.csv',
              header=TRUE,stringsAsFactor = TRUE,sep=",",fileEncoding = 'euc-kr')
aks2$빈집털이 <- ifelse(aks2$빈집털이 == "있음", 1, 0)
aks2$보안 <- ifelse(aks2$보안 == "가입",1,0)
aks2$경비견<-ifelse(aks2$경비견=="있음",1,0)
head(aks2)

#glm()실행 로지스틱회귀분석
aks.model<-glm(formula = 빈집털이01 ~ 부재시간+대화+건축년수+보안01+경비견01,family = binomial,data=aks)
summary(aks.model)

#exp() 계수 변환
exp(aks.model$coefficients)

#적합도 검정
#귀무가설 채택 : 적합하다. p값 < 0.05이면 귀무가설 기각, 즉 적합하지 않다.
install.packages('ResourceSelection')
library(ResourceSelection)
library(tidyverse)
hoslem.test(x=aks.model$y,y=fitted(aks.model))

#AIC BIC
extractAIC(aks.model)
extractAIC(aks.model, k = log(nrow(aks.model$data)))

#상대적 평가
aks.out_null<-glm(빈집털이01~1,family = binomial,data=aks)
anova(aks.out_null, aks.model, test = "Chisq")

#AIC BIC 회귀모델 독립변수 마다 반복해서 AIC 가장 작은 모델 표현
step(aks.out_null, direction = "both",
     scope = (~부재시간+대화+건축년수+보안01+경비견01))

#다중공선성 확인
library(carData)
library(car)
vif(aks.model)
