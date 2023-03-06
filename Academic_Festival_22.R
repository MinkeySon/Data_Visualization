library(tidyverse)
dat<-read.csv("land_data.csv",header=T)
dat<-as_tibble(dat)
library(dbplyr)

#tibble 로 변환

#지목 1(전),2(목)
dat1<-dat %>% filter(JIMOK==1 | JIMOK==2)
head(dat1)



#다중 회귀분석 시작(model) e = exponent
model<-lm(PNILP~JIMOK+PAREA+GEO_HL+ROAD_SIDE,data=dat1)
summary(model)
#추정회귀식
#PNILP(공시지가) = -1.772e+06 + -1.435e+05*JIMOK + -1.985e+02*PAREA 
# + 3.868e+05*GEO_HL + -1.235e+05*ROAD_SIDE
plot(model)
install.packages('car')
library(car)
vif(model)
result = relweights

#가중치 plot
relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar<-ncol(R)
  rxx<-R[2:nvar,2:nvar]
  rxy<-R[2:nvar,1]
  svd<-eigen(rxx)
  evec<-svd$vectors
  ev<-svd$values
  delta<-diag(sqrt(ev))
  lambda<-evec%*%delta %*% t(evec)
  lambdasq<-lambda^2
  beta<-solve(lambda)%*% rxy
  rsquare<-colSums(beta^2)
  rawwgt<-lambdasq%*%beta^2
  import<-(rawwgt/rsquare)*100
  import<-as.data.frame(import)
  row.names(import)<-names(fit$model[2:nvar])
  names(import)<-'Weights'
  import<-import[order(import),1,drop=FALSE]
  dotchart(import$Weights,labels=row.names(import),
    xlab='% of R-Square',pch=19,
    main = 'Relative Importance of Predictor Variables',
    sub = paste('Total R-Square=',round(rsquare,digits=3)),
    ...)
  return(import)
}
Weights <- relweights(model,col='blue')

#가중치 시각화
library(ggplot2)
plotRelWeights=function(fit){
  data<-relweights(fit)
  data$Predictors<-rownames(data)
  p<-ggplot(data=data,aes(x=reorder(Predictors,Weights),y=Weights,fill=Predictors))+
    geom_bar(stat="identity",width=0.5)+
    ggtitle("Relative Importance of Predictor Variables")+
    ylab(paste0("% of R-square \n(Total R-Square=",attr(data,"R-square"),")"))+
    geom_text(aes(y=Weights-0.1,label=paste(round(Weights,1),"%")),hjust=1)+
    guides(fill=FALSE)+
    coord_flip()
  p
}
plotRelWeights(model)





























