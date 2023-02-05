
library(readr)
library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(mgcViz)


my_summary<-function(mod,data){
  rsq<-as.numeric(cor(mod$fitted.values,data[as.character(mod$formula)[2]]))
  adj_rsq<-summary(mod)$r.sq
  aic<-AIC(mod)
  
  return(c(rsq=rsq,adj_rsq=adj_rsq,aic=aic))
}


### DATA IMPORT ###
#data_trips_gdp_score_ted_edu <- ...

data2<-data_trips_gdp_score_ted_edu

data2$country<-gsub('.{2}$','',data2$nuts2)
data2$year<-as.factor(data2$year)
data2$country<-as.factor(data2$country)
data2$tot_flow<-data2$flow_in+data2$flow_out
data2$log_flow<-log(data2$tot_flow+1)
data2$uni_score<-(data2$score)^(1/3)


#nuts2
mod2s<-gam(log_flow~s(ted,bs='cr',k=6)+s(uni_score,bs='cr')+s(edu_index,bs='cr',k=5)+s(country,bs="re")+s(year,bs='re'),data=data2)
mod2b<-gam(log_flow~s(ted,bs='cr',k=6,by=year)+s(uni_score,bs='cr',by=year)+s(edu_index,bs='cr',by=year,k=5)+s(country,bs="re")+s(year,bs='re'),data=data2)

a2<-anova_np(mod2b,mod2s,data2) #1

