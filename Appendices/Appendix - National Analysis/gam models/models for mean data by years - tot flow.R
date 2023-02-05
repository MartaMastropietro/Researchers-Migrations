library(readr)
library(readxl)
library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(mgcViz)

library(ISLR)
library(mgcv)
library(voxel)
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(ggplotify)

 
my_summary<-function(mod,data){
  rsq<-as.numeric(cor(mod$fitted.values,data[as.character(mod$formula)[2]]))
  adj_rsq<-summary(mod)$r.sq
  aic<-AIC(mod)
  
  return(c(rsq=rsq,adj_rsq=adj_rsq,aic=aic))
}


setwd("D:/Marta/Politecnico/Tesi/National data and analysis")


data <- read_csv("data_by_country.csv")
data$year<-as.factor(data$year)
data$country<-as.factor(data$country)
data$tot_flow<-data$flow_in+data$flow_out
data$log_flow<-log(data$tot_flow)
data$uni_score<-data$r3score

cor(data$flow_in,data$flow_out) #0.99, models for flow in and out are almost the same, we can use total 

#models by year 
mod1<-gam(tot_flow~s(ted,bs='cr')+s(uni_score,bs='cr')+s(log(gdp),bs='cr')+s(edu_index,bs='cr')+s(year,bs='re'),data=data)
print(plot(getViz(mod1), allTerms = T), pages = 1) #quite bad model
my_summary(mod1,data) #0.7849553    0.5719289

#try log tot flow 
mod2<-gam(log_flow~s(ted,bs='cr')+s(uni_score,bs='cr')+s(log(gdp),bs='cr')+s(edu_index,bs='cr')+s(year,bs='re'),data=data)
print(plot(getViz(mod2), allTerms = T), pages = 1)
my_summary(mod2,data) #0.9187389   0.821329   -> overfit 


#model reduction, erase gdp
mod3<-gam(log_flow~s(ted,bs="cr")+s(uni_score,bs='cr')+s(edu_index,bs='cr')+s(year,bs='re'),data=data)
print(plot(getViz(mod3), allTerms = T), pages = 1)
my_summary(mod3,data) #0.8761900    0.7532703 1061.1309572 
summary(mod3) 

aov23<-anova_np(mod2,mod3,data) # 0.59


#model reduction, erase gdp, edu
mod4<-gam(log_flow~s(ted,bs="cr")+s(uni_score,bs='cr')+s(year,bs='re'),data=data)
print(plot(getViz(mod4), allTerms = T), pages = 1)
my_summary(mod4,data) #0.8688558    0.7425798 1073.1176807 
summary(mod4) 

aov24<-anova_np(mod2,mod4,data) # 0.59

mod5<-gam(log_flow~s(ted,bs="cr",k=4)+s(uni_score,bs='cr')+s(year,bs='re'),data=data)
print(plot(getViz(mod5), allTerms = T), pages = 1)
my_summary(mod5,data) # 0.8688558    0.7425798 1073.1176807 
x11()
plot(log(data[which(data$year=="2011"),]$ted),data[which(data$year=="2011"),]$log_flow)

#it looks like year by year the behavior is quite similar 

mod6<-gam(log_flow~s(uni_score,bs='cr')+s(year,bs='re'),data=data)
aov26<-anova_np(mod2,mod6,data) # 0.002
aov56<-anova_np(mod5,mod6,data) #0.005
aov25<-anova_np(mod2,mod5,data) #0.2 -> model 5 is final model

#plot carini

vars <- c("uni_score", "ted")
map(vars, function(x){
  p <- plotGAM(mod5, smooth.cov = x) 
  g <- ggplotGrob(p)}) %>% {grid.arrange(grobs = (.), ncol = 2, nrow = 1)}
    

###################################################################

#we try to erase dependence on years 
#we did anovas on years for in and out, we can combine the two and try model for mean over years

#pca on years

data_pca<-data.frame()
for (c in unique(data$country)){
  flow_vec<-data$tot_flow[which(data$country==c)]
  year_vec<-data$year[which(data$country==c)]
  flow_vec<-flow_vec[order(year_vec,decreasing = F)]
  newrow<-c(c)
  newrow<-append(newrow,flow_vec)
  data_pca=rbind(data_pca,newrow)
}
colnames(data_pca)<-c("country","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")
data_pca$`2009`<-as.numeric(data_pca$`2009`)
data_pca$`2010`<-as.numeric(data_pca$`2010`)
data_pca$`2011`<-as.numeric(data_pca$`2011`)
data_pca$`2012`<-as.numeric(data_pca$`2012`)
data_pca$`2013`<-as.numeric(data_pca$`2013`)
data_pca$`2014`<-as.numeric(data_pca$`2014`)
data_pca$`2015`<-as.numeric(data_pca$`2015`)
data_pca$`2016`<-as.numeric(data_pca$`2016`)
data_pca$`2017`<-as.numeric(data_pca$`2017`)
data_pca$`2018`<-as.numeric(data_pca$`2018`)
data_pca$`2019`<-as.numeric(data_pca$`2019`)
data_pca$`2020`<-as.numeric(data_pca$`2020`)

#data 
p<-princomp(data_pca[,-1],scores=T)
summary(p) 
p$loadings #first comp explains 0.78 of total var, it is a mean -> we can use a mean of data over years 


#normalize data 
data_pca_norm<-data_pca
data_pca_norm[,-1]<-scale(data_pca_norm[,-1])
p<-princomp(data_pca_norm[,-1],scores=T)
summary(p) 
p$loadings #first comp explains 0.78 of total var, it is a mean -> we can use a mean of data over years 

# we use mean of mean and std to bring data almost back to original scale 
m<-colMeans(data_pca[,-1])
std<-apply(data_pca[,-1],2,sd)
mean_std<-mean(std)
mean_m<-mean(m)

data_sum<-data.frame(data_pca_norm[,1])
data_sum$flow_mean<-apply(data_pca_norm[,-1],1,mean) #flow in

data_sum$flow_mean<-data_sum$flow_mean*mean_std+mean_m


#####

# construction of dataset aggregated by countries, mean on years

data_mean<-data.frame(data_sum)
colnames(data_mean)<-c("country","flow_mean")

micro_ted<-data[,1:3]
micro_ted<-aggregate(ted~country,micro_ted,mean)

micro_score<-data[,c(1,2,4)]
micro_score<-aggregate(score~country,micro_score,mean)

micro_gdp<-data[,c(1,2,7)]
micro_gdp<-aggregate(gdp~country,micro_gdp,mean)

micro_edu<-data[,c(1,2,8)]
micro_edu<-aggregate(edu_index~country,micro_edu,mean)

data_mean<-merge(data_mean,micro_ted)
data_mean<-merge(data_mean,micro_score)
data_mean<-merge(data_mean,micro_gdp)
data_mean<-merge(data_mean,micro_edu)

data_mean$r3score<-(data_mean$score)^(1/3)
data_mean$log_flow<-log(data_mean$flow_mean)
#models 

#complete linear
mod7<-gam(log_flow~ted+r3score+edu_index+log(gdp),data=data_mean)
print(plot(getViz(mod7), allTerms = T), pages = 1) #
my_summary(mod7,data_mean) #  0.9249781  0.8333667
shapiro.test(mod7$residuals) #normal
summary(mod7) #only ted , score look significant 


mod8<-gam(log_flow~ted+r3score,data=data_mean)
print(plot(getViz(mod8), allTerms = T), pages = 1) #
my_summary(mod8,data_mean) # 0.9248250  0.8449657
shapiro.test(mod8$residuals) #normal
summary(mod8) # both significant

anova(mod8,mod7,test="F") #we dont reject, no evidence to say models are different -> we can keep small 

mod8_sm<-gam(log_flow~ted+s(r3score,bs="cr",k=6),data=data_mean)
print(plot(getViz(mod8_sm), allTerms = T), pages = 1) #
my_summary(mod8_sm,data_mean) #0.9416136  0.8737424
shapiro.test(mod8_sm$residuals) #normal ?
summary(mod8_sm) # both significant

anova(mod8,mod8_sm,test="F") #0.02 we can say both

plot(data_mean$ted,data_mean$log_flow)
plot(data_mean$r3score,data_mean$log_flow)

#plot 
score_grid=seq(range(data_mean$r3score)[1],range(data_mean$r3score)[2],length.out = 100)
ted_grid=seq(range(data_mean$ted)[1],range(data_mean$ted)[2],length.out = 100)
grid=expand.grid(score_grid,ted_grid)
names(grid)=c('r3score','ted')
pred=predict(mod8_sm,newdata=grid) 
pred
persp3d(score_grid,ted_grid,pred,col='pink',border="black",lwd=0.3)
with(data_mean, points3d(r3score,ted,log_flow,col='black',size=5))


