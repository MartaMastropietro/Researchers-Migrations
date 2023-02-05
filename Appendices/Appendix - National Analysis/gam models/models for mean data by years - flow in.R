
library(readr)
library(readxl)
library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)


##### 
#dataset construction / import 

# setwd("D:/Marta/Politecnico/Tesi/Analisi nazionali")
# trips_by_country <- read_csv("trips_by_country.csv")
# uniscore_by_country <- read_csv("uniscore_by_country.csv")
# log_ted_by_country <- read_csv("log(ted)_by_country.csv")
# gdp_pc_by_country <- read_excel("gdp_pc_by_country.xlsx")
# gdp_pc_by_country<-gdp_pc_by_country[,-1]
# colnames(gdp_pc_by_country)<-c("country","year","gdp")
# 
# gdp_pc_by_country$country[which(gdp_pc_by_country$country=="GB")]<-"UK"
# gdp_pc_by_country$country[which(gdp_pc_by_country$country=="GR")]<-"EL"
# gdp_pc_by_country$country[which(gdp_pc_by_country$country=="HZ")]<-"HR"
# 
# 
# edu_index_by_country_imputed <- read_csv("edu_index_by_country imputed.csv")
# edu_index_by_country_imputed<-edu_index_by_country_imputed[,-3]
# colnames(edu_index_by_country_imputed)<-c("country","year","edu_index")
# 
# edu_index_by_country_imputed$country[which(edu_index_by_country_imputed$country=="GB")]<-"UK"
# edu_index_by_country_imputed$country[which(edu_index_by_country_imputed$country=="GR")]<-"EL"
# 
# 
# data<-merge(log_ted_by_country,uniscore_by_country)
# data<-merge(data,trips_by_country)
# data<-merge(data,gdp_pc_by_country)
# data<-merge(data,edu_index_by_country_imputed)
# data$r3score<-(data$score)^(1/3)
# 
# data$year<-as.factor(data$year)
# data$country<-as.factor(data$country)
# colnames(data)<-c("year","country","ted","score","flow_in","flow_out","gdp","edu_index","r3score")
# 
# write.csv(data,"data_by_country.csv",row.names = F)

setwd("D:/Marta/Politecnico/Tesi/Analisi nazionali")
data <- read_csv("data_by_country.csv")
data$year<-as.factor(data$year)
data$country<-as.factor(data$country)

######

### FLOW IN 

mod1<-gam(log(flow_in+1)~s(ted,bs='cr')+s(r3score,bs='cr')+s(log(gdp),bs='cr')+s(year,bs='re'),data=data)
summary(mod1)
plot(mod1)


##### 

#pca on years

data_pca<-data.frame()
for (c in unique(data$country)){
  flow_vec<-data$flow_in[which(data$country==c)]
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
data_sum$flow_in_mean<-apply(data_pca_norm[,-1],1,mean) #flow in

data_sum$flow_in_mean<-data_sum$flow_in_mean*mean_std+mean_m


#####

# construction of dataset aggregated by countries, mean on years

data_mean<-data.frame(data_sum)
colnames(data_mean)<-c("country","flow_in_mean")

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


##### 

## models without edu index 

mod2<-gam(flow_in_mean~s(ted,bs="cr")+s(score,bs="cr")+s(gdp,bs="cr"),data=data_mean)
summary(mod2) #0.959
shapiro.test(mod2$residuals) #normal
plot(mod2)

#

data_mean$r3score<-(data_mean$score)^(1/3)
mod3<-gam(log(flow_in_mean+1)~s(ted,bs="cr")+s(r3score,bs="cr")+s(log(gdp),bs="cr"),data=data_mean)
summary(mod3) #0.925
shapiro.test(mod3$residuals) #normal
plot(mod3)

#no gdp

mod4_full<-gam(log(flow_in_mean+1)~s(ted,bs="cr")+s(r3score,bs="cr"),data=data_mean)
summary(mod4_full) #0.928
shapiro.test(mod4_full$residuals) #normal
plot(mod4_full)

mod4<-gam(log(flow_in_mean+1)~s(ted,bs="cr")+s(r3score,bs="cr",k=6),data=data_mean)
summary(mod4) #0.879   
shapiro.test(mod4$residuals) #normal
plot(mod4)

anova(mod4,mod4_full,test="F")
#p=0.0177  we lose something 

#
mod_cor<-gam(r3score~s(ted,bs="cr"),data=data_mean)
summary(mod_cor)
plot(mod_cor)

x11()
plot(data_mean$ted,data_mean$r3score)


## models with edu index

mod_edu<-gam(log(flow_in_mean)~s(edu_index,bs="cr"),data=data_mean)
summary(mod_edu) #rsq 0.22
shapiro.test(mod_edu$residuals) #normal
plot(mod_edu)

mod_edu_l<-lm(log(flow_in_mean)~edu_index,data=data_mean)
summary(mod_edu_l) #rsq 0.25
shapiro.test(mod_edu_l$residuals)
plot(mod_edu_l)
anova(mod_edu_l,mod_edu,test="F") #1.832e-08
plot(data_mean$edu_index,log(data_mean$flow_in_mean))

mod_uni<-gam(log(flow_in_mean)~s(r3score,bs="cr"),data=data_mean)
summary(mod_uni) #rsq 0.74
shapiro.test(mod_uni$residuals) #normal
plot(mod_uni)

mod_ted<-gam(log(flow_in_mean)~s(ted,bs="cr"),data=data_mean)
summary(mod_ted) #rsq 0.72
shapiro.test(mod_ted$residuals) #normal
plot(mod_ted)

mod_ted_l<-lm(log(flow_in_mean)~ted,data=data_mean)
summary(mod_ted_l) #rsq 0.72
shapiro.test(mod_ted_l$residuals) #no normal
plot(mod_ted_l)

anova(mod_ted_l,mod_ted,test="F") #p=0.10

mod_gdp<-gam(log(flow_in_mean)~s(log(gdp),bs="cr"),data=data_mean)
summary(mod_gdp) #0.28
shapiro.test(mod_gdp$residuals) #normal
plot(mod_gdp)

mod_uni_ted<-gam(log(flow_in_mean)~s(r3score,bs="cr")+s(ted,bs="cr"),data=data_mean)
summary(mod_uni_ted) #0.928   
shapiro.test(mod_uni_ted$residuals) #normal
plot(mod_uni_ted)

anova(mod_uni,mod_uni_ted,test="F")
anova(mod_ted,mod_uni_ted,test="F")
#reject both, uni and ted both meaningful

mod_uni_gdp<-gam(log(flow_in_mean)~s(r3score,bs="cr")+s(log(gdp),bs="cr"),data=data_mean)
summary(mod_uni_gdp) #0.741   
shapiro.test(mod_uni_gdp$residuals) #normal
plot(mod_uni_gdp)
#gdp linear contribute, not significant
anova(mod_uni,mod_uni_gdp,test="F") #dont reject, can keep small model with only uni

mod_ted_edu<-gam(log(flow_in_mean)~s(ted,bs="cr")+s(edu_index,bs="cr"),data=data_mean)
summary(mod_ted_edu) #rsq 0.779
shapiro.test(mod_ted_edu$residuals) #normal
plot(mod_ted_edu)
anova(mod_ted,mod_ted_edu,test="F") #reject, gdp is useful with ted

mod_uni_edu<-gam(log(flow_in_mean)~s(r3score,bs="cr")+s(edu_index,bs="cr"),data=data_mean)
summary(mod_uni_edu) #rsq 0.756
shapiro.test(mod_uni_edu$residuals) #normal
plot(mod_uni_edu)
#edu linear contribute, not significant
anova(mod_uni,mod_uni_edu,test="F") #dont reject, can keep small model with only uni

mod_uni_ted_gdp<-gam(log(flow_in_mean)~s(r3score,bs="cr")+s(ted,bs="cr")+s(log(gdp),bs="cr"),data=data_mean)
summary(mod_uni_ted_gdp) #rsq 0.925
shapiro.test(mod_uni_ted_gdp$residuals) #normal
plot(mod_uni_ted_gdp)
anova(mod_uni_ted,mod_uni_ted_gdp,test="F")#no reject, we can use reduced


mod_uni_ted_edu<-gam(log(flow_in_mean)~s(r3score,bs="cr")+s(ted,bs="cr")+s(edu_index,bs="cr"),data=data_mean)
summary(mod_uni_ted_edu) #rsq 0.925
shapiro.test(mod_uni_ted_edu$residuals) #normal -> gdp not significant
plot(mod_uni_ted_edu)
anova(mod_uni_ted,mod_uni_ted_edu,test="F")#no reject, we can use reduced


mod_uni_ted_l<-gam(log(flow_in_mean)~s(r3score,bs="cr")+ted,data=data_mean)
summary(mod_uni_ted_l) #rsq 0.894
shapiro.test(mod_uni_ted_l$residuals) #normal
plot(mod_uni_ted_l)
anova(mod_uni_ted_l,mod_uni_ted,test="F") #reject, nonlinear contribute useful

mod_uni_ted_df7<-gam(log(flow_in_mean)~s(r3score,bs="cr",k=7)+s(ted,bs="cr"),data=data_mean)
summary(mod_uni_ted_df7) #rsq 0.912
shapiro.test(mod_uni_ted_df7$residuals) #normal
plot(mod_uni_ted_df7)

mod_uni_ted_df6<-gam(log(flow_in_mean)~s(r3score,bs="cr",k=6)+s(ted,bs="cr"),data=data_mean)
summary(mod_uni_ted_df6) #rsq 0.88
shapiro.test(mod_uni_ted_df6$residuals) #normal
plot(mod_uni_ted_df6)

anova(mod_uni_ted_df7,mod_uni_ted,test="F") #no reject, can reduce
anova(mod_uni_ted_df6,mod_uni_ted,test="F") #reject, cant reduce
anova(mod_uni_ted_df6,mod_uni_ted_df7,test="F")
#to be precise we should keep df=7, but with 6 the smoothness is better 

#plot 
score_grid=seq(range(data_mean$r3score)[1],range(data_mean$r3score)[2],length.out = 100)
ted_grid=seq(range(data_mean$ted)[1],range(data_mean$ted)[2],length.out = 100)
grid=expand.grid(score_grid,ted_grid)
names(grid)=c('r3score','ted')
pred=predict(mod_uni_ted,newdata=grid) 
pred
persp3d(score_grid,ted_grid,pred,col='pink',border="black",lwd=0.3)
with(data_mean, points3d(r3score,ted,log(flow_in_mean+1),col='black',size=5))


## backward selection with linear ted
plot(data_mean$ted,log(data_mean$flow_in_mean))
plot(mod_ted)
summary(mod_ted) #rsq 0.728

mod9<-gam(log(flow_in_mean)~s(r3score,bs="cr")+s(log(gdp),bs="cr")+ted+s(edu_index,bs="cr"),data=data_mean)
summary(mod9)
shapiro.test(mod9$residuals)

mod10<-gam(log(flow_in_mean)~s(r3score,bs="cr")+s(log(gdp),bs="cr")+ted,data=data_mean)
summary(mod10)
shapiro.test(mod10$residuals)

mod11<-gam(log(flow_in_mean)~s(r3score,bs="cr")+ted,data=data_mean)
summary(mod11)
shapiro.test(mod11$residuals)

anova(mod11,mod9,test="F")

mod12<-gam(log(flow_in_mean)~s(r3score,bs="cr",k=6)+s(ted,bs="cr"),data=data_mean)
summary(mod12)
shapiro.test(mod12$residuals)
plot(mod12)

anova(mod11,mod12,test="F")

