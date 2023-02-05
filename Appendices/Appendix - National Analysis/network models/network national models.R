setwd("D:/Marta/Politecnico/Tesi/National data and analysis/network models")

library(readr)
library(mgcv)
library(mgcViz)


my_summary<-function(mod,data){
  rsq<-as.numeric(cor(mod$fitted.values,data[as.character(mod$formula)[2]]))
  adj_rsq<-summary(mod)$r.sq
  aic<-AIC(mod)
  
  return(c(rsq=rsq,adj_rsq=adj_rsq,aic=aic))
}
# options
options(scipen = 100, digits = 4)


network_data_by_country<- read_csv("network_data_by_country.csv")
data<-network_data_by_country
colnames(data)
data$count<-as.numeric(data$count)
data$year<-as.factor(data$year)
data$log_count<-log(data$count+1)
data$uniscore_send<-data$r3score_send
data$uniscore_rec<-data$r3score_rec

#complete model
mod0<-gam(log_count ~s(ted_send,bs="cr") + s(score_send,bs="cr") + s(gdp_send,bs="cr") + s(edu_send,bs="cr") +
                 s(ted_rec,bs="cr") + s(score_rec,bs="cr") + s(gdp_rec,bs="cr") + s(edu_rec,bs="cr") +s(dist,bs="cr") + s(year,bs="re"), data=data)
summary(mod0) #0.667
cor(mod0$fitted.values,data$log_count) # 0.81
print(plot(getViz(mod0), allTerms = T), pages = 1)

#complete model, transformed var
mod1<-gam(log_count ~s(ted_send,bs="cr") + s(uniscore_send,bs="cr") + s(log(gdp_send),bs="cr") + s(edu_send,bs="cr") +
            s(ted_rec,bs="cr") + s(uniscore_rec,bs="cr") + s(log(gdp_rec),bs="cr") + s(edu_rec,bs="cr") +s(log(dist),bs="cr") + s(year,bs="re"), data=data)
summary(mod1) #0.667 
cor(mod1$fitted.values,data$log_count) # 0.818
print(plot(getViz(mod1), allTerms = T), pages = 1)

#complete model, transformed var, df reduced 
mod2<-gam(log_count ~s(ted_send,bs="cr",k=6) + s(uniscore_send,bs="cr",k=6) + s(log(gdp_send),bs="cr",k=6) + s(edu_send,bs="cr",k=6) +
            s(ted_rec,bs="cr",k=6) + s(uniscore_rec,bs="cr",k=6) + s(log(gdp_rec),bs="cr",k=6) + s(edu_rec,bs="cr",k=6) +s(log(dist),bs="cr",k=6) + s(year,bs="re",k=6), data=data)
summary(mod2) #0.656
cor(mod2$fitted.values,data$log_count) # 0.81
print(plot(getViz(mod2), allTerms = T), pages = 1)

a_c<-anova_np(mod0,mod2,data) #too  many na's, based on rsquared we decide to keep smoothed model mod2

#model score+ted
mod4<-gam(log_count ~s(ted_send,bs="cr") + s(uniscore_send,bs="cr")+
            s(ted_rec,bs="cr") + s(uniscore_rec,bs="cr") +s(log(dist),bs="cr") + s(year,bs="re"), data=data)
summary(mod4) #0.651
cor(mod4$fitted.values,data$log_count) # 0.807
print(plot(getViz(mod4), allTerms = T), pages = 1)

#complete vs score+ted
a_s_t<-anova_np(mod1,mod4,data) #0.16 can reduce 

#score+ted , df 
mod4df<-gam(log_count ~s(ted_send,bs="cr",k=6) + s(uniscore_send,bs="cr",k=6)+
            s(ted_rec,bs="cr",k=6) + s(uniscore_rec,bs="cr",k=6) +s(log(dist),bs="cr",k=6) + s(year,bs="re"), data=data)
summary(mod4df) #0.651
cor(mod4df$fitted.values,data$log_count) # 0.807
print(plot(getViz(mod4df), allTerms = T), pages = 1)

#score+ted vs score+ted,df
a_s_t_df<-anova_np(mod4,mod4df,data) # na t_0 , we decide to smooth


mod_score<-gam(log_count ~s(uniscore_send,bs="cr")+s(uniscore_rec,bs="cr") +s(log(dist),bs="cr") + s(year,bs="re"), data=data)
my_summary(mod_score,data) #0.7813     0.6092

mod_ted<-gam(log_count ~s(ted_send,bs="cr")+s(ted_rec,bs="cr")  +s(log(dist),bs="cr") + s(year,bs="re"), data=data)
my_summary(mod_ted,data) #0.7154     0.5103 

aov_score<-anova_np(mod4,mod_score,data) #0
aov_ted<-anova_np(mod4,mod_ted,data) #0.01-0.02 
#reject both, we need two at least -> one possible model is score ted 

#we try to see if score ted is in fact the best 
mod_edu<-gam(log_count ~ s(edu_send,bs="cr") + s(edu_rec,bs="cr") +s(log(dist),bs="cr") + s(year,bs="re"), data=data)
my_summary(mod_edu,data) #0.5664     0.3186

mod_gdp<-gam(log_count ~ s(log(gdp_send),bs="cr") + s(log(gdp_rec),bs="cr")+s(log(dist),bs="cr") + s(year,bs="re"), data=data)
my_summary(mod_gdp,data) #0.5825     0.3371

#makes sense to keep uniscore as first var, we check combinations 
mod_s_t<-gam(log_count ~s(ted_send,bs="cr") + s(uniscore_send,bs="cr") +
               s(ted_rec,bs="cr") + s(uniscore_rec,bs="cr") +s(log(dist),bs="cr") + s(year,bs="re"), data=data)
my_summary(mod_s_t,data)#0.8078     0.6508

mod_s_e<-gam(log_count ~s(uniscore_send,bs="cr")+ s(edu_send,bs="cr") +
                s(uniscore_rec,bs="cr")  + s(edu_rec,bs="cr") +s(log(dist),bs="cr") + s(year,bs="re"), data=data)
my_summary(mod_s_e,data)#0.7932     0.6273

mod_s_g<-gam(log_count ~ s(uniscore_send,bs="cr") + s(log(gdp_send),bs="cr")
               + s(uniscore_rec,bs="cr") + s(log(gdp_rec),bs="cr") +s(log(dist),bs="cr") + s(year,bs="re"), data=data)
my_summary(mod_s_g,data)#0.7910     0.6239

#score ted is the best 
final_mod<-mod_s_t
print(plot(getViz(final_mod), allTerms = T), pages = 1)

#smoothed
final_mod_smooth<-gam(log_count ~s(log(ted_send),bs="cr",k=4) + s(uniscore_send,bs="cr",k=6)+
                        s(log(ted_rec),bs="cr",k=4) + s(uniscore_rec,bs="cr",k=6) +s(log(dist),bs="cr",k=5) + s(year,bs="re"), data=data)
my_summary(final_mod_smooth,data) #0.7950     0.6311
print(plot(getViz(final_mod_smooth), allTerms = T), pages = 1)

aov_c_final_smooth<-anova_np(mod1, final_mod_smooth,data) #0, technically we lose something


#try to add linguistic proximity
#same language covariate
library(readr)
language_mapper <- read_csv("D:/Marta/Politecnico/Tesi/Github/ORCiD_Thesis/Dataset nuts2/Coovariates/language_mapper.csv")

l_map_send<-language_mapper
colnames(l_map_send)<-c("country_send","lan_fam_sen")
l_map_rec<-language_mapper
colnames(l_map_rec)<-c("country_rec","lan_fam_rec")
data$country_send<-data$sender
data$country_rec<-data$receiver
data<-merge(data,l_map_send,all = T)
data<-merge(data,l_map_rec,all = T)
data$same_lan <- data$lan_fam_sen == data$lan_fam_rec
data$same_lan <- as.factor(data$same_lan)

final_mod_sm_lan<-gam(log_count ~s(log(ted_send),bs="cr",k=4) + s(uniscore_send,bs="cr",k=6)+
                        s(log(ted_rec),bs="cr",k=4) + s(uniscore_rec,bs="cr",k=6) +s(log(dist),bs="cr",k=5) + s(year,bs="re")+same_lan, data=data)
my_summary(final_mod_sm_lan,data)#0.8010     0.6406 22277.5131 
print(plot(getViz(final_mod_sm_lan), allTerms = T), pages = 1)

aov_final_lan<-anova_np(final_mod_sm_lan,final_mod_smooth,data) #1 we can avoid using this 

#final model -> final_model_smooth with uniscore and ted

final<-gam(log_count ~s(ted_send,bs="cr", k=8) + s(uniscore_send,bs="cr",k=6)+
             s(ted_rec,bs="cr",k=8) + s(uniscore_rec,bs="cr",k=6) +s(log(dist),bs="cr",k=5) + s(year,bs="re"), data=data)
my_summary(final,data) #0.7950     0.6311
print(plot(getViz(final), allTerms = T), pages = 1)

#plot carini
data$log_flow<-data$log_count
data$uniscore_sender<-data$uniscore_send
data$uniscore_receiver<-data$uniscore_rec
data$ted_sender<-data$ted_send
data$ted_receiver<-data$ted_rec
data$log_dist<-log(data$dist)

final<-gam(log_flow ~s(uniscore_sender,bs="cr",k=6)+ s(uniscore_receiver,bs="cr",k=6) +s(ted_sender,bs="cr", k=8) + 
             s(ted_receiver,bs="cr",k=8) +s(log_dist,bs="cr",k=5) + s(year,bs="re"), data=data)
library(ISLR)
library(mgcv)
library(voxel)
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(ggplotify)

vars <- c("uniscore_sender", "uniscore_receiver", "ted_sender", "ted_receiver", "log_dist")
map(vars, function(x){
  p <- plotGAM(final, smooth.cov = x) 
  g <- ggplotGrob(p)
}) %>%
  {grid.arrange(grobs = (.), ncol = 3, nrow = 2)}
