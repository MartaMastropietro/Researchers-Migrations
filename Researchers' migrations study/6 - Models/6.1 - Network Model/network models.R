library(mgcv)
library(readr)
library(mgcViz)

seed = 27011999
set.seed(seed)


### IMPORT DATA ###
#network_data <- ...
#language_mapper <- ...


data <- network_data

data$same_country<-as.factor(data$same_country)
data$year<-as.factor(data$year)

data$uniscore_sender <- (data$score_sender)^(1/3)
data$uniscore_receiver <- (data$score_receiver)^(1/3)

#same language covariate
l_map_send<-language_mapper
colnames(l_map_send)<-c("country_send","lan_fam_sen")
l_map_rec<-language_mapper
colnames(l_map_rec)<-c("country_rec","lan_fam_rec")
data$country_send<-gsub('.{2}$','',data$sender)
data$country_rec<-gsub('.{2}$','',data$receiver)
data<-merge(data,l_map_send,all = T)
data<-merge(data,l_map_rec,all = T)
data$lan_fam_sen[which(gsub('.{1}$','',data$sender)=="BE2")]<-"germanic"
data$lan_fam_rec[which(gsub('.{1}$','',data$receiver)=="BE2")]<-"germanic"
data$lan_fam_sen[which(gsub('.{1}$','',data$sender)=="CH01")]<-"romance"
data$lan_fam_rec[which(gsub('.{1}$','',data$receiver)=="CH01")]<-"romance"
data$lan_fam_sen[which(gsub('.{1}$','',data$sender)=="CH07")]<-"romance"
data$lan_fam_rec[which(gsub('.{1}$','',data$receiver)=="CH07")]<-"romance"
data$same_lan <- data$lan_fam_sen == data$lan_fam_rec
data$same_lan <- as.factor(data$same_lan)

data$lcount <- log(data$count + 1)

set.seed(seed)
#FULL MODEL
mod_full<-gam(lcount ~ s(log(gdp_sender),bs="cr") + s(uniscore_sender,bs="cr") + s(ted_sender,bs="cr") + s(edu_sender,bs="cr") + s(log(gdp_receiver),bs="cr") + s(uniscore_receiver,bs="cr") + s(ted_receiver,bs="cr") + s(edu_receiver,bs="cr") + s(log(dist),bs="cr") + same_country + same_lan + s(year,bs="re"), data=data)
summary(mod_full)#GCV=0.048368      
R2_modfull <- cor(mod_full$fitted.values, log(data$count+1))
R2_modfull #0.5030085
print(plot(getViz(mod_full), allTerms = T), pages = 1)


#MODEL SELECTION
#try different basis, penalisations and basis numbers
mod_sel <- gam(lcount ~ s(uniscore_sender,bs="cr", k=4) + s(uniscore_receiver,bs="cr", k=4) + s(log(dist),bs="cr", k=6) + same_country + same_lan + s(year,bs="re"), data=data)
summary(mod_sel) #GCV=0.049344  
R2_sel <- cor(mod_sel$fitted.values, log(data$count+1))
R2_sel #0.4876929
print(plot(getViz(mod_sel), allTerms = T), pages = 1)



#FINAL MODEL
mod_reg <- gam(lcount ~ s(uniscore_sender,bs="cr",k = 5) + s(ted_sender,bs="cr",k = 5) + s(uniscore_receiver,bs="cr",k = 5) + s(ted_receiver,bs="cr",k = 5) + s(log(dist),bs="cr") + same_country + s(year,bs="re"), data=data)
summary(mod_reg)  # GCV=0.048846    
R2_reg <- cor(mod_reg$fitted.values, log(data$count+1))
R2_reg#0.4955394
print(plot(getViz(mod_reg), allTerms = T), pages = 1)


