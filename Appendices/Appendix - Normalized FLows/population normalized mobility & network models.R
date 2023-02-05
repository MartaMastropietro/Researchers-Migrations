

library(mgcv)
library(mgcViz)
library(readr)

### MOBILITY 


#pop_imputed_2009.2020 <- read.csv("C:/Users/user/Downloads/pop_imputed_2009-2020.csv")
pop_imputed_2009.2020 <- read_csv("Population data/pop_imputed_2009-2020.csv")

pimp <- pop_imputed_2009.2020
head(pimp)
pimp <- pimp[,c(1,2,4)]

#data_trips_gdp_score_ted_edu <- read.csv("C:/Users/user/Desktop/universit?/dare/TESI/Git_repo/ORCiD_Thesis/Analyses/Dataset nuts2/data_trips_gdp_score_ted_edu.csv", sep=";")
data_trips_gdp_score_ted_edu <- read_delim("data_trips_gdp_score_ted_edu.csv", 
                                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

data<- merge(data_trips_gdp_score_ted_edu, pimp)


# log flow out ~ log gdp, r3score, ted, edu index, re country, re year
data$year <- as.factor(data$year)
data$country <- gsub(".{2}$","", data$nuts2)
data$country <- as.factor(data$country)
data$ted <- as.numeric(data$ted)
data$flow_tot<-data$flow_in+data$flow_out

data$flow_tot <- (data$flow_tot/data$pop_im)*100000 #normalized

data$uni_score <- (data$score)^(1/3)
data$log_flow <- log(data$flow_tot +1) # normalized

#full model no smooth
mod_full<-gam(log_flow~ log(gdp)+s(uni_score,bs='cr', k=5)+s(ted,bs='cr', k=6)+s(edu_index, bs='cr')+s(country,bs='re')+s(year,bs='re'),data=data)
summary(mod_full)
cor(mod_full$fitted.values, data$log_flow) #0.5918541
print(plot(getViz(mod_full), allTerms = T), pages = 1)

mod_reduced<-gam(log_flow~ s(uni_score,bs='cr', k=5)+s(ted,bs='cr', k=6)+s(edu_index, bs='cr')+s(country,bs='re')+s(year,bs='re'),data=data)
summary(mod_reduced)
cor(mod_reduced$fitted.values, data$log_flow) #0.589706
print(plot(getViz(mod_reduced), allTerms = T), pages = 1)

a1<- anova_np(mod_full,mod_reduced, data) #0.48, no reject, reduced model is fine

mod<-gam(log_flow~ s(uni_score,bs='cr', k=3)+s(ted,bs='cr', k=6)+s(edu_index,bs='cr')+s(country,bs='re')+s(year,bs='re'),data=data)
cor(mod$fitted.values, data$log_flow) 
print(plot(getViz(mod), allTerms = T), pages = 1)


mod_no_UK<-gam(log_flow~ s(uni_score,bs='cr', k=3)+s(ted,bs='cr', k=6)+s(edu_index,bs='cr')+s(country,bs='re')+s(year,bs='re'),data=data[-which(data$nuts2=="UKI0"),])
cor(mod_no_UK$fitted.values, data$log_flow[-which(data$nuts2=="UKI0")])
print(plot(getViz(mod_no_UK), allTerms = T), pages = 1)

#erasing london, uniscore goes back to the behavior without normalizing. 


### NETWORK
setwd("D:/Marta/Politecnico/Tesi")
pop_imputed_2009.2020 <- read_csv("Population data/pop_imputed_2009-2020.csv")

pimp <- pop_imputed_2009.2020
head(pimp)
pimp <- pimp[,c(1,2,4)]
network_data <- read_csv("NUTS 2 analysis/network analysis/network_data.csv")

data <- network_data
data$same_country<-as.factor(data$same_country)
data$year<-as.factor(data$year)

data$uniscore_sender <- (data$score_sender)^(1/3)
data$uniscore_receiver <- (data$score_receiver)^(1/3)

#same language covariate

language_mapper <- read.csv("Github/ORCiD_Thesis/Analyses/Dataset nuts2/Coovariates/language_mapper.csv")
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

# normalizing with sender pop
pimp_sender <- pimp
colnames(pimp_sender) <- c("sender", "year", "pop")
data1 <- merge(data, pimp_sender)
data1$lcount <- log(data1$count/data1$pop + 1)

mod1 <- gam(lcount ~ s(uniscore_sender,bs="cr",k = 5) + s(ted_sender,bs="cr",k = 5) + s(uniscore_receiver,bs="cr",k = 5) + s(ted_receiver,bs="cr",k = 5) + s(log(dist),bs="cr") + same_country + s(year,bs="re"), data=data1)
cor(mod1$fitted.values, data1$lcount) # 0.2060306

mod01<-gam(lcount ~ s(log(dist),bs="cr") + same_country + s(year,bs="re"), data=data1)
cor(mod01$fitted.values, data1$lcount) # 0.1892651
print(plot(getViz(mod1), allTerms = T), pages = 1)

a01<-anova_np(mod1, mod01, data1,B=300) #0

#normalizing with receiver pop
pimp_receiver <- pimp
colnames(pimp_receiver) <- c("receiver", "year", "pop_rec")
data2 <- merge(data, pimp_receiver)
data2$lcount <- log(data2$count/data2$pop_rec + 1)

mod2 <- gam(lcount ~ s(uniscore_sender,bs="cr",k = 5) + s(ted_sender,bs="cr",k = 5) + s(uniscore_receiver,bs="cr",k = 5) + s(ted_receiver,bs="cr",k = 5) + s(log(dist),bs="cr") + same_country + s(year,bs="re"), data=data2)
cor(mod2$fitted.values, data2$lcount) # 0.1886475

mod02<-gam(lcount ~ s(log(dist),bs="cr") + same_country + s(year,bs="re"), data=data2)
cor(mod02$fitted.values, data2$lcount)# 0.1720684
print(plot(getViz(mod2), allTerms = T), pages = 1)

a02<-anova_np(mod2, mod02, data2, B=300) #0

#normalizing with sum of the two
data3<-data1
pimp_receiver <- pimp
colnames(pimp_receiver) <- c("receiver", "year", "pop_rec")
data3<-merge(data3, pimp_receiver)
data3$log_flow <- log(data3$count/(data3$pop + data3$pop_rec)*100000 + 1)
data3$log_dist <- log(data3$dist)

mod3 <- gam(log_flow ~ s(uniscore_sender,bs="cr",k = 4) + s(ted_sender,bs="cr",k = 5) + s(uniscore_receiver,bs="cr",k = 4) + s(ted_receiver,bs="cr",k = 5) + s(log(dist),bs="cr") + same_country + s(year,bs="re"), data=data3)
cor(mod3$fitted.values, data3$log_flow) #  0.4292577
print(plot(getViz(mod3), allTerms = T), pages = 1)

mod03<-gam(log_flow ~ s(log(dist),bs="cr") + same_country + s(year,bs="re"), data=data3)
cor(mod03$fitted.values, data3$log_flow)# 0.4022638
print(plot(getViz(mod03), allTerms = T), pages = 1)

a03<-anova_np(mod3, mod03, data3, B=300) #0

mod_3s <- gam(log_flow ~ s(uniscore_sender,bs="cr",k = 4) +s(uniscore_receiver,bs="cr",k = 4) + s(log(dist),bs="cr") + same_country + s(year,bs="re"), data=data3)
cor(mod_3s$fitted.values, data3$log_flow) # 0.4284103
print(plot(getViz(mod_3s), allTerms = T), pages = 1)

a3s<-anova_np(mod3, mod_3s, data3, B=300) #0

a_st0<-anova_np(mod3, mod03, data3, B=300) #0
