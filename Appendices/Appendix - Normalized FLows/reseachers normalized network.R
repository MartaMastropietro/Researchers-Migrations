
library(mgcv)
library(mgcViz)
library(readr)

network_data <- read_csv("NUTS 2 analysis/network analysis/network_data.csv")
data <- network_data

data$same_country<-as.factor(data$same_country)
data$year<-as.factor(data$year)

data$uniscore_sender <- (data$score_sender)^(1/3)
data$uniscore_receiver <- (data$score_receiver)^(1/3)

#same language covariate
language_mapper <- read_csv("Github/ORCiD_Thesis/Analyses/Dataset nuts2/Coovariates/language_mapper.csv")

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

researchers_count <- read_csv("Github/ORCiD_Thesis/Analyses/Dataset nuts2/researchers_count.csv")

#normalizing for sender population
res_sender <- researchers_count
colnames(res_sender) <- c("sender", "year", "res_s")
data <- merge(data, res_sender)
data$lcount1 <- log(data$count/data$res_s + 1)
data$lcount1[which(is.na(data$lcount1))] <- 0

mod_full1 <- gam(lcount1 ~ s(log(gdp_sender),bs="cr") + s(uniscore_sender,bs="cr") + s(ted_sender,bs="cr") + s(edu_sender,bs="cr") + s(log(gdp_receiver),bs="cr") + s(uniscore_receiver,bs="cr") + s(ted_receiver,bs="cr") + s(edu_receiver,bs="cr") + s(log(dist),bs="cr") + same_country + same_lan + s(year,bs="re"), data=data)
cor(mod_full1$fitted.values, data$lcount1) # 0.2323894
print(plot(getViz(mod_full1), allTerms = T), pages = 1)

mod_red1 <- gam(lcount1 ~ s(uniscore_sender,bs="cr",k = 5) + s(ted_sender,bs="cr",k = 5) + s(uniscore_receiver,bs="cr",k = 5) + s(ted_receiver,bs="cr",k = 5) + s(log(dist),bs="cr") + same_country + s(year,bs="re"), data=data)
cor(mod_red1$fitted.values, data$lcount1) # 0.2292523
print(plot(getViz(mod_red1), allTerms = T), pages = 1)

mod01<-gam(lcount1 ~ s(log(dist),bs="cr") + same_country + s(year,bs="re"), data=data)
cor(mod01$fitted.values, data$lcount1)# 0.2211095

a1 <- anova_np(mod_big = mod_full1, mod_small = mod01, data = data, B=100)
a1$p.val #0

a1_reg <- anova_np(mod_big = mod_full1, mod_small = mod_red1, data = data, B=100)
a1_reg$p.val #0

a1_small<-anova_np(mod_big = mod_red1, mod_small = mod01, data = data, B=100)
a1_small$p.val #0



#normalizing for receiver population 
res_receiver <- researchers_count
colnames(res_receiver) <- c("receiver", "year", "res_r")
data <- merge(data, res_receiver)
data$lcount2 <- log(data$count/data$res_r + 1)
data$lcount2[which(is.na(data$lcount2))] <- 0

mod_full2 <- gam(lcount2 ~ s(log(gdp_sender),bs="cr") + s(uniscore_sender,bs="cr") + s(ted_sender,bs="cr") + s(edu_sender,bs="cr") + s(log(gdp_receiver),bs="cr") + s(uniscore_receiver,bs="cr") + s(ted_receiver,bs="cr") + s(edu_receiver,bs="cr") + s(log(dist),bs="cr") + same_country + same_lan + s(year,bs="re"), data=data)
cor(mod_full2$fitted.values, data$lcount2) # 0.2601978
print(plot(getViz(mod_full2), allTerms = T), pages = 1)

mod_red2 <- gam(lcount2 ~ s(uniscore_sender,bs="cr",k = 5) + s(ted_sender,bs="cr",k = 5) + s(uniscore_receiver,bs="cr",k = 5) + s(ted_receiver,bs="cr",k = 5) + s(log(dist),bs="cr") + same_country + s(year,bs="re"), data=data)
cor(mod_red2$fitted.values, data$lcount2) # 0.2569613
print(plot(getViz(mod_red2), allTerms = T), pages = 1)

mod02<-gam(lcount2 ~ s(log(dist),bs="cr") + same_country + s(year,bs="re"), data=data)
cor(mod02$fitted.values, data$lcount2)# 0.2483055

a2 <- anova_np(mod_big = mod_full2, mod_small = mod02, data = data, B=100)
a2$p.val #0

a2_reg <- anova_np(mod_big = mod_full2, mod_small = mod_red2, data = data, B=100)
a2_reg$p.val #0

a2_small<-anova_np(mod_big = mod_red2, mod_small = mod02, data = data, B=100)
a2_small$p.val #0

#normalizin for sum of receiver sender 
data$lcount3 <- log(data$count/(data$res_r + data$res_s) + 1)
data$lcount3[which(is.na(data$lcount3))] <- 0

mod_full <- gam(lcount3 ~ s(log(gdp_sender),bs="cr") + s(uniscore_sender,bs="cr") + s(ted_sender,bs="cr") + s(edu_sender,bs="cr") + s(log(gdp_receiver),bs="cr") + s(uniscore_receiver,bs="cr") + s(ted_receiver,bs="cr") + s(edu_receiver,bs="cr") + s(log(dist),bs="cr") + same_country + same_lan + s(year,bs="re"), data=data)
cor(mod_full$fitted.values, data$lcount3) # 0.4244116
print(plot(getViz(mod_full), allTerms = T), pages = 1)

mod_ut <- gam(lcount3 ~ s(uniscore_sender,bs="cr",k = 5) + s(ted_sender,bs="cr",k = 6) + s(uniscore_receiver,bs="cr",k = 5) + s(ted_receiver,bs="cr",k = 6) + s(log(dist),bs="cr") + same_country + s(year,bs="re"), data=data)
cor(mod_ut$fitted.values, data$lcount3) # 0.4197173
print(plot(getViz(mod_ut), allTerms = T), pages = 1)

mod_u <- gam(lcount3 ~ s(uniscore_sender,bs="cr",k = 5) + s(uniscore_receiver,bs="cr",k = 5) + s(log(dist),bs="cr") + same_country + s(year,bs="re"), data=data)
cor(mod_u$fitted.values, data$lcount3) # 0.4191118
print(plot(getViz(mod_u), allTerms = T), pages = 1)

mod0<-gam(lcount3 ~ s(log(dist),bs="cr") + same_country + s(year,bs="re"), data=data)
cor(mod0$fitted.values, data$lcount3) # 0.4136602

a3 <- anova_np(mod_big = mod_full, mod_small = mod_ut, data = data, B=100)
a3$p.val #0

a3_reg <- anova_np(mod_big = mod_full, mod_small =mod_u , data = data, B=100)
a3_reg$p.val #0

a3_small<-anova_np(mod_big = mod_u, mod_small = mod0, data = data, B=100)
a3_small$p.val #0

a3_ut<-anova_np(mod_big = mod_ut, mod_small = mod0, data = data, B=100)
a3_ut$p.val #0