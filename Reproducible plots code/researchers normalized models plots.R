library(mgcv)
library(voxel)
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(ggplotify)

### IMPORT DATA ###
#language_mapper <- ...
#network_data <- ...
#researchers_count <- ...



#RESEARCHERS NORMALIZED MODELS: SENDING REGION
#data
data <- network_data
#correct variables
data$same_country<-as.factor(data$same_country)
data$year<-as.factor(data$year)
#define University Score
data$uniscore_sender <- (data$score_sender)^(1/3)
data$uniscore_receiver <- (data$score_receiver)^(1/3)
#apply log scale to distance
data$log_dist <- log(data$dist)
#same language covariate
#####
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
#####


#normalizing for sender population
res_sender <- researchers_count
colnames(res_sender) <- c("sender", "year", "res_s")
data <- merge(data, res_sender)
data$lcount_s <- log(data$count/data$res_s + 1)
data$lcount_s[which(is.na(data$lcount_s))] <- 0 #regions with no researchers have normalized flow equal to zero

#fit model
mod_s <- gam(lcount_s ~ s(uniscore_sender,bs="cr",k = 5) + s(ted_sender,bs="cr",k = 5) + s(uniscore_receiver,bs="cr",k = 5) + s(ted_receiver,bs="cr",k = 5) + s(log_dist,bs="cr") + same_country + s(year,bs="re"), data=data)


#plot
#produce plot
vars <- c("uniscore_sender", "uniscore_receiver", "ted_sender", "ted_receiver", "log_dist")
labs <- c("A", "B", "C", "D", "E")
names <- c("uni - sender", "uni - receiver", "TED - sender", "TED - receiver", "log dist")
map(vars, function(x){
  lab <- labs[which(vars == x)]
  name <- names[which(vars == x)]
  p <- plotGAM(mod_s, smooth.cov = x) + ggtitle(lab) + xlab(name) + ylab("Effect on adjusted log flow") + scale_fill_grey()
  g <- ggplotGrob(p)
}) %>%
  {grid.arrange(grobs = (.), ncol = 3, nrow = 2)}

#RECEIVER
#normalizing for receiver population 
res_receiver <- researchers_count
colnames(res_receiver) <- c("receiver", "year", "res_r")
data <- merge(data, res_receiver)
data$lcount_r <- log(data$count/data$res_r + 1)
data$lcount_r[which(is.na(data$lcount_r))] <- 0


#fit model
mod_r <- gam(lcount_r ~ s(uniscore_sender,bs="cr",k = 5) + s(ted_sender,bs="cr",k = 5) + s(uniscore_receiver,bs="cr",k = 5) + s(ted_receiver,bs="cr",k = 5) + s(log_dist,bs="cr") + same_country + s(year,bs="re"), data=data)


#plot
#produce plot
vars <- c("uniscore_sender", "uniscore_receiver", "ted_sender", "ted_receiver", "log_dist")
labs <- c("A", "B", "C", "D", "E")
names <- c("uni - sender", "uni - receiver", "TED - sender", "TED - receiver", "log dist")
map(vars, function(x){
  lab <- labs[which(vars == x)]
  name <- names[which(vars == x)]
  p <- plotGAM(mod_r, smooth.cov = x) + ggtitle(lab) + xlab(name) + ylab("Effect on adjusted log flow") + scale_fill_grey()
  g <- ggplotGrob(p)
}) %>%
  {grid.arrange(grobs = (.), ncol = 3, nrow = 2)}


#BOTH

#normalizin for sum of receiver sender 
data$lcount_t <- log(data$count/(data$res_r + data$res_s) + 1)
data$lcount_t[which(is.na(data$lcount_t))] <- 0

#fit model
mod_t <- gam(lcount_t ~ s(uniscore_sender,bs="cr",k = 5) + s(ted_sender,bs="cr",k = 5) + s(uniscore_receiver,bs="cr",k = 5) + s(ted_receiver,bs="cr",k = 5) + s(log_dist,bs="cr") + same_country + s(year,bs="re"), data=data)


#plot
#produce plot
vars <- c("uniscore_sender", "uniscore_receiver", "ted_sender", "ted_receiver", "log_dist")
labs <- c("A", "B", "C", "D", "E")
names <- c("uni - sender", "uni - receiver", "TED - sender", "TED - receiver", "log dist")
map(vars, function(x){
  lab <- labs[which(vars == x)]
  name <- names[which(vars == x)]
  p <- plotGAM(mod_t, smooth.cov = x) + ggtitle(lab) + xlab(name) + ylab("Effect on adjusted log flow") + scale_fill_grey()
  g <- ggplotGrob(p)
}) %>%
  {grid.arrange(grobs = (.), ncol = 3, nrow = 2)}
