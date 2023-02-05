library(mgcv)
library(voxel)
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(ggplotify)


### IMPORT DATA ###
#language_mapper <- ...
#network_data <- ...
#data_trips_gdp_score_ted_edu <- ...



#FINAL NETWORK MODEL
data <- network_data
data$same_country<-as.factor(data$same_country)
data$year<-as.factor(data$year)

#define university score
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

#define response variable
data$lcount <- log(data$count + 1)

#fit model
mod_reg <- gam(lcount ~ s(uniscore_sender,bs="cr",k = 5) + s(ted_sender,bs="cr",k = 5) + s(uniscore_receiver,bs="cr",k = 5) + s(ted_receiver,bs="cr",k = 5) + s(log_dist,bs="cr") + same_country + s(year,bs="re"), data=data)

#produce plot

vars <- c("uniscore_sender", "uniscore_receiver", "ted_sender", "ted_receiver", "log_dist")
labs <- c("A", "B", "C", "D", "E")
names <- c("uni - sender", "uni - receiver", "TED - sender", "TED - receiver", "log dist")
map(vars, function(x){
  lab <- labs[which(vars == x)]
  name <- names[which(vars == x)]
  p <- plotGAM(mod_reg, smooth.cov = x) + ggtitle(lab) + xlab(name) + ylab("Effect on log flow") + scale_fill_grey()
  g <- ggplotGrob(p)
}) %>%
  {grid.arrange(grobs = (.), ncol = 3, nrow = 2)}




#FINAL MOBILITY MODEL
data<- data_trips_gdp_score_ted_edu
  
  
#clean variables
data$year <- as.factor(data$year)
data$country <- gsub(".{2}$","", data$nuts2)
data$country <- as.factor(data$country)
data$ted <- as.numeric(data$ted)

#define university score
data$uni_score <- (data$score)^(1/3)

#define response
data$flow_tot<-data$flow_in+data$flow_out
data$log_flow <- log(data$flow_tot +1)

#fit model
mod<-gam(log_flow~ s(uni_score,bs='cr')+s(ted,bs='cr', k=6)+s(edu_index,bs='cr',k=5)+s(country,bs='re')+s(year,bs='re'),data=data)

#produce plot

vars <- c("uni_score", "ted", "edu_index")
labs <- c("A", "B", "C")
names <- c("uni", "TED", "edu")
map(vars, function(x){
  lab <- labs[which(vars == x)]
  name <- names[which(vars == x)]
  p <- plotGAM(mod, smooth.cov = x) + ggtitle(lab) + xlab(name) + ylab("Effect on mobility") + scale_fill_grey()
  g <- ggplotGrob(p)
}) %>%
  {grid.arrange(grobs = (.), ncol = 3, nrow = 1)}
