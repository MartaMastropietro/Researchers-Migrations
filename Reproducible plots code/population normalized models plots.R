library(mgcv)
library(voxel)
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(ggplotify)

### IMPORT DATA ###
#language_mapper <- ...
#network_data <- ...
#pop_imputed_2009_2020 <- ...
#data_trips_gdp_score_ted_edu <- ...

#POPULATION NORMALIZED MODELS: MOBILITY
#population imputed to NUTS2
pimp <- pop_imputed_2009_2020
pimp <- pimp[,c(1,2,4)]
#regional characteristics
data_reg_char <- data_trips_gdp_score_ted_edu
data<- merge(data_reg_char, pimp)
#correct variables
data$year <- as.factor(data$year)
data$country <- gsub(".{2}$","", data$nuts2)
data$country <- as.factor(data$country)
data$ted <- as.numeric(data$ted)
#define University Score
data$uni_score <- (data$score)^(1/3)

#define response variable
data$flow_tot<-data$flow_in+data$flow_out
data$flow_tot <- (data$flow_tot/data$pop_im)*100000 #normalized
data$log_flow <- log(data$flow_tot +1)

#fit models with and without London
mod<-gam(log_flow~ s(uni_score,bs='cr', k=3)+s(ted,bs='cr', k=6)+s(edu_index,bs='cr')+s(country,bs='re')+s(year,bs='re'),data=data)
mod_no_UKI0<-gam(log_flow~ s(uni_score,bs='cr', k=3)+s(ted,bs='cr', k=6)+s(edu_index,bs='cr')+s(country,bs='re')+s(year,bs='re'),data=data[-which(data$nuts2=="UKI0"),])

#plotting

vars <- c("uni_score", "ted", "edu_index", "2uni_score", "2ted", "2edu_index")
labs <- c("A", "B", "C", "D", "E", "F")
names <- c("uni", "TED", "edu")
map(vars, function(x){
  lab <- labs[which(vars == x)]
  if(substr(x,1,1) == 2){
    v <- substr(x,2,nchar(x))
    name <- names[which(vars == v)]
    p <- plotGAM(mod_no_UKI0, smooth.cov = v) + ggtitle(lab) + xlab(name) + ylab("Effect on mobility per capita") + scale_fill_grey()
    g <- ggplotGrob(p)
  }
  else{
    name <- names[which(vars == x)]
    p <- plotGAM(mod, smooth.cov = x) + ggtitle(lab) + xlab(name) + ylab("Effect on mobility per capita") + scale_fill_grey()
    g <- ggplotGrob(p)
  }
}) %>%
  {grid.arrange(grobs = (.), ncol = 3, nrow = 2)}

#NETWORK MODEL, BOTH REGIONs' POPULATION
#population imputed to NUTS2
pimp <- pop_imputed_2009_2020
pimp <- pimp[,c(1,2,4)]
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
#add sender population
pimp_sender <- pimp
colnames(pimp_sender) <- c("sender", "year", "pop")
data <- merge(data, pimp_sender)
#add receiver population
pimp_receiver <- pimp
colnames(pimp_receiver) <- c("receiver", "year", "pop_rec")
data<-merge(data, pimp_receiver)

#define response variable
data$log_flow <- log(data$count/(data$pop + data$pop_rec)*100000 + 1)

#fit model
mod <- gam(log_flow ~ s(uniscore_sender,bs="cr",k = 4) + s(ted_sender,bs="cr",k = 5) + s(uniscore_receiver,bs="cr",k = 4) + s(ted_receiver,bs="cr",k = 5) + s(log_dist,bs="cr") + same_country + s(year,bs="re"), data=data)

#plot
#produce plot
vars <- c("uniscore_sender", "uniscore_receiver", "ted_sender", "ted_receiver", "log_dist")
labs <- c("A", "B", "C", "D", "E")
names <- c("uni - sender", "uni - receiver", "TED - sender", "TED - receiver", "log dist")
map(vars, function(x){
  lab <- labs[which(vars == x)]
  name <- names[which(vars == x)]
  p <- plotGAM(mod, smooth.cov = x) + ggtitle(lab) + xlab(name) + ylab("Effect on log flow") + scale_fill_grey()
  g <- ggplotGrob(p)
}) %>%
  {grid.arrange(grobs = (.), ncol = 3, nrow = 2)}
