library(mgcv)
library(voxel)
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(ggplotify)

### IMPORT DATA ###
#data_by_country <- ...
#network_data_by_country<- ...


#NATIONAL MODELS: Mobility
#data
data <- data_by_country
  
#correct variables
data$year<-as.factor(data$year)
data$country<-as.factor(data$country)
data$uni_score<-data$r3score

#response
data$l_flow<-log(data$flow_in+data$flow_out)

#fit
mod<-gam(l_flow~s(ted,bs="cr",k=4)+s(uni_score,bs='cr')+s(year,bs='re'),data=data)

#plotting

vars <- c("uni_score", "ted")
labs <- c("A", "B")
names <- c("uni", "TED")
map(vars, function(x){
  lab <- labs[which(vars == x)]
  name <- names[which(vars == x)]
  p <- plotGAM(mod, smooth.cov = x) + ggtitle(lab) + xlab(name) + ylab("Effect on mobility") + scale_fill_grey()
  g <- ggplotGrob(p)
}) %>%
  {grid.arrange(grobs = (.), ncol = 2, nrow = )}

#NATIONAL MODELS: Network
#data
data <- network_data_by_country
  
#correct variables
data$count<-as.numeric(data$count)
data$year<-as.factor(data$year)
data$uniscore_send <- data$r3score_send
data$uniscore_rec <- data$r3score_rec
data$l_dist <- log(data$dist)
#response
data$log_count<-log(data$count+1)

#fit
mod_net<-gam(log_count ~s(ted_send,bs="cr",k=4) + s(uniscore_send,bs="cr",k=4)+s(ted_rec,bs="cr",k=4) + s(uniscore_rec,bs="cr",k=5) +s(l_dist,bs="cr",k=3) + s(year,bs="re"), data=data)

#plot
vars <- c("uniscore_send", "uniscore_rec", "ted_send", "ted_rec", "l_dist")
labs <- c("A", "B", "C", "D", "E")
names <- c("uni - sender", "uni - receiver", "TED - sender", "TED - receiver", "log dist")
map(vars, function(x){
  lab <- labs[which(vars == x)]
  name <- names[which(vars == x)]
  p <- plotGAM(mod_net, smooth.cov = x) + ggtitle(lab) + xlab(name) + ylab("Effect on log flow") + scale_fill_grey()
  g <- ggplotGrob(p)
}) %>%
  {grid.arrange(grobs = (.), ncol = 3, nrow = 2)}
