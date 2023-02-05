library(mgcv)
library(voxel)
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(ggplotify)


### IMPORT DATA ###
#data_trips_gdp_score_ted_edu <- ...

data <- data_trips_gdp_score_ted_edu
  
#correct variables
data$year <- as.factor(data$year)
data$country <- gsub(".{2}$","", data$nuts2)
data$country <- as.factor(data$country)
data$ted <- as.numeric(data$ted)

#define University Score
data$uni_score <- (data$score)^(1/3)

#MODEL FOR OUTGOING FLOWS

#define response variable
data$log_flow <- log(data$flow_out +1)

#fit model
mod<-gam(log_flow~ s(uni_score,bs='cr',k=6)+s(ted,bs='cr', k=6)+s(edu_index,bs='cr')+s(country,bs='re')+s(year,bs='re'),data=data)

#plot
vars <- c("uni_score", "ted", "edu_index")
labs <- c("A", "B", "C")
names <- c("uni", "TED", "edu")
map(vars, function(x){
  lab <- labs[which(vars == x)]
  name <- names[which(vars == x)]
  p <- plotGAM(mod, smooth.cov = x) + ggtitle(lab) + xlab(name) + ylab("Effect on outgoing researchers") + scale_fill_grey()
  g <- ggplotGrob(p)
}) %>%
  {grid.arrange(grobs = (.), ncol = 3, nrow = 1)}

#MODEL FOR INGOING FLOWS

#define response variable
data$log_flow <- log(data$flow_in +1)

#fit model
mod <- gam(log_flow~ s(uni_score,bs='cr',k=6)+s(ted,bs='cr', k=6)+s(edu_index,bs='cr')+s(country,bs='re')+s(year,bs='re'),data=data)

#plot
vars <- c("uni_score", "ted", "edu_index")
labs <- c("A", "B", "C")
names <- c("uni", "TED", "edu")
map(vars, function(x){
  lab <- labs[which(vars == x)]
  name <- names[which(vars == x)]
  p <- plotGAM(mod, smooth.cov = x) + ggtitle(lab) + xlab(name) + ylab("Effect on ingoing researchers") + scale_fill_grey()
  g <- ggplotGrob(p)
}) %>%
  {grid.arrange(grobs = (.), ncol = 3, nrow = 1)}
