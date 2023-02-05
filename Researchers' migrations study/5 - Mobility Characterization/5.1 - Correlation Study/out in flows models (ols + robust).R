
library(readr)
library(igraph)
library(brainGraph)
library(networkR)
library(robustbase)
library(RobStatTM)
library(MASS)

### IMPORT DATA ###
#data_trips_gdp_score_ted_edu <- ...
  
  
data<-data_trips_gdp_score_ted_edu[,1:4]
data<-aggregate(.~nuts2,sum,data=data)
data<-data[,-2]

cor(data$flow_in,data$flow_out)

#regression ols
mod_ols<-lm(flow_out~flow_in, data=data)
summary(mod_ols)

#robust regression
data<-data.frame(data)
mod_lts<- ltsreg(flow_out~flow_in,data=data) 
summary(mod_lts)

plot(data[,2:3])
abline(mod_ols, col="red", lwd=2)
abline(mod_lts, col="darkblue", lwd=2)

