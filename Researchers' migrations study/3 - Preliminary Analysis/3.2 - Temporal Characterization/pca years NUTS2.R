#NUTS2 check flows over years 

library(readr)
library(ggplot2)

### IMPORT DATA ###
#data_trips_gdp_score_ted_edu <- ...

data<-data_trips_gdp_score_ted_edu
data$tot_flow<-as.numeric(data$flow_in+data$flow_out)
data$year<-as.factor(data$year)
data$country <- gsub(".{2}$","", data$nuts2)
data$country <- as.factor(data$country)

#pca on years, we construct dataset 
data_pca<-data.frame()
for (c in unique(data$nuts2)){
  flow_vec<-data$tot_flow[which(data$nuts2==c)]
  year_vec<-data$year[which(data$nuts2==c)]
  flow_vec<-flow_vec[order(year_vec,decreasing = F)]
  newrow<-c(c)
  newrow<-append(newrow,flow_vec)
  data_pca=rbind(data_pca,newrow)
}
colnames(data_pca)<-c("nuts2","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")
data_pca$`2009`<-as.numeric(data_pca$`2009`)
data_pca$`2010`<-as.numeric(data_pca$`2010`)
data_pca$`2011`<-as.numeric(data_pca$`2011`)
data_pca$`2012`<-as.numeric(data_pca$`2012`)
data_pca$`2013`<-as.numeric(data_pca$`2013`)
data_pca$`2014`<-as.numeric(data_pca$`2014`)
data_pca$`2015`<-as.numeric(data_pca$`2015`)
data_pca$`2016`<-as.numeric(data_pca$`2016`)
data_pca$`2017`<-as.numeric(data_pca$`2017`)
data_pca$`2018`<-as.numeric(data_pca$`2018`)
data_pca$`2019`<-as.numeric(data_pca$`2019`)
data_pca$`2020`<-as.numeric(data_pca$`2020`)

#on data
p<-princomp(data_pca[,-1],scores=T)
summary(p) 
p$loadings #first comp explains 33,84% of total variance. First comp is not a mean


#on normalize data 
data_pca_norm<-data_pca
data_pca_norm[,-1]<-scale(data_pca_norm[,-1])
p_norm<-princomp(data_pca_norm[,-1],scores=T)
summary(p_norm) 
p_norm$loadings #first comp explains 19.84% of total variance, even if normalized we cant use only first comp (here a mean)

##### FLOW IN
library(readr)
data_trips_gdp_score_ted_edu <- read_delim("D:/Marta/Politecnico/Tesi/FINAL M/data_trips_gdp_score_ted_edu.csv", 
                                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

data<-data_trips_gdp_score_ted_edu
data$tot_flow<-as.numeric(data$flow_in)
data$year<-as.factor(data$year)
data$country <- gsub(".{2}$","", data$nuts2)
data$country <- as.factor(data$country)

#plots tot flow over years 
boxplot(data$tot_flow~data$year)

data_hist<-data[,c(2,9)]
data_hist<-aggregate(tot_flow~year,FUN=sum, data=data_hist)

hist(data_hist$tot_flow)

plot(data_hist$year,data_hist$tot_flow,xlab="years",ylab="total flow")


#pca on years, we construct dataset 
data_pca<-data.frame()
for (c in unique(data$nuts2)){
  flow_vec<-data$tot_flow[which(data$nuts2==c)]
  year_vec<-data$year[which(data$nuts2==c)]
  flow_vec<-flow_vec[order(year_vec,decreasing = F)]
  newrow<-c(c)
  newrow<-append(newrow,flow_vec)
  data_pca=rbind(data_pca,newrow)
}
colnames(data_pca)<-c("nuts2","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")
data_pca$`2009`<-as.numeric(data_pca$`2009`)
data_pca$`2010`<-as.numeric(data_pca$`2010`)
data_pca$`2011`<-as.numeric(data_pca$`2011`)
data_pca$`2012`<-as.numeric(data_pca$`2012`)
data_pca$`2013`<-as.numeric(data_pca$`2013`)
data_pca$`2014`<-as.numeric(data_pca$`2014`)
data_pca$`2015`<-as.numeric(data_pca$`2015`)
data_pca$`2016`<-as.numeric(data_pca$`2016`)
data_pca$`2017`<-as.numeric(data_pca$`2017`)
data_pca$`2018`<-as.numeric(data_pca$`2018`)
data_pca$`2019`<-as.numeric(data_pca$`2019`)
data_pca$`2020`<-as.numeric(data_pca$`2020`)

#on data
p<-princomp(data_pca[,-1],scores=T)
summary(p) 
p$loadings #first comp explains 33,26% of total variance. First comp is not a mean


#on normalize data 
data_pca_norm<-data_pca
data_pca_norm[,-1]<-scale(data_pca_norm[,-1])
p_norm<-princomp(data_pca_norm[,-1],scores=T)
summary(p_norm) 
p_norm$loadings #first comp explains 19.67% of total variance, even if normalized we cant use only first comp (here a mean)



##### FLOW OUT
library(readr)
data_trips_gdp_score_ted_edu <- read_delim("D:/Marta/Politecnico/Tesi/FINAL M/data_trips_gdp_score_ted_edu.csv", 
                                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

data<-data_trips_gdp_score_ted_edu
data$tot_flow<-as.numeric(data$flow_out)
data$year<-as.factor(data$year)
data$country <- gsub(".{2}$","", data$nuts2)
data$country <- as.factor(data$country)

#plots tot flow over years 
boxplot(data$tot_flow~data$year)

data_hist<-data[,c(2,9)]
data_hist<-aggregate(tot_flow~year,FUN=sum, data=data_hist)

hist(data_hist$tot_flow)

plot(data_hist$year,data_hist$tot_flow,xlab="years",ylab="total flow")


#pca on years, we construct dataset 
data_pca<-data.frame()
for (c in unique(data$nuts2)){
  flow_vec<-data$tot_flow[which(data$nuts2==c)]
  year_vec<-data$year[which(data$nuts2==c)]
  flow_vec<-flow_vec[order(year_vec,decreasing = F)]
  newrow<-c(c)
  newrow<-append(newrow,flow_vec)
  data_pca=rbind(data_pca,newrow)
}
colnames(data_pca)<-c("nuts2","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")
data_pca$`2009`<-as.numeric(data_pca$`2009`)
data_pca$`2010`<-as.numeric(data_pca$`2010`)
data_pca$`2011`<-as.numeric(data_pca$`2011`)
data_pca$`2012`<-as.numeric(data_pca$`2012`)
data_pca$`2013`<-as.numeric(data_pca$`2013`)
data_pca$`2014`<-as.numeric(data_pca$`2014`)
data_pca$`2015`<-as.numeric(data_pca$`2015`)
data_pca$`2016`<-as.numeric(data_pca$`2016`)
data_pca$`2017`<-as.numeric(data_pca$`2017`)
data_pca$`2018`<-as.numeric(data_pca$`2018`)
data_pca$`2019`<-as.numeric(data_pca$`2019`)
data_pca$`2020`<-as.numeric(data_pca$`2020`)

#on data
p<-princomp(data_pca[,-1],scores=T)
summary(p) 
p$loadings #first comp explains 34.21% of total variance. First comp is not a mean


#on normalize data 
data_pca_norm<-data_pca
data_pca_norm[,-1]<-scale(data_pca_norm[,-1])
p_norm<-princomp(data_pca_norm[,-1],scores=T)
summary(p_norm) 
p_norm$loadings #first comp explains 19.95% of total variance, even if normalized we cant use only first comp (here a mean)

