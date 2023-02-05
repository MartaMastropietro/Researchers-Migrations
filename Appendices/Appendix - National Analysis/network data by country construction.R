## data construction
library(readr)
country_years <- read_csv("country_years.csv")

#covariates
data_by_country <- read_csv("data_by_country.csv")

data_send<-data_by_country[,-c(5,6)]
colnames(data_send)<-c("year","sender","ted_send","score_send","gdp_send","edu_send","r3score_send")

data_rec<-data_by_country[,-c(5,6)]
colnames(data_rec)<-c("year","receiver","ted_rec","score_rec","gdp_rec","edu_rec","r3score_rec")

data1<-merge(data_send,data_rec)
data1<-data1[-which(data1$sender==data1$receiver),]


#dist
library(readr)
countries_distances <- read_delim("countries_distances.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
data2<-merge(data1,countries_distances)

#count to do
data_or <- read_csv("ORCID_Trips_wip.csv")
data_or$sender<-gsub('.{2}$','',data_or$sender)
data_or$receiver<-gsub('.{2}$','',data_or$receiver)
data_or<-data_or[which(nchar(data_or$sender)==2),]
data_or<-data_or[which(nchar(data_or$receiver)==2),]
data_or<-data_or[which(data_or$year>2008),]
data_or<-data_or[which(data_or$year<2021),]

library(dplyr)
data_agg<-data_or %>% group_by(sender,receiver,year) %>% summarise_each(funs(sum))
data_agg<-data_agg[which(data_agg$sender %in% country_years$country),]
data_agg<-data_agg[which(data_agg$receiver %in% country_years$country),]
data_agg<-data_agg[-which(data_agg$sender==data_agg$receiver),]

data3<-merge(data2,data_agg,all.x=T,all.y=F)
data3$count[which(is.na((data3$count)))]<-0

sum(data3$count==0) #almost half
write.csv(data3,"network_data_by_country.csv",row.names = F)