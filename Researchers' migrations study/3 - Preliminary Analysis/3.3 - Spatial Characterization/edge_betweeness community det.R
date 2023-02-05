library(igraph)
library(brainGraph)
library(readr)

### IMPORT DATA ###
#network_data <- ...
#data_trips_gdp_score_ted_edu <- ...
  
  
# NUTS 2 K-CORE, non normalized data

#cumulative net 
data <- network_data
data <- aggregate(count ~ sender+receiver, sum, data = data)

data_gr <- data[-3]
gr <- graph_from_edgelist(el=as.matrix(data_gr), directed=T)
clus<-cluster_edge_betweenness(graph=gr,weights = 1/(1+data$count),directed=T)
clus$edge.betweenness
clus$membership

##########

# year by year 
data_2009<-network_data[which(network_data$year=="2009"),c(1,2,4)]
data_2010<-network_data[which(network_data$year=="2010"),c(1,2,4)]
data_2011<-network_data[which(network_data$year=="2011"),c(1,2,4)]
data_2012<-network_data[which(network_data$year=="2012"),c(1,2,4)]
data_2013<-network_data[which(network_data$year=="2013"),c(1,2,4)]
data_2014<-network_data[which(network_data$year=="2014"),c(1,2,4)]
data_2015<-network_data[which(network_data$year=="2015"),c(1,2,4)]
data_2016<-network_data[which(network_data$year=="2016"),c(1,2,4)]
data_2017<-network_data[which(network_data$year=="2017"),c(1,2,4)]
data_2018<-network_data[which(network_data$year=="2018"),c(1,2,4)]
data_2019<-network_data[which(network_data$year=="2019"),c(1,2,4)]
data_2020<-network_data[which(network_data$year=="2020"),c(1,2,4)]

data_gr_2009 <- data_2009[-3]
gr_2009 <- graph_from_edgelist(el=as.matrix(data_gr_2009), directed=T)
clus_2009<-cluster_edge_betweenness(graph=gr_2009,weights = 1/(1+data_2009$count),directed=T)
clus_2009$edge.betweenness
clus_2009$membership

data_gr_2010 <- data_2010[-3]
gr_2010 <- graph_from_edgelist(el=as.matrix(data_gr_2010), directed=T)
clus_2010<-cluster_edge_betweenness(graph=gr_2010,weights = 1/(1+data_2010$count),directed=T)
clus_2010$edge.betweenness
clus_2010$membership

data_gr_2011 <- data_2011[-3]
gr_2011 <- graph_from_edgelist(el=as.matrix(data_gr_2011), directed=T)
clus_2011<-cluster_edge_betweenness(graph=gr_2011,weights = 1/(1+data_2011$count),directed=T)
clus_2011$edge.betweenness
clus_2011$membership

data_gr_2012 <- data_2012[-3]
gr_2012 <- graph_from_edgelist(el=as.matrix(data_gr_2012), directed=T)
clus_2012<-cluster_edge_betweenness(graph=gr_2012,weights = 1/(1+data_2012$count),directed=T)
clus_2012$edge.betweenness
clus_2012$membership

data_gr_2013 <- data_2013[-3]
gr_2013 <- graph_from_edgelist(el=as.matrix(data_gr_2013), directed=T)
clus_2013<-cluster_edge_betweenness(graph=gr_2013,weights = 1/(1+data_2013$count),directed=T)
clus_2013$edge.betweenness
clus_2013$membership

data_gr_2014 <- data_2014[-3]
gr_2014 <- graph_from_edgelist(el=as.matrix(data_gr_2014), directed=T)
clus_2014<-cluster_edge_betweenness(graph=gr_2014,weights = 1/(1+data_2014$count),directed=T)
clus_2014$edge.betweenness
clus_2014$membership

data_gr_2015 <- data_2015[-3]
gr_2015 <- graph_from_edgelist(el=as.matrix(data_gr_2015), directed=T)
clus_2015<-cluster_edge_betweenness(graph=gr_2015,weights = 1/(1+data_2015$count),directed=T)
clus_2015$edge.betweenness
clus_2015$membership

data_gr_2016 <- data_2016[-3]
gr_2016 <- graph_from_edgelist(el=as.matrix(data_gr_2016), directed=T)
clus_2016<-cluster_edge_betweenness(graph=gr_2016,weights = 1/(1+data_2016$count),directed=T)
clus_2016$edge.betweenness
clus_2016$membership

data_gr_2017 <- data_2017[-3]
gr_2017 <- graph_from_edgelist(el=as.matrix(data_gr_2017), directed=T)
clus_2017<-cluster_edge_betweenness(graph=gr_2017,weights = 1/(1+data_2017$count),directed=T)
clus_2017$edge.betweenness
clus_2017$membership

data_gr_2018 <- data_2018[-3]
gr_2018 <- graph_from_edgelist(el=as.matrix(data_gr_2018), directed=T)
clus_2018<-cluster_edge_betweenness(graph=gr_2018,weights = 1/(1+data_2018$count),directed=T)
clus_2018$edge.betweenness
clus_2018$membership

data_gr_2019 <- data_2019[-3]
gr_2019 <- graph_from_edgelist(el=as.matrix(data_gr_2019), directed=T)
clus_2019<-cluster_edge_betweenness(graph=gr_2019,weights = 1/(1+data_2019$count),directed=T)
clus_2019$edge.betweenness
clus_2019$membership

data_gr_2020 <- data_2020[-3]
gr_2020 <- graph_from_edgelist(el=as.matrix(data_gr_2020), directed=T)
clus_2020<-cluster_edge_betweenness(graph=gr_2020,weights = 1/(1+data_2020$count),directed=T)
clus_2020$edge.betweenness
clus_2020$membership