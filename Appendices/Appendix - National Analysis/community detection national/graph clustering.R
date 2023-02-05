library(igraph)

setwd("D:/Marta/Politecnico/Tesi")

library(readr)
network_data <- read_csv("NUTS 2 analysis/Network analysis/network_data.csv")
network_data_nuts1 <- read_csv("NUTS 1 data and analysis/network models/network_data_nuts1.csv")
network_data_by_country <- read_csv("National data and analysis/network models/network_data_by_country.csv")

### NUTS2 ### 

data<-network_data
data <- aggregate(count ~ sender+receiver, sum, data = data)

data_gr <- data[-3]
gr <- graph_from_edgelist(el=as.matrix(data_gr), directed=T)

cl<-cluster_infomap(graph=gr,e.weights = data$count)
cl$membership
cl$names

meb<-membership(cl)
sizes(cl)

nuts <- cl$names

nuts[which(meb==31)] #almost countries


#other cluster 
#cl_1<-cluster_edge_betweenness(graph=gr,weights = 1/(1+data$count),directed=T)
#cl_1$names[which(membership(cl_1)==1)]
#sizes(cl_1) #one only


### NUTS1 ### 

data1<-network_data_nuts1
data1 <- aggregate(count ~ sender+receiver, sum, data = data1)

data1_gr <- data1[-3]
gr1 <- graph_from_edgelist(el=as.matrix(data1_gr), directed=T)

cl1<-cluster_infomap(graph=gr1,e.weights = data1$count)
cl1$membership
cl1$names

meb1<-membership(cl1)
sizes(cl1)

nuts1 <- cl1$names

nuts1[which(meb1==15)] #interesting language/cultural groups more than real high/low mobility

#other cluster 
#cl1_1<-cluster_edge_betweenness(graph=gr1,weights = 1/(1+data1$count),directed=T)
#cl1_1$names[which(membership(cl1_1)==1)]
#sizes(cl1_1) #one only

### NUTS0 ### 

data0<-network_data_by_country
data0 <- aggregate(count ~ sender+receiver, sum, data = data0)

data0_gr <- data0[-3]
gr0 <- graph_from_edgelist(el=as.matrix(data0_gr), directed=T)

cl0<-cluster_infomap(graph=gr0,e.weights = data0$count)
cl0$membership
cl0$names

meb0<-membership(cl0)
sizes(cl0)

nuts0 <- cl0$names

nuts0[which(meb0==2)] #one only

#other cluster 
#cl0_1<-cluster_edge_betweenness(graph=gr0,weights = 1/(1+data0$count),directed=T)
#cl0_1$names[which(membership(cl0_1)==1)]
#sizes(cl0_1) #one only
