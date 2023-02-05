library(igraph)

set.seed(27011999)

### IMPORT DATA ###
#network_data <- ...
#data_trips_gdp_score_ted_edu <- ...

#cumulative net
data <- network_data
data <- aggregate(count ~ sender+receiver, sum, data = data)

data_g <- data[,-3]
graph <- graph_from_edgelist(el=as.matrix(data_g), directed=T)
cl <- cluster_infomap(graph = graph, e.weights = data$count, nb.trials = 100)
length(cl)
nuts <- cl$names
m<-membership(cl)


#GROUP VISUALIZATION
nuts[which(m==1)]
nuts[which(m==2)]
nuts[which(m==3)]
nuts[which(m==4)]
nuts[which(m==5)]
nuts[which(m==6)]
nuts[which(m==7)]
nuts[which(m==8)]
nuts[which(m==9)]
nuts[which(m==10)]
nuts[which(m==11)]
nuts[which(m==12)]
nuts[which(m==13)]
nuts[which(m==14)]
nuts[which(m==15)]
nuts[which(m==16)]
nuts[which(m==17)]
nuts[which(m==18)]
nuts[which(m==19)]
nuts[which(m==20)]
nuts[which(m==21)]
nuts[which(m==22)]
nuts[which(m==23)]
nuts[which(m==24)]
nuts[which(m==25)]
nuts[which(m==26)]
nuts[which(m==27)]
nuts[which(m==28)]
nuts[which(m==29)]
nuts[which(m==30)]

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
cl_2009<-cluster_infomap(graph = gr_2009, e.weights = data_2009$count, nb.trials = 100)
length(cl_2009) 
m_2009<-membership(cl_2009)

data_gr_2010 <- data_2010[-3]
gr_2010 <- graph_from_edgelist(el=as.matrix(data_gr_2010), directed=T)
cl_2010<-cluster_infomap(graph = gr_2010, e.weights = data_2010$count, nb.trials = 100)
length(cl_2010) 
m_2010<-membership(cl_2010)

data_gr_2011 <- data_2011[-3]
gr_2011 <- graph_from_edgelist(el=as.matrix(data_gr_2011), directed=T)
cl_2011<-cluster_infomap(graph = gr_2011, e.weights = data_2011$count, nb.trials = 100)
length(cl_2011) 
m_2011<-membership(cl_2011)

data_gr_2012 <- data_2012[-3]
gr_2012 <- graph_from_edgelist(el=as.matrix(data_gr_2012), directed=T)
cl_2012<-cluster_infomap(graph = gr_2012, e.weights = data_2012$count, nb.trials = 100)
length(cl_2012) 
m_2012<-membership(cl_2012)

data_gr_2013 <- data_2013[-3]
gr_2013 <- graph_from_edgelist(el=as.matrix(data_gr_2013), directed=T)
cl_2013<-cluster_infomap(graph = gr_2013, e.weights = data_2013$count, nb.trials = 100)
length(cl_2013) 
m_2013<-membership(cl_2013)

data_gr_2014 <- data_2014[-3]
gr_2014 <- graph_from_edgelist(el=as.matrix(data_gr_2014), directed=T)
cl_2014<-cluster_infomap(graph = gr_2014, e.weights = data_2014$count, nb.trials = 100)
length(cl_2014) 
m_2014<-membership(cl_2014)

data_gr_2015 <- data_2015[-3]
gr_2015 <- graph_from_edgelist(el=as.matrix(data_gr_2015), directed=T)
cl_2015<-cluster_infomap(graph = gr_2015, e.weights = data_2015$count, nb.trials = 100)
length(cl_2015) 
m_2015<-membership(cl_2015)

data_gr_2016 <- data_2016[-3]
gr_2016 <- graph_from_edgelist(el=as.matrix(data_gr_2016), directed=T)
cl_2016<-cluster_infomap(graph = gr_2016, e.weights = data_2016$count, nb.trials = 100)
length(cl_2016) 
m_2016<-membership(cl_2016)

data_gr_2017 <- data_2017[-3]
gr_2017 <- graph_from_edgelist(el=as.matrix(data_gr_2017), directed=T)
cl_2017<-cluster_infomap(graph = gr_2017, e.weights = data_2017$count, nb.trials = 100)
length(cl_2017) 
m_2017<-membership(cl_2017)

data_gr_2018 <- data_2018[-3]
gr_2018 <- graph_from_edgelist(el=as.matrix(data_gr_2018), directed=T)
cl_2018<-cluster_infomap(graph = gr_2018, e.weights = data_2018$count, nb.trials = 100)
length(cl_2018) 
m_2018<-membership(cl_2018)

data_gr_2019 <- data_2019[-3]
gr_2019 <- graph_from_edgelist(el=as.matrix(data_gr_2019), directed=T)
cl_2019<-cluster_infomap(graph = gr_2019, e.weights = data_2019$count, nb.trials = 100)
length(cl_2019) 
m_2019<-membership(cl_2019)

data_gr_2020 <- data_2020[-3]
gr_2020 <- graph_from_edgelist(el=as.matrix(data_gr_2020), directed=T)
cl_2020<-cluster_infomap(graph = gr_2020, e.weights = data_2020$count, nb.trials = 100)
length(cl_2020) 
m_2020<-membership(cl_2020)

