
library(igraph)
library(brainGraph)
library(readr)


network_data_by_country <- read_csv("D:/Marta/Politecnico/Tesi/National data and analysis/network models/network_data_by_country.csv")

data_by_country <- read_csv("D:/Marta/Politecnico/Tesi/National data and analysis/data_by_country.csv")

network_data <- network_data_by_country
data_trips_gdp_score_ted_edu <- data_by_country



# mean over years 
data <- network_data
data <- aggregate(count ~ sender+receiver, sum, data = data)

data_gr <- data[-3]
gr <- graph_from_edgelist(el=as.matrix(data_gr), directed=T)

set.seed(27011999)
cl <- cluster_infomap(graph = gr, e.weights = data$count, nb.trials = 100)


nuts <- cl$names
m<-membership(cl)
sizes(cl)
length(unique(m)) #1 > one community found 
