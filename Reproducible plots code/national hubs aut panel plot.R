#hubs and authorities scores plots NATIONAL

library(voxel)
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(ggplotify)
library(igraph)
library(brainGraph)
library(readr)


### IMPORT DATA ###
#network_data_by_country <- ...

data <- network_data_by_country
data <- aggregate(count ~ sender+receiver, sum, data = data)

#define graph
data_gr <- data[-3]
gr <- graph_from_edgelist(el=as.matrix(data_gr), directed=T)
E(gr)$weight<-data$count

#define weights
hubs<-hub_score(gr)
aut<-authority_score(gr)
scores<-data.frame(hubs=hubs$vector,authorities=aut$vector) 


#plot
vars <- c("foobar1", "foobar2")
map(vars, function(x){
  if(x == "foobar1"){
    p <-ggplot(scores, aes(hubs)) + geom_histogram(bins=31) + theme_bw()+ theme_classic() + ggtitle("A") + xlab("Hub score") + ylab("Count") + scale_fill_grey()
    g <- ggplotGrob(p)
  }
  else{
    p <-ggplot(scores, aes(authorities)) + geom_histogram(bins=31) + theme_bw()+ theme_classic() + ggtitle("B") + xlab("Authority score") + ylab("Count") + scale_fill_grey()
    g <- ggplotGrob(p)
  }
}) %>%
  {grid.arrange(grobs = (.), ncol = 2, nrow = 1)}
