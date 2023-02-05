# plots hubs authorities and s-cores


library(readr)
library(igraph)
library(brainGraph)
library(networkR)
library(ggplot2)
library("ggpubr")


### IMPORT DATA ###
#data_trips_gdp_score_ted_edu <- ...
#network_data <- ...


### cumulative data over years 
data <- network_data
data <- aggregate(count ~ sender+receiver, sum, data = data)
data_cov<-data_trips_gdp_score_ted_edu
data_cov$flow_tot<-data_cov$flow_in+data_cov$flow_out
data_cov<-aggregate(.~nuts2,mean,data=data_cov)
data_cov<-data_cov[,-2]
data_gr <- data[-3]
gr <- graph_from_edgelist(el=as.matrix(data_gr), directed=T)
E(gr)$weight<-data$count
vert<-V(gr)
vert<-names(vert)
mat<-as_adjacency_matrix(gr,attr="weight")



# hubs aut 

hubs<-hub_score(gr)
aut<-authority_score(gr)

vert<-V(gr)
vert<-names(vert)

#plots 

scores<-data.frame(hubs=hubs$vector,authorities=aut$vector)

# hubs aut scores correspondance plot
ggplot(scores, aes(x=hubs, y=authorities))+ geom_point() + theme_bw()+ theme_classic()


# scores


s.core_inout <- function (data_sender_receiver_count, mode) 
{
  colnames(data_sender_receiver_count) <- c("send", "rec", "count")
  data <- data_sender_receiver_count
  s.core <- data.frame(region = unique(union(data$send, data$rec)), s.core = replicate(length(unique(union(data$send, data$rec))), 0), order =   replicate(length(unique(union(data$send, data$rec))), 0))
  iter <- 1
  contr <- length(unique(union(data$send, data$rec)))
  repeat {
    if(mode == "in"){
      str.tmp <- aggregate(count ~ rec, FUN = sum, data = data)
      colnames(str.tmp)[which(colnames(str.tmp)=="rec")] <- "region"
      str.tmp <- str.tmp[-which(str.tmp$count == 0),]
    }
    else if(mode == "out"){
      str.tmp <- aggregate(count ~ send, FUN = sum, data = data)
      colnames(str.tmp)[which(colnames(str.tmp)=="send")] <- "region"
    }
    else{
      str.tmp <- data.frame(region = unique(union(data$send, data$rec)), count = replicate(length(unique(union(data$send, data$rec))), 0))
      str.tmp <- str.tmp[order(str.tmp$region),]
      str.tmp_rec <- aggregate(count ~ rec, FUN = sum, data = data)
      str.tmp_rec <- str.tmp_rec[order(str.tmp_rec$rec),]
      str.tmp$count[which(str.tmp$region %in% str.tmp_rec$rec)] <- str.tmp_rec$count
      str.tmp_send <- aggregate(count ~ send, FUN = sum, data = data)
      str.tmp_send <- str.tmp_send[order(str.tmp_send$send),]
      str.tmp$count[which(str.tmp$region %in% str.tmp_send$send)] <- str.tmp$count[which(str.tmp$region %in% str.tmp_send$send)] + str.tmp_send$count
    }
    s.thr <- min(str.tmp$count[which(str.tmp$count > 0)])
    v.remove <- str.tmp$region[which(str.tmp$count <= s.thr & str.tmp$count > 0)]
    if (length(v.remove) > 0L) {
      other_regions <- str.tmp$region[-which(str.tmp$region %in% v.remove)]
      other_regions <- other_regions[which(other_regions %in% str.tmp$region[which(str.tmp$count > 0)])]
      s.core$s.core[which(s.core$region %in% v.remove)] <- s.thr
      s.core$order[which(s.core$region %in% v.remove)] <- iter
      data <- data[-which(data$send %in% v.remove),]
      data <- data[-which(data$rec %in% v.remove),]
      iter <- iter+1
    }
    if (sum(data$count) == 0L) {
      s.core$s.core[which(s.core$region %in% other_regions)] <- s.thr
      s.core$order[which(s.core$region %in% other_regions)] <- iter-1
      break
    }
  }
  return(s.core)
}


scores<-s.core_inout(data, mode = "all")
scores_tree <- scores
colnames(scores_tree) <- c("region", "s_core_all", "order_all")

scores<-s.core_inout(data, mode = "in")
scores_tree <- merge(scores_tree, scores)
colnames(scores_tree) <- c("region", "s_core_all", "order_all", "s_core_in", "order_in")

scores<-s.core_inout(data, mode = "out")
scores_tree <- merge(scores_tree, scores)
colnames(scores_tree) <- c("region", "s_core_all", "order_all", "s_core_in", "order_in", "s_core_out", "order_out")


#plots 

scores_plot<-scores_tree

# thresholds plot
ggplot(scores_plot, aes(s_core_all))+xlab("s-core threshold") + geom_histogram(bins=294) + theme_bw()+ theme_classic()


scores_plot$count <- 0
for(i in 1:dim(scores_plot)[1]){
  scores_plot$count[i] <- sum(scores_plot$order_all == scores_plot$order_all[i])
}
plot(scores_plot$order_all, scores_plot$count)

scores_plot_clean<-scores_plot[-which(duplicated(scores_plot$order_all)),]

# shell numerosity plot
ggplot(scores_plot_clean, aes(x=order_all, y=count))+xlab("shell")+ylab("shell numerosity")+ geom_point() + theme_bw()+ theme_classic()

