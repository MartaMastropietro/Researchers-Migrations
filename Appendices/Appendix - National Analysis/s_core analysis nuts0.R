library(igraph)
library(brainGraph)
library(readr)
library(ggplot2)

network_data_by_country <- read_csv("D:/Marta/Politecnico/Tesi/National data and analysis/network models/network_data_by_country.csv")

data_by_country <- read_csv("D:/Marta/Politecnico/Tesi/National data and analysis/data_by_country.csv")


data <- network_data_by_country
data <- aggregate(count ~ sender+receiver, sum, data = data)


data_cov<-data_by_country
data_cov$flow_tot<-data_cov$flow_in+data_cov$flow_out
data_cov<-aggregate(.~country,mean,data=data_cov)
data_cov<-data_cov[,-2]

data_gr <- data[-3]
gr <- graph_from_edgelist(el=as.matrix(data_gr), directed=T)
vert<-V(gr)
vert<-names(vert)

E(gr)$weight<-data$count
mat<-as_adjacency_matrix(gr,attr="weight")


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

# scores<-s.core_inout(data, mode = "in")
# scores_tree <- merge(scores_tree, scores)
# colnames(scores_tree) <- c("region", "s_core_all", "order_all", "s_core_in", "order_in")
# 
# scores<-s.core_inout(data, mode = "out")
# scores_tree <- merge(scores_tree, scores)
# colnames(scores_tree) <- c("region", "s_core_all", "order_all", "s_core_in", "order_in", "s_core_out", "order_out")


#plots 

scores_plot<-scores_tree

ggplot(scores_plot, aes(s_core_all))+xlab("s-core threshold") + geom_histogram(bins=31) + theme_bw()+ theme_classic()


scores_plot$count <- 0
for(i in 1:dim(scores_plot)[1]){
  scores_plot$count[i] <- sum(scores_plot$order_all == scores_plot$order_all[i])
}
plot(scores_plot$order_all, scores_plot$count)

scores_plot_clean<-scores_plot[-which(duplicated(scores_plot$order_all)),]

ggplot(scores_plot_clean, aes(x=order_all, y=count))+xlab("shell")+ylab("shell numerosity")+ geom_point() + theme_bw()+ theme_classic()


#year corresp 75%
core<-data.frame(vert=vert, cor=scores_tree$order_all)
core$mob<-"low"
core$mob[which(core$cor>quantile(core$cor, 0.75))]<-"high"

years <- 2009:2020

results <- data.frame(ugly_high = replicate(12,0), ugly_low = replicate(12,0), accuracy= replicate(12,0))

for(y in years){
  data_temp <- network_data_by_country[which(network_data_by_country$year==y),]
  data_temp <- aggregate(count ~ sender+receiver, sum, data = data_temp)
  data_gr_temp <- data_temp[-3]
  
  cor_temp<-s.core_inout(data_temp, mode = "all")$order
  vert_temp<-vert
  
  core_temp<-data.frame(vert=vert_temp, cor=cor_temp)
  
  core_temp$mob<-"low"
  core_temp$mob[which(core_temp$cor>quantile(core_temp$cor, 0.75))]<-"high"
  
  
  results$ugly_high[y-2008] <- sum(core_temp$vert[which(core_temp$mob=="high")] %in% core$vert[which(core$mob=="low")])
  results$ugly_low[y-2008] <- sum(core_temp$vert[which(core_temp$mob=="low")] %in% core$vert[which(core$mob=="high")])
  results$accuracy[y-2008] <- (sum(core_temp$vert[which(core_temp$mob=="high")] %in% core$vert[which(core$mob=="high")])+sum(core_temp$vert[which(core_temp$mob=="low")] %in% core$vert[which(core$mob=="low")]))/length(core$mob)
}
results

# ugly_high ugly_low  accuracy
#         1        1 0.9354839
#         1        1 0.9354839
#         1        1 0.9354839
#         1        1 0.9354839
#         1        1 0.9354839
#         1        1 0.9354839
#         1        1 0.9354839
#         1        1 0.9354839
#         0        0 1.0000000
#         0        0 1.0000000
#         0        0 1.0000000
#         0        0 1.0000000


#old
#####

# mean over years 
data <- network_data_by_country
data <- aggregate(count ~ sender+receiver, sum, data = data)

data_cov<-data_by_country
data_cov$flow_tot<-data_cov$flow_in+data_cov$flow_out
data_cov<-aggregate(.~country,mean,data=data_cov)
data_cov<-data_cov[,-2]

data_gr <- data[-3]
gr <- graph_from_edgelist(el=as.matrix(data_gr), directed=T)

E(gr)$weight<-data$count
mat<-as_adjacency_matrix(gr,attr="weight")

cor<-s_core(g=gr,W=mat)
max(cor)
vert<-V(gr)
vert<-names(vert)

core<-data.frame(country=vert, cor=cor)
q<-quantile(cor, 0.9)
core$mob<-"low"
core$mob[which(core$cor>q)]<-"high"
boxplot(core$cor~core$mob)

data_clus<-merge(data_cov,core)

aov_score<-anova_combination(data_clus$score,data_clus$mob,B=1000) #0.001

aov_edu<-anova_combination(data_clus$edu,data_clus$mob,B=1000)#0.01

aov_gdp<-anova_combination(data_clus$ted,data_clus$mob,B=1000)#0.001

aov_ted<-anova_combination(data_clus$ted,data_clus$mob,B=1000)#0.001


# year by year, equivalence in time? 

core<-data.frame(vert=vert, cor=cor)
q<-quantile(cor, 0.9)
core$mob<-"low"
core$mob[which(core$cor>med)]<-"high"

years <- 2009:2020

results <- data.frame(ugly_high = replicate(12,0), ugly_low = replicate(12,0), accuracy= replicate(12,0))

for(y in years){
  data_temp <- network_data_by_country[which(network_data_by_country$year==y),]
  data_temp <- aggregate(count ~ sender+receiver, sum, data = data_temp)
  data_gr_temp <- data_temp[-3]
  gr_temp <- graph_from_edgelist(el=as.matrix(data_gr_temp), directed=T)
  
  
  E(gr_temp)$weight<-data_temp$count
  mat_temp<-as_adjacency_matrix(gr_temp,attr="weight")
  
  
  
  cor_temp<-s_core(g=gr_temp,W=mat_temp)
  vert_temp<-V(gr_temp)
  vert_temp<-names(vert_temp)
  
  
  
  core_temp<-data.frame(vert=vert_temp, cor=cor_temp)
  med_temp<-median(cor_temp)
  core_temp$mob<-"low"
  core_temp$mob[which(core_temp$cor>med_temp)]<-"high"
  
  
  results$ugly_high[y-2008] <- sum(core_temp$vert[which(core_temp$mob=="high")] %in% core$vert[which(core$mob=="low")])
  results$ugly_low[y-2008] <- sum(core_temp$vert[which(core_temp$mob=="low")] %in% core$vert[which(core$mob=="high")])
  results$accuracy[y-2008] <- (sum(core_temp$vert[which(core_temp$mob=="high")] %in% core$vert[which(core$mob=="high")])+sum(core_temp$vert[which(core_temp$mob=="low")] %in% core$vert[which(core$mob=="low")]))/length(core$mob)
}
results

# ugly_high ugly_low  accuracy
#         2        2 0.8709677
#         2        2 0.8709677
#         3        3 0.8064516
#         0        0 1.0000000
#         1        1 0.9354839
#         2        2 0.8709677
#         1        1 0.9354839
#         1        1 0.9354839
#         1        1 0.9354839
#         0        0 1.0000000
#         1        1 0.9354839
#         0        0 1.0000000
# 


#### 


