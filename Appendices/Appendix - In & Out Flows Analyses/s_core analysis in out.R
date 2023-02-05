library(igraph)
library(brainGraph)
library(readr)
library(dplyr)


### IMPORT DATA ###
#network_data <- 
#data_trips_gdp_score_ted_edu <- 



# NUTS 2 K-CORE

# mean over years 
data <- network_data
data <- aggregate(count ~ sender+receiver, sum, data = data)

data_cov<-data_trips_gdp_score_ted_edu
data_cov$flow_tot<-data_cov$flow_in+data_cov$flow_out
data_cov<-aggregate(.~nuts2,mean,data=data_cov)
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

scores<-s.core_inout(data, mode = "in")
scores_tree <- merge(scores_tree, scores)
colnames(scores_tree) <- c("region", "s_core_all", "order_all", "s_core_in", "order_in")

scores<-s.core_inout(data, mode = "out")
scores_tree <- merge(scores_tree, scores)
colnames(scores_tree) <- c("region", "s_core_all", "order_all", "s_core_in", "order_in", "s_core_out", "order_out")

length(unique(scores_tree$order_out))

n_s<-scores_tree$region[which(scores_tree$order_all >= quantile(scores_tree$order_all, 0.89))]

setdiff(n_s, n_ha)
setdiff(n_ha, n_s)


### ALL ANALYSES

#plots 

scores_plot<-scores_tree

ggplot(scores_plot, aes(s_core_all))+xlab("s-core threshold") + geom_histogram(bins=294) + theme_bw()+ theme_classic()


scores_plot$count <- 0
for(i in 1:dim(scores_plot)[1]){
  scores_plot$count[i] <- sum(scores_plot$order_all == scores_plot$order_all[i])
}
plot(scores_plot$order_all, scores_plot$count)

scores_plot_clean<-scores_plot[-which(duplicated(scores_plot$order_all)),]

ggplot(scores_plot_clean, aes(x=order_all, y=count))+xlab("shell")+ylab("shell numerosity")+ geom_point() + theme_bw()+ theme_classic()


#year corresp 90%
core<-data.frame(vert=vert, cor=scores_tree$order_all)
core$mob<-"low"
core$mob[which(core$cor>quantile(core$cor, 0.9))]<-"high"

years <- 2009:2020

results <- data.frame(ugly_high = replicate(12,0), ugly_low = replicate(12,0), accuracy= replicate(12,0))

for(y in years){
  data_temp <- network_data[which(network_data$year==y),]
  data_temp <- aggregate(count ~ sender+receiver, sum, data = data_temp)
  data_gr_temp <- data_temp[-3]
  
  cor_temp<-s.core_inout(data_temp, mode = "all")$order
  vert_temp<-vert
  
  
  
  
  core_temp<-data.frame(vert=vert_temp, cor=cor_temp)
  
  core_temp$mob<-"low"
  core_temp$mob[which(core_temp$cor>quantile(core_temp$cor, 0.9))]<-"high"
  
  
  results$ugly_high[y-2008] <- sum(core_temp$vert[which(core_temp$mob=="high")] %in% core$vert[which(core$mob=="low")])
  results$ugly_low[y-2008] <- sum(core_temp$vert[which(core_temp$mob=="low")] %in% core$vert[which(core$mob=="high")])
  results$accuracy[y-2008] <- (sum(core_temp$vert[which(core_temp$mob=="high")] %in% core$vert[which(core$mob=="high")])+sum(core_temp$vert[which(core_temp$mob=="low")] %in% core$vert[which(core$mob=="low")]))/length(core$mob)
}
results

# ugly_high ugly_low  accuracy
#        16       16 0.8911565
#        19       19 0.8707483
#        20       20 0.8639456
#        19       19 0.8707483
#        17       17 0.8843537
#        20       20 0.8639456
#        18       18 0.8775510
#        17       17 0.8843537
#        18       18 0.8775510
#        20       20 0.8639456
#        20       20 0.8639456
#        23       23 0.8435374


# correspondence high hubs aut high score 


hubs<-hub_score(gr)
aut<-authority_score(gr)

nh<-names(sort(hubs$vector, decreasing=T))[1:50]
#"UKI0" "UKH1" "UKJ1" "PT16" "PT11" "UKM7" "UKG1" "ES30" "FR10" "ES51" "UKJ2" "UKD3" "UKF1" "UKE4" "ES61" "UKG3" "UKK1"
#"UKJ3" "PT17" "UKE3" "UKM8" "ITC4" "CH01" "UKD7" "ES52" "UKC2" "UKE2" "FRK2" "NL33" "ITI4" "IE06" "NL32" "UKL2" "ES41"
#"CH04" "UKK4" "UKK2" "ITI1" "UKJ4" "UKC1" "DE30" "UKL1" "DE12" "ES11" "DE21" "UKH2" "DK01" "SE11" "NL31" 

na<-names(sort(aut$vector, decreasing=T))[1:50]
#"UKI0" "UKJ1" "UKH1" "PT16" "PT11" "UKG1" "ES30" "ES51" "UKM7" "UKJ2" "UKD3" "FR10" "UKF1" "UKE4" "UKM8" "UKG3" "UKK1"
#"CH01" "ES61" "UKJ3" "UKD7" "UKK4" "NL33" "IE06" "UKE2" "UKE3" "PT17" "UKL2" "CH04" "ES52" "ITC4" "UKC2" "FRK2" "DE30"
#"NL32" "DK01" "ES41" "SE11" "ITI4" "DE21" "AT13" "UKJ4" "UKK2" "BE10" "ES11" "UKH3" "UKH2" "ES21" "UKL1" "DEA2"

nha<-union(na,nh)

ns<-scores_tree$region[which(scores_tree$order_all %in% sort(scores_tree$order_all, decreasing=T)[1:55])]
#"AT13" "CH01" "CH02" "CH03" "CH04" "DE12" "DE21" "DE30" "DEA2" "ES30" "ES41" "ES51" "ES52" "ES61" "FR10" "FRK2" "IE06" "ITC4" "ITH3"
#"ITH5" "ITI1" "ITI4" "NL22" "NL31" "NL32" "NL33" "PT11" "PT16" "PT17" "SE11" "SE12" "UKC2" "UKD3" "UKD7" "UKE2" "UKE3" "UKE4" "UKF1"
#"UKG1" "UKG3" "UKH1" "UKI0" "UKJ1" "UKJ2" "UKJ3" "UKK1" "UKK4" "UKL2" "UKM7" "UKM8"

setdiff(nha, ns) #"UKJ4" "UKK2" "UKH3" "UKH2" "ES21" "UKL1" "UKC1" "UKD4"
setdiff(ns, nha) #"BE24" "CH02" "CH03" "FRL0" "ITH3" "ITH5" "NL22" "SE12"

#top 10%
core_hub_aut<-data.frame(nuts2=vert,score_h=hubs$vector,score_a=aut$vector)
q10_h<-quantile(hubs$vector,0.90)
q10_a<-quantile(aut$vector,0.90)
core_hub_aut$mob<-"low"
core_hub_aut$mob[which(core_hub_aut$score_h>q10_h)]<-"high"
core_hub_aut$mob[which(core_hub_aut$score_a>q10_a)]<-"high"
data_clus_hub_aut<-merge(data_cov,core_hub_aut)
n_ha<-data_clus_hub_aut$nuts2[which(data_clus_hub_aut$mob=="high")]

n_s<-scores_tree$region[which(scores_tree$order_all %in% sort(scores_tree$order_all, decreasing=T)[1:34])]

setdiff(n_ha, n_s) #"CH04" "IE06" "UKK4" "UKL2"
setdiff(n_s, n_ha) #"ES41" "ITI1" "NL31" "NL32"

1-8/34


### IN ANALYSES

#plots 

scores_plot<-scores_tree

ggplot(scores_plot, aes(s_core_in))+xlab("s-core threshold") + geom_histogram(bins=294) + theme_bw()+ theme_classic()


scores_plot$count <- 0
for(i in 1:dim(scores_plot)[1]){
  scores_plot$count[i] <- sum(scores_plot$order_in == scores_plot$order_in[i])
}
plot(scores_plot$order_in, scores_plot$count)

scores_plot_clean<-scores_plot[-which(duplicated(scores_plot$order_in)),]

ggplot(scores_plot_clean, aes(x=order_in, y=count))+xlab("shell")+ylab("shell numerosity")+ geom_point() + theme_bw()+ theme_classic()


#year corresp 90%
core<-data.frame(vert=vert, cor=scores_tree$order_in)
core$mob<-"low"
core$mob[which(core$cor>quantile(core$cor, 0.9))]<-"high"

years <- 2009:2020

results <- data.frame(ugly_high = replicate(12,0), ugly_low = replicate(12,0), accuracy= replicate(12,0))

for(y in years){
  data_temp <- network_data[which(network_data$year==y),]
  data_temp <- aggregate(count ~ sender+receiver, sum, data = data_temp)
  data_gr_temp <- data_temp[-3]
  
  cor_temp<-s.core_inout(data_temp, mode = "in")$order
  vert_temp<-vert
  
  
  
  
  core_temp<-data.frame(vert=vert_temp, cor=cor_temp)
  
  core_temp$mob<-"low"
  core_temp$mob[which(core_temp$cor>quantile(core_temp$cor, 0.9))]<-"high"
  
  
  results$ugly_high[y-2008] <- sum(core_temp$vert[which(core_temp$mob=="high")] %in% core$vert[which(core$mob=="low")])
  results$ugly_low[y-2008] <- sum(core_temp$vert[which(core_temp$mob=="low")] %in% core$vert[which(core$mob=="high")])
  results$accuracy[y-2008] <- (sum(core_temp$vert[which(core_temp$mob=="high")] %in% core$vert[which(core$mob=="high")])+sum(core_temp$vert[which(core_temp$mob=="low")] %in% core$vert[which(core$mob=="low")]))/length(core$mob)
}
results

# ugly_high ugly_low  accuracy
#        14       15 0.9013605
#        19       19 0.8707483
#        18       20 0.8707483
#        18       18 0.8775510
#        17       17 0.8843537
#        19       19 0.8707483
#        18       18 0.8775510
#        18       18 0.8775510
#        20       20 0.8639456
#        19       19 0.8707483
#        20       21 0.8605442
#        24       24 0.8367347

# correspondence high hubs aut high score 


hubs<-hub_score(gr)
aut<-authority_score(gr)

nh<-names(sort(hubs$vector, decreasing=T))[1:50]
#"UKI0" "UKH1" "UKJ1" "PT16" "PT11" "UKM7" "UKG1" "ES30" "FR10" "ES51" "UKJ2" "UKD3" "UKF1" "UKE4" "ES61" "UKG3" "UKK1"
#"UKJ3" "PT17" "UKE3" "UKM8" "ITC4" "CH01" "UKD7" "ES52" "UKC2" "UKE2" "FRK2" "NL33" "ITI4" "IE06" "NL32" "UKL2" "ES41"
#"CH04" "UKK4" "UKK2" "ITI1" "UKJ4" "UKC1" "DE30" "UKL1" "DE12" "ES11" "DE21" "UKH2" "DK01" "SE11" "NL31" 

na<-names(sort(aut$vector, decreasing=T))[1:50]
#"UKI0" "UKJ1" "UKH1" "PT16" "PT11" "UKG1" "ES30" "ES51" "UKM7" "UKJ2" "UKD3" "FR10" "UKF1" "UKE4" "UKM8" "UKG3" "UKK1"
#"CH01" "ES61" "UKJ3" "UKD7" "UKK4" "NL33" "IE06" "UKE2" "UKE3" "PT17" "UKL2" "CH04" "ES52" "ITC4" "UKC2" "FRK2" "DE30"
#"NL32" "DK01" "ES41" "SE11" "ITI4" "DE21" "AT13" "UKJ4" "UKK2" "BE10" "ES11" "UKH3" "UKH2" "ES21" "UKL1" "DEA2"

nha<-union(na,nh)

ns<-scores_tree$region[which(scores_tree$order_in %in% sort(scores_tree$order_in, decreasing=T)[1:55])]
# "AT13" "BE10" "BE24" "CH01" "CH02" "CH03" "CH04" "DE12" "DE21" "DE30" "DEA2" "DK01" "ES11" "ES21" "ES30" "ES41" "ES51" "ES52" "ES61"
# "FR10" "FRK2" "FRL0" "IE06" "ITC4" "ITH5" "ITI1" "ITI4" "NL22" "NL31" "NL32" "NL33" "PT11" "PT16" "PT17" "SE11" "SE12" "UKC2" "UKD3"
# "UKD7" "UKE2" "UKE3" "UKE4" "UKF1" "UKG1" "UKG3" "UKH1" "UKI0" "UKJ1" "UKJ2" "UKJ3" "UKK1" "UKK4" "UKL2" "UKM7" "UKM8"

setdiff(nha, ns) #"UKJ4" "UKK2" "UKH3" "UKH2" "UKL1" "UKC1" "UKD4"
setdiff(ns, nha) #"BE24" "CH02" "CH03" "FRL0" "ITH5" "NL22" "SE12"

#top 10%
core_hub_aut<-data.frame(nuts2=vert,score_h=hubs$vector,score_a=aut$vector)
q10_h<-quantile(hubs$vector,0.90)
q10_a<-quantile(aut$vector,0.90)
core_hub_aut$mob<-"low"
core_hub_aut$mob[which(core_hub_aut$score_h>q10_h)]<-"high"
core_hub_aut$mob[which(core_hub_aut$score_a>q10_a)]<-"high"
data_clus_hub_aut<-merge(data_cov,core_hub_aut)
n_ha<-data_clus_hub_aut$nuts2[which(data_clus_hub_aut$mob=="high")]

n_s<-scores_tree$region[which(scores_tree$order_in %in% sort(scores_tree$order_in, decreasing=T)[1:34])]

setdiff(n_ha, n_s) #"CH04" "IE06" "ITC4" "ITI4" "UKC2"
setdiff(n_s, n_ha) #"ES41" "NL31" "NL32" "SE11" "SE12"

1-10/34


### OUT ANALYSES

#plots 

scores_plot<-scores_tree

ggplot(scores_plot, aes(s_core_out))+xlab("s-core threshold") + geom_histogram(bins=294) + theme_bw()+ theme_classic()


scores_plot$count <- 0
for(i in 1:dim(scores_plot)[1]){
  scores_plot$count[i] <- sum(scores_plot$order_out == scores_plot$order_out[i])
}
plot(scores_plot$order_out, scores_plot$count)

scores_plot_clean<-scores_plot[-which(duplicated(scores_plot$order_out)),]

ggplot(scores_plot_clean, aes(x=order_out, y=count))+xlab("shell")+ylab("shell numerosity")+ geom_point() + theme_bw()+ theme_classic()


#year corresp 90%
core<-data.frame(vert=vert, cor=scores_tree$order_out)
core$mob<-"low"
core$mob[which(core$cor>quantile(core$cor, 0.9))]<-"high"

years <- 2009:2020

results <- data.frame(ugly_high = replicate(12,0), ugly_low = replicate(12,0), accuracy= replicate(12,0))

for(y in years){
  data_temp <- network_data[which(network_data$year==y),]
  data_temp <- aggregate(count ~ sender+receiver, sum, data = data_temp)
  data_gr_temp <- data_temp[-3]
  
  cor_temp<-s.core_inout(data_temp, mode = "out")$order
  vert_temp<-vert
  
  
  
  
  core_temp<-data.frame(vert=vert_temp, cor=cor_temp)
  
  core_temp$mob<-"low"
  core_temp$mob[which(core_temp$cor>quantile(core_temp$cor, 0.9))]<-"high"
  
  
  results$ugly_high[y-2008] <- sum(core_temp$vert[which(core_temp$mob=="high")] %in% core$vert[which(core$mob=="low")])
  results$ugly_low[y-2008] <- sum(core_temp$vert[which(core_temp$mob=="low")] %in% core$vert[which(core$mob=="high")])
  results$accuracy[y-2008] <- (sum(core_temp$vert[which(core_temp$mob=="high")] %in% core$vert[which(core$mob=="high")])+sum(core_temp$vert[which(core_temp$mob=="low")] %in% core$vert[which(core$mob=="low")]))/length(core$mob)
}
results

# ugly_high ugly_low  accuracy
#        20       22 0.8571429
#        20       20 0.8639456
#        19       22 0.8605442
#        21       21 0.8571429
#        18       19 0.8741497
#        21       21 0.8571429
#        21       21 0.8571429
#        19       19 0.8707483
#        19       20 0.8673469
#        20       20 0.8639456
#        19       19 0.8707483
#        21       22 0.8537415

# correspondence high hubs aut high score 


hubs<-hub_score(gr)
aut<-authority_score(gr)

nh<-names(sort(hubs$vector, decreasing=T))[1:50]
#"UKI0" "UKH1" "UKJ1" "PT16" "PT11" "UKM7" "UKG1" "ES30" "FR10" "ES51" "UKJ2" "UKD3" "UKF1" "UKE4" "ES61" "UKG3" "UKK1"
#"UKJ3" "PT17" "UKE3" "UKM8" "ITC4" "CH01" "UKD7" "ES52" "UKC2" "UKE2" "FRK2" "NL33" "ITI4" "IE06" "NL32" "UKL2" "ES41"
#"CH04" "UKK4" "UKK2" "ITI1" "UKJ4" "UKC1" "DE30" "UKL1" "DE12" "ES11" "DE21" "UKH2" "DK01" "SE11" "NL31" 

na<-names(sort(aut$vector, decreasing=T))[1:50]
#"UKI0" "UKJ1" "UKH1" "PT16" "PT11" "UKG1" "ES30" "ES51" "UKM7" "UKJ2" "UKD3" "FR10" "UKF1" "UKE4" "UKM8" "UKG3" "UKK1"
#"CH01" "ES61" "UKJ3" "UKD7" "UKK4" "NL33" "IE06" "UKE2" "UKE3" "PT17" "UKL2" "CH04" "ES52" "ITC4" "UKC2" "FRK2" "DE30"
#"NL32" "DK01" "ES41" "SE11" "ITI4" "DE21" "AT13" "UKJ4" "UKK2" "BE10" "ES11" "UKH3" "UKH2" "ES21" "UKL1" "DEA2"

nha<-union(na,nh)

ns<-scores_tree$region[which(scores_tree$order_out %in% sort(scores_tree$order_out, decreasing=T)[1:55])]
#"AT13" "BE10" "BE24" "CH01" "CH04" "DE12" "DE21" "DE30" "DEA2" "DK01" "ES11" "ES30" "ES41" "ES51" "ES52" "ES61" "FR10" "FRK2" "IE06"
#"ITC4" "ITH3" "ITH5" "ITI1" "ITI4" "NL22" "NL31" "NL32" "NL33" "PT11" "PT16" "PT17" "SE11" "SE12" "UKC1" "UKC2" "UKD3" "UKD7" "UKE2"
#"UKE3" "UKE4" "UKF1" "UKG1" "UKG3" "UKH1" "UKI0" "UKJ1" "UKJ2" "UKJ3" "UKK1" "UKK2" "UKK4" "UKL1" "UKL2" "UKM7" "UKM8"

setdiff(nha, ns) #"UKJ4" "UKH3" "UKH2" "ES21" "UKD4"
setdiff(ns, nha) #"BE24" "ITH3" "ITH5" "NL22" "SE12"

#top 10%
core_hub_aut<-data.frame(nuts2=vert,score_h=hubs$vector,score_a=aut$vector)
q10_h<-quantile(hubs$vector,0.90)
q10_a<-quantile(aut$vector,0.90)
core_hub_aut$mob<-"low"
core_hub_aut$mob[which(core_hub_aut$score_h>q10_h)]<-"high"
core_hub_aut$mob[which(core_hub_aut$score_a>q10_a)]<-"high"
data_clus_hub_aut<-merge(data_cov,core_hub_aut)
n_ha<-data_clus_hub_aut$nuts2[which(data_clus_hub_aut$mob=="high")]

n_s<-scores_tree$region[which(scores_tree$order_out %in% sort(scores_tree$order_out, decreasing=T)[1:34])]

setdiff(n_ha, n_s) #"CH01" "CH04" "IE06" "UKK4" "UKL2"
setdiff(n_s, n_ha) #"ES41" "ITH3" "ITH5" "ITI1" "NL32"


