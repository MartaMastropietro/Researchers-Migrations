#hubs and aut nuts0

library(igraph)
library(brainGraph)
library(readr)
library(ggplot2)
library("ggpubr")

network_data_by_country <- read_csv("D:/Marta/Politecnico/Tesi/National data and analysis/network models/network_data_by_country.csv")

data_by_country <- read_csv("D:/Marta/Politecnico/Tesi/National data and analysis/data_by_country.csv")


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

hubs<-hub_score(gr)
aut<-authority_score(gr)

scores<-data.frame(hubs=hubs$vector,authorities=aut$vector) 

vert<-V(gr)
vert<-names(vert)

cor(aut$vector,hubs$vector) #0.9623466
ggplot(scores, aes(x=hubs, y=authorities))+ geom_point() + theme_bw()+ theme_classic()

#plot
h<-ggplot(scores, aes(hubs)) + geom_histogram(bins=31) + theme_bw()+ theme_classic()
a<-ggplot(scores, aes(authorities)) + geom_histogram(bins = 31) + theme_bw()+ theme_classic()
ggarrange(h,a)


#test partition 0.75
core_hub_aut<-data.frame(country=vert,score_h=hubs$vector,score_a=aut$vector)
q25_h<-quantile(hubs$vector,0.75)
q25_a<-quantile(aut$vector,0.75)
core_hub_aut$mob<-"low"
core_hub_aut$mob[which(core_hub_aut$score_h>q25_h)]<-"high"
core_hub_aut$mob[which(core_hub_aut$score_a>q25_a)]<-"high"
data_clus_hub_aut<-merge(data_cov,core_hub_aut)
n_ha<-data_clus_hub_aut$country[which(data_clus_hub_aut$mob=="high")]
# "CH" "DE" "ES" "FR" "IT" "NL" "PT" "SE" "UK"

aov_flux_hub_aut<-anova_npc(data_clus_hub_aut$flow_tot,data_clus_hub_aut$mob,B=1000) #0, 0
aov_score_hub_aut<-anova_npc(data_clus_hub_aut$score,data_clus_hub_aut$mob,B=1000) #0, 0.01
aov_edu_hub_aut<-anova_npc(data_clus_hub_aut$edu_index,data_clus_hub_aut$mob,B=1000)#0.088, 0.465
aov_gdp_hub_aut<-anova_npc(data_clus_hub_aut$gdp,data_clus_hub_aut$mob,B=1000)#0.334, 0.498
aov_ted_hub_aut<-anova_npc(data_clus_hub_aut$ted,data_clus_hub_aut$mob,B=1000)#0.002 , 0.946


#same as score?
scores<-s.core_inout(data, mode = "all")

n_s<-scores$region[which(scores$order %in% sort(scores$order, decreasing=T)[1:9])]

# "BE" "CH" "DE" "ES" "FR" "IT" "NL" "PT" "UK"

setdiff(n_ha,n_s) # "SE" in hubs and aut but not scoreness
setdiff(n_s,n_ha) #"BE" 



# time omog

results_hubs <- data.frame(ugly_high = replicate(12,0), ugly_low = replicate(12,0), accuracy= replicate(12,0))
results_aut <- data.frame(ugly_high = replicate(12,0), ugly_low = replicate(12,0), accuracy= replicate(12,0))

core_hubs<-data.frame(vert=vert, cor=hubs$vector)
med_hubs<-quantile(core_hubs$cor,0.75)
core_hubs$mob<-"low"
core_hubs$mob[which(core_hubs$cor>med_hubs)]<-"high"

core_aut<-data.frame(vert=vert, cor=aut$vector)
med_aut<-quantile(core_aut$cor,0.75)
core_aut$mob<-"low"
core_aut$mob[which(core_aut$cor>med_aut)]<-"high"

corr=rep(0,12)

years=2009:2020
for(y in years){
  
  data_temp <- network_data_by_country[which(network_data_by_country$year==y),]
  data_temp <- aggregate(count ~ sender+receiver, sum, data = data_temp)
  data_gr_temp <- data_temp[-3]
  gr_temp <- graph_from_edgelist(el=as.matrix(data_gr_temp), directed=T)
  
  
  E(gr_temp)$weight<-data_temp$count
  mat_temp<-as_adjacency_matrix(gr_temp,attr="weight")
  
  
  hubs_temp<-hub_score(gr_temp)$vector
  aut_temp<-authority_score(gr_temp)$vector
  
  corr[y-2008]<-cor(hubs_temp,aut_temp)
  vert_temp<-V(gr_temp)
  vert_temp<-names(vert_temp)
  
  
  core_temp_hubs<-data.frame(vert=vert_temp, cor=hubs_temp)
  med_temp_hubs<-quantile(hubs_temp,0.75)
  core_temp_hubs$mob<-"low"
  core_temp_hubs$mob[which(core_temp_hubs$cor>med_temp_hubs)]<-"high"
  
  results_hubs$ugly_high[y-2008] <- sum(core_temp_hubs$vert[which(core_temp_hubs$mob=="high")] %in% core_hubs$vert[which(core_hubs$mob=="low")])
  results_hubs$ugly_low[y-2008] <- sum(core_temp_hubs$vert[which(core_temp_hubs$mob=="low")] %in% core_hubs$vert[which(core_hubs$mob=="high")])
  results_hubs$accuracy[y-2008] <- (sum(core_temp_hubs$vert[which(core_temp_hubs$mob=="high")] %in% core_hubs$vert[which(core_hubs$mob=="high")])+sum(core_temp_hubs$vert[which(core_temp_hubs$mob=="low")] %in% core_hubs$vert[which(core_hubs$mob=="low")]))/length(core_hubs$mob)
  
  core_temp_aut<-data.frame(vert=vert_temp, cor=aut_temp)
  med_temp_aut<-quantile(aut_temp,0.75)
  core_temp_aut$mob<-"low"
  core_temp_aut$mob[which(core_temp_aut$cor>med_temp_aut)]<-"high"
  
  results_aut$ugly_high[y-2008] <- sum(core_temp_aut$vert[which(core_temp_aut$mob=="high")] %in% core_aut$vert[which(core_aut$mob=="low")])
  results_aut$ugly_low[y-2008] <- sum(core_temp_aut$vert[which(core_temp_aut$mob=="low")] %in% core_aut$vert[which(core_aut$mob=="high")])
  results_aut$accuracy[y-2008] <- (sum(core_temp_aut$vert[which(core_temp_aut$mob=="high")] %in% core_aut$vert[which(core_aut$mob=="high")])+sum(core_temp_aut$vert[which(core_temp_aut$mob=="low")] %in% core_aut$vert[which(core_aut$mob=="low")]))/length(core_aut$mob)
  
}
results_hubs

# ugly_high ugly_low  accuracy
# 1          0        0 1.0000000
# 2          0        0 1.0000000
# 3          0        0 1.0000000
# 4          0        0 1.0000000
# 5          0        0 1.0000000
# 6          0        0 1.0000000
# 7          0        0 1.0000000
# 8          0        0 1.0000000
# 9          0        0 1.0000000
# 10         0        0 1.0000000
# 11         1        1 0.9354839
# 12         1        1 0.9354839

results_aut

# ugly_high ugly_low  accuracy
# 1          1        1 0.9354839
# 2          2        2 0.8709677
# 3          1        1 0.9354839
# 4          2        2 0.8709677
# 5          1        1 0.9354839
# 6          1        1 0.9354839
# 7          1        1 0.9354839
# 8          0        0 1.0000000
# 9          1        1 0.9354839
# 10         0        0 1.0000000
# 11         1        1 0.9354839
# 12         1        1 0.9354839
