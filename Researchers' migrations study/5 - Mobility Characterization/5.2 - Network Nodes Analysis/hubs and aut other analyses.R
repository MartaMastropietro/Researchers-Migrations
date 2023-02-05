#hubs and aut other analyses 

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

#hubs aut 

hubs<-hub_score(gr)
aut<-authority_score(gr)

scores<-data.frame(hubs=hubs$vector,authorities=aut$vector) 

vert<-V(gr)
vert<-names(vert)

## robust hubs aut 
out1<- c("ES30" ,"ES51", "FR10", "PT11", "PT16", "UKD3", "UKF1" ,"UKG1", "UKH1", "UKI0", "UKJ1", "UKJ2" ,"UKM7")
data <- network_data
data <- aggregate(count ~ sender+receiver, sum, data = data)
data1<-data[-union(which(data$sender %in% out1), which(data$receiver %in% out1)),]
data_gr1 <- data1[-3]
gr1 <- graph_from_edgelist(el=as.matrix(data_gr1), directed=T)
E(gr1)$weight<-data1$count

hubs1<-hub_score(gr1)
aut1<-authority_score(gr1)
scores1<-data.frame(hubs=hubs1$vector,authorities=aut1$vector)

cor(hubs1$vector, aut1$vector) #0.9785466

h1<-ggplot(scores1, aes(hubs)) + geom_histogram(bins=294) + theme_bw()+ theme_classic()
a1<-ggplot(scores1, aes(authorities)) + geom_histogram(bins = 294) + theme_bw()+ theme_classic()
ggarrange(h1,a1)

scores1<-s.core_inout(data1, mode = "all")
scores_tree1 <- scores1
colnames(scores_tree1) <- c("region", "s_core_all", "order_all")

scores1<-s.core_inout(data1, mode = "in")
scores_tree1 <- merge(scores_tree1, scores1)
colnames(scores_tree1) <- c("region", "s_core_all", "order_all", "s_core_in", "order_in")

scores1<-s.core_inout(data1, mode = "out")
scores_tree1 <- merge(scores_tree1, scores1)
colnames(scores_tree1) <- c("region", "s_core_all", "order_all", "s_core_in", "order_in", "s_core_out", "order_out")

n_s<-scores_tree1$region[which(scores_tree1$order_all >= quantile(scores_tree1$order_all, 0.89))]

nh1<-names(which(hubs1$vector >= quantile(hubs1$vector, 0.9)))
#"UKI0" "UKH1" "UKJ1" "PT16" "PT11" "UKM7" "UKG1" "ES30" "FR10" "ES51" "UKJ2" "UKD3" "UKF1" "UKE4" "ES61" "UKG3" "UKK1"
#"UKJ3" "PT17" "UKE3" "UKM8" "ITC4" "CH01" "UKD7" "ES52" "UKC2" "UKE2" "FRK2" "NL33" "ITI4" "IE06" "NL32" "UKL2" "ES41"
#"CH04" "UKK4" "UKK2" "ITI1" "UKJ4" "UKC1" "DE30" "UKL1" "DE12" "ES11" "DE21" "UKH2" "DK01" "SE11" "NL31" 

na1<-names(which(aut1$vector >= quantile(aut1$vector, 0.9)))
#"UKI0" "UKJ1" "UKH1" "PT16" "PT11" "UKG1" "ES30" "ES51" "UKM7" "UKJ2" "UKD3" "FR10" "UKF1" "UKE4" "UKM8" "UKG3" "UKK1"
#"CH01" "ES61" "UKJ3" "UKD7" "UKK4" "NL33" "IE06" "UKE2" "UKE3" "PT17" "UKL2" "CH04" "ES52" "ITC4" "UKC2" "FRK2" "DE30"
#"NL32" "DK01" "ES41" "SE11" "ITI4" "DE21" "AT13" "UKJ4" "UKK2" "BE10" "ES11" "UKH3" "UKH2" "ES21" "UKL1" "DEA2"

n_ha1<-union(na1,nh1)

setdiff(n_s, n_ha1) #"BE21" "BE23"
setdiff(n_ha1, n_s) #"DE40" "FRK2"

n1<-names(sort(hubs1$vector, decreasing=T))[1:50]
n2<-names(sort(hubs$vector, decreasing=T))[13:63]

setdiff(n1,n2) 
#"NL41" "NL11" "CH02" "ITC1" "CH03" "ITF3" "SE22" "SE21" "NL42" "NL21" "DE91" "BE23" "DE71" "DE60" "DE40"
#"DE14" "DE13" "ITH4" "BE21" "FI1B" "DEA1" "ITC3" "PL91" "ITG1" "CZ01" "DEB3"
setdiff(n2,n1) 
#"UKF1" "UKE4" "UKG3" "UKK1" "UKJ3" "PT17" "UKE3" "UKM8" "UKD7" "ES52" "UKC2" "UKE2" "UKL2" "ES41" "UKK4"
#"UKK2" "UKJ4" "UKC1" "UKL1" "ES11" "UKH2" "UKD4" "UKH3" "PT18" "EL30" "FRL0" "UKN0"

n3<-names(sort(aut1$vector, decreasing=T))[1:50]

n4<-names(sort(aut$vector, decreasing=T))[13:63]

setdiff(n3,n4) 
setdiff(n4,n3) 


## ukf1 has hubs scre of 0.20, its the first with "low" scores

## edge bet among top 10%
core_hub_aut<-data.frame(nuts2=vert,score_h=hubs$vector,score_a=aut$vector)
q10_h<-quantile(hubs$vector,0.90)
q10_a<-quantile(aut$vector,0.90)
core_hub_aut$mob<-"low"
core_hub_aut$mob[which(core_hub_aut$score_h>q10_h)]<-"high"
core_hub_aut$mob[which(core_hub_aut$score_a>q10_a)]<-"high"
data_clus_hub_aut<-merge(data_cov,core_hub_aut)
n_ha<-data_clus_hub_aut$nuts2[which(data_clus_hub_aut$mob=="high")]
data_edge<-data[intersect(which(data$sender %in% n_ha), which(data$receiver %in% n_ha)),]
data_gr_edge <- data_edge[-3]
gr_edge <- graph_from_edgelist(el=as.matrix(data_gr_edge), directed=T)
clus<-cluster_edge_betweenness(graph=gr_edge,weights = 1/(1+data_edge$count),directed=T)
clus$edge.betweenness
clus$membership


## hubs aut 2010
data_2010 <- network_data[which(network_data$year==2010),c(1,2,4)]
data_gr_2010 <- data_2010[-3]
gr_2010 <- graph_from_edgelist(el=as.matrix(data_gr_2010), directed=T)
E(gr_2010)$weight<-data_2010$count
hubs_2010<-hub_score(gr_2010)
aut_2010<-authority_score(gr_2010)
cor_2010<-cor(aut_2010$vector,hubs_2010$vector)
cor_2010
qh10<-quantile(hubs_2010$vector,0.9)
qa10<-quantile(aut_2010$vector,0.9)

sort(hubs_2010$vector, decreasing=T) # "CH01" "ES42" "ES52" "ES62" "ES70" "FRE1" "NL33" "SE11" "UKD7" "UKF1"
sort(aut_2010$vector,decreasing=T) #"ES21" "ES24" "FRJ2" "ITC4" "ITF3" "ITI1" "ITI4" "NL32" "PT20" "UKG3"

nh<-names(hubs_2010$vector[which(hubs_2010$vector>qh10)])
na<-names(aut_2010$vector[which(aut_2010$vector>qa10)])

setdiff(nh,na)
setdiff(na,nh)

scores10<-data.frame(hubs=hubs_2010$vector,autorities=aut_2010$vector)

h<-ggplot(scores10, aes(hubs)) + geom_histogram(bins=294) + theme_bw()+ theme_classic()
a<-ggplot(scores10, aes(autorities)) + geom_histogram(bins = 294) + theme_bw()+ theme_classic()
ggarrange(h,a, labels=c("hubs distribution","authorities distribution"))

plot(hubs_2010$vector, aut_2010$vector)

ind<-intersect(which(hubs_2010$vector<qh10), which(aut_2010$vector<qh10))
cor(hubs_2010$vector[ind], aut_2010$vector[ind])




## anova year by year , 90% -> score and ted coherent, other differ some years

#
data <- network_data[which(network_data$year=='2009'),c(1,2,4)]
data_cov<-data_trips_gdp_score_ted_edu[which(data_trips_gdp_score_ted_edu$year=='2009'),]
data_cov$flow_tot<-data_cov$flow_in+data_cov$flow_out

data_gr <- data[-3]
gr <- graph_from_edgelist(el=as.matrix(data_gr), directed=T)

E(gr)$weight<-data$count

hubs<-hub_score(gr)
aut<-authority_score(gr)

core_hub_aut<-data.frame(nuts2=vert,score_h=hubs$vector,score_a=aut$vector)
q10_h<-quantile(core_hub_aut$score_h,0.90)
q10_a<-quantile(core_hub_aut$score_a,0.90)
core_hub_aut$mob<-"low"
core_hub_aut$mob[which(core_hub_aut$score_h>q10_h)]<-"high"
core_hub_aut$mob[which(core_hub_aut$score_a>q10_a)]<-"high"
data_clus_hub_aut<-merge(data_cov,core_hub_aut)

aov_flux_hub_aut<-anova_npc(data_clus_hub_aut$flow_tot,data_clus_hub_aut$mob,B=1000) #0, 0
aov_score_hub_aut<-anova_npc(data_clus_hub_aut$score,data_clus_hub_aut$mob,B=1000) #0, 0
aov_edu_hub_aut<-anova_npc(data_clus_hub_aut$edu_index,data_clus_hub_aut$mob,B=1000)#0.15, 0.08
aov_gdp_hub_aut<-anova_npc(data_clus_hub_aut$gdp,data_clus_hub_aut$mob,B=1000)#0.01, 0.9
aov_ted_hub_aut<-anova_npc(data_clus_hub_aut$ted,data_clus_hub_aut$mob,B=1000)#0, 0.06
#


#
data <- network_data[which(network_data$year=='2010'),c(1,2,4)]
data_cov<-data_trips_gdp_score_ted_edu[which(data_trips_gdp_score_ted_edu$year=='2010'),]
data_cov$flow_tot<-data_cov$flow_in+data_cov$flow_out

data_gr <- data[-3]
gr <- graph_from_edgelist(el=as.matrix(data_gr), directed=T)

E(gr)$weight<-data$count

hubs<-hub_score(gr)
aut<-authority_score(gr)

core_hub_aut<-data.frame(nuts2=vert,score_h=hubs$vector,score_a=aut$vector)
q10_h<-quantile(core_hub_aut$score_h,0.90)
q10_a<-quantile(core_hub_aut$score_a,0.90)
core_hub_aut$mob<-"low"
core_hub_aut$mob[which(core_hub_aut$score_h>q10_h)]<-"high"
core_hub_aut$mob[which(core_hub_aut$score_a>q10_a)]<-"high"
data_clus_hub_aut<-merge(data_cov,core_hub_aut)

aov_flux_hub_aut<-anova_npc(data_clus_hub_aut$flow_tot,data_clus_hub_aut$mob,B=1000) #0, 0
aov_score_hub_aut<-anova_npc(data_clus_hub_aut$score,data_clus_hub_aut$mob,B=1000) #0, 0
aov_edu_hub_aut<-anova_npc(data_clus_hub_aut$edu_index,data_clus_hub_aut$mob,B=1000)#0.67, 0.54
aov_gdp_hub_aut<-anova_npc(data_clus_hub_aut$gdp,data_clus_hub_aut$mob,B=1000)#0.67, 0.514
aov_ted_hub_aut<-anova_npc(data_clus_hub_aut$ted,data_clus_hub_aut$mob,B=1000)#0, 0.9
#


#
data <- network_data[which(network_data$year=='2011'),c(1,2,4)]
data_cov<-data_trips_gdp_score_ted_edu[which(data_trips_gdp_score_ted_edu$year=='2011'),]
data_cov$flow_tot<-data_cov$flow_in+data_cov$flow_out

data_gr <- data[-3]
gr <- graph_from_edgelist(el=as.matrix(data_gr), directed=T)

E(gr)$weight<-data$count

hubs<-hub_score(gr)
aut<-authority_score(gr)

core_hub_aut<-data.frame(nuts2=vert,score_h=hubs$vector,score_a=aut$vector)
q10_h<-quantile(core_hub_aut$score_h,0.90)
q10_a<-quantile(core_hub_aut$score_a,0.90)
core_hub_aut$mob<-"low"
core_hub_aut$mob[which(core_hub_aut$score_h>q10_h)]<-"high"
core_hub_aut$mob[which(core_hub_aut$score_a>q10_a)]<-"high"
data_clus_hub_aut<-merge(data_cov,core_hub_aut)

aov_flux_hub_aut<-anova_npc(data_clus_hub_aut$flow_tot,data_clus_hub_aut$mob,B=1000) #0, 0.001
aov_score_hub_aut<-anova_npc(data_clus_hub_aut$score,data_clus_hub_aut$mob,B=1000) #0, 0
aov_edu_hub_aut<-anova_npc(data_clus_hub_aut$edu_index,data_clus_hub_aut$mob,B=1000)#0.70, 0.70
aov_gdp_hub_aut<-anova_npc(data_clus_hub_aut$gdp,data_clus_hub_aut$mob,B=1000)#0.086, 0.57
aov_ted_hub_aut<-anova_npc(data_clus_hub_aut$ted,data_clus_hub_aut$mob,B=1000)#0, 0.7
#


#
data <- network_data[which(network_data$year=='2012'),c(1,2,4)]
data_cov<-data_trips_gdp_score_ted_edu[which(data_trips_gdp_score_ted_edu$year=='2012'),]
data_cov$flow_tot<-data_cov$flow_in+data_cov$flow_out

data_gr <- data[-3]
gr <- graph_from_edgelist(el=as.matrix(data_gr), directed=T)

E(gr)$weight<-data$count

hubs<-hub_score(gr)
aut<-authority_score(gr)

core_hub_aut<-data.frame(nuts2=vert,score_h=hubs$vector,score_a=aut$vector)
q10_h<-quantile(core_hub_aut$score_h,0.90)
q10_a<-quantile(core_hub_aut$score_a,0.90)
core_hub_aut$mob<-"low"
core_hub_aut$mob[which(core_hub_aut$score_h>q10_h)]<-"high"
core_hub_aut$mob[which(core_hub_aut$score_a>q10_a)]<-"high"
data_clus_hub_aut<-merge(data_cov,core_hub_aut)

aov_flux_hub_aut<-anova_npc(data_clus_hub_aut$flow_tot,data_clus_hub_aut$mob,B=1000) #0, 0.001
aov_score_hub_aut<-anova_npc(data_clus_hub_aut$score,data_clus_hub_aut$mob,B=1000) #0, 0
aov_edu_hub_aut<-anova_npc(data_clus_hub_aut$edu_index,data_clus_hub_aut$mob,B=1000)#0.70, 0.70
aov_gdp_hub_aut<-anova_npc(data_clus_hub_aut$gdp,data_clus_hub_aut$mob,B=1000)#0.086, 0.57
aov_ted_hub_aut<-anova_npc(data_clus_hub_aut$ted,data_clus_hub_aut$mob,B=1000)#0, 0.7
#


#
data <- network_data[which(network_data$year=='2013'),c(1,2,4)]
data_cov<-data_trips_gdp_score_ted_edu[which(data_trips_gdp_score_ted_edu$year=='2013'),]
data_cov$flow_tot<-data_cov$flow_in+data_cov$flow_out

data_gr <- data[-3]
gr <- graph_from_edgelist(el=as.matrix(data_gr), directed=T)

E(gr)$weight<-data$count

hubs<-hub_score(gr)
aut<-authority_score(gr)

core_hub_aut<-data.frame(nuts2=vert,score_h=hubs$vector,score_a=aut$vector)
q10_h<-quantile(core_hub_aut$score_h,0.90)
q10_a<-quantile(core_hub_aut$score_a,0.90)
core_hub_aut$mob<-"low"
core_hub_aut$mob[which(core_hub_aut$score_h>q10_h)]<-"high"
core_hub_aut$mob[which(core_hub_aut$score_a>q10_a)]<-"high"
data_clus_hub_aut<-merge(data_cov,core_hub_aut)

aov_flux_hub_aut<-anova_npc(data_clus_hub_aut$flow_tot,data_clus_hub_aut$mob,B=1000) #0, 0
aov_score_hub_aut<-anova_npc(data_clus_hub_aut$score,data_clus_hub_aut$mob,B=1000) #0, 0
aov_edu_hub_aut<-anova_npc(data_clus_hub_aut$edu_index,data_clus_hub_aut$mob,B=1000)#0, 0.08
aov_gdp_hub_aut<-anova_npc(data_clus_hub_aut$gdp,data_clus_hub_aut$mob,B=1000)#0.17, 0.253
aov_ted_hub_aut<-anova_npc(data_clus_hub_aut$ted,data_clus_hub_aut$mob,B=1000)#0, 0.7
#


#
data <- network_data[which(network_data$year=='2014'),c(1,2,4)]
data_cov<-data_trips_gdp_score_ted_edu[which(data_trips_gdp_score_ted_edu$year=='2014'),]
data_cov$flow_tot<-data_cov$flow_in+data_cov$flow_out

data_gr <- data[-3]
gr <- graph_from_edgelist(el=as.matrix(data_gr), directed=T)

E(gr)$weight<-data$count

hubs<-hub_score(gr)
aut<-authority_score(gr)

core_hub_aut<-data.frame(nuts2=vert,score_h=hubs$vector,score_a=aut$vector)
q10_h<-quantile(core_hub_aut$score_h,0.90)
q10_a<-quantile(core_hub_aut$score_a,0.90)
core_hub_aut$mob<-"low"
core_hub_aut$mob[which(core_hub_aut$score_h>q10_h)]<-"high"
core_hub_aut$mob[which(core_hub_aut$score_a>q10_a)]<-"high"
data_clus_hub_aut<-merge(data_cov,core_hub_aut)

aov_flux_hub_aut<-anova_npc(data_clus_hub_aut$flow_tot,data_clus_hub_aut$mob,B=1000) #0, 0
aov_score_hub_aut<-anova_npc(data_clus_hub_aut$score,data_clus_hub_aut$mob,B=1000) #0, 0
aov_edu_hub_aut<-anova_npc(data_clus_hub_aut$edu_index,data_clus_hub_aut$mob,B=1000)#0.006, 0.86
aov_gdp_hub_aut<-anova_npc(data_clus_hub_aut$gdp,data_clus_hub_aut$mob,B=1000)#0.059, 0.13
aov_ted_hub_aut<-anova_npc(data_clus_hub_aut$ted,data_clus_hub_aut$mob,B=1000)#0, 0.7
#


#
data <- network_data[which(network_data$year=='2015'),c(1,2,4)]
data_cov<-data_trips_gdp_score_ted_edu[which(data_trips_gdp_score_ted_edu$year=='2015'),]
data_cov$flow_tot<-data_cov$flow_in+data_cov$flow_out

data_gr <- data[-3]
gr <- graph_from_edgelist(el=as.matrix(data_gr), directed=T)

E(gr)$weight<-data$count

hubs<-hub_score(gr)
aut<-authority_score(gr)

core_hub_aut<-data.frame(nuts2=vert,score_h=hubs$vector,score_a=aut$vector)
q10_h<-quantile(core_hub_aut$score_h,0.90)
q10_a<-quantile(core_hub_aut$score_a,0.90)
core_hub_aut$mob<-"low"
core_hub_aut$mob[which(core_hub_aut$score_h>q10_h)]<-"high"
core_hub_aut$mob[which(core_hub_aut$score_a>q10_a)]<-"high"
data_clus_hub_aut<-merge(data_cov,core_hub_aut)

aov_flux_hub_aut<-anova_npc(data_clus_hub_aut$flow_tot,data_clus_hub_aut$mob,B=1000) #0, 0
aov_score_hub_aut<-anova_npc(data_clus_hub_aut$score,data_clus_hub_aut$mob,B=1000) #0, 0
aov_edu_hub_aut<-anova_npc(data_clus_hub_aut$edu_index,data_clus_hub_aut$mob,B=1000)#0 , 0.126
aov_gdp_hub_aut<-anova_npc(data_clus_hub_aut$gdp,data_clus_hub_aut$mob,B=1000)#0.198, 0.107
aov_ted_hub_aut<-anova_npc(data_clus_hub_aut$ted,data_clus_hub_aut$mob,B=1000)#0, 0.98
#


#
data <- network_data[which(network_data$year=='2016'),c(1,2,4)]
data_cov<-data_trips_gdp_score_ted_edu[which(data_trips_gdp_score_ted_edu$year=='2016'),]
data_cov$flow_tot<-data_cov$flow_in+data_cov$flow_out

data_gr <- data[-3]
gr <- graph_from_edgelist(el=as.matrix(data_gr), directed=T)

E(gr)$weight<-data$count

hubs<-hub_score(gr)
aut<-authority_score(gr)

core_hub_aut<-data.frame(nuts2=vert,score_h=hubs$vector,score_a=aut$vector)
q10_h<-quantile(core_hub_aut$score_h,0.90)
q10_a<-quantile(core_hub_aut$score_a,0.90)
core_hub_aut$mob<-"low"
core_hub_aut$mob[which(core_hub_aut$score_h>q10_h)]<-"high"
core_hub_aut$mob[which(core_hub_aut$score_a>q10_a)]<-"high"
data_clus_hub_aut<-merge(data_cov,core_hub_aut)

aov_flux_hub_aut<-anova_npc(data_clus_hub_aut$flow_tot,data_clus_hub_aut$mob,B=1000) #0, 0.006
aov_score_hub_aut<-anova_npc(data_clus_hub_aut$score,data_clus_hub_aut$mob,B=1000) #0, 0
aov_edu_hub_aut<-anova_npc(data_clus_hub_aut$edu_index,data_clus_hub_aut$mob,B=1000)#0.009, 0.153
aov_gdp_hub_aut<-anova_npc(data_clus_hub_aut$gdp,data_clus_hub_aut$mob,B=1000)#0.424, 0.204
aov_ted_hub_aut<-anova_npc(data_clus_hub_aut$ted,data_clus_hub_aut$mob,B=1000)#0, 0.7
#


#
data <- network_data[which(network_data$year=='2017'),c(1,2,4)]
data_cov<-data_trips_gdp_score_ted_edu[which(data_trips_gdp_score_ted_edu$year=='2017'),]
data_cov$flow_tot<-data_cov$flow_in+data_cov$flow_out

data_gr <- data[-3]
gr <- graph_from_edgelist(el=as.matrix(data_gr), directed=T)

E(gr)$weight<-data$count

hubs<-hub_score(gr)
aut<-authority_score(gr)

core_hub_aut<-data.frame(nuts2=vert,score_h=hubs$vector,score_a=aut$vector)
q10_h<-quantile(core_hub_aut$score_h,0.90)
q10_a<-quantile(core_hub_aut$score_a,0.90)
core_hub_aut$mob<-"low"
core_hub_aut$mob[which(core_hub_aut$score_h>q10_h)]<-"high"
core_hub_aut$mob[which(core_hub_aut$score_a>q10_a)]<-"high"
data_clus_hub_aut<-merge(data_cov,core_hub_aut)

aov_flux_hub_aut<-anova_npc(data_clus_hub_aut$flow_tot,data_clus_hub_aut$mob,B=1000) #0, 0.00
aov_score_hub_aut<-anova_npc(data_clus_hub_aut$score,data_clus_hub_aut$mob,B=1000) #0, 0
aov_edu_hub_aut<-anova_npc(data_clus_hub_aut$edu_index,data_clus_hub_aut$mob,B=1000)#0.003, 0.08
aov_gdp_hub_aut<-anova_npc(data_clus_hub_aut$gdp,data_clus_hub_aut$mob,B=1000)#0.103, 0.18
aov_ted_hub_aut<-anova_npc(data_clus_hub_aut$ted,data_clus_hub_aut$mob,B=1000)#0, 0.4
#


#
data <- network_data[which(network_data$year=='2018'),c(1,2,4)]
data_cov<-data_trips_gdp_score_ted_edu[which(data_trips_gdp_score_ted_edu$year=='2018'),]
data_cov$flow_tot<-data_cov$flow_in+data_cov$flow_out

data_gr <- data[-3]
gr <- graph_from_edgelist(el=as.matrix(data_gr), directed=T)

E(gr)$weight<-data$count

hubs<-hub_score(gr)
aut<-authority_score(gr)

core_hub_aut<-data.frame(nuts2=vert,score_h=hubs$vector,score_a=aut$vector)
q10_h<-quantile(core_hub_aut$score_h,0.90)
q10_a<-quantile(core_hub_aut$score_a,0.90)
core_hub_aut$mob<-"low"
core_hub_aut$mob[which(core_hub_aut$score_h>q10_h)]<-"high"
core_hub_aut$mob[which(core_hub_aut$score_a>q10_a)]<-"high"
data_clus_hub_aut<-merge(data_cov,core_hub_aut)

aov_flux_hub_aut<-anova_npc(data_clus_hub_aut$flow_tot,data_clus_hub_aut$mob,B=1000) #0, 0.001
aov_score_hub_aut<-anova_npc(data_clus_hub_aut$score,data_clus_hub_aut$mob,B=1000) #0, 0
aov_edu_hub_aut<-anova_npc(data_clus_hub_aut$edu_index,data_clus_hub_aut$mob,B=1000)#0.001, 0.05
aov_gdp_hub_aut<-anova_npc(data_clus_hub_aut$gdp,data_clus_hub_aut$mob,B=1000)#0.135, 0.57
aov_ted_hub_aut<-anova_npc(data_clus_hub_aut$ted,data_clus_hub_aut$mob,B=1000)#0, 0.8
#



#
data <- network_data[which(network_data$year=='2019'),c(1,2,4)]
data_cov<-data_trips_gdp_score_ted_edu[which(data_trips_gdp_score_ted_edu$year=='2019'),]
data_cov$flow_tot<-data_cov$flow_in+data_cov$flow_out

data_gr <- data[-3]
gr <- graph_from_edgelist(el=as.matrix(data_gr), directed=T)

E(gr)$weight<-data$count

hubs<-hub_score(gr)
aut<-authority_score(gr)

core_hub_aut<-data.frame(nuts2=vert,score_h=hubs$vector,score_a=aut$vector)
q10_h<-quantile(core_hub_aut$score_h,0.90)
q10_a<-quantile(core_hub_aut$score_a,0.90)
core_hub_aut$mob<-"low"
core_hub_aut$mob[which(core_hub_aut$score_h>q10_h)]<-"high"
core_hub_aut$mob[which(core_hub_aut$score_a>q10_a)]<-"high"
data_clus_hub_aut<-merge(data_cov,core_hub_aut)

aov_flux_hub_aut<-anova_npc(data_clus_hub_aut$flow_tot,data_clus_hub_aut$mob,B=1000) #0, 0
aov_score_hub_aut<-anova_npc(data_clus_hub_aut$score,data_clus_hub_aut$mob,B=1000) #0, 0
aov_edu_hub_aut<-anova_npc(data_clus_hub_aut$edu_index,data_clus_hub_aut$mob,B=1000)#0.002, 0.106
aov_gdp_hub_aut<-anova_npc(data_clus_hub_aut$gdp,data_clus_hub_aut$mob,B=1000)#0.048, 0.99
aov_ted_hub_aut<-anova_npc(data_clus_hub_aut$ted,data_clus_hub_aut$mob,B=1000)#0, 0.87
#



#
data <- network_data[which(network_data$year=='2020'),c(1,2,4)]
data_cov<-data_trips_gdp_score_ted_edu[which(data_trips_gdp_score_ted_edu$year=='2020'),]
data_cov$flow_tot<-data_cov$flow_in+data_cov$flow_out

data_gr <- data[-3]
gr <- graph_from_edgelist(el=as.matrix(data_gr), directed=T)

E(gr)$weight<-data$count

hubs<-hub_score(gr)
aut<-authority_score(gr)

core_hub_aut<-data.frame(nuts2=vert,score_h=hubs$vector,score_a=aut$vector)
q10_h<-quantile(core_hub_aut$score_h,0.90)
q10_a<-quantile(core_hub_aut$score_a,0.90)
core_hub_aut$mob<-"low"
core_hub_aut$mob[which(core_hub_aut$score_h>q10_h)]<-"high"
core_hub_aut$mob[which(core_hub_aut$score_a>q10_a)]<-"high"
data_clus_hub_aut<-merge(data_cov,core_hub_aut)

aov_flux_hub_aut<-anova_npc(data_clus_hub_aut$flow_tot,data_clus_hub_aut$mob,B=1000) #0, 0.003
aov_score_hub_aut<-anova_npc(data_clus_hub_aut$score,data_clus_hub_aut$mob,B=1000) #0, 0
aov_edu_hub_aut<-anova_npc(data_clus_hub_aut$edu_index,data_clus_hub_aut$mob,B=1000)#0.001, 0.185
aov_gdp_hub_aut<-anova_npc(data_clus_hub_aut$gdp,data_clus_hub_aut$mob,B=1000)#0.01, 0.422
aov_ted_hub_aut<-anova_npc(data_clus_hub_aut$ted,data_clus_hub_aut$mob,B=1000)#0, 0.7
#
