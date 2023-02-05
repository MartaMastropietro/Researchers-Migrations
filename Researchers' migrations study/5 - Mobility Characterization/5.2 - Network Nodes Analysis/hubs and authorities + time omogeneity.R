
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

vert<-V(gr)
vert<-names(vert)

#analyses 

cor(aut$vector,hubs$vector) #0.9832276

summary(hubs$vector)
summary(aut$vector)

plot(log(hubs$vector), log(aut$vector))
plot(hubs$vector, aut$vector)

length(unique(hubs$vector)) #287 over 294
length(unique(aut$vector)) #287 over 294

names(sort(hubs$vector, decreasing=T))[1:50]
#"UKI0" "UKH1" "UKJ1" "PT16" "PT11" "UKM7" "UKG1" "ES30" "FR10" "ES51" "UKJ2" "UKD3" "UKF1" "UKE4" "ES61" "UKG3" "UKK1"
#"UKJ3" "PT17" "UKE3" "UKM8" "ITC4" "CH01" "UKD7" "ES52" "UKC2" "UKE2" "FRK2" "NL33" "ITI4" "IE06" "NL32" "UKL2" "ES41"
#"CH04" "UKK4" "UKK2" "ITI1" "UKJ4" "UKC1" "DE30" "UKL1" "DE12" "ES11" "DE21" "UKH2" "DK01" "SE11" "NL31" 

names(sort(aut$vector, decreasing=T))[1:50]
#"UKI0" "UKJ1" "UKH1" "PT16" "PT11" "UKG1" "ES30" "ES51" "UKM7" "UKJ2" "UKD3" "FR10" "UKF1" "UKE4" "UKM8" "UKG3" "UKK1"
#"CH01" "ES61" "UKJ3" "UKD7" "UKK4" "NL33" "IE06" "UKE2" "UKE3" "PT17" "UKL2" "CH04" "ES52" "ITC4" "UKC2" "FRK2" "DE30"
#"NL32" "DK01" "ES41" "SE11" "ITI4" "DE21" "AT13" "UKJ4" "UKK2" "BE10" "ES11" "UKH3" "UKH2" "ES21" "UKL1" "DEA2"

#plots hist ggplot 

scores<-data.frame(hubs=hubs$vector,authorities=aut$vector)

h<-ggplot(scores, aes(hubs)) + geom_histogram(bins=294) + theme_bw()+ theme_classic()
a<-ggplot(scores, aes(authorities)) + geom_histogram(bins = 294) + theme_bw()+ theme_classic()
ggarrange(h,a)


ggplot(scores, aes(x=hubs, y=authorities))+ geom_point() + theme_bw()+ theme_classic()

#anova hubs and aut, division on value 0.25 (union)
core_hub_aut<-data.frame(nuts2=vert,score_h=hubs$vector,score_a=aut$vector)
core_hub_aut$mob<-"low"
core_hub_aut$mob[which(core_hub_aut$score_h>0.25)]<-"high"
core_hub_aut$mob[which(core_hub_aut$score_a>0.25)]<-"high" #only UKF1 addedd
data_clus_hub_aut<-merge(data_cov,core_hub_aut)
n_ha25<-data_clus_hub_aut$nuts2[which(data_clus_hub_aut$mob=="high")]
#  "ES30" "ES51" "FR10" "PT11" "PT16" "UKD3" "UKF1" "UKG1" "UKH1" "UKI0" "UKJ1" "UKJ2" "UKM7"

n_s25<-names(sort(coreness, decreasing=T)[1:13])
# "PT11" "UKJ1" "UKI0" "UKH1" "ES51" "ES30" "ES61" "ES52" "UKG1" "UKJ2" "UKM7" "UKM8" "UKF1"

setdiff(n_ha25,n_s25) #"FR10" "PT16" "UKD3"
setdiff(n_s25,n_ha25) #"ES61" "ES52" "UKM8"


aov_flux_hub_aut<-anova_npc(data_clus_hub_aut$flow_tot,data_clus$mob,B=1000) #0, 0
aov_score_hub_aut<-anova_npc(data_clus_hub_aut$score,data_clus_hub_aut$mob,B=1000) #0.001, 0.001
aov_edu_hub_aut<-anova_npc(data_clus_hub_aut$edu_index,data_clus_hub_aut$mob,B=1000)#0.13, 0.586
aov_gdp_hub_aut<-anova_npc(data_clus_hub_aut$gdp,data_clus_hub_aut$mob,B=1000)#0.21, 0.679
aov_ted_hub_aut<-anova_npc(data_clus_hub_aut$ted,data_clus_hub_aut$mob,B=1000)#0, 0.926


#anova hubs and aut, division on median (union)
core_hub_aut<-data.frame(nuts2=vert,score_h=hubs$vector,score_a=aut$vector)
med_h<-median(hubs$vector)
med_a<-median(aut$vector)
core_hub_aut$mob<-"low"
core_hub_aut$mob[which(core_hub_aut$score_h>med_h)]<-"high"
core_hub_aut$mob[which(core_hub_aut$score_a>med_a)]<-"high"
data_clus_hub_aut<-merge(data_cov,core_hub_aut)
data_clus_hub_aut$nuts2[which(data_clus_hub_aut$mob=="high")]

aov_flux_hub_aut<-anova_npc(data_clus_hub_aut$flow_tot,data_clus$mob,B=1000) #0, 0
aov_score_hub_aut<-anova_npc(data_clus_hub_aut$score,data_clus_hub_aut$mob,B=1000) #0, 0
aov_edu_hub_aut<-anova_npc(data_clus_hub_aut$edu_index,data_clus_hub_aut$mob,B=1000)#0, 0.7
aov_gdp_hub_aut<-anova_npc(data_clus_hub_aut$gdp,data_clus_hub_aut$mob,B=1000)#0, 0.6
aov_ted_hub_aut<-anova_npc(data_clus_hub_aut$ted,data_clus_hub_aut$mob,B=1000)#0, 0.5

#anova hubs and aut, division on 75% quantile (union)
core_hub_aut<-data.frame(nuts2=vert,score_h=hubs$vector,score_a=aut$vector)
q75_h<-quantile(hubs$vector,0.75)
q75_a<-quantile(aut$vector,0.75)
core_hub_aut$mob<-"low"
core_hub_aut$mob[which(core_hub_aut$score_h>q75_h)]<-"high"
core_hub_aut$mob[which(core_hub_aut$score_a>q75_a)]<-"high"
data_clus_hub_aut<-merge(data_cov,core_hub_aut)
data_clus_hub_aut$nuts2[which(data_clus_hub_aut$mob=="high")]

aov_flux_hub_aut<-anova_npc(data_clus_hub_aut$flow_tot,data_clus$mob,B=1000) #0, 0
aov_score_hub_aut<-anova_npc(data_clus_hub_aut$score,data_clus_hub_aut$mob,B=1000) #0, 0
aov_edu_hub_aut<-anova_npc(data_clus_hub_aut$edu_index,data_clus_hub_aut$mob,B=1000)#0, 0.244
aov_gdp_hub_aut<-anova_npc(data_clus_hub_aut$gdp,data_clus_hub_aut$mob,B=1000)#0, 0.963
aov_ted_hub_aut<-anova_npc(data_clus_hub_aut$ted,data_clus_hub_aut$mob,B=1000)#0, 0.8

#anova hubs and aut, division on 90% quantile (union)
core_hub_aut<-data.frame(nuts2=vert,score_h=hubs$vector,score_a=aut$vector)
q10_h<-quantile(hubs$vector,0.90)
q10_a<-quantile(aut$vector,0.90)
core_hub_aut$mob<-"low"
core_hub_aut$mob[which(core_hub_aut$score_h>q10_h)]<-"high"
core_hub_aut$mob[which(core_hub_aut$score_a>q10_a)]<-"high"
data_clus_hub_aut<-merge(data_cov,core_hub_aut)
n_ha<-data_clus_hub_aut$nuts2[which(data_clus_hub_aut$mob=="high")]
# "CH01" "CH04" "ES30" "ES51" "ES52" "ES61" "FR10" "FRK2" "IE06" "ITC4" "ITI4" "NL33" "PT11" "PT16" "PT17" "UKC2" "UKD3" "UKD7"
# "UKE2" "UKE3" "UKE4" "UKF1" "UKG1" "UKG3" "UKH1" "UKI0" "UKJ1" "UKJ2" "UKJ3" "UKK1" "UKK4" "UKL2" "UKM7" "UKM8"

#same as score?
n_s<-names(sort(coreness, decreasing=T)[1:34])
# "PT11" "UKJ1" "UKI0" "UKH1" "ES51" "ES30" "ES61" "ES52" "UKG1" "UKJ2" "UKM7" "UKM8" "UKF1" "UKD3" "UKE4" "UKG3" "PT17"
# "ES41" "FR10" "CH01" "UKK1" "UKD7" "UKE3" "UKE2" "NL33" "NL32" "FRK2" "SE11" "SE12" "UKJ3" "UKK4" "UKL2" "NL31" "CH04"

setdiff(n_ha,n_s) #"IE06" "ITC4" "ITI4" "PT16" "UKC2" in hubs and aut but not scoreness
setdiff(n_s,n_ha) #"ES41" "NL32" "SE11" "SE12" "NL31" 


aov_flux_hub_aut<-anova_npc(data_clus_hub_aut$flow_tot,data_clus$mob,B=1000) #0, 0
aov_score_hub_aut<-anova_npc(data_clus_hub_aut$score,data_clus_hub_aut$mob,B=1000) #0, 0
aov_edu_hub_aut<-anova_npc(data_clus_hub_aut$edu_index,data_clus_hub_aut$mob,B=1000)#0.03, 0.190
aov_gdp_hub_aut<-anova_npc(data_clus_hub_aut$gdp,data_clus_hub_aut$mob,B=1000)#0.08, 0.315
aov_ted_hub_aut<-anova_npc(data_clus_hub_aut$ted,data_clus_hub_aut$mob,B=1000)#0, 0.854


#anova hubs and aut, division on median (intersection) - scale coherent as before 
core_hub_aut<-data.frame(nuts2=vert,score_h=hubs$vector,score_a=aut$vector)
med_h<-median(hubs$vector)
med_a<-median(aut$vector)
core_hub_aut$mob<-"low"
core_hub_aut$mob[intersect(which(core_hub_aut$score_h>med_h),which(core_hub_aut$score_a>med_h))]<-"high"
data_clus_hub_aut<-merge(data_cov,core_hub_aut)
data_clus_hub_aut$nuts2[which(data_clus_hub_aut$mob=="high")]

aov_flux_hub_aut<-anova_npc(data_clus_hub_aut$flow_tot,data_clus$mob,B=1000) #0
aov_score_hub_aut<-anova_npc(data_clus_hub_aut$score,data_clus_hub_aut$mob,B=1000) #0
aov_edu_hub_aut<-anova_npc(data_clus_hub_aut$edu_index,data_clus_hub_aut$mob,B=1000)#0
aov_gdp_hub_aut<-anova_npc(data_clus_hub_aut$gdp,data_clus_hub_aut$mob,B=1000)#0
aov_ted_hub_aut<-anova_npc(data_clus_hub_aut$ted,data_clus_hub_aut$mob,B=1000)#0


#anova hubs and aut, division on 75% quantile (intersection) - scale coherent as before 
core_hub_aut<-data.frame(nuts2=vert,score_h=hubs$vector,score_a=aut$vector)
q75_h<-quantile(hubs$vector,0.75)
q75_a<-quantile(aut$vector,0.75)
core_hub_aut$mob<-"low"
core_hub_aut$mob[intersect(which(core_hub_aut$score_h>q75_h),which(core_hub_aut$score_a>q75_a))]<-"high"
data_clus_hub_aut<-merge(data_cov,core_hub_aut)
data_clus_hub_aut$nuts2[which(data_clus_hub_aut$mob=="high")]

aov_flux_hub_aut<-anova_npc(data_clus_hub_aut$flow_tot,data_clus$mob,B=1000) #0
aov_score_hub_aut<-anova_npc(data_clus_hub_aut$score,data_clus_hub_aut$mob,B=1000) #0
aov_edu_hub_aut<-anova_npc(data_clus_hub_aut$edu_index,data_clus_hub_aut$mob,B=1000)#0
aov_gdp_hub_aut<-anova_npc(data_clus_hub_aut$gdp,data_clus_hub_aut$mob,B=1000)#0
aov_ted_hub_aut<-anova_npc(data_clus_hub_aut$ted,data_clus_hub_aut$mob,B=1000)#0

#anova hubs and aut, division on 90% quantile (intersection)
core_hub_aut<-data.frame(nuts2=vert,score_h=hubs$vector,score_a=aut$vector)
q90_h<-quantile(hubs$vector,0.90)
q90_a<-quantile(aut$vector,0.90)
core_hub_aut$mob<-"low"
core_hub_aut$mob[intersect(which(core_hub_aut$score_h>q90_h),which(core_hub_aut$score_a>q90_a))]<-"high"
data_clus_hub_aut<-merge(data_cov,core_hub_aut)
data_clus_hub_aut$nuts2[which(data_clus_hub_aut$mob=="high")]

aov_flux_hub_aut<-anova_npc(data_clus_hub_aut$flow_tot,data_clus$mob,B=1000) #0, 0
aov_score_hub_aut<-anova_npc(data_clus_hub_aut$score,data_clus_hub_aut$mob,B=1000) #0, 0
aov_edu_hub_aut<-anova_npc(data_clus_hub_aut$edu_index,data_clus_hub_aut$mob,B=1000)#0.012, 0.284
aov_gdp_hub_aut<-anova_npc(data_clus_hub_aut$gdp,data_clus_hub_aut$mob,B=1000)#0.224 , 0.194
aov_ted_hub_aut<-anova_npc(data_clus_hub_aut$ted,data_clus_hub_aut$mob,B=1000)#0, 0.68


### year by year test hubs and aut on 0.9 partition
################################

data <- network_data
data <- aggregate(count ~ sender+receiver, sum, data = data)

data_cov<-data_trips_gdp_score_ted_edu
data_cov$flow_tot<-data_cov$flow_in+data_cov$flow_out
data_cov<-aggregate(.~nuts2,mean,data=data_cov)
data_cov<-data_cov[,-2]

data_gr <- data[-3]
gr <- graph_from_edgelist(el=as.matrix(data_gr), directed=T)

E(gr)$weight<-data$count
mat<-as_adjacency_matrix(gr,attr="weight")

hubs<-hub_score(gr)
aut<-authority_score(gr)

vert<-V(gr)
vert<-names(vert)

results_hubs <- data.frame(ugly_high = replicate(12,0), ugly_low = replicate(12,0), accuracy= replicate(12,0))
results_aut <- data.frame(ugly_high = replicate(12,0), ugly_low = replicate(12,0), accuracy= replicate(12,0))

core_hubs<-data.frame(vert=vert, cor=hubs$vector)
med_hubs<-quantile(core_hubs$cor,0.9)
core_hubs$mob<-"low"
core_hubs$mob[which(core_hubs$cor>med_hubs)]<-"high"

core_aut<-data.frame(vert=vert, cor=aut$vector)
med_aut<-quantile(core_aut$cor,0.9)
core_aut$mob<-"low"
core_aut$mob[which(core_aut$cor>med_aut)]<-"high"

corr=rep(0,12)

years=2009:2020
for(y in years){
  
  data_temp <- network_data[which(network_data$year==y),]
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
  med_temp_hubs<-quantile(hubs_temp,0.9)
  core_temp_hubs$mob<-"low"
  core_temp_hubs$mob[which(core_temp_hubs$cor>med_temp_hubs)]<-"high"
  
  results_hubs$ugly_high[y-2008] <- sum(core_temp_hubs$vert[which(core_temp_hubs$mob=="high")] %in% core_hubs$vert[which(core_hubs$mob=="low")])
  results_hubs$ugly_low[y-2008] <- sum(core_temp_hubs$vert[which(core_temp_hubs$mob=="low")] %in% core_hubs$vert[which(core_hubs$mob=="high")])
  results_hubs$accuracy[y-2008] <- (sum(core_temp_hubs$vert[which(core_temp_hubs$mob=="high")] %in% core_hubs$vert[which(core_hubs$mob=="high")])+sum(core_temp_hubs$vert[which(core_temp_hubs$mob=="low")] %in% core_hubs$vert[which(core_hubs$mob=="low")]))/length(core_hubs$mob)
  
  core_temp_aut<-data.frame(vert=vert_temp, cor=aut_temp)
  med_temp_aut<-quantile(aut_temp,0.9)
  core_temp_aut$mob<-"low"
  core_temp_aut$mob[which(core_temp_aut$cor>med_temp_aut)]<-"high"
  
  results_aut$ugly_high[y-2008] <- sum(core_temp_aut$vert[which(core_temp_aut$mob=="high")] %in% core_aut$vert[which(core_aut$mob=="low")])
  results_aut$ugly_low[y-2008] <- sum(core_temp_aut$vert[which(core_temp_aut$mob=="low")] %in% core_aut$vert[which(core_aut$mob=="high")])
  results_aut$accuracy[y-2008] <- (sum(core_temp_aut$vert[which(core_temp_aut$mob=="high")] %in% core_aut$vert[which(core_aut$mob=="high")])+sum(core_temp_aut$vert[which(core_temp_aut$mob=="low")] %in% core_aut$vert[which(core_aut$mob=="low")]))/length(core_aut$mob)
  
  }
results_hubs
#ugly_high ugly_low  accuracy
#       11       11 0.9251701
#       12       12 0.9183673
#       10       10 0.9319728
#        5        5 0.9659864
#        5        5 0.9659864
#        5        5 0.9659864
#        5        5 0.9659864
#        3        3 0.9795918
#        3        3 0.9795918
#        4        4 0.9727891
#        3        3 0.9795918
#        4        4 0.9727891

results_aut
#ugly_high ugly_low  accuracy
#       11       11 0.9251701
#       11       11 0.9251701
#       11       11 0.9251701
#        4        4 0.9727891
#        4        4 0.9727891
#        4        4 0.9727891
#        3        3 0.9795918
#        4        4 0.9727891
#        2        2 0.9863946
#        2        2 0.9863946
#        2        2 0.9863946
#        6        6 0.95918

corr
# 0.7691095 0.4196477 0.7494913 0.9289836 0.9509189 0.9568957 0.9364166 0.9550411 0.9693446 0.9713338 0.9843573 0.9655058

corr_data<-data.frame( years=2009:2020, correlation=corr)

ggplot(corr_data, aes(x=years, y=correlation)) + geom_point()+ theme_bw()+ theme_classic()

### 2010 analysis
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

sort(hubs_2010$vector, decreasing=T)
sort(aut_2010$vector,decreasing=T)

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

### preparing data hubs and aut to plot division of regions , 0.9 quantile

names(hubs$vector[which(hubs$vector>quantile(hubs$vector,.9))])
names(aut$vector[which(aut$vector>quantile(aut$vector,.9))])
names<-union(names(hubs$vector[which(hubs$vector>quantile(hubs$vector,.9))]),names(aut$vector[which(aut$vector>quantile(aut$vector,.9))]))
sort(hubs$vector[which(hubs$vector>quantile(hubs$vector,.9))])
nuts2<-vert

division_90<-data.frame(nuts2=nuts2,values=0)
division_90$values[which(division_90$nuts2 %in% names)]<-1
setwd("D:/Marta/Politecnico/Tesi/immagini")
write.csv(division_90,"division mean hubs and aut 0.9.csv",row.names = F)

plot(2009:2020, corr, xlab = "year",ylab="correlation hubs and authorities scores")
abline(h=mean(corr))

### preparing data s-core median

names<-names(coreness[which(coreness>median(coreness))])
nuts2<-vert
division_median<-data.frame(nuts2=nuts2, values=0)
division_median$values[which(division_median$nuts2 %in% names)]<-1
setwd("D:/Marta/Politecnico/Tesi/immagini")
write.csv(division_median,"division mean s-core median.csv",row.names = F)
