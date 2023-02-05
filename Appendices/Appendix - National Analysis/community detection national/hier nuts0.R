data <- read.csv("C:/Users/user/Desktop/università/dare/TESI/Git_repo/ORCiD_Thesis/Analyses/National analyses & data/network_data_by_country.csv")
data <- aggregate(count ~ sender+receiver, sum, data = data)

summary(data$count)
trips <- data.frame(place_1 = "PLACE1", place_2 = "PLACE2", dist = -80)

library(progress)
pb <- progress_bar$new(total = dim(data)[1])

pb$tick(0)
for(i in 1:dim(data)[1]){
  if(as.factor(data$sender[i]):as.factor(data$receiver[i]) %in% as.factor(trips$place_2):as.factor(trips$place_1)){
    ind <- intersect(which(trips$place_1 == data$receiver[i]), which(trips$place_2 == data$sender[i]))
    trips$dist[ind] <- as.numeric(trips$dist[ind]) + as.numeric(data$count[i])
  }
  else{
    newrow <- c(data$sender[i], data$receiver[i], data$count[i])
    trips <- rbind(trips, newrow)
  }
  pb$tick()
}
trips <- trips[-1,]
safers <- trips


trips$dist <- as.numeric(trips$dist)
trips$dist <- 1/(1+trips$dist)
summary(trips$dist)

countries <- unique(c(trips$place_2,trips$place_1))

ds <- matrix(1,nrow = length(countries),ncol = length(countries) )

pb <- progress_bar$new(total = length(countries))
pb$tick(0)

for(i in 1:length(countries)){
  for(j in 1:length(countries)){
    if(i < j){
      
    }
    else if(i==j){
      ds[i,j] = 0
    }
    else{
      if(as.factor(countries[i]):as.factor(countries[j]) %in% as.factor(trips$place_1):as.factor(trips$place_2)){
        ds[i,j] <- ds[j,i] <- trips$dist[intersect(which(trips$place_1 == countries[i]), which(trips$place_2==countries[j]))]
      }
    }
  }
  pb$tick()
}

dis <- as.dist(ds)

hc_ward = hclust(dis, method ="ward.D")
plot(hc_ward)
clus <- cutree(hc_ward, k=3)
clus
block_ward <- c(12,24,8,28,16,25,1,11,26,18,4,13,7,31,2,23)
block_ward_s <- c(16,25,1,11,26,18,4,13,7,31,2,23)
countries[which(clus==1)]
countries[which(clus==2)]
countries[which(clus==3)]

hc_comp = hclust(dis, method ="complete")
plot(hc_comp)
block_com<-c(12,24,8,28,16,11,26,18,13,4,7,31,2,29,1,25)
block_com_s<-c(16,11,26,18,13,4,7,31,2,29,1,25)
clus <- cutree(hc_comp, k=5)
countries[which(clus==1)]
countries[which(clus==2)]
countries[which(clus==3)]
countries[which(clus==4)]
countries[which(clus==5)]


hc_sin = hclust(dis, method ="single")
plot(hc_sin)


hc_avg = hclust(dis, method ="average")
plot(hc_avg)
block_avg <- c(16,12,24,8,28,25,1,11,26,18,4,13,7,31,2,23)
block_avg_s <- c(11,26,18,4,13,7,31,2,23)

clus <- cutree(hc_avg, k=5)
countries[which(clus==1)]
countries[which(clus==2)]
countries[which(clus==3)]
countries[which(clus==4)]
countries[which(clus==5)]


const_block <-unique(c(block_avg,block_com,block_ward))
const_block_s <-unique(c(block_avg_s,block_com_s,block_ward_s))
countries[const_block]
countries[const_block_s]
countries[const_block[-which(const_block %in% const_block_s)]]
