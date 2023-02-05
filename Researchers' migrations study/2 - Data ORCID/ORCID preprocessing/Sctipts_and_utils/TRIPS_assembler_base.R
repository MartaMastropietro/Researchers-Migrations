#load("C:/Users/user/Desktop/forORCID/ORCID_data_affiliations.RData")
country_list <- c("AT","BE","BG","CY","CZ","DE","DK","EE","ES","FI","FR","GB","GR","HR","HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK","IS","NO","CH","LI")
data_extr <- data[,c(1,4,7,9,14)]

#remove all indices that appear only once (no movement is possible)
to_keep <- data_extr$ORCiD[duplicated(data_extr$ORCiD)]
data_extr <- data_extr[which(data_extr$ORCiD %in% to_keep),]

#remove all IDs that never visited country_list
to_keep <- data_extr$ORCiD[which(data_extr$country %in% country_list)]
data_extr <- data_extr[which(data_extr$ORCiD %in% to_keep),]

trips <- data.frame(cbind(sender=c("SENDER"), receiver= c("RECEIVER"), year=c(-1), count=c(-10^18)))


indices <- unique(data_extr$ORCiD)


library(progress)
pb <- progress_bar$new(total=length(indices))
pb$tick(0)

for(i in indices){
  data_in <- data_extr[which(data_extr$ORCiD==i),]
  data_in<-data_in[order(data_in$start_year, data_in$end_year),]
    for(j in (nrow(data_in)-1)){
      if((!is.na(data_in$NUTS2[j]))&&(!is.na(data_in$NUTS2[j+1]))&&(data_in$NUTS2[j] != data_in$NUTS2[j+1])){
        if(nrow(merge(data.frame(sender=data_in$NUTS2[j],receiver=data_in$NUTS2[j+1],year=data_in$start_year[j+1]), trips[,1:3]))>0){
#        if(as.factor(data_in$NUTS2[j]):as.factor(data_in$NUTS2[j+1]):as.factor(data_in$start_year[j+1]) %in% as.factor(trips$sender):as.factor(trips$receiver):as.factor(trips$year)){
          index <- intersect(intersect(which(trips$sender==data_in$NUTS2[j]), which(trips$receiver==data_in$NUTS2[j+1])), which(trips$year==data_in$start_year[j+1]))
          trips$count[index] <- as.numeric(trips$count[index])+1
        }
        else if(nrow(merge(data.frame(sender=data_in$NUTS2[j],receiver=data_in$NUTS2[j+1],year=data_in$end_year[j]), trips[,1:3]))>0){
#        else if(as.factor(data_in$NUTS2[j]):as.factor(data_in$NUTS2[j+1]):as.factor(data_in$end_year[j]) %in% as.factor(trips$sender):as.factor(trips$receiver):as.factor(trips$year)){
          index <- intersect(intersect(which(trips$sender==data_in$NUTS2[j]), which(trips$receiver==data_in$NUTS2[j+1])), which(trips$year==data_in$end_year[j]))
          trips$count[index] <- as.numeric(trips$count[index])+1
        }
        else{
          if(is.na(as.numeric(data_in$start_year[j+1]))){
            if(!is.na(as.numeric(data_in$end_year[j]))){
              row <- c(sender=data_in$NUTS2[j], receiver=data_in$NUTS2[j+1], year=as.numeric(data_in$end_year[j]), count = as.numeric(1))
              trips <- rbind(trips,row)
            }
          }
          else{
              row <- c(sender=data_in$NUTS2[j], receiver=data_in$NUTS2[j+1], year=as.numeric(data_in$start_year[j+1]), count = as.numeric(1))
              trips <- rbind(trips,row)
            }
        }
      }
    }
  
  
  pb$tick()
}

trips <- trips[-1,]

#save(trips, file="C:/Users/user/Desktop/forORCID/ORCID_Trips_wip.RData")
write.table(trips,file = "C:/Users/user/Desktop/forORCID/ORCID_Trips_wip.csv", sep = ",", row.names = FALSE)
