#load("C:/Users/user/Desktop/forORCID/ORCID_Trips_wip.RData")
trips_in <- data.frame(receiver=c("receiver"), year=c(0), count=c(-15))
trips_out <- data.frame(sender=c("receiver"), year=c(0), count=c(-15))
trips_tot <- data.frame(NUTS=c("NUTS"), year=c(0), flow_in=c(-15), flow_out=c(-15))

library(progress)
pb <- progress_bar$new(total=nrow(trips))
pb$tick(0)

for(i in 1:nrow(trips)){
  if(nchar(trips$sender[i])==4){
    if(as.factor(trips$sender[i]):as.factor(trips$year[i]) %in% as.factor(trips_out$sender):as.factor(trips_out$year)){
      ind <- intersect(which(trips_out$sender==trips$sender[i]), which(trips_out$year==trips$year[i]))
      trips_out$count[ind] <- as.numeric(trips_out$count[ind]) + as.numeric(trips$count[ind])
    }
    else{
      row <- c(sender = trips$sender[i], year = trips$year[i], count = trips$count[i])
      trips_out <- rbind(trips_out, row)
    }
    if(as.factor(trips$sender[i]):as.factor(trips$year[i]) %in% as.factor(trips_tot$NUTS):as.factor(trips_tot$year)){
      ind <- intersect(which(trips_tot$NUTS==trips$sender[i]), which(trips_tot$year==trips$year[i]))
      trips_tot$flow_out[ind] <- as.numeric(trips_tot$flow_out[ind]) + as.numeric(trips$count[ind])
    }
    else{
      row <- c(NUTS = trips$sender[i], year = trips$year[i],flow_in = 0, flow_out = trips$count[i])
      trips_tot <- rbind(trips_tot, row)
    }
  }
  if(nchar(trips$receiver[i])==4){
    if(as.factor(trips$receiver[i]):as.factor(trips$year[i]) %in% as.factor(trips_in$receiver):as.factor(trips_in$year)){
      ind <- intersect(which(trips_in$receiver==trips$receiver[i]), which(trips_in$year==trips$year[i]))
      trips_in$count[ind] <- as.numeric(trips_in$count[ind]) + as.numeric(trips$count[ind])
    }
    else{
      row <- c(receiver = trips$receiver[i], year = trips$year[i], count = trips$count[i])
      trips_in <- rbind(trips_in, row)
    }
    if(as.factor(trips$receiver[i]):as.factor(trips$year[i]) %in% as.factor(trips_tot$NUTS):as.factor(trips_tot$year)){
      ind <- intersect(which(trips_tot$NUTS==trips$receiver[i]), which(trips_tot$year==trips$year[i]))
      trips_tot$flow_in[ind] <- as.numeric(trips_tot$flow_in[ind]) + as.numeric(trips$count[ind])
    }
    else{
      row <- c(NUTS = trips$receiver[i], year = trips$year[i],flow_in = trips$count[i], flow_out = 0)
      trips_tot <- rbind(trips_tot, row)
    }
  }
  pb$tick()
}

head(trips_in)
trips_in <- trips_in[-1,]
head(trips_out)
trips_out <- trips_out[-1,]
head(trips_tot)
trips_tot <- trips_tot[-1,]

trips_agg <- trips_tot
rownames(trips_in) <- NULL
rownames(trips_out) <- NULL
rownames(trips_agg) <- NULL

#save(trips_in, file="C:/Users/user/Desktop/forORCID/ORCID_Trips_in_wip.RData")
write.table(trips_in,file = "C:/Users/user/Desktop/forORCID/ORCID_Trips_in_wip.csv", sep = ",", row.names = FALSE)

#save(trips_out, file="C:/Users/user/Desktop/forORCID/ORCID_Trips_out_wip.RData")
write.table(trips_out,file = "C:/Users/user/Desktop/forORCID/ORCID_Trips_out_wip.csv", sep = ",", row.names = FALSE)

#save(trips_agg, file="C:/Users/user/Desktop/forORCID/ORCID_Trips_aggregated.RData")
write.table(trips_agg,file = "C:/Users/user/Desktop/forORCID/ORCID_Trips_aggregated.csv", sep = ",", row.names = FALSE)
