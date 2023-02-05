library(progress)
country <- "DE"
colnames(pc2020_IT_NUTS.2021_v1.0)<-c("NUTS3", "PCode")
pc2020_IT_NUTS.2021_v1.0 <- pc2020_IT_NUTS.2021_v1.0[-1,]

data_or <- ORCID_2021_activities_extract_full
data_pc <- geonames.postal.codes_filtered
data_nuts <- pc2020_DE_NUTS.2021_v4.0

levels(as.factor(data_pc$admin.name1[which(data_pc$country.code == country)]))
levels(as.factor(data_pc$admin.name2[which(data_pc$country.code == country)]))
levels(as.factor(data_pc$admin.name3[which(data_pc$country.code == country)]))

indices <- which(data_or$country == country)

data_or$region <- tolower(data_or$region)
data_or$city <- tolower(data_or$city)
data_pc$admin.name1 <- tolower(data_pc$admin.name1)
data_pc$admin.name2 <- tolower(data_pc$admin.name2)
data_pc$admin.name3 <- tolower(data_pc$admin.name3)
data_or$city <- gsub(" ", "", data_or$city)
data_or$city <- gsub("-", "", data_or$city)
data_or$region <- gsub(" ", "", data_or$region)
data_or$region <- gsub("-", "", data_or$region)
data_pc$admin.name1 <- gsub(" ", "", data_pc$admin.name1)
data_pc$admin.name1 <- gsub("-", "", data_pc$admin.name1)
data_pc$admin.name2 <- gsub(" ", "", data_pc$admin.name2)
data_pc$admin.name2 <- gsub("-", "", data_pc$admin.name2)
data_pc$admin.name3 <- gsub(" ", "", data_pc$admin.name3)
data_pc$admin.name3 <- gsub("-", "", data_pc$admin.name3)


data_or$city[which(data_or$city=="")]<-""

cities1 <- levels(as.factor(data_pc$admin.name2[which(data_pc$country.code==country)]))
cities2 <- levels(as.factor(data_pc$admin.name3[which(data_pc$country.code==country)]))


manuals <- data.frame(cbind(Region = c("Region"), City = c("City")))

pb <- progress_bar$new(total=length(indices))
pb$tick(0)

for(i in indices){
  ind2 <- intersect(data_or[i,6], cities1)
  if(length(ind2) != 0 && ind2 != ""){
     poss_code <- data_pc$postal.code[which(data_pc$admin.name2==data_or[i,6])]
     found <- FALSE
     for(c in poss_code){
       if(length(which(data_nuts$PCode==c))!=0){
         code <- c
         found <- TRUE
         break
       }
     }
     if(found){
       data_or$NUTS3[i] <- data_nuts$NUTS3[which(data_nuts$PCode==code)]
     }
   }
  else{
     ind3 <- intersect(data_or[i,6], cities2)
     if(length(ind3) != 0 && ind3 != ""){
       poss_code <- data_pc$postal.code[which(data_pc$admin.name3==data_or[i,6])]
       found <- FALSE
       for(c in poss_code){
         if(length(which(data_nuts$PCode==c))!=0){
           code <- c
           found <- TRUE
           break
         }
       }
       if(found){
         data_or$NUTS3[i] <- data_nuts$NUTS3[which(data_nuts$PCode==code)]
       }
     }
     else{
       ind4 <- intersect(data_or[i,5], cities1)
       if(length(ind4) != 0 && ind4 != ""){
         poss_code <- data_pc$postal.code[which(data_pc$admin.name2==data_or[i,5])]
         found <- FALSE
         for(c in poss_code){
           if(length(which(data_nuts$PCode==c))!=0){
             code <- c
             found <- TRUE
             break
           }
         }
         if(found){
           data_or$NUTS3[i] <- data_nuts$NUTS3[which(data_nuts$PCode==code)]
         }
       }
       else{
         ind5 <- intersect(data_or[i,5], cities2)
         if(length(ind5) != 0 && ind5 != ""){
           poss_code <- data_pc$postal.code[which(data_pc$admin.name3==data_or[i,5])]
           found <- FALSE
           for(c in poss_code){
             if(length(which(data_nuts$PCode==c))!=0){
               code <- c
               found <- TRUE
               break
             }
           }
           if(found){
             data_or$NUTS3[i] <- data_nuts$NUTS3[which(data_nuts$PCode==code)]
           }
         }
       }
       
     }
   }
  
  if(data_or$NUTS3[i]=="None"){
    manuals <- rbind(manuals, c(data_or$region[i], data_or$city[i]))
  }
  pb$tick()
}

#levels(as.factor(data_or$NUTS3))
manuals
