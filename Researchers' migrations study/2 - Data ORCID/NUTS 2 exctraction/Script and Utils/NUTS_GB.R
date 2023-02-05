library(progress)
country <- "GB"
colnames(pc2020_UK_NUTS.2021_v.3.0)<-c("NUTS3", "PCode")
pc2020_UK_NUTS.2021_v.3.0 <- pc2020_UK_NUTS.2021_v.3.0[-1,]

data_or <- data
data_pc <- geonames.postal.codes_filtered
data_nuts <- pc2020_UK_NUTS.2021_v.3.0
data_nuts$PCode <- gsub(" .*", "", data_nuts$PCode)

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


data_or$city[which(data_or$city=="london")]<-"greaterlondon"
data_or$city[which(data_or$city=="londres")]<-"greaterlondon"
data_or$city[which(data_or$city=="uxbridge")]<-"greaterlondon"
data_or$city[which(data_or$city=="teddington")]<-"greaterlondon"
data_or$city[which(data_or$city=="edinburgh")]<-"cityofedinburgh"
data_or$city[which(data_or$city=="belfast")]<-"belfastgreater"
data_or$city[which(data_or$city=="londonderry")]<-"belfastgreater"
data_or$city[which(data_or$city=="coleraine")]<-"belfastgreater"
data_or$city[which(data_or$city=="newtownabbey")]<-"belfastgreater"
data_or$city[which(data_or$city=="glasgow")]<-"glasgowcity"
data_or$city[which(data_or$city=="oxford")]<-"oxfordshire"
data_or$city[which(data_or$city=="abingdon")]<-"oxfordshire"
data_or$city[which(data_or$city=="wallingford")]<-"oxfordshire"
data_or$city[which(data_or$city=="didcot")]<-"oxfordshire"
data_or$city[which(data_or$city=="cambridge")]<-"cambridgeshire"
data_or$city[which(data_or$city=="peterborough")]<-"cambridgeshire"
data_or$city[which(data_or$city=="lincoln")]<-"lincolnshire"
data_or$city[which(data_or$city=="manchester")]<-"greatermanchester"
data_or$city[which(data_or$city=="salford")]<-"greatermanchester"
data_or$city[which(data_or$city=="bolton")]<-"greatermanchester"
data_or$city[which(data_or$city=="stockport")]<-"greatermanchester"
data_or$city[which(data_or$city=="bristol")]<-"gloucestershire"
data_or$city[which(data_or$city=="pontypridd")]<-"rhonddacynontaff"
data_or$city[which(data_or$city=="sheffield")]<-"southyorkshire"
data_or$city[which(data_or$city=="doncaster")]<-"southyorkshire"
data_or$city[which(data_or$city=="leeds")]<-"westyorkshire"
data_or$city[which(data_or$city=="wakefield")]<-"westyorkshire"
data_or$city[which(data_or$city=="huddersfield")]<-"westyorkshire"
data_or$city[which(data_or$city=="bradford")]<-"westyorkshire"
data_or$city[which(data_or$city=="nottingham")]<-"nottinghamshire"
data_or$city[which(data_or$city=="derby")]<-"nottinghamshire"
data_or$city[which(data_or$city=="birmingham")]<-"westmidlands"
data_or$city[which(data_or$city=="wolverhampton")]<-"westmidlands"
data_or$city[which(data_or$city=="penicuik")]<-"midlothian"
data_or$city[which(data_or$city=="york")]<-"northyorkshire"
data_or$city[which(data_or$city=="middlesbrough")]<-"northyorkshire"
data_or$city[which(data_or$city=="southampton")]<-"hampshire"
data_or$city[which(data_or$city=="winchester")]<-"hampshire"
data_or$city[which(data_or$city=="portsmouth")]<-"hampshire"
data_or$city[which(data_or$city=="liverpool")]<-"merseyside"
data_or$city[which(data_or$city=="brighton")]<-"westsussex"
data_or$city[which(data_or$city=="aberystwyth")]<-"gwynedd"
data_or$city[which(data_or$city=="newcastleupontyne")]<-"northumberland"
data_or$city[which(data_or$city=="newcastle")]<-"northumberland"
data_or$city[which(data_or$city=="sunderland")]<-"northumberland"
data_or$city[which(data_or$city=="norwich")]<-"suffolk"
data_or$city[which(data_or$city=="aberdeen")]<-"aberdeencity"
data_or$city[which(data_or$city=="coventry")]<-"warwickshire"
data_or$city[which(data_or$city=="leicester")]<-"leicestershire"
data_or$city[which(data_or$city=="dundee")]<-"dundeecity"
data_or$city[which(data_or$city=="miltonkeynes")]<-"buckinghamshire"
data_or$city[which(data_or$city=="bath")]<-"somerset"
data_or$city[which(data_or$city=="cranfield")]<-"bedfordshire"
data_or$city[which(data_or$city=="luton")]<-"bedfordshire"
data_or$city[which(data_or$city=="standrews")]<-"fife"
data_or$city[which(data_or$city=="st.andrews")]<-"fife"
data_or$city[which(data_or$city=="hull")]<-"eastridingofyorkshire"
data_or$city[which(data_or$city=="gloucester")]<-"gloucestershire"
data_or$city[which(data_or$city=="darlington")]<-"durham"
data_or$city[which(data_or$city=="reading")]<-"berkshire"
data_or$city[which(data_or$city=="slough")]<-"berkshire"
data_or$city[which(data_or$city=="plymouth")]<-"devon"
data_or$city[which(data_or$city=="exeter")]<-"devon"
data_or$city[which(data_or$city=="chester")]<-"cheshire"
data_or$city[which(data_or$city=="warrington")]<-"cheshire"
data_or$city[which(data_or$city=="macclesfield")]<-"cheshire"
data_or$city[which(data_or$city=="preston")]<-"lancashire"
data_or$city[which(data_or$city=="lancaster")]<-"lancashire"
data_or$city[which(data_or$city=="blackpool")]<-"lancashire"
data_or$city[which(data_or$city=="ormskirk")]<-"lancashire"
data_or$city[which(data_or$city=="bailrigg")]<-"lancashire"
data_or$city[which(data_or$city=="swindon")]<-"wiltshire"
data_or$city[which(data_or$city=="northampton")]<-"northamptonshire"
data_or$city[which(data_or$city=="hatfield")]<-"hertfordshire"
data_or$city[which(data_or$city=="stevenage")]<-"hertfordshire"
data_or$city[which(data_or$city=="bournemouth")]<-"dorset"
data_or$city[which(data_or$city=="poole")]<-"dorset"
data_or$city[which(data_or$city=="loughborough")]<-"leicestershire"
data_or$city[which(data_or$city=="musselburgh")]<-"eastlothian"
data_or$city[which(data_or$city=="bangor")]<-"gwynedd"
data_or$city[which(data_or$city=="bedford")]<-"bedfordshire"
data_or$city[which(data_or$city=="stokeontrent")]<-"staffordshire"
data_or$city[which(data_or$city=="guildford")]<-"surrey"
data_or$city[which(data_or$city=="egham")]<-"surrey"
data_or$city[which(data_or$city=="canterbury")]<-"kent"
data_or$city[which(data_or$city=="colchester")]<-"essex"
data_or$city[which(data_or$city=="worcester")]<-"worcestershire"


data_or$region[which(data_or$region=="london")]<-"greaterlondon"
data_or$region[which(data_or$region=="glasgow")]<-"glasgowcity"
data_or$region[which(data_or$region=="northernireland")]<-"belfastgreater"
data_or$region[which(data_or$region=="salford")]<-"greatermanchester"

cities1 <- levels(as.factor(data_pc$admin.name2[which(data_pc$country.code==country)]))
cities2 <- levels(as.factor(data_pc$admin.name3[which(data_pc$country.code==country)]))


manuals <- data.frame(cbind(Region = c("Region"), City = c("City"), ID = c("id")))

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
    manuals <- rbind(manuals, c(data_or$region[i], data_or$city[i], i))
  }
  pb$tick()
}

#levels(as.factor(data$NUTS3))
#manuals
#write.table(manuals,file = "C:/Users/user/Desktop/manuals_GB.csv", sep = ",", row.names = FALSE)

data$NUTS3[indices] <- data_or$NUTS3[indices]
save(data, file="C:/Users/user/Desktop/forORCID/ORCID_2021_activities_extract_uptoGB.RData")
