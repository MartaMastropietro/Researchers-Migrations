library(progress)
country <- "AT"
colnames(pc2020_AT_NUTS.2021_v1.0)<-c("NUTS3", "PCode")
pc2020_AT_NUTS.2021_v1.0 <- pc2020_AT_NUTS.2021_v1.0[-1,]

data_or <- data
data_pc <- geonames.postal.codes_filtered
data_nuts <- pc2020_AT_NUTS.2021_v1.0

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

cities1 <- levels(as.factor(data_pc$admin.name2[which(data_pc$country.code==country)]))
cities2 <- levels(as.factor(data_pc$admin.name3[which(data_pc$country.code==country)]))
regions <- levels(as.factor(data_pc$admin.name1[which(data_pc$country.code==country)]))

data_or$city[which(data_or$city=="")]<-""
data_or$city[which(data_or$city=="vienna")]<-"wien"
data_or$city[which(data_or$city=="vieann")]<-"wien"
data_or$city[which(data_or$city=="vianna")]<-"wien"
data_or$city[which(data_or$city=="a1200wien")]<-"wien"
data_or$city[which(data_or$city=="vinna")]<-"wien"
data_or$city[which(data_or$city=="vienn")]<-"wien"
data_or$city[which(data_or$city=="vienne")]<-"wien"
data_or$city[which(data_or$city=="vein")]<-"wien"
data_or$city[which(data_or$city=="vienna|wien")]<-"wien"
data_or$city[which(data_or$city=="vienna,wien")]<-"wien"
data_or$city[which(data_or$city=="wien/vienna")]<-"wien"
data_or$city[which(data_or$city=="viennna")]<-"wien"
data_or$city[which(data_or$city=="viena")]<-"wien"
data_or$city[which(data_or$city=="vienna(austria)")]<-"wien"
data_or$city[which(data_or$city=="vienna,austria")]<-"wien"
data_or$city[which(data_or$city=="vienna,at")]<-"wien"
data_or$city[which(data_or$city=="krems")]<-"kremsanderdonau"
data_or$city[which(data_or$city=="3500krems")]<-"kremsanderdonau"
data_or$city[which(data_or$city=="8700leoben")]<-"leoben"
data_or$city[which(data_or$city=="tulln")]<-"tullnanderdonau"
data_or$city[which(data_or$city=="klosteneuburg")]<-"tullnanderdonau"
data_or$city[which(data_or$city=="klosterneubrg")]<-"tullnanderdonau"
data_or$city[which(data_or$city=="sanktpã¶lten")]<-"st.pã¶lten"
data_or$city[which(data_or$city=="st.poelten")]<-"st.pã¶lten"
data_or$city[which(data_or$city=="st.poì^lten")]<-"st.pã¶lten"
data_or$city[which(data_or$city=="stpolten")]<-"st.pã¶lten"
data_or$city[which(data_or$city=="poelten")]<-"st.pã¶lten"
data_or$city[which(data_or$city=="stpoelten")]<-"st.pã¶lten"
data_or$city[which(data_or$city=="st.polen")]<-"st.pã¶lten"
data_or$city[which(data_or$city=="3100st.pã¶lten")]<-"st.pã¶lten"
data_or$city[which(data_or$city=="hagenberg")]<-"hagenbergimmã¼hlkreis"
data_or$city[which(data_or$city=="styria")]<-"steiermark"
data_or$city[which(data_or$city=="salzbug")]<-"salzburg"
data_or$city[which(data_or$city=="innsbruck,austria")]<-"innsbruck"
data_or$city[which(data_or$city=="sanktvalentin")]<-"st.valentin"
data_or$city[which(data_or$city=="donawitz")]<-"steiermark"
data_or$city[which(data_or$city=="kostendorf")]<-"salzburg"
data_or$city[which(data_or$city=="buers")]<-"vorarlberg"
data_or$city[which(data_or$city=="6020")]<-"tirol"
data_or$city[which(data_or$city=="sanatoriumstraãÿe43,6511zams")]<-"tirol"
data_or$city[which(data_or$city=="4020linz")]<-"oberã¶sterreich"
data_or$city[which(data_or$city=="gratz")]<-"graz"
data_or$city[which(data_or$city=="hall")]<-"hallintirol"
data_or$city[which(data_or$city=="eduardwallnã¶ferzentrum1,6060hall")]<-"hallintirol"
data_or$city[which(data_or$city=="hainburganderdonau")]<-"hainburga.d.donau"
data_or$city[which(data_or$city=="siebersdorf")]<-"seibersdorf"
data_or$city[which(data_or$city=="grafendorf")]<-"grafendorfbeihartberg"
data_or$city[which(data_or$city=="sanktveitanderglan")]<-"st.veitanderglan"
data_or$city[which(data_or$city=="klagenfurt")]<-"klagenfurtamwã¶rthersee"
data_or$city[which(data_or$city=="9020klagenfurt")]<-"klagenfurtamwã¶rthersee"
data_or$city[which(data_or$city=="secunderabad&klagenfurt")]<-"klagenfurtamwã¶rthersee"
data_or$region[which(data_or$region=="vienna")]<-"wien"
data_or$region[which(data_or$region=="tyrol")]<-"tirol"
data_or$region[which(data_or$region=="loweraustria")]<-"niederã¶sterreich"
data_or$region[which(data_or$region=="upperaustria")]<-"oberã¶sterreich"
data_or$region[which(data_or$region=="oberã¶sterrreich")]<-"oberã¶sterreich"
data_or$region[which(data_or$region=="oberoesterreich")]<-"oberã¶sterreich"
data_or$region[which(data_or$region=="styria")]<-"steiermark"
data_or$region[which(data_or$region=="styria,at")]<-"steiermark"
data_or$region[which(data_or$region=="carinthia")]<-"kã¤rnten"
data_or$region[which(data_or$region=="kã¤rnten(carinthia)")]<-"kã¤rnten"
data_or$region[which(data_or$region=="a1070wien")]<-"wien"
data_or$region[which(data_or$region=="pã¶lten")]<-"st.pã¶lten"


data_or$city[which(data_or$city=="lunzamsee")]<-"scheibbs"
data_or$city[which(data_or$city=="badenbeiwien")]<-"baden"

manuals <- data.frame(cbind(Region = c("Region"), City = c("City")))

pb <- progress_bar$new(total=length(indices))
pb$tick(0)

for(i in indices){
  ind1 <- intersect(data_or[i,5], regions)
  if(length(ind1) != 0 && ind1 != ""){
    poss_code <- data_pc$postal.code[which(data_pc$admin.name1==data_or[i,5])]
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
          else{
            ind6 <- intersect(data_or[i,6], regions)
            if(length(ind6) != 0 && ind6 != ""){
              poss_code <- data_pc$postal.code[which(data_pc$admin.name1==data_or[i,6])]
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
    }
  }
  if(data_or$NUTS3[i]=="None"){
    manuals <- rbind(manuals, c(data_or$region[i], data_or$city[i]))
  }
  pb$tick()
}

#levels(as.factor(data_or$NUTS3[which(data_or$country==country)]))
#manuals

data$NUTS3[indices] <- data_or$NUTS3[indices]
save(data, file="C:/Users/user/Desktop/forORCID/ORCID_2021_activities_extract_uptoAT.RData")
