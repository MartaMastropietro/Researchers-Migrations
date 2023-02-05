setwd("D:/Marta/Politecnico/Tesi/Github/ORCiD_Thesis/Dataset")
TED_imputed_2009_2020 <- read_delim("Coovariates/TED 2009-2020 imputed.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)
ORCID_Trips_aggregated <- read_csv("Trips/ORCID_Trips_aggregated.csv")
UniScore_2009_2020 <- read_csv("Coovariates/UniScore 2009-2020.csv")

ORCID_Trips_aggregated$country<-gsub('.{2}$','',ORCID_Trips_aggregated$nuts2)
TED_imputed_2009_2020$country<-gsub('.{2}$','',TED_imputed_2009_2020$nuts2)
UniScore_2009_2020$country<-gsub('.{2}$','',UniScore_2009_2020$NUTS2)

data_or<-ORCID_Trips_aggregated[,-1]
data_ted<-TED_imputed_2009_2020[,-1]
data_ted$ted_im<-exp(data_ted$ted_im)
data_uni<-UniScore_2009_2020[,-1]
data_or_in<-data_or[,-3]
data_or_out<-data_or[,-2]

data_or_in<-aggregate(flow_in~year:country,data_or_in,sum)
data_or_out<-aggregate(flow_out~year:country,data_or_out,sum)

data_or_in<-data_or_in[which(data_or_in$year>2008),]
data_or_in<-data_or_in[which(data_or_in$year<2021),]

data_or_out<-data_or_out[which(data_or_out$year>2008),]
data_or_out<-data_or_out[which(data_or_out$year<2021),]

data_or_in<-data_or_in[which(data_or_in$country!="LI"),]
data_or_out<-data_or_out[which(data_or_out$country!="LI"),]

data_or_final<-data_or_in
data_or_final$flow_out<-data_or_out$flow_out

setwd("D:/Marta/Politecnico/Tesi/Analisi nazionali")

write.csv(data_or_final,"trips_by_country.csv",row.names = F)

data_ted_final<-aggregate(ted_im~year:country,data_ted,sum)
data_ted_final$ted_im<-log(data_ted_final$ted_im)
write.csv(data_ted_final,"log(ted)_by_country.csv",row.names = F)

nuts_years <- read_excel("D:/Marta/Politecnico/Tesi/Github/ORCiD_Thesis/Dataset/nuts_years.xlsx")
country_year<-nuts_years
country_year$country<-gsub('.{2}$','',country_year$nuts2)
country_year<-country_year[,-1]
country_year<-unique(country_year)
# write.csv(country_year,"country_years.csv",row.names = F)


data_uni_final<-aggregate(Score~year:country,data_uni,sum)
data_uni_final1<-merge(data_uni_final,country_year,all=T)
data_uni_final1$Score[which(is.na(data_uni_final1$Score))]=0
write.csv(data_uni_final1,"uniscore_by_country.csv",row.names = F)

#edu index national in edu index folder, under imputations 

