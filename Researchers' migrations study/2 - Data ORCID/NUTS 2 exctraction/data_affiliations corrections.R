load("C:/Users/user/Desktop/università/dare/TESI/Git_repo/ORCiD_Thesis/Analyses/Dataset nuts2/NUTS 2 exctraction/ORCID_data_affiliations.RData")

summary(as.factor(nchar(data$NUTS3)))
data$NUTS3[which(nchar(data$NUTS3)==5)] <- "DE71"

data$NUTS2 <- data$NUTS3
data <- data[,-14]
save(data, file="C:/Users/user/Desktop/università/dare/TESI/Git_repo/ORCiD_Thesis/Analyses/Dataset nuts2/NUTS 2 exctraction/ORCID_data_affiliations.RData")
