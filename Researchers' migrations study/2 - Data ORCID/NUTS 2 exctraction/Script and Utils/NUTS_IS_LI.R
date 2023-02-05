country <- "IS"
data_or <- data
indices <- which(data_or$country == country)

data_or$NUTS3[indices] <- "IS001"

data$NUTS3[indices] <- data_or$NUTS3[indices]


country <- "LI"
data_or <- data
indices <- which(data_or$country == country)

data_or$NUTS3[indices] <- "LI000"

data$NUTS3[indices] <- data_or$NUTS3[indices]


save(data, file="C:/Users/user/Desktop/forORCID/ORCID_2021_activities_extract_uptoIS_LI.RData")
