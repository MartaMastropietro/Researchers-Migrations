country <- "LU"
data_or <- data
indices <- which(data_or$country == country)

data_or$NUTS3[indices] <- "LU000"

data$NUTS3[indices] <- data_or$NUTS3[indices]

country <- "LV"
data_or <- data
indices <- which(data_or$country == country)

data_or$NUTS3[indices] <- "LV003"

data$NUTS3[indices] <- data_or$NUTS3[indices]

country <- "MT"
data_or <- data
indices <- which(data_or$country == country)

data_or$NUTS3[indices] <- "MT001"

data$NUTS3[indices] <- data_or$NUTS3[indices]


save(data, file="C:/Users/user/Desktop/forORCID/ORCID_2021_activities_extract_uptoMT.RData")
