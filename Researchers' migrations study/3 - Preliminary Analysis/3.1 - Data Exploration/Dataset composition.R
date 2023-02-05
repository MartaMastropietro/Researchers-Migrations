
### DATA LOADING ###
load("...\Researchers' migrations analyses\2 - Data ORCID\NUTS 2 exctraction\ORCID_data_affiliations.RData")

data$NUTS3[which(data$NUTS3=="DE712")] <- "DE71"

IDs <- c()

library(progress)
pb <- progress_bar$new(total = dim(data)[1])
pb$tick(0)

indeces <- which(nchar(data$NUTS3)==4)
ind_time <- union(intersect(which(data$start_year >= 2009),which(data$start_year < 2021)), intersect(which(data$end_year >= 2009),which(data$end_year < 2021))) 

ind <- intersect(indeces, ind_time)

IDs <- unique(data$ORCiD[ind])

IDs_EDU <- unique(data$ORCiD[intersect(ind, which(data$aff_type=="EDU"))])

IDs_Phd <- unique(data$ORCiD[intersect(ind, which(data$is_phd=="True"))])

IDs_Pub <- unique(data$ORCiD[intersect(ind, which(data$has_published=="True"))])

IDs_temp <- unique(union(union(IDs_EDU, IDs_Phd), IDs_Pub))

IDS_normal <- IDs[-which(IDs %in% IDs_temp)]

#how many have reported education?
length(IDs_EDU)/length(IDs) # 0.5926013

#how many Phds?
length(IDs_Phd)/length(IDs) # 0.3999817

#how many have published?
length(IDs_Pub)/length(IDs) # 0.7215416

#how many normal people?
length(IDS_normal)/length(IDs) # 0.09520554
