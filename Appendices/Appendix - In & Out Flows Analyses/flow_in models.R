
library(mgcv)
library(mgcViz)
library(readr)

my_summary<-function(mod,data){
  rsq<-as.numeric(cor(mod$fitted.values,data[as.character(mod$formula)[2]]))
  adj_rsq<-summary(mod)$r.sq
  aic<-AIC(mod)
  
  return(c(rsq=rsq,adj_rsq=adj_rsq,aic=aic))
}

options(scipen=999)

data_trips_gdp_score_ted_edu <- read_delim("D:/Marta/Politecnico/Tesi/data_trips_gdp_score_ted_edu.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
data <- data_trips_gdp_score_ted_edu
cor(data$flow_in,data$flow_out)#0.988

# log flow out ~ log gdp, r3score, ted, edu index, re country, re year
data$year <- as.factor(data$year)
data$country <- gsub(".{2}$","", data$nuts2)
data$country <- as.factor(data$country)
data$ted <- as.numeric(data$ted)

data$uni_score <- (data$score)^(1/3)
data$log_flow <- log(data$flow_in +1)

#complete model smoth
mod<-gam(log_flow~ s(log(gdp),bs='cr')+s(uni_score,bs='cr',k=6)+s(log(ted),bs='cr',k=6)+s(edu_index, bs='cr')+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod,data) #  0.6812008     0.4561774 12000.5405912
print(plot(getViz(mod), allTerms = T), pages = 1)

#single
mod_ted<-gam(log_flow~ s(log(ted),bs='cr',k=6)+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod_ted,data) # 0.5874739     0.3368861 12693.3562988 
mod_score<-gam(log_flow~ s(uni_score,bs='cr',k=6)+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod_score,data)# 0.6260403     0.3848228 12425.6632806 
mod_gdp<-gam(log_flow~ s(log(gdp),bs='cr')+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod_gdp,data) #0.5389214     0.2815298 12975.5920929 
mod_edu<-gam(log_flow~ s(edu_index, bs='cr')+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod_edu,data)# 0.5754162     0.3227901 12766.5224751 

a_t<-anova_np(mod,mod_ted,data)
a_s<-anova_np(mod,mod_score,data)
a_e<-anova_np(mod,mod_edu,data)
a_g<-anova_np(mod,mod_gdp,data)

#linear gdp, no edu
mod0<-gam(log_flow~ log(gdp)+s(uni_score,bs='cr',k=6)+s(ted,bs='cr', k=6)+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod0,data) # 0.6637087     0.4330515 12143.2819621 
print(plot(getViz(mod0), allTerms = T), pages = 1)

a0<-anova_np(mod,mod0,data)

#only ted score
mod1<-gam(log_flow~ s(uni_score,bs='cr',k=6)+s(ted,bs='cr', k=6)+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod1,data) # 0.6578422     0.4252332 12191.3792604 
print(plot(getViz(mod1), allTerms = T), pages = 1)

a1<-anova_np(mod,mod1,data) #no reject, ted score enough 

#only score edu 
mod2<-gam(log_flow~ s(uni_score,bs='cr',k=6)+s(edu_index,bs='cr')+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod2,data) # 0.6615963     0.4302971 12159.4721091 
print(plot(getViz(mod2), allTerms = T), pages = 1)

a2<-anova_np(mod,mod2,data) #no reject, edu score enough 

#only score gdp
mod3<-gam(log_flow~ s(uni_score,bs='cr',k=6)+log(gdp)+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod3,data) # 0.6395219     0.4020551 12325.4496946 
print(plot(getViz(mod3), allTerms = T), pages = 1)

a3<-anova_np(mod,mod3,data) #0.06 limit 

#ted score edu
mod4<-gam(log_flow~ s(uni_score,bs='cr',k=6)+s(ted,bs='cr', k=6)+s(edu_index,bs='cr')+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod4,data) # 0.6798476     0.4545468 12009.6776015 
print(plot(getViz(mod4), allTerms = T), pages = 1)

a4<-anova_np(mod,mod4,data) #no reject 

#final model: score+ted or score+edu or score+ted+edu