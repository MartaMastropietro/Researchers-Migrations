
anova_np<-function(mod_big,mod_small,data,seed=999,B=1000){
  
  
  rsq_big<-summary(mod_big)$r.sq
  aic_big<-mod_big$aic
  
  rsq_small<-summary(mod_small)$r.sq
  aic_small<-mod_small$aic
  
  fit <- mod_small$fitted.values
  res <- mod_small$residuals
  
  a <- anova(mod_small, mod_big, test = "F")
  T_0 <- a$F[2]
  if(is.na(T_0)){
    warning("T_0 is NaN, test is stopped")
    return(list(p.val=NULL,T0=NULL,T.stat=NULL,rsq_small=rsq_small,rsq_big=rsq_big,aic_small=aic_small,aic_big=aic_big))
  }
  
  library(progress)
  pb <- progress_bar$new(total=B)
  pb$tick(0)
  
  T_stat <- c()
  data_perm<-data
  
  set.seed(seed)
  for(b in 1:B){
    success=F
    res_perm <- sample(res)
    y_perm <- fit + res_perm
    
    data_perm[as.character(mod_small$formula)[2]] <- y_perm
    try({
    mod_perm <- gam( mod_big$formula, data=data_perm)
    success=T}
    
    
    )
    if (success){
      
      a_perm <- anova(mod_small, mod_perm, test = "F")
      T_stat <- append(T_stat, a_perm$F[2]) 
    }
    if(!success){
      T_stat <- append(T_stat, NULL) 
    }
    pb$tick()
  }
  
  p_val <- sum(T_stat >= T_0)/B
  
  if(is.na(p_val)){
    T_stat_upper<-T_stat
    T_stat_upper[which(is.na(T_stat_upper))] <- T_0+1
    T_stat_lower<-T_stat
    T_stat_lower[which(is.na(T_stat_lower))] <- T_0-1
    p_val <- c(lower = sum(T_stat_lower >= T_0)/B, upper = sum(T_stat_upper >= T_0)/B)
    warning("NaN are produced, p-value bounds are provided instead")
  }
  
  ret<-list(p.val=p_val,T0=T_0,T.stat=T_stat,rsq_small=rsq_small,rsq_big=rsq_big,aic_small=aic_small,aic_big=aic_big)
  return(ret)
  
}

