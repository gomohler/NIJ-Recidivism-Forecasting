score_models<-function(train,prd,cutoff){
  
  train$Race=as.integer(train$Race)
  MSE=mean((prd-train$Y1)^2)
  prnd=as.numeric(prd>=cutoff)
  
  FP1=sum(prnd[train$Race==1]==1&train$Y1[train$Race==1]==0)/sum(train$Y1[train$Race==1]==0)
  FP2=sum(prnd[train$Race==2]==1&train$Y1[train$Race==2]==0)/sum(train$Y1[train$Race==2]==0)
 
  FAIRSCORE=(1-MSE)*(1-abs(FP1-FP2))
  
  output=c(MSE,FP1,FP2,FAIRSCORE)
  return(output)
}

score_models_bootstrap<-function(train,prd,cutoff){
  set.seed(1)
  for(i in 1:1000){
    ind=sample(nrow(train),nrow(train),T)
    dtrain=train[ind,]
    pdrd=prd[ind]
    if(i==1){
    z=score_models(dtrain,pdrd,cutoff)
    otpt=data.frame(MSE=z[1],FP1=z[2],FP2=z[3],FAIRSCORE=z[4])
    }else{
    z=score_models(dtrain,pdrd,cutoff)
    otptt=data.frame(MSE=z[1],FP1=z[2],FP2=z[3],FAIRSCORE=z[4])  
    otpt=rbind(otpt,otptt)
    }
  }
  
  return(otpt %>% dplyr::summarise(MSE=qwraps2::mean_sd(MSE,denote_sd = "paren",digits=3),
                                  FP1=qwraps2::mean_sd(FP1,denote_sd = "paren",digits=3),
                                  FP2=qwraps2::mean_sd(FP2,denote_sd = "paren",digits=3),
                                  FAIRSCORE=qwraps2::mean_sd(FAIRSCORE,denote_sd = "paren",digits=3)))
}



