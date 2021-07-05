fair_shrink<-function(prediction,label,race,epsilon,cutoff){
  BSmax=0
  epsBest=0
  BSvec=numeric(length(epsilon))
  i=0
  for(eps in epsilon){
    i=i+1
  pred_tmp=prediction  
  pred_tmp[prediction>=cutoff&race==1]=pred_tmp[prediction>=cutoff&race==1]-eps
  pred_tmp[prediction>=cutoff&race==1&pred_tmp<cutoff]=cutoff-.0001
  
  prnd=as.numeric(pred_tmp>=cutoff)
  FP1=sum(prnd[race==1]==1&label[race==1]==0)/sum(label[race==1]==0)
  FP2=sum(prnd[race==2]==1&label[race==2]==0)/sum(label[race==2]==0)
  
  MSE=mean((pred_tmp-label)^2)
  BS=(1-MSE)*(1-abs(FP1-FP2))
 
  BSvec[i]=BS
  if(BS>BSmax){
    BSmax=BS
    epsBest=eps
    predBest=pred_tmp
  }
  
  }
  
  return(list(BSmax,epsBest,predBest,BSvec))
  
}