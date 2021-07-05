source('create_new_features.R')
source('score_models.R')
source('fair_shrink.R')
source('define_vectors.R')
library(xgboost)

for(cutoff in c(.3,.5,.7)){
set.seed(3)

all_ids=new_train$ID
all_ids=sample(all_ids,length(all_ids),F)
train_ids=sort(all_ids[1:9000])
test_ids=sort(all_ids[9001:18000])

new_train$Gender=as.integer(new_train$Gender)
new_train$Race=as.integer(new_train$Race)
new_train$Age_at_Release=as.integer(new_train$Age_at_Release)
new_train$Prison_Years=as.integer(new_train$Prison_Years)
new_train$Total_Arrests=as.integer(new_train$Total_Arrests)
new_train$Total_Convictions=as.integer(new_train$Total_Convictions)
new_train$Education_Level=as.integer(new_train$Education_Level)
new_train$Dependents=as.integer(new_train$Dependents)
new_train$gang=as.numeric(new_train$Gang_Affiliated=="true")


dtrain=new_train[is.element(new_train$ID,train_ids),]
dtest=new_train[is.element(new_train$ID,test_ids),]


train_table=data.frame(model=character(11),MSE=character(11),
                       FPB=character(11),FPW=character(11),FAIRSCORE=character(11))

test_table=data.frame(model=character(11),MSE=character(11),
                       FPB=character(11),FPW=character(11),FAIRSCORE=character(11))


############ glm with MSE loss

model_glm_basic=glm(Y1~Gender+Race+gang+Age_at_Release+
                      Prison_Years+Total_Arrests+Total_Convictions+Education_Level+
                      Dependents,data=dtrain)



ptrain=predict(model_glm_basic,dtrain)
ptest=predict(model_glm_basic,dtest)

ptrain[ptrain<0]=0
ptest[ptest<0]=0

train_table[1,]=c("GLM",score_models_bootstrap(dtrain,ptrain,cutoff))
test_table[1,]=c("GLM",score_models_bootstrap(dtest,ptest,cutoff))

########## logistic regression

model_glm_logit=glm(Y1~Gender+Race+gang+Age_at_Release+
                      Prison_Years+Total_Arrests+Total_Convictions+Education_Level+
                      Dependents,data=dtrain,family="binomial")



ptrain=predict(model_glm_logit,dtrain,type="response")
ptest=predict(model_glm_logit,dtest,type="response")

ptrain[ptrain<0]=0
ptest[ptest<0]=0

train_table[2,]=c("GLM_binom",score_models_bootstrap(dtrain,ptrain,cutoff))
test_table[2,]=c("GLM_binom",score_models_bootstrap(dtest,ptest,cutoff))


########## truncated linear regression

ptrain=predict(model_glm_basic,dtrain)
ptest=predict(model_glm_basic,dtest)

ptrain[ptrain<0]=0
ptest[ptest<0]=0

ptrain[ptrain>=cutoff]=cutoff-.0001
ptest[ptest>=cutoff]=cutoff-.0001

train_table[3,]=c("GLM_trunc",score_models_bootstrap(dtrain,ptrain,cutoff))
test_table[3,]=c("GLM_trunc",score_models_bootstrap(dtest,ptest,cutoff))

########## linear regression with shrinkage

ptrain=predict(model_glm_basic,dtrain)
ptest=predict(model_glm_basic,dtest)

ptrain[ptrain<0]=0
ptest[ptest<0]=0

fairshrout=fair_shrink(ptrain,dtrain$Y1,as.integer(dtrain$Race),seq(0,.1,.001),cutoff)
ptrain=fairshrout[[3]]
fsout2=fair_shrink(ptest,dtest$Y1,as.integer(dtest$Race),fairshrout[[2]],cutoff)
ptest=fsout2[[3]]

train_table[4,]=c("GLM_shrink",score_models_bootstrap(dtrain,ptrain,cutoff))
test_table[4,]=c("GLM_shrink",score_models_bootstrap(dtest,ptest,cutoff))


########### regression with convex surrogate loss

y <- as.numeric(dtrain$Y1)

X <- as.matrix(dtrain[,c("Gender","Race","gang","Age_at_Release",
                         "Prison_Years","Total_Arrests","Total_Convictions",
                         "Education_Level","Dependents")])
Xt <- as.matrix(dtest[,c("Gender","Race","gang","Age_at_Release",
                         "Prison_Years","Total_Arrests","Total_Convictions",
                         "Education_Level","Dependents")])

yt <- as.numeric(dtest$Y1)

int <- rep(1, length(y))
intt <- rep(1, length(yt))

X <- cbind(int, X)
Xt <- cbind(intt, Xt)

outv=define_vectors(X,y,2)
v1=outv[[1]]
v2=outv[[2]]

bst_score=0
bst_lambda=0
lams=seq(0,1,.01)
for(lambda in lams){
Nr=dim(X)[1]

betas <- solve(t(X) %*% X+Nr*lambda*(t(v1)%*%v1-t(v1)%*%v2-t(v2)%*%v1+t(v2)%*%v2)) %*% t(X) %*% y

ptrain=X%*%betas
ptest=Xt%*%betas

ptrain[ptrain<0]=0
ptest[ptest<0]=0

fscr=score_models(dtrain,ptrain,cutoff)[4]
if(fscr>bst_score){
  bst_score=fscr
  bst_lambda=lambda
}
}

betas <- solve(t(X) %*% X+Nr*bst_lambda*(t(v1)%*%v1-t(v1)%*%v2-t(v2)%*%v1+t(v2)%*%v2)) %*% t(X) %*% y

ptrain=X%*%betas
ptest=Xt%*%betas

ptrain[ptrain<0]=0
ptest[ptest<0]=0

train_table[5,]=c("GLM_surrogate",score_models_bootstrap(dtrain,ptrain,cutoff))
test_table[5,]=c("GLM_surrogate",score_models_bootstrap(dtest,ptest,cutoff))

v1%*%betas-v2%*%betas

########## BFGS applied to NIJFM


race=dtrain$Race
meansq_loss <- function(X,y,race,cutoff,betas) {   
  prd=X%*%betas
  smout=score_models(data.frame(Y1=y,Race=race),prd,cutoff)
  return(-smout[4])
}

bta=as.matrix(as.numeric(model_glm_basic$coefficients))
optim_output <- optim(par = bta,    
                      fn = meansq_loss,
                      X=X,y=y,race=race,cutoff=cutoff,method = "BFGS",
                      control=list(maxit=500))

A=optim_output$par

ptrain=X%*%A
ptest=Xt%*%A

ptrain[ptrain<0]=0
ptest[ptest<0]=0

train_table[6,]=c("GLM_optim",score_models_bootstrap(dtrain,ptrain,cutoff))
test_table[6,]=c("GLM_optim",score_models_bootstrap(dtest,ptest,cutoff))


########## regression with balanced data

N1=sum(dtrain$Race==1)
N2=sum(dtrain$Race==2)
dtrain$weights=1
dtrain$weights[dtrain$Race==2]=N1/N2
dtrain$weights=dtrain$weights/sum(dtrain$weights)

model_glm_balance=glm(Y1~Gender+Race+gang+Age_at_Release+
                      Prison_Years+Total_Arrests+Total_Convictions+Education_Level+
                      Dependents,data=dtrain,
                      weights=dtrain$weights)



ptrain=predict(model_glm_balance,dtrain)
ptest=predict(model_glm_balance,dtest)

ptrain[ptrain<0]=0
ptest[ptest<0]=0

train_table[7,]=c("GLM_balance",score_models_bootstrap(dtrain,ptrain,cutoff))
test_table[7,]=c("GLM_balance",score_models_bootstrap(dtest,ptest,cutoff))

####### regression fit separately to each group

model_glm_group1=glm(Y1~Gender+gang+Age_at_Release+
                        Prison_Years+Total_Arrests+Total_Convictions+Education_Level+
                        Dependents,data=dtrain[dtrain$Race==1,])
model_glm_group2=glm(Y1~Gender+gang+Age_at_Release+
                       Prison_Years+Total_Arrests+Total_Convictions+Education_Level+
                       Dependents,data=dtrain[dtrain$Race==2,])


ptrain=predict(model_glm_group1,dtrain)
ptest=predict(model_glm_group1,dtest)

ptrain[dtrain$Race==2]=predict(model_glm_group2,dtrain[dtrain$Race==2,])
ptest[dtest$Race==2]=predict(model_glm_group2,dtest[dtest$Race==2,])

ptrain[ptrain<0]=0
ptest[ptest<0]=0

train_table[8,]=c("GLM_group",score_models_bootstrap(dtrain,ptrain,cutoff))
test_table[8,]=c("GLM_group",score_models_bootstrap(dtest,ptest,cutoff))

###############  xgboost

features=c("Gender","Race","gang","Age_at_Release",
  "Prison_Years","Total_Arrests","Total_Convictions","Education_Level",
  "Dependents")
dtrainxgb=dtrain[,features]
dtestxgb=dtest[,features]
ylabel=as.numeric(dtrain$Y1)

mtrainxgb <- xgb.DMatrix(data = as.matrix(dtrainxgb), label = as.numeric(unlist(ylabel)))


set.seed(1)
modelxgb<-xgboost(data = mtrainxgb,
                  min_split_loss=.01,
                  missing_value=-9999.0,
                  nrounds=570,
                  random_state=1234,
                  alpha=0.0,
                  lambda=1.0,
                  smooth_interval=200,
                  subsample=1,
                  tree_method="auto",
                  learning_rate=.05,
                  max_bin=256,
                  max_delta_step=0.0,
                  max_depth=1,
                  colsample_bytree=1,
                  interval=10,
                  colsample_bylevel=1.0,
                  min_child_weight=1.0,
                  verbose=0,
                  objective = "reg:squarederror")

ptrain=predict(modelxgb,as.matrix(dtrainxgb))
ptest=predict(modelxgb,as.matrix(dtestxgb))

ptrain[ptrain<0]=0
ptest[ptest<0]=0

mean((ptest-as.numeric(dtest$Y1))^2)

train_table[9,]=c("xgboost",score_models_bootstrap(dtrain,ptrain,cutoff))
test_table[9,]=c("xgboost",score_models_bootstrap(dtest,ptest,cutoff))

############### xgb cutoff at decision threshold

ptraincut=ptrain
ptestcut=ptest

ptraincut[ptrain>=cutoff]=cutoff-.0001
ptestcut[ptest>=cutoff]=cutoff-.0001

train_table[10,]=c("xgb_trunc",score_models_bootstrap(dtrain,ptraincut,cutoff))
test_table[10,]=c("xgb_trunc",score_models_bootstrap(dtest,ptestcut,cutoff))

##### xgb with shrinkage

fairshrout=fair_shrink(ptrain,dtrain$Y1,as.integer(dtrain$Race),seq(0,.1,.001),cutoff)
ptrainshrk=fairshrout[[3]]
fsout2=fair_shrink(ptest,dtest$Y1,as.integer(dtest$Race),fairshrout[[2]],cutoff)
ptestshrk=fsout2[[3]]

train_table[11,]=c("xgb_shrink",score_models_bootstrap(dtrain,ptrainshrk,cutoff))
test_table[11,]=c("xgb_shrink",score_models_bootstrap(dtest,ptestshrk,cutoff))

##################
print(train_table)
print(test_table)

write.csv(train_table,paste0("train_table",cutoff,".csv"),row.names=F)
write.csv(test_table,paste0("test_table",cutoff,".csv"),row.names=F)
}
