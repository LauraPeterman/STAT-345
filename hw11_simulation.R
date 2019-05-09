generate_data<- function(n,p){
  covariates<-matrix(data=rnorm(n*p), nrow=n, ncol=p)
  responses<- rnorm(n)
  foo<-list(covariates, responses)
  return(foo)
}

model_select<- function(n,p, cutoff){
  pp<- generate_data(n,p)
  resp<-pp[[2]]
  covar<-pp[[1]]
  sn<-seq(from=1, to=(p))
  colnames(covar)=sn
qq<-lm(resp~covar)
zap<-summary(qq)
zappy<-data.frame(zap$coefficients)
sap<-cbind(sn,zappy[-1,])
sap2<-sap$sn[sap$Pr...t..>cutoff]
wish<-lm(resp~covar[,sap2])
wishy<-summary(wish)
washy<-data.frame(wishy$coefficients)
washy<-washy[,4]
shoe<-ifelse((length(washy)<p), washy, NA)
return(shoe)
}