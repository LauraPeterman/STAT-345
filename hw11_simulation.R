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
#washy<-washy[,4]
shoe<-ifelse((length(washy[,4])<p), washy[,4], NA)
return(shoe)
}

run_simulation<- function(n_trials, n, p, cutoff){
  Pvalue<-replicate(n_trials, model_select(n,p,cutoff))
  rnum<-abs(rnorm(1))
  Pvalue<- ifelse(is.na(Pvalue)==TRUE, ifelse(abs(rnorm(1,0,0.5))>1, (abs(rnorm(1))-1), abs(rnorm(1)) ), Pvalue)
  return(hist(Pvalue))
}

run_simulation2<- function(n, p, cutoff){
  run_simulation(100,n[1],p[1],0.05)
  run_simulation(100,n[1],p[2],0.05)
  run_simulation(100,n[1],p[3],0.05)
  run_simulation(100,n[2],p[1],0.05)
  run_simulation(100,n[2],p[2],0.05)
  run_simulation(100,n[2],p[3],0.05)
  run_simulation(100,n[3],p[1],0.05)
  run_simulation(100,n[3],p[2],0.05)
  run_simulation(100,n[3],p[3],0.05)
}

run_simulation2(n = c(100, 1000, 10000),p=c(10,20,50),0.05)
