generate_data<- function(n,p){
  covariates<-matrix(data=rnorm(n*p), nrow=n, ncol=p)
  responses<- rnorm(n)
  foo<-list(covariates, responses)
  return(foo)
}