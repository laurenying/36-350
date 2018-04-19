print("Hello, world!")

generate_data=function(n,p){
  covariates=matrix(rnorm(n*p),n,p)
  responses=rnorm(n)
  return (list(covariates,responses))
}
 

model_select=function(covariates, responses,cutoff){
  res=lm(responses ~ covariates)
  res.sum=summary(res)
  p.val=res.sum$coefficients[2:(ncol(covariates)+1),"Pr(>|t|)"]
  ind=which(p.val<=cutoff)
  new=lm(responses~covariates[,ind])
  new.sum=summary(new)
  new.pval=new.sum$coefficients[,"Pr(>|t|)"]
  return(new.pval)
  
}
