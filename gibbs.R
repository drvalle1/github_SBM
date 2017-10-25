rm(list=ls(all=TRUE))
library('Rcpp')
set.seed(3)

setwd('U:\\independent studies\\SBM\\github_SBM')
sourceCpp('rcpp_func.cpp')
source('gibbs functions.R')
dat=data.matrix(read.csv('fake data.csv',as.is=T))
nind=nrow(dat)
nquest=ncol(dat)
ngroup.stude=10
ngroup.quest=10

#get initial values
theta=rep(1/ngroup.stude,ngroup.stude)
phi=rep(1/ngroup.quest,ngroup.quest)
z=sample(1:ngroup.stude,size=nind,replace=T)
w=sample(1:ngroup.quest,size=nquest,replace=T)
tmp=runif(ngroup.stude*ngroup.quest)
psi=matrix(tmp,ngroup.stude,ngroup.quest)

#priors
gamma.v=gamma.u=1

#useful stuff
dat1m=1-dat
ngibbs=1000
vec.psi=matrix(NA,ngibbs,ngroup.stude*ngroup.quest)
vec.theta=matrix(NA,ngibbs,ngroup.stude)
vec.phi=matrix(NA,ngibbs,ngroup.quest)
param=list(z=z,w=w,psi=psi,theta=theta,phi=phi)

#start gibbs sampler
options(warn=2)
for (i in 1:ngibbs){
  print(i)
  param$psi=sample.psi(param=param,dat=dat,
                       ngroup.stude=ngroup.stude,ngroup.quest=ngroup.quest)
  # param$psi=rbind(cbind(psi.true,0.01,0.01,0.01,0.01,0.01,0.01,0.01),0.01,0.01,0.01,0.01,0.01)  
  lpsi=log(param$psi)
  l1mpsi=log(1-param$psi)
  
  param$theta=sample.theta(param=param,ngroup.stude=ngroup.stude,gamma.v=gamma.v)
  # param$theta=c(theta.true,rep(0,ngroup.stude-length(theta.true)))
  
  param$phi=sample.phi(param=param,ngroup.quest=ngroup.quest,gamma.u=gamma.u)
  # param$phi=c(phi.true,rep(0,ngroup.quest-length(phi.true)))
  
  param$z=samplez(ltheta=log(param$theta),dat=dat,dat1m=dat1m,lpsi=lpsi,l1mpsi=l1mpsi,
                 w=param$w-1,runi=runif(nrow(dat)))
  # param$z=z.true
  
  param$w=samplew(lphi=log(param$phi),dat=dat,dat1m=dat1m,lpsi=lpsi,l1mpsi=l1mpsi,
                 z=param$z-1,runi=runif(ncol(dat)))
  # param$w=w.true
  
  #store results
  vec.psi[i,]=param$psi
  vec.theta[i,]=param$theta
  vec.phi[i,]=param$phi
}
