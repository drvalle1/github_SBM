rm(list=ls(all=TRUE))
set.seed(3)

setwd('U:\\independent studies\\SBM\\github_SBM')
ngroup.stude=5
ngroup.quest=3

#get parameters
tmp=runif(ngroup.stude)
theta.true=theta=tmp/sum(tmp)
tmp=runif(ngroup.quest)
phi.true=phi=tmp/sum(tmp)

set.seed(4)
psi=matrix(c(0.05,0.5,0.95,
             0.5,0.05,0.95,
             0.05,0.95,0.5,
             0.5,0.95,0.05,
             0.5,0.5,0.05),ngroup.stude,ngroup.quest,byrow=T)
psi.true=psi

#get latent variables
nind=1000
tmp=rmultinom(1,size=nind,prob=theta)
tmp1=rep(1:ngroup.stude,times=tmp)
z.true=z=tmp1 #if not scrambled
# z=sample(tmp1,nind); 

nquest=50
tmp=rmultinom(1,size=nquest,prob=phi)
tmp1=rep(1:ngroup.quest,times=tmp)
w.true=w=tmp1 #if not scrambled
# w=sample(tmp1,nquest)

#generate data
y=matrix(NA,nind,nquest)
for (i in 1:nind){
  for (j in 1:nquest){
    y[i,j]=rbinom(1,size=1,prob=psi[z[i],w[j]])    
  }
}
image(y)

write.csv(y,'fake data.csv',row.names=F)