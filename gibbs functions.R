sample.psi=function(param,dat,ngroup.stude,ngroup.quest){
  tmp=getql(z=param$z-1,w=param$w-1,dat=dat,
            ngrstude=ngroup.stude,ngrquest = ngroup.quest)
  tmp1=rbeta(ngroup.stude*ngroup.quest,tmp$nql1+1,tmp$nql0+1)
  # qqq=tmp$nql1/(tmp$nql1+tmp$nql0)
  matrix(tmp1,ngroup.stude,ngroup.quest)
}
sample.theta=function(param,ngroup.stude,gamma.v){
  nk=rep(0,ngroup.stude)
  tmp=table(param$z)
  nk[as.numeric(names(tmp))]=tmp
  ind=ngroup.stude:1
  invcumsum=cumsum(nk[ind])[ind]
  vk=rbeta(ngroup.stude,nk+1,invcumsum-nk+gamma.v)
  vk[ngroup.stude]=1
  theta=convertSBtoNormal(vk)
  # sum(theta)
  theta
}
sample.phi=function(param,ngroup.quest,gamma.u){
  mk=rep(0,ngroup.quest)
  tmp=table(param$w)
  mk[as.numeric(names(tmp))]=tmp
  ind=ngroup.quest:1
  invcumsum=cumsum(mk[ind])[ind]
  uk=rbeta(ngroup.quest,mk+1,invcumsum-mk+gamma.u)
  uk[ngroup.quest]=1
  phi=convertSBtoNormal(uk)
  # sum(phi)
  phi
}