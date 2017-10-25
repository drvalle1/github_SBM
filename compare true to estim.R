plot(param$theta,type='h')
ind.stud=c(6,4,3,5,1)
theta=param$theta[ind.stud]
theta;theta.true

plot(param$phi,type='h')
ind.quest=c(2,1,4)
phi=param$phi[ind.quest]
phi;phi.true

psi=param$psi[,ind.quest]
psi1=psi[ind.stud,]
rango=c(0,1)
plot(psi1,psi.true,xlim=rango,ylim=rango)
lines(rango,rango)

k=data.frame(ztrue=z.true,zestim=param$z)
table(k)

k=data.frame(wtrue=w.true,westim=param$w)
table(k)