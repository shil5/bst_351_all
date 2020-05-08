g=NULL
t=rweibull(20,2)
n=length(t)
g1=9.8
for (i in 1:10)
{
  l1=(n/g1)+sum(log(t))-sum((t^g1)*log(t))
  l2=(-n/(g1^2))-sum((t^g1)*log(t)^2)
  g1=g1-(l1/l2)
  g[i]=g1
}
plot(g,type='o',ylim=c(0,10),ylab = 'Gamma',col='red')
g

