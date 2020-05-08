# EXPONENTIAL
x<-seq(from=0,to=10,by=0.1)
ps<-ths<-cs<-sfs<-hxs<-Hxs<-NULL
for(i in 1:5){
  theta<-0.5*i
  ths<-c(ths,theta)
  #PDF
  pdf<-dexp(x,rate=theta) 
  ps<-rbind(ps,pdf)
  #CDF
  cdf<-pexp(x,rate=theta) #lower.tail=TRUE means P(X<=x)(default)
  cs<-rbind(cs,cdf)
  #SF
  sf<-pexp(x,rate=theta,lower.tail=FALSE)# P[X>x] or use sf<-1-cdf 
  sfs<-rbind(sfs,sf)
  #HAZARD FN
  hx<-(pdf/sf)
  hxs<-rbind(hxs,hx)
  #CUM HAZARD FN
  integrand<-function(x){theta*x^0}  # !!!DOUBT for function
  Hx<-c()
  for(i in 1:101){
    objj<-integrate(integrand,lower=0,upper=x[i]) 
    #unlist() or str() not reqd. over integrate
    Hx<-c(Hx,objj$value)
  }
  #or use:
  # IMP Hxs<--log(1-cs) or Hxs<--log(sfs)
  #or
  # IMP Hxs<--pexp(t,r,lower=FALSE,log=TRUE)
  Hxs<-rbind(Hxs,Hx)
}
#PDF
rownames(ps)<-ths
colnames(ps)<-x;ps
matplot(x,t(ps),type="l",main="PDF for X~exp(Theta)",lty=1,lwd=2,col=1:5,ylab="PDF: f(x)",
        ylim=c(0,2),xlim=c(0,6))
# oR ths<-seq(0.5,2.5,0.5)
legend("topright",inset=0.01,legend=ths,lty=1,lwd=2,col=1:5,bg=("white"),
       horiz=F,title="Value of Theta") 
#CDF
rownames(cs)<-ths
colnames(cs)<-x;cs
matplot(x,t(cs),type="l",main="CDF for X~exp(Theta)",lty=1,lwd=2,col=1:5,ylab="CDF: F(x)",
        ylim=c(0,1),xlim=c(0,6))
legend("topright",inset=0.01,legend=ths,lty=1,lwd=2,col=1:5,bg=("white"),
       horiz=F,title="Value of Theta")
#SF
rownames(sfs)<-ths
colnames(sfs)<-x;sfs
matplot(x,t(sfs),type="l",main="SF for X~exp(Theta)",lty=1,lwd=2,col=1:5,ylab="SF: S(x)",
        ylim=c(0,1),xlim=c(0,6))
legend("topright",inset=0.01,legend=ths,lty=1,lwd=2,col=1:5,bg=("white"),
       horiz=F,title="Value of Theta")
#HAZARD FN
rownames(hxs)<-ths
colnames(hxs)<-x;hxs
matplot(x,t(hxs),type="l",main="HAZARD FN. for X~exp(Theta)",lty=1,lwd=2,col=1:5,ylab="HF: h(x)",
        ylim=c(0,3),xlim=c(0,6))
legend("topright",inset=0.01,legend=ths,lty=1,lwd=2,col=1:5,bg=("white"),
       horiz=F,title="Value of Theta")
#CUM HAZARD FN
rownames(Hxs)<-ths
colnames(Hxs)<-x;hxs
matplot(x,t(Hxs),type="l",main="CUMULATIVE HAZARD FN. for X~exp(Theta)",lty=1,lwd=2,
        col=1:5,ylab="CHF: H(x)",ylim=c(0,10),xlim=c(0,10))
legend("topleft",inset=0.01,legend=ths,lty=1,lwd=2,col=1:5,bg=("white"),
       horiz=F,title="Value of Theta")
