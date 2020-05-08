# WEIBULL DISTN
#METHOD 1
#a or ?? is SHAPE par 
b<-1 #b or ?? is SCALE par 
x<-seq(from=0,to=10,by=0.1)
ps<-as<-cs<-sfs<-hxs<-Hxs<-NULL
for(i in 1:7){
  a<-0.5*i
  as<-c(as,a)
  #PDF
  pdf<-dweibull(x,a,b) 
  ps<-rbind(ps,pdf)
  #CDF
  cdf<-pweibull(x,a,b) #lower.tail=TRUE means P(X<=x)(default)
  cs<-rbind(cs,cdf)
  #SF
  sf<-pweibull(x,a,b,lower.tail=TRUE) #or 1-cs
  sfs<-rbind(sfs,sf)
  #HAZARD FN
  hxs<-ps/sfs
  #CUM HAZARD FN
  Hxs<--log(sfs)
}
#METHOD 2
#a or ?? is SHAPE par 
b<-1 #b or ?? is SCALE par 
x<-seq(from=0,to=10,by=0.1)
ps<-as<-cs<-sfs<-hxs<-Hxs<-NULL
for(i in 1:7){
  a<-0.5*i
  as<-c(as,a)
  #PDF
  pdf<-dweibull(x,a,b) 
  ps<-rbind(ps,pdf)
  #CDF
  cdf<-pweibull(x,a,b) #lower.tail=TRUE means P(X<=x)(default)
  cs<-rbind(cs,cdf)
  #SF
  sf<-1-cdf
  sfs<-rbind(sfs,sf)
  #HAZARD FN
  hx<-(pdf/sf)
  hxs<-rbind(hxs,hx)
  #CUM HAZARD FN
  integrand<-function(x){(a/b^a)*(x^(a-1))}  # !!!DOUBT for function
  Hx<-c()
  for(i in 1:101){
    objj<-integrate(integrand,lower=0.00001,upper=x[i]) #DOUBT WHY NOT ZERO
    Hx<-c(Hx,objj$value)
  }
  Hxs<-rbind(Hxs,Hx)
}
#PDF
rownames(ps)<-as
colnames(ps)<-x;ps
matplot(x,t(ps),type="l",main="PDF for X~weibull(??)",lty=1,lwd=2,col=1:5,ylab="PDF: f(x)",
        ylim=c(0,1.4),xlim=c(0,4))
legend("topright",inset=0.01,legend=as,lty=1,lwd=2,col=1:5,bg=("white"),
       horiz=F,title="Value of ??") 
#CDF
rownames(cs)<-as
colnames(cs)<-x;cs
matplot(x,t(cs),type="l",main="CDF for X~weibull(??)",lty=1,lwd=2,col=1:5,ylab="CDF: F(x)",
        ylim=c(0,1),xlim=c(0,6))
legend("topright",inset=0.01,legend=as,lty=1,lwd=2,col=1:5,bg=("white"),
       horiz=F,title="Value of ??")
#SF
rownames(sfs)<-as
colnames(sfs)<-x;sfs
matplot(x,t(sfs),type="l",main="SF for X~weibull(??)",lty=1,lwd=2,col=1:5,ylab="SF: S(x)",
        ylim=c(0,1),xlim=c(0,6))
legend("topright",inset=0.01,legend=as,lty=1,lwd=2,col=1:5,bg=("white"),
       horiz=F,title="Value of ??")
#HAZARD FN
rownames(hxs)<-as
colnames(hxs)<-x;hxs
matplot(x,t(hxs),type="l",main="HAZARD FN. for X~weibull(??)",lty=1,lwd=2,col=1:5,ylab="HF: h(x)",
        ylim=c(0,5),xlim=c(0,4))
legend("topright",legend=as,lty=1,lwd=2,col=1:5,bg=("white"),
       horiz=F,title="Value of ??")
#CUM HAZARD FN
rownames(Hxs)<-as
colnames(Hxs)<-x;hxs
matplot(x,t(Hxs),type="l",main="CUMULATIVE HAZARD FN. for X~weibull(??)",lty=1,lwd=2,
        col=1:5,ylab="CHF: H(x)",ylim=c(0,4),xlim=c(0,4))
legend("bottomright",legend=as,lty=1,lwd=2,col=1:5,bg=("white"),
       horiz=F,title="Value of ??")
