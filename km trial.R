# SHILPA 2017IMSST008 
# KM estimation Deshpande Table 5.7
Ti<-c(142,149,320,345,560,805,1130,1720,2480,4210,5230,6890)
  length(Ti)
Ni<-c(1,1,1,0,1,1,0,1,0,0,1,1)
  length(Ni)
ctrl<-data.frame(Ti,Ni);ctrl
library(survival)
km.ctrl<-survfit(Surv(ctrl$Ti,ctrl$Ni==1))
  summary (km.ctrl)
#Groupwise survival curves
Ti<-c(5,5,8,8,12,16,23,27,30, 33, 43, 45, 9, 13, 13, 18, 23, 28, 31,
          34,45,48,161)
length(Ti)
Ni<-c(rep(1,5),0,rep(1,6),1,1,0,1,1,0,1,1,0,1,0)
pulse<-c(rep(1,12),rep(2,11))
final<- data.frame(Ti,Ni,pulse);final
surv.bygr<-survfit(Surv(Ti,Ni==1)~ pulse)
plot(surv.bygr,conf.int=F,xlab= "Ti",ylab= "survival fn",
     main= "2 Constructed S(t) curves",cex=.6)
