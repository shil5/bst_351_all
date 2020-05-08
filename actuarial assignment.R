# SHILPA 2017IMSST008 
#Table 5.3 Actuarial estimate(life table)
Nt<-c(200,195,184,167,157,147,126,114,105,101)
Dt<-c(5,10,12,8,10,15,9,8,4,3)
Ct<-c(0,1,5,2,0,6,3,1,0,1) 
length(Nt)
ini<-data.frame(Nt,Dt,Ct);ini #Initial Table
Ntstar<-(Nt-(Ct/2));Ntstar #average

q<-d/Ntstar;q
p<-1-q;p 
survp<-cumprod(p)
survp 
data.frame(Nt,round(p,3),round(survp,3))