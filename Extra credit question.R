# SHILPA 2017IMSST008
# Using following method we can using iterations, we can obtain estimated 
#       values of alpha and beta for X~gamma(alpha, beta)
x0<-x1<-h<-cnt<-NULL
x0<-c(1)
h<-x1<-c(0)
e<-0.0001
max<-10
f<-function(x){
  return (x*cos(x)-x^2)
}
df<-function(x){
  return (cos(x)-x*sin(x)-2*x)
}
cnt<-c(0:max);cnt # count 0 indicates initial state
for(i in 1:max){
  print(paste("Iteration no.: ", i))
  print(x1)
  h<-c(h,f(x0[i])/df(x0[i]));h
  x1<-c(x1,x0[i]-h[i+1]);x1
  if(abs(h[i+1])<e){
    print(paste("Final value of x1 : ", x1[i]))
    break
  }
  else{
    x0<-c(x0,x1[i+1])
 }
}
if(abs(h[i+1])>e){
  i
print("Solution does not converge.")}
data.frame(x1[2:(i+1)])