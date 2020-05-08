x0<-x1<-h<-cnt<-NULL
x0<-c(1)
h<-x1<-c(0)
allerr<-0.0001
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
  h<-c(h,f(x0[i])/df(x0[i]));h
  x1<-c(x1,x0[i]-h[i+1]);x1
  if(abs(h[i+1])<allerr){
    print(paste("No. of iterations: ", i,"    And Final value is : ", x1))
    break
  }
  else{i
  x0<-c(x0,x1[i+1])
  #break
  }
}
print("Solution does not converge.")
final<-data.frame(x0,x1,h);final

#cnt - a counter which keeps track of the no. of iterations performed
#max - maximum number of iterations to be performed
#df(x) - the derivative of f(x) with respect to x
#x0 - the value of root at nth iteration
#x1 - the value of root at (n+1)th iteration
#allerr - allowed error
