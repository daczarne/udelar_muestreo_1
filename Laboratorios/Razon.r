library(survey)

N<-8
n<-4
k<-1:N
y<-c(1,2,4,4,7,7,7,8)
x<-c(4,5,5,6,8,7,7,5)
tx<-sum(x)

M<-choose(N,n)
S<-combn(k, n)

t.pi<-combn(y, n, function(x) N*mean(x))
#data.frame(table(t.pi)/M)

t.ra<-(tx/combn(x, n, function(x) N*mean(x)))*t.pi
#data.frame(table(t.ra)/M)

par(mfrow = c(1, 2))
hist(t.pi, breaks=15,freq=FALSE,xlim=c(20,60))
hist(t.ra, breaks=15,freq=FALSE,xlim=c(20,60))

mean(t.pi)
var(t.pi)*(M-1)/M

mean(t.ra)
var(t.ra)*(M-1)/M

cor(x,y)


d<-data.frame(k=k,y=y,x=x,fcpf=N)
s<-d[c(1,2,3,4),]
p<-svydesign(id=~1, data=s, fpc=~fcpf)

ra<-svyratio(~y, ~x, p)
ra
predict(ra, total=sum(x))
