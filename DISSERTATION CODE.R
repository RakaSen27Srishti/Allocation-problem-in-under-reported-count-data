rm(list=ls())
set.seed(seed=123)

R=1000
n=50
r=5
lambda=5
pi=0.9

y=matrix(0,nrow=n,ncol=R)
y=replicate(R,rpois(n,lambda))
y
ysum=apply(y,2,sum)
ysum
ymean=apply(y,2,mean)
ymean

A=matrix(0,nrow=n,ncol=R)
for(i in 1:n)
{
for(j in 1:R)
{
   
   A[i,j]=rbinom(1,y[i,j],pi)
}
}
ycurl=A
ycurl
ycurl.total=apply(ycurl,2,sum)
ycurl.total
ycurlmean=apply(ycurl,2,mean)
ycurlmean

place.WOR=matrix(0,nrow=r,ncol=R)
for(j in 1:R)
{
 place.WOR[,j]=sample(1:n,r)     
}

ydata.WOR=matrix(0,nrow=r,ncol=R)
for(i in 1:r)
{
 for(j in 1:R)
 {
  ydata.WOR[i,j]=y[place.WOR[i,j],j]
}
}
ydata.WOR
ydata.sum.WOR=apply(ydata.WOR,2,sum)
ydata.sum.WOR

ycurl.WOR=matrix(0,nrow=r,ncol=R)
for(i in 1:r)
{
 for(j in 1:R)
 {
   ycurl.WOR[i,j]=ycurl[place.WOR[i,j],j]
 }
}
ycurl.data.WOR=ycurl.WOR
ycurl.data.WOR
ycurl.sum.WOR=apply(ycurl.WOR,2,sum)
ycurl.sum.WOR

zi=ydata.WOR-ycurl.data.WOR
zi
zmean=apply(zi,2,mean)
zmean

pi.hat.WOR=ycurl.sum.WOR/ydata.sum.WOR
pi.hat.WOR
pi.hat.pseudo=sum(pi.hat.WOR)/R
pi.hat.pseudo

lambda.hat.WOR=(ydata.sum.WOR+ycurl.total-ycurl.sum.WOR)/(r+(n-r)*pi.hat.WOR)
lambda.hat.WOR

lambda.hat.pseudo=sum(lambda.hat.WOR)/R

lambda.hat.pseudo
pi.hat.pseudo

