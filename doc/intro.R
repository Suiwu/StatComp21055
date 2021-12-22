## -----------------------------------------------------------------------------
library(StatComp21055)
library(bestsubset)
library(BB)
library(glmnet)
library(shapes)
library(frechet)
data("brains")
## 以年龄、性别和组别作为预测变量
X=cbind(brains$age,brains$grp,brains$sex)[1:10,]
Y=brains$x[,,1:10]
## 预览一下shapes data 的样子
print(Y[,,1])

## -----------------------------------------------------------------------------
Fm=Fmean(Y)
Fm

## -----------------------------------------------------------------------------
## 预测一下自变量是X[1,]处的Y值
yhat=FREShape(Y,X,X[1,])

cbind(Y[,,1],yhat)


## -----------------------------------------------------------------------------
r2=R2(Y,X)
r2

## -----------------------------------------------------------------------------
n=30
p=8
support.size=5
dat=sim.data(n,p,support.size)
dat

## -----------------------------------------------------------------------------
methods=c("Lasso","Forward stepwise","Relaxed lasso")
rho.vec =c(0.35)
beta.type = 1
snr.vec = exp(seq(log(0.05),log(6),length=5))
file.list=compute.index(n,p,nval=5,methods = methods,file=NULL,rho.vec=rho.vec,snr.vec=snr.vec,beta.type = beta.type)


## -----------------------------------------------------------------------------
library(ggplot2)
#file.list = system(paste0("./data/rd/sim.n",n,".p",p,".*.rds"),intern=TRUE)
method.nums = c(1,2,3)

rho = 0.35

## risk
plot_index(file.list, what="risk",method.nums=c(1,2,3), method.names=methods,
               make.pdf=FALSE,fig.dir=NULL,file.name="risk", h=4, w=4)

## -----------------------------------------------------------------------------
plot_index(file.list, what="error",method.nums=c(1,2,3), method.names=methods,
               make.pdf=FALSE,fig.dir=".",file.name="error", h=4, w=4)

## -----------------------------------------------------------------------------
plot_index(file.list, what="nonzero",method.nums=c(1,2,3), method.names=methods,
               make.pdf=FALSE,fig.dir=".",file.name="nonzero", h=4, w=4)

## -----------------------------------------------------------------------------
plot_index(file.list, what="F",method.nums=c(1,2,3), method.names=methods,
               make.pdf=FALSE,fig.dir=".",file.name="F", h=4, w=4)

## -----------------------------------------------------------------------------
plot_index(file.list, what="prop",method.nums=c(1,2,3), method.names=methods,
               make.pdf=FALSE,fig.dir=".",file.name="prop", h=4, w=4)

## -----------------------------------------------------------------------------
a=1
b=1
n=25
N=10000

X=gibbsR(a,b,n,N)
plot(X[,1],X[,2],xlab = "x",ylab = "y",main = "gibbsR")

## -----------------------------------------------------------------------------
Xc=gibbsC(a,b,n,N)
plot(Xc[,1],Xc[,2],xlab = "x",ylab = "y",main="gibbsC")

## -----------------------------------------------------------------------------
qqplot(X[,1],Xc[,1],xlab = "gibbsR",ylab = "gibbsC",main="第1维变量QQ图")
abline(0,1,col = "red")

qqplot(X[,2],Xc[,2], xlab = "gibbsR",ylab = "gibbsC",main="第2维变量QQ图")
abline(0,1,col = "red")

## -----------------------------------------------------------------------------
library(microbenchmark)
ts=microbenchmark(gibbR=gibbsR(a,b,n,N), gibbC=gibbsC(a,b,n,N))
summary(ts)[,c(1,3,5,6)]

