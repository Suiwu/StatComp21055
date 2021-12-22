## -----------------------------------------------------------------------------
a=c("今天","吃饭")
b=c("你","了吗?")
d=paste0(a,b,collapse="")  ## 拼接字符串a,b
d

## -----------------------------------------------------------------------------
## 从均值为100，方差为1的正态分布中，随机生成30个数
x <- rnorm(30, mean=100, sd=1)
print(round(x,2))

## 30个随机数的散点图
plot(x,main="散点图")


## 30个随机数的直方图
hist(x, col=rainbow(15), 
     main='正态随机数', xlab='', ylab='频数')


## -----------------------------------------------------------------------------
## 用ggplot2画图
library(ggplot2)
data(mpg)  ## 导入mpg数据集

ggplot(data=mpg,mapping = aes(x=cty,y=hwy,color=factor(year)))+
  geom_point()+stat_smooth(method = 'loess')+
  scale_shape_manual(values = c(2,5))+
  labs(y = '每加仑高速公路行驶距离', x = '每加仑城市公路行驶距离',
       title = '汽车油耗与型号', size = '排量', colour = '车型')+
  theme(plot.title = element_text(hjust = 0.5))


## -----------------------------------------------------------------------------
library(xtable)
## 显示表格
knitr::kable(head(mpg))

## -----------------------------------------------------------------------------
Rayleigh =function(sigma, n){
  for(i in 1:n) {
    U=runif(n)
    V=1-U
    X = sigma * sqrt(-2 * log(V))
  }
  return(X)
}
sigma = 2
n = 1000

hist(Rayleigh(sigma, n),main = "Rayleigh",xlab="")

## ----eval=FALSE---------------------------------------------------------------
#  sigma=c(1:9)
#  n=1000
#  par(mfrow=c(3,3))
#  for(i in 1:9){
#    title=paste0("sigma=",sigma[i])
#    hist(Rayleigh(sigma[i], n),main = title,xlab="")
#  }

## -----------------------------------------------------------------------------
n=1000
X1=rnorm(n,0,1)
X2=rnorm(n,3,1)
u <- runif(n)
p1=as.integer(u < 0.75) 
p2=1-p1
Z=p1*X1+p2*X2
hist(Z,main = "p1=0.75")

## ---- eval=FALSE--------------------------------------------------------------
#  p=seq(0,1,0.1)
#  par(mfrow=c(3,4))
#  for(i in 1:11){
#    p1=as.integer(u < p[i])
#    p2=1-p1
#    Z=p1*X1+p2*X2
#    title=paste0("p1=",p[i])
#    hist(Z,main=title)
#  }

## -----------------------------------------------------------------------------
Poisson_Gamma=function(n, t, lambda, r, beta) {
  N =rpois(n, lambda * t)
  X=sapply(N, function(N, r, beta) sum(rgamma(N, r,beta)), r,beta)
  return(X)
}

test=function(n, t, lambda, r, beta) {
  x=Poisson_Gamma(n, t, lambda, r,beta)
  ## 样本均值
  sm=mean(x) 
  ## 理论均值
  vm=var(x)
  ## 样本方差
  tm=lambda * t * r/beta
  ## 理论方差
  tv=lambda * t * (1 + r) * r/beta^2
  
  ## 输出结果样式
  cat("r=",r,"beta=",beta,"\n")
  cat("样本均值:", sm, "  ")
  cat("理论均值:", vm, "\n")
  cat("样本方差:", tm, "  ")
  cat("理论方差:", tv, "\n\n")
}

## 参数值
n=1000
lambda_seq=c(1:3)
r_seq=c(1:3)
beta_seq=c(1:3)
t=10

for (lambda in lambda_seq) {
  for (r in r_seq) {
    for (beta in beta_seq) {
      test(n, t, lambda,r,beta)
    }
  }
}


## -----------------------------------------------------------------------------
MCBeta = function(x,n=10000){
  t=runif(n, min = 0, max = x)
  theta.hat=x*mean(t*t*(1-t)*(1-t))/beta(3,3)
  return(theta.hat)
}
x=seq(0.1,0.9,0.1)

MC.Beta=pbeta=rep(0,9)
for(i in 1:9)
{
  MC.Beta[i]=MCBeta(x[i])
  pbeta[i]=pbeta(x[i],3,3)
}
out=rbind(MC.Beta,pbeta)

rownames(out)=c("MC.Beta","pbeta")
colname=NULL
for(i in 9:1){
  name=paste("x=",x[i])
  colname=cbind(name,colname)
}
colnames(out)=colname

out

## -----------------------------------------------------------------------------
rayleigh=function(scale, n) {
  rayleigh=antithetic=numeric(n)
  for (i in 1:n) {
    U =runif(n)
    V = 1 - U
    rayleigh = scale * sqrt(-2 * log(U))
    antithetic = scale * sqrt(-2 * log(V))
    out$ray=rayleigh
    out$ant=antithetic
  }
  return(out)
}

scale=2
n=1000
out=rayleigh(scale, n)
var1 = var(out$ray)
var2 =(var(out$ray) + var(out$ant) + 2 * cov(out$ray, out$ant)) / 4
reduction = ((var1 - var2) / var1)
cat("reduction=",100*reduction,"%")

## -----------------------------------------------------------------------------
x =seq(1,10,0.02)
y = x^2/sqrt(2*pi)* exp((-x^2/2))
y1 = exp(-x)
y2 =1 / (x^2)

gs =c(expression(g(x)==e^{-x^2/2}*x^2/sqrt(2*pi)),expression(f[1](x)==1/(x^2)),expression(f[2](x)==x*e^{(1-x^2)/4}/sqrt(2*pi)))

plot(x, y, type = "l", ylab = "", ylim = c(0,0.5),main='density function')
lines(x, y1, lty = 2,col="red")
lines(x, y2, lty = 3,col="blue")
legend("topright", legend = 0:2,lty = 1:3,col=c("black","red","blue"))

plot(x, y/y1,type = "l",lty = 2, col="red",main = 'ratios')
lines(x, y/y2, lty = 3,col="blue")
legend("topright", legend = 1:2,lty = 2:3,col=c("red","blue"))


## -----------------------------------------------------------------------------
f1 = function(x) { exp(-x) }
f2 = function(x) { (pi * (1 + x^2))^(-1) * (x >= 1) }
g =function(x) {x^2*exp(-x^2/2)/sqrt(2*pi)*(x>1)}

m = 10^7
x1 = rexp(m)
x2 = rcauchy(m)
x2[which(x2 < 1)] = 1 

fg = cbind(g(x1) / f1(x1), g(x2) / f2(x2))

theta.hat = se = numeric(2)
theta.hat =c(mean(fg[,1]), mean(fg[,2]))
se = c(sd(fg[,1]), sd(fg[,2]))
rbind(theta.hat, se)

## -----------------------------------------------------------------------------
g =function(x) {x^2*exp(-x^2/2)/sqrt(2*pi)*(x>1)}
m = 1e4                 
u = runif(m) 
x = 1/(1-u)  
fg = g(x)*x^2
theta.hat = mean(fg)
print(theta.hat)
theta =integrate(g,1,Inf)
theta

## -----------------------------------------------------------------------------
n <- 20
m <- 1000 
alpha <- .05
UCL <- replicate(m,expr = {
  x <- rchisq(n, df = 2)
  (n-1) * var(x) / qchisq(alpha, df = n-1)
})
cat("coverage probability=", mean(UCL > 4))

## -----------------------------------------------------------------------------
n <- 20 
alpha <- .05
x <- rchisq(n, df = 2)
prob <- replicate(m,expr = {
  x <- rchisq(n, df = 2)
  abs(mean(x)-2) < sd(x) * qt(alpha/2, df = n-1,lower.tail = FALSE)/sqrt(n)
})
cat("coverage probability=", mean(prob))

## -----------------------------------------------------------------------------
num<-c(50,100,200,500,1000) # 不同样本量
m<-10000

er<-NULL
for (n in num){
  cv<-qt(0.975,n-1)
  er1<-mean(sapply(1:m,FUN = function(o){
  x<-rchisq(n,1)
  m<-mean(x)
  se<-sqrt(var(x))
  abs((m-1)*sqrt(n)/se)>=cv
  })) 
  er2<-mean(sapply(1:m,FUN = function(o){
  x<-runif(n,0,2)
  m<-mean(x)
  se<-sqrt(var(x))
  abs((m-1)*sqrt(n)/se)>=cv
  }))
  er3<-mean(sapply(1:m,FUN = function(o){
  x<-rexp(n,1)
  m<-mean(x)
  se<-sqrt(var(x))
  abs((m-1)*sqrt(n)/se)>=cv
  }))
  er<-cbind(er,c(er1,er2,er3))
}
colnames(er)<-num
rownames(er)<-c("Chi(1)","U(0,2)","exp(1)")
knitr::kable(er)

## -----------------------------------------------------------------------------
mat <-
  matrix(c(6510, 3490, 10000, 6760, 3240, 10000, 13270, 6730, 20000), 3, 3,
         dimnames = list(
           c("Rejected", "Accepted", "total"),
           c("method A", "method B", "total")
         ))
mat

## ----eval=FALSE---------------------------------------------------------------
#  nn <- c(10,20,30,50,100,500)  # 样本容量
#  alpha <- 0.05                 # 显著性水平
#  d <- 2                        # 随机变量的维数
#  b0 <- qchisq(1-alpha,df=d*(d+1)*(d+2)/6)*6/nn  # 每种样本容量临界值向量
#  
#  # 计算多元样本偏度统计量
#  mul.sk <- function(x){
#    n <- nrow(x) # 样本个数
#    xbar <- colMeans(x)
#    sigma.hat <- (n-1)/n*cov(x) # MLE估计
#  
#    b <- 0
#    for(i in 1:nrow(x)){
#      for(j in 1:nrow(x)){
#        b <- b+((x[i,]-xbar)%*%solve(sigma.hat)%*%(x[j,]-xbar))^3
#      }
#    }
#    return(b/(n^2))
#  }
#  
#  # 计算第一类错误的经验估计
#  library(mvtnorm)
#  set.seed(200)
#  p.reject <- vector(mode = "numeric",length = length(nn)) # 保存模拟结果
#  
#  m <- 1000
#  
#  for(i in 1:length(nn)){
#    mul.sktests <- vector(mode = "numeric",length = m)
#    for(j in 1:m){
#      data <- rmvnorm(nn[i],mean = rep(0,d))
#      mul.sktests[j] <- as.integer(mul.sk(data)>b0[i])
#    }
#    p.reject[i] <- mean(mul.sktests)
#  }
#  p.reject

## ----eval=FALSE---------------------------------------------------------------
#  summ <- rbind(nn,p.reject)
#  rownames(summ) <- c("n","estimate")
#  knitr::kable(summ)

## ----eval=FALSE---------------------------------------------------------------
#  alpha <- 0.1
#  n <- 30      # 样本大小
#  m <- 2000    # 重复次数
#  epsilon <- c(seq(0,0.15,0.01),seq(0.15,1,0.05))
#  N <- length(epsilon)
#  power <- vector(mode = "numeric",length = N)
#  b0 <- qchisq(1-alpha,df=d*(d+1)*(d+2)/6)*6/n  #临界值
#  
#  # 对这列epsilon分别求power
#  for(j in 1:N){
#    e <- epsilon[j]
#    mul.sktests <- numeric(m)
#    for(i in 1:m){
#      # 生成混合分布
#      u <- sample(c(1,0),size = n,replace = T,prob = c(1-e,e))
#      data1 <- rmvnorm(n,sigma = diag(1,d))
#      data2 <- rmvnorm(n,sigma = diag(100,d))
#      data <- u*data1+(1-u)*data2
#      mul.sktests[i] <- as.integer(mul.sk(data)>b0)
#    }
#    power[j] <- mean(mul.sktests)
#  }
#  
#  # 绘制功效函数
#  plot(epsilon,power,type="b",xlab=bquote(epsilon),ylim=c(0,1))
#  abline(h=0.1,lty=3,col="lightblue")
#  se <- sqrt(power*(1-power)/m)  # 绘制标准误差
#  lines(epsilon,power-se,lty=3)
#  lines(epsilon,power+se,lty=3)

## -----------------------------------------------------------------------------
Spearman_rank_test <- function(x, y, B = 1e4){
  t0 = cor(x,y,method = "spearman")
  perm = numeric(B)
  z = c(x,y)
  for(i in 1:B){
    samp = sample(z)
    perm[i] = cor(samp[1:length(x)], samp[(length(x)+1):length(z)], method = "spearman")
  }
  p_value = mean(abs(perm)>=abs(t0))
  return(list(statistic = t0, 'p.value' = p_value))
}

## -----------------------------------------------------------------------------
n = 50
x = rnorm(n, 0, 1)
y = rnorm(n, 0, 1)

## -----------------------------------------------------------------------------
Spearman_rank_test(x, y)
cor.test(x, y, method = "spearman")

## -----------------------------------------------------------------------------
n = 50
x = rnorm(n, 0, 1)
y = x + rnorm(n, 0, 1)

## -----------------------------------------------------------------------------
Spearman_rank_test(x, y)
cor.test(x, y, method = "spearman")

## ----eval=FALSE---------------------------------------------------------------
#  library(bootstrap)
#  library(boot)
#  library(RANN)
#  library(energy)
#  library(Ball)
#  
#  Tn <- function(z, ix, size, k = 3){
#    n1 = size[1]
#    n2 = size[2]
#    n = n1+n2
#    if(is.vector(z))
#      z = data.frame(z,0)
#    z = z[ix,]
#    NN = nn2(data=z, k = k+1)
#    b1 = NN$nn.idx[1:n1,-1]
#    b2 = NN$nn.idx[(n1+1):n,-1]
#    i1 = sum(b1<n1+0.5)
#    i2 = sum(b2>n1+0.5)
#    (i1+i2)/(k+n)
#  }
#  eqdist.nn <- function(z, size, k, R = R){
#    res = boot(data = z, statistic = Tn, R = R, sim = "permutation", size = size, k = k)
#    stat = c(res$t0, res$t)
#    p_val = mean(stat>stat[1])
#    return(p_val)
#  }
#  p_val <- function(x,y, R = 999, k = 3){
#    x = as.matrix(x)
#    y = as.matrix(y)
#    n1 = nrow(x)
#    n2 = nrow(y)
#    N = c(n1,n2)
#    z = rbind(x,y)
#    p_nn = eqdist.nn(z, size = N, k, R = R)
#    p_energy = eqdist.etest(z,sizes=N,R = R)$p.value
#    p_ball = bd.test(x=x,y=y,num.permutations = R)$p.value
#    names(p_ball) = "ball"
#    return(c(NN = p_nn, energy = p_energy, p_ball))
#  }

## ----eval=FALSE---------------------------------------------------------------
#  res = replicate(200, expr = {
#    x = rnorm(50)
#    y = rnorm(50, 0, 1.7)
#    p_val(x, y)
#  })
#  alpha = 0.05
#  apply(res, 1, function(x)mean(x<alpha))

## ----eval=FALSE---------------------------------------------------------------
#  res = replicate(200, expr = {
#    x = rnorm(40, 1, 1)
#    y = rnorm(40, 0.6, 2)
#    p_val(x, y)
#  })
#  alpha = 0.05
#  apply(res, 1, function(x)mean(x<alpha))

## ----eval=FALSE---------------------------------------------------------------
#  res = replicate(200, expr = {
#    x = runif(40, -3, 3)
#    y = rt(60, 5)
#    p_val(x, y)
#  })
#  alpha = 0.05
#  apply(res, 1, function(x) mean(x<alpha))

## ----eval=FALSE---------------------------------------------------------------
#  res = replicate(200, expr = {
#    x = runif(30, -3, 3)
#    y = rt(300, 5)
#    p_val(x, y)
#  })
#  alpha = 0.05
#  apply(res, 1, function(x) mean(x<alpha))

## -----------------------------------------------------------------------------
set.seed(123)
## standard Cauchy 
f <- function(x) {
  return(1/(pi*(1+x^2)))
}

m <- 10000
x <- numeric(m)
x[1] <- rnorm(1)
k <- 0
u <- runif(m)
for (i in 2:m) {
  xt <- x[i-1]
  y <- rnorm(1, mean = xt)
  num <- f(y) * dnorm(xt, mean = y)
  den <- f(xt) * dnorm(y, mean = xt)
  if (u[i] <= num/den) x[i] <- y else {
    x[i] <- xt
    k <- k+1 #y is rejected
  }
}


## MC过程图
plot(1:m, x, type="l", main="", ylab="x")
b <- 1000
y <- x[b:m]
a <- ppoints(1000)
Qc <- qcauchy(a) 
Q <- quantile(x, a)


## QQ图
qqplot(Qc, Q, main="",xlim=c(-5,5),ylim=c(-5,5),xlab="Standard Cauchy Quantiles", ylab="Sample Quantiles")

## 直方图
hist(y, breaks="scott", main="", xlab="", freq=FALSE)
lines(Qc, f(Qc))


## -----------------------------------------------------------------------------
set.seed(2)

N <- 5000 #length of chain
burn <- 1000 #burn-in length
a <- 2
b <- 3
n <- 1000
X <- matrix(0, N, 2) #the chain, a bivariate sample

#initialize
x0=2
y0=0.5
###### generate the chain #####
X[1, ] <- c(x0,y0) #initialize
for (i in 2:N) {
  y <- X[i-1, 2]
  X[i, 1] <- rbinom(1,size=n,prob = y)
  x <- X[i, 1]
  X[i, 2] <- rbeta(1,x+a,n-x+b)
}
lab <- burn + 1
out <- X[lab:N, ]
plot(X[,1],X[,2],xlab = "x",ylab = "y")

## -----------------------------------------------------------------------------
cat("协方差矩阵\n")
cov(out) ## 协方差矩阵

cat("相关系数矩阵\n")
cor(out) ## 相关系数矩阵
plot(out, main="", cex=.5, xlab=bquote(X[1]),
ylab=bquote(X[2]), ylim=range(out[,2]))  ## 分布图

## -----------------------------------------------------------------------------
# 计算Gelman-Rubin statistic的函数
Gelman.Rubin <- function(psi) {
  # psi[i,j] is the statistic psi(X[i,1:j])
  # for chain in i-th row of X
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)

  psi.means <- rowMeans(psi)     #row means
  B <- n * var(psi.means)        #between variance est.
  psi.w <- apply(psi, 1, "var")  #within variances
  W <- mean(psi.w)               #within est.
  v.hat <- W*(n-1)/n + (B/n)     #upper variance est.
  r.hat <- v.hat / W             #G-R statistic
  return(r.hat)
        }

## -----------------------------------------------------------------------------
# 生成标准柯西分布的Metropolis chain
# 提议函数仍取9.3中使用的对称正态分布 N(0,X[t]^2)
# X1为初始值
Standard_Cauchy_Chain <- function(N, X1){
  X <- numeric(N)
  X[1] <- X1    #初始值
  for(i in 2:N){
    Xt <- X[i-1]
    Y <- rnorm(1,0,abs(Xt))
    r <- dt(Y,1)*dnorm(Xt,0,abs(Y))/dt(Xt,1)/dnorm(Y,0,abs(Xt))
    U <- runif(1)
    if(r > 1) r <- 1
    if(U <= r) X[i] <- Y
    else X[i] <- Xt
  }
  return(X)
}

## -----------------------------------------------------------------------------
k <- 4      
N <- 8000
b <- 1000     #burn-in length
X1 <- c(0.1,0.2,0.1,0.2)    #初始值

# 生成4条样本
set.seed(12345)
X <- matrix(0, nrow = k, ncol = N)
for(i in 1:k){
  X[i,] <- Standard_Cauchy_Chain(N, X1[i])
}

# compute diagnostic statistics
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
  psi[i,] <- psi[i,] / (1:ncol(psi))
print(Gelman.Rubin(psi))

## -----------------------------------------------------------------------------
# 四条样本的psi
for (i in 1:k)
  if(i==1){
    plot((b+1):N,psi[i, (b+1):N],ylim=c(-1,1), type="l",
         xlab='Index', ylab=bquote(phi))
  }else{
      lines(psi[i, (b+1):N], col=i)
  }
par(mfrow=c(1,1)) 

## -----------------------------------------------------------------------------
par(mfrow=c(1,1)) 
#plot the sequence of R-hat statistics
rhat <- rep(0, N)
for (j in (b+1):N)
  rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[(b+1):N], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
# 生成二元随机变量的Gibbs sampler
# X1为初始值
Bivariate.Gibbs <- function(N, X1){
  a <- b <- 1
  X <- matrix(0, N, 2)
  X[1,] <- X1    #初始值
  for(i in 2:N){
    X2 <-  X[i-1, 2]
    X[i,1] <- rbinom(1,25,X2)
    X1 <- X[i,1]
    X[i,2] <- rbeta(1,X1+a,25-X1+b)
  }
  return(X)
}

## -----------------------------------------------------------------------------
k <- 4          
N <- 8000 
b <- 1000    #burn-in length
X1 <- cbind(c(2,7,10,15),runif(4)) #初始值

#生成4条样本，每个第一维的放在X中，第二维的放在Y中
set.seed(12345)
X <- matrix(0, nrow=k, ncol=N)
Y <- matrix(0, nrow=k, ncol=N)
for (i in 1:k){
  BG <- Bivariate.Gibbs(N, X1[i,])
  X[i, ] <- BG[,1]
  Y[i, ] <- BG[,2]
}

## -----------------------------------------------------------------------------
# 先考虑第一维样本X

#compute diagnostic statistics
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
  psi[i,] <- psi[i,] / (1:ncol(psi))

#plot the sequence of R-hat statistics
rhat <- rep(0, N)
for (j in (b+1):N)
  rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[(b+1):N], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
# 再考虑第二维样本Y

#compute diagnostic statistics
psi <- t(apply(Y, 1, cumsum))
for (i in 1:nrow(psi))
  psi[i,] <- psi[i,] / (1:ncol(psi))

#plot the sequence of R-hat statistics
rhat <- rep(0, N)
for (j in (b+1):N)
  rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[(b+1):N], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
thek <- function(k, a, d){
  (-1)^k/exp(lgamma(k+1)+k*log(2)) * exp((k+1)*log(sum(a^2))-log(2*k+1)-log(2*k+2)) * exp(lgamma((d+1)/2)+lgamma(k+1.5)-lgamma(k+d/2+1))#用到了gamma函数和阶乘的恒等式
}

## -----------------------------------------------------------------------------
sumk <- function(a, d){
  k <- 0
  s <- 0
  while(abs(thek(k, a, d))>1e-5){#tolerance
    s <- s+thek(k, a, d)
    k <- k+1
  }
  return(s)
}

## -----------------------------------------------------------------------------
a <- c(1,2)
d <- length(a)
s <- sumk(a,d)
paste("The sum =", s)

## -----------------------------------------------------------------------------
k <- c(4:25, 100, 500, 1000)
###11.5
beijif <- function(u, kf){
  (1+u^2/kf)^(-(kf+1)/2)
}
g <- function(a, kg){
  ckl <- sqrt(a^2*(kg-1)/(kg-a^2))
  LHS <- 2/sqrt(pi*(kg-1)) * exp(lgamma(kg/2)-lgamma((kg-1)/2)) * integrate(beijif, lower = 0, upper = ckl, kf=kg-1)$value
  ckr <- sqrt(a^2*kg/(kg+1-a^2))
  RHS <-2/sqrt(pi*kg) * exp(lgamma((kg+1)/2)-lgamma(kg/2)) * integrate(beijif, lower = 0, upper = ckr, kf=kg)$value
  LHS-RHS
}

solution5 <- numeric(length(k))
for (i in 1:length(k)) {
  solution5[i] <- uniroot(g, c(1,2), kg=k[i])$root
}

###11.4
h <- function (a,kh) {
  (1-pt(sqrt(a^2*(kh-1) / (kh-a^2)), df=kh-1)) - (1-pt(sqrt(a^2*kh / (kh+1-a^2)), df=kh))
}

solution4 <- numeric(length(k))
for (i in 1:length(k)) {
  solution4[i] <- uniroot(h, c(1,2), kh=k[i])$root
}

###Compare
print(cbind(k=k, exercice4=solution4, exercice4=solution5))

## -----------------------------------------------------------------------------
data <- c(0.54,0.48,0.33,0.43,1.00,1.00,0.91,1.00,0.21,0.85)
tau <- 1
n <- length(data)
n1 <- sum(data<tau)
n2 <- n-n1
lam0 <- 0
lam1 <- 1#初始值


i <- 1
while (abs(lam0-lam1)>1e-10) {
  lam0 <- lam1
  # E step
  E <- function(lam) n*log(1/lam)-1/lam*sum(data[data<tau])-n2/lam*(tau+lam0)
  # M step
  lam1 <- optimize(E, lower = 0, upper = 2, maximum = TRUE)$maximum
}

# MLE 
# lam <- 1


lik <- function(lam){
  lik1 <- sapply(data[data<tau], function(x) {
    dexp(x,rate=1/lam)
  })
  lik2 <- sapply(data[data==tau],function(x){
    1-pexp(tau,rate = 1/lam)
  })
  prod(c(lik1,lik2))
}
MLE <- optimize(lik, lower = 0, upper = 2, maximum = TRUE)$maximum
print(cbind(EM=lam1, MLE))

## ----results='hide'-----------------------------------------------------------
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(100)
lapply(trims, function(trim) mean(x, trim = trim))
lapply(trims, mean, x = x)

## -----------------------------------------------------------------------------
formulas <- list(
mpg ~ disp,
mpg ~ I(1 / disp),
mpg ~ disp + wt,
mpg ~ I(1 / disp) + wt
)
lapply(formulas,lm,data=mtcars)

## -----------------------------------------------------------------------------
bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), rep = TRUE)
  mtcars[rows, ]
})

lapply(bootstraps,function(t) lm(mpg~disp,data=t))

## -----------------------------------------------------------------------------
rsq <- function(mod) summary(mod)$r.squared

## Ex1
cat("这是Ex1 4个模型的R-square\n")
lapply(lapply(formulas,lm,data=mtcars), rsq)

## Ex2
cat("这是Ex2 10个模型的R-square\n")
lapply(lapply(bootstraps,function(t) lm(mpg~disp,data=t)), rsq)

## -----------------------------------------------------------------------------
df= data.frame(a = 1:4, b= 5:8, c =9:12)

## 每一列取标准差
vapply(as.list(df),sd,numeric(1))

## -----------------------------------------------------------------------------
df2=data.frame(a = 1:4, b=c("x","y","z","s"), c =9:12)
vapply(df2[vapply(df2, is.numeric, logical(1))], sd, numeric(1))

## -----------------------------------------------------------------------------
library(parallel)
mcsapply <- function(n, func){
  core <- makeCluster(4)    # 使用4个核
  res <- parSapply(core, n, func)   # 并行计算，n为次数，func为函数
  stopCluster(core)         # 关闭核
}

## ---- eval=FALSE--------------------------------------------------------------
#  R2 <- function(i){
#    index <- sample(1:nrow(mtcars), rep = TRUE)
#    m <- lm(mpg ~ disp, data = mtcars[index,])
#    return(summary(m)$r.squared)
#  }
#  
#  # 使用sapply函数进行10次
#  system.time(sapply(1:10, R2))
#  # 使用mcsapply函数进行10次
#  system.time(mcsapply(1:10, R2))
#  
#  
#  # 使用sapply函数进行10000次
#  system.time(sapply(1:10000, R2))
#  # 使用mcsapply函数进行10000次
#  system.time(mcsapply(1:10000, R2))

## -----------------------------------------------------------------------------
a=1
b=1
N=10000
n=25

gibbsR=function(a,b,n,N){
  X=matrix(0, N, 2)  #样本阵
  X[1,]=c(0,0.5)
  for(i in 2:N){
    X2= X[i-1, 2]
    X[i,1]=rbinom(1,25,X2)
    X1=X[i,1]
    X[i,2]=rbeta(1,X1+a,25-X1+b)
  }
  return(X)
}

X=gibbsR(a,b,n,N)

plot(X[,1],X[,2],xlab = "x",ylab = "y",main = "gibbsR")

## ----eval=FALSE---------------------------------------------------------------
#  library(Rcpp)
#  dir="D:/Software/Github/2021Fall/Statistical_Computing/Homework/hw10/"
#  sourceCpp(paste0(dir,"R.cpp"))
#  Xc=gibbsC(a,b,n,N)
#  plot(Xc[,1],Xc[,2],xlab = "x",ylab = "y",main="gibbsC")

## ---- eval=FALSE--------------------------------------------------------------
#  qqplot(X[,1],Xc[,1],xlab = "gibbsR",ylab = "gibbsC",main="第1维变量QQ图")
#  abline(0,1,col = "red")
#  
#  qqplot(X[,2],Xc[,2], xlab = "gibbsR",ylab = "gibbsC",main="第2维变量QQ图")
#  abline(0,1,col = "red")

## ----eval=FALSE---------------------------------------------------------------
#  library(microbenchmark)
#  ts=microbenchmark(gibbR=gibbsR(a,b,n,N), gibbC=gibbsC(a,b,N,n))
#  summary(ts)[,c(1,3,5,6)]

