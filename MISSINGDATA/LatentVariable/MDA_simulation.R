#devtools::install_github("zovialpapai/PolyGibbs")

set.seed(714)
library(PolyGibbs)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(MatchIt)
require(truncnorm)
require(MASS)

# 주어진 조건 설정
mu1 <- c(-1, 0, 1)
mu2 <- c(-2, 1, 2)

sigma1 <- matrix(c(1, 0.7, 0.7,
                   0.7, 1, 0.7,
                   0.7, 0.7, 1), nrow = 3, byrow = TRUE)

sigma2 <- matrix(c(1, 0.7, 0.7,
                   0.7, 1, 0.7,
                   0.7, 0.7, 1), nrow = 3, byrow = TRUE)

# 데이터 생성
n1 <- 100
n2 <- 300
X1 <- MASS::mvrnorm(n1, mu = mu1, Sigma = sigma1)
X1_star = cbind(rep(1,n1),X1)

colnames(X1) = c("X1","X2","X3")
group1 = data.frame(group=1,X1)

X2 <- MASS::mvrnorm(n2, mu = mu2, Sigma = sigma2)
X2_star = cbind(rep(1,n2),X2)

colnames(X2) = c("X1","X2","X3")
group2 = data.frame(group=2,X2)

X = bind_rows(group1,group2)
X_design = cbind(intercept=1, X[,c("X1","X2","X3")])
X_design = as.matrix(X_design)

#true beta
beta = matrix(c(0.5,0.7,0.3,0.5,0.5,-0.1,0.1,0.2,0.15,
                0.3,-0.2,-0.1,-0.2,0.25,0.7,0.2,-0.5,0.15,
                -0.15,0.5),byrow=TRUE,nrow=4)


#group1

Y1 = matrix(rep(0,n1*5),nrow=n1)
for(i in 1:5){
  Y1[,i] = rbinom(n1,1,pnorm(X1_star %*% beta[,i]))
}
colnames(Y1) = c("Y1","Y2","Y3","Y4","Y5")
y1 = data.frame(group=1,Y1)

#group2
Y2 = matrix(rep(0,n2*5),nrow=n2)
for(i in 1:5){
  Y2[,i] = rbinom(n2,1,pnorm(X2_star %*% beta[,i]))
}
colnames(Y2) = c("Y1","Y2","Y3","Y4","Y5")
y2 = data.frame(group=2,Y2)
Y = bind_rows(y1,y2)
Y = Y[,-1]

# prior
nIter = 10^5
burn_in = 10^3
prior = 2
prior_mean = rep(1,4)
prior_var = diag(10, 4)

Output1 = BinaryGibbs_fit(X_design[,-1], Y[,1], nIter, prior, burn_in, prior_mean, prior_var)
estimates1 = Output1$estimates
Z1 = X_design%*%estimates1
estimates1

Output2 = BinaryGibbs_fit(X_design[,-1], Y[,2], nIter, prior, burn_in, prior_mean, prior_var)
estimates2 = Output2$estimates
Z2 = X_design%*%estimates2
estimates2

Output3 = BinaryGibbs_fit(X_design[,-1], Y[,3], nIter, prior, burn_in, prior_mean, prior_var)
estimates3 = Output3$estimates
Z3 = X_design%*%estimates3
estimates3

Output4 = BinaryGibbs_fit(X_design[,-1], Y[,4], nIter, prior, burn_in, prior_mean, prior_var)
estimates4 = Output4$estimates
Z4 = X_design%*%estimates4
estimates4

Output5 = BinaryGibbs_fit(X_design[,-1], Y[,5], nIter, prior, burn_in, prior_mean, prior_var)
estimates5 = Output5$estimates
Z5 = X_design%*%estimates5
estimates5

###############################################################################################

YZ <- as.data.frame(cbind(Y, Z1,Z2,Z3,Z4,Z5))
plot1=ggplot(data=YZ)+geom_histogram(position='identity',alpha=0.5,mapping = aes(x=Z1, fill=as.factor(Y1)))
ggsave(filename="D:\\고려대 족보\\2021\\대학원\\결측자료분석(송주원)\\final\\plot1.png",plot=plot1,width=6,height=4,dpi = 300)
plot2=ggplot(data=YZ)+geom_histogram(position='identity',alpha=0.5,mapping = aes(x=Z2, fill=as.factor(Y2)))
ggsave(filename="D:\\고려대 족보\\2021\\대학원\\결측자료분석(송주원)\\final\\plot2.png",plot=plot2,width=6,height=4,dpi = 300)
plot3=ggplot(data=YZ)+geom_histogram(position='identity',alpha=0.5,mapping = aes(x=Z3, fill=as.factor(Y3)))
ggsave(filename="D:\\고려대 족보\\2021\\대학원\\결측자료분석(송주원)\\final\\plot3.png",plot=plot3,width=6,height=4,dpi = 300)
plot4=ggplot(data=YZ)+geom_histogram(position='identity',alpha=0.5,mapping = aes(x=Z4, fill=as.factor(Y4)))
ggsave(filename="D:\\고려대 족보\\2021\\대학원\\결측자료분석(송주원)\\final\\plot4.png",plot=plot4,width=6,height=4,dpi = 300)
plot5=ggplot(data=YZ)+geom_histogram(position='identity',alpha=0.5,mapping = aes(x=Z5, fill=as.factor(Y5)))
ggsave(filename="D:\\고려대 족보\\2021\\대학원\\결측자료분석(송주원)\\final\\plot5.png",plot=plot5,width=6,height=4,dpi = 300)


##############################################################################################

# dataset
data = cbind(X_design[,-1],YZ,c(rep(1,n1),rep(0,n2)))
colnames(data) = c("X1","X2","X3","Y1","Y2","Y3","Y4","Y5","Z1","Z2","Z3","Z4","Z5","group")
write.csv(data, file = "D:\\고려대 족보\\2021\\대학원\\결측자료분석(송주원)\\final\\simul_dat.csv", row.names = FALSE)

