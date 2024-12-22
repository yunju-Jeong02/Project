devtools::install_github("zovialpapai/PolyGibbs")

set.seed(714)
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
X2 <- MASS::mvrnorm(n2, mu = mu2, Sigma = sigma2)

# 상수항 추가
X1_star <- cbind(1, X1)
X2_star <- cbind(1, X2)

#true beta
beta = matrix(c(0.5,0.7,0.3,0.5,0.5,-0.1,0.1,0.2,0.15,
                0.3,-0.2,-0.1,-0.2,0.25,0.7,0.2,-0.5,0.15,
                -0.15,0.5),byrow=TRUE,nrow=4)
#data

#group1
Y1 = matrix(rep(0,n1*5),nrow=n1)
for(i in 1:5){
  Y1[,i] = rbinom(n1,1,pnorm(X1_star %*% beta[,i]))
}

#group2
Y2 = matrix(rep(0,n2*5),nrow=n2)
for(i in 1:5){
  Y2[,i] = rbinom(n2,1,pnorm(X2_star %*% beta[,i]))
}



