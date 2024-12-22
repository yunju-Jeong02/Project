set.seed(714)
library(PolyGibbs)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(MatchIt)
require(truncnorm)
require(MASS)

data = read.csv("D:\\고려대 족보\\2021\\대학원\\결측자료분석(송주원)\\final\\simul_dat.csv",header=TRUE)
data = data %>% dplyr::select(-group)
data1 = data

# X1을 missing으로
z = model.matrix(~Y1 + Y2 + Y3 + Y4 + Y5, data=data)
head(z)

#변수 구성이 좀 어려움 ... 바꾸면 되니까 머..
component = 0.5*z[,2] + 1.1*z[,3] -3.7*z[,4] + 2.5*z[,5] - 0.3*z[,6]
data$propensity = exp(component)/(1+exp(component))

set.seed(715)
#사분위수 구하기
data$quartile <- cut(data$propensity,
                         breaks = quantile(data$propensity, probs = seq(0, 1, 0.25)),
                         include.lowest = TRUE,
                         labels = 1:4)

#  그룹별 가중치 부여
weights <- 1:4  # 1분위수에 가장 낮은 가중치
data$weight <- as.numeric(as.character(data$quartile))
data$weight <- weights[data$weight]

# 인덱스 샘플링
#10%
mar_10<- sample(1:nrow(data), size = 0.1*nrow(data), prob = data$weight, replace = FALSE)
complete_mar10 = data1
complete_mar10$X1[mar_10] = NA
sum(is.na(complete_mar10$X1))
write.csv(complete_mar10, file = "D:\\고려대 족보\\2021\\대학원\\결측자료분석(송주원)\\final\\sim_complete_mar10.csv", row.names = FALSE)

#40%
mar_40<- sample(1:nrow(data), size = 0.4*nrow(data), prob = data$weight, replace = FALSE)
complete_mar40 = data1
complete_mar40$X1[mar_40] = NA
sum(is.na(complete_mar40$X1))
write.csv(complete_mar40, file = "D:\\고려대 족보\\2021\\대학원\\결측자료분석(송주원)\\final\\sim_complete_mar40.csv", row.names = FALSE)

############################################################################################################################################

mar10 = read_csv("D:\\고려대 족보\\2021\\대학원\\결측자료분석(송주원)\\final\\sim_complete_mar10.csv")
mar40 = read_csv("D:\\고려대 족보\\2021\\대학원\\결측자료분석(송주원)\\final\\sim_complete_mar40.csv")

mar10$m = ifelse(is.na(mar10$X1),1,0)
mar40$m = ifelse(is.na(mar40$X1),1,0)

############################################################################################################################################

impute_match <- function(data) {
  # 데이터 유효성 검사
  if (!"id" %in% colnames(data) | !"subclass" %in% colnames(data)) {
    stop("데이터 프레임에 'id' 또는 'subclass' 열이 없습니다.")
  }
  
  if (!"weights" %in% colnames(data)) {
    stop("데이터 프레임에 'weights' 열이 없습니다.")
  }
  
  # X1이 NA일 때 같은 subclass의 X1 값으로 대체
  data <- data %>%
    group_by(subclass) %>%
    mutate(X1_1 = ifelse(is.na(X1), sample(X1[is.na(X1)==FALSE],1), X1),X1_2 = ifelse(is.na(X1), sample(X1[is.na(X1)==FALSE],1), X1),
           X1_3 = ifelse(is.na(X1), sample(X1[is.na(X1)==FALSE],1), X1),X1_4 = ifelse(is.na(X1), sample(X1[is.na(X1)==FALSE],1), X1),
           X1_5 = ifelse(is.na(X1), sample(X1[is.na(X1)==FALSE],1), X1)) %>%
    ungroup()
  
  return(data)
}


metric = function(data1,data2){
  multi_dat = impute_match(data1)
  imp_dat = multi_dat[multi_dat$weights == 1.0,]
  no_dat = data2[is.na(data2$X1)==FALSE,]
  no_dat = no_dat %>% mutate(X1_1=X1,X1_2=X1,X1_3=X1,X1_4=X1,X1_5=X1)
  imp_dat = imp_dat %>% dplyr::select(-c(id,X1,weights,subclass,ps))
  no_dat = no_dat %>% dplyr::select(-X1)
  
  final = rbind(no_dat,imp_dat)
  
  dataset = final[,c("X1_1","X1_2","X1_3","X1_4","X1_5")]
  D = 5
  Wd = apply(dataset,2,var)
  W = mean(Wd)
  thetad = apply(dataset,2,mean)
  B = (1/4)*sum((thetad-mean(thetad))^2)
  total_var =  W + (1 + 1/D) * B
  fmi = (1+1/D) * (B/total_var)
  return(list(total_var = total_var, fmi = fmi, final = final))
}

# mar 10
ps = glm(m ~ X2 + X3 + Z1 + Z2 + Z3 + Z4 + Z5, data = mar10, family = binomial(link = "logit"))
mar101 = mar10
mar101$ps = predict(ps,type="response")
method2 = matchit(m ~ X2+X3+Z1+Z2+Z3+Z4+Z5+ps,data=mar101,method = "nearest",distance="mahalanobis",ratio=10,replace=TRUE)
dat = get_matches(method2)

res = metric(dat,mar10)$final
res <- res %>% mutate(X1 = rowMeans(across(c(X1_1, X1_2, X1_3, X1_4, X1_5))))

c(metric(dat,mar10)$total_var,metric(dat,mar10)$fmi,var(data1$X1))

