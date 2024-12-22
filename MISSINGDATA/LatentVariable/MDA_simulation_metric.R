set.seed(714)

library(PolyGibbs)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(MatchIt)
require(truncnorm)
require(MASS)

data = read.csv("D:\\고려대 족보\\2021\\대학원\\결측자료분석(송주원)\\final\\simul_dat.csv",header=TRUE)

#######################################################################################
#metric

# 1. calculate X distance

avg_x_dis = function(data, variables) {
  group = data$group
  
  std_diff_x = function(x_treated, x_control) {
    mean_diff = mean(x_treated) - mean(x_control)
    pooled_sd = sqrt((var(x_treated) + var(x_control)) / 2)
    return(mean_diff / pooled_sd)
  }
  
  # making treat and control
  std_diffs = sapply(variables, function(var) {
    treated = data[[var]][group == 1]
    control = data[[var]][group == 0]
    abs(std_diff_x(treated, control))
  })
  
  return(mean(std_diffs))
}



# 2. calculate q distance
avg_q_dis = function(data,variables){
  data1 = data
  formula = as.formula(paste("group","~",paste(variables,collapse="+")))
  ps_model = glm(formula,data=data1,family=binomial)
  data1$pscore = predict(ps_model,type="response")
  
  e_treated = data1$pscore[data1$group==1]
  e_control = data1$pscore[data1$group==0]
  
  std_diff_q = function(e_treated,e_control){
    q_treated = log(e_treated/(1-e_treated))
    q_control = log(e_control/(1-e_control))
    
    mean_diff = mean(q_treated)-mean(q_control)
    pooled_sd = sqrt((var(q_treated)+var(q_control))/2)
    
    return(mean_diff/pooled_sd)
  }
  return(std_diff_q(e_treated,e_control))
}

# 3. calculate y concordance
concor_d = function(data,method){
  treated = data %>% filter(group==1)
  matched = data[na.omit(method$match.matrix[,1]),]
  
  summation = sum(sum(treated$Y1==matched$Y1),sum(treated$Y2==matched$Y2),sum(treated$Y3==matched$Y3),
                  sum(treated$Y4==matched$Y4), sum(treated$Y5==matched$Y5))
  return(summation/500)
}

#######################################################################################

#method1star
method1star = matchit(group ~ X1+X2+X3+Y1+Y2+Y3+Y4+Y5,data=data,"nearest")
match_data1star = match.data(method1star)

method1star_res = c(avg_x_dis(match_data1star,c("X1","X2","X3")),avg_q_dis(match_data1star,c("X1","X2","X3","Y1","Y2","Y3","Y4","Y5")),concor_d(match_data1star,method1star))

#method1
method1 = matchit(group ~ X1+X2+X3+Z1+Z2+Z3+Z4+Z5,data=data,"nearest")
match_data1 = match.data(method1)

method1_res = c(avg_x_dis(match_data1,c("X1","X2","X3")),avg_q_dis(match_data1,c("X1","X2","X3","Z1","Z2","Z3","Z4","Z5")),concor_d(match_data1,method1))

#method2
ps = glm(group ~ X1 + X2 + X3 + Z1 + Z2 + Z3 + Z4 + Z5, data = data, family = binomial(link = "logit"))
data1 = data
data1$ps = predict(ps,type="response")
method2 = matchit(group ~X1+X2+X3+Z1+Z2+Z3+Z4+Z5+ps,data=data1,method = "nearest",distance="mahalanobis")
match_data2 = match.data(method2)

method2_res = c(avg_x_dis(match_data2,c("X1","X2","X3")),avg_q_dis(match_data2,c("X1","X2","X3","Z1","Z2","Z3","Z4","Z5")),concor_d(match_data2,method2))


result_situation1 = cbind(method1star_res,method1_res,method2_res)
result_situation1

#######################################################################################
# apply random ordering

process_matching1star <- function(data, n_iterations = 1000) {
  
  # 결과를 저장할 벡터 초기화
  avg_x_dis_results <- numeric(n_iterations)
  avg_q_dis_results <- numeric(n_iterations)
  concor_d_results <- numeric(n_iterations)
  
  for (i in 1:n_iterations) {
    
    # 1. 데이터 섞기
    shuffled_data <- data[sample(nrow(data)), ]
    
    # 2. Matchit을 이용한 매칭
    method1star <- matchit(group ~ X1 + X2 + X3 + Y1 + Y2 + Y3 + Y4 + Y5, 
                           data = shuffled_data, 
                           method = "nearest")
    match_data1star <- match.data(method1star)
    
    # 3. 각 계산 수행
    avg_x_dis_results[i] <- avg_x_dis(match_data1star, c("X1", "X2", "X3"))
    avg_q_dis_results[i] <- avg_q_dis(match_data1star, c("X1", "X2", "X3", "Y1", "Y2", "Y3", "Y4", "Y5"))
    concor_d_results[i] <- concor_d(match_data1star, method1star)
  }
  
  # 4. 각 계산의 평균값 반환
  result <- list(
    avg_x_dis_mean = mean(avg_x_dis_results),
    avg_q_dis_mean = mean(avg_q_dis_results),
    concor_d_mean = mean(concor_d_results)
  )
  
  return(result)
}


result_1star <- process_matching1star(data, n_iterations = 1000)
result_1star


process_matching1 <- function(data, n_iterations = 1000) {
  
  # 결과를 저장할 벡터 초기화
  avg_x_dis_results <- numeric(n_iterations)
  avg_q_dis_results <- numeric(n_iterations)
  concor_d_results <- numeric(n_iterations)
  
  for (i in 1:n_iterations) {
    
    # 1. 데이터 섞기
    shuffled_data <- data[sample(nrow(data)), ]
    
    # 2. Matchit을 이용한 매칭
    method1 = matchit(group ~ X1+X2+X3+Z1+Z2+Z3+Z4+Z5,data=shuffled_data,"nearest")
    match_data1 = match.data(method1)
    
    # 3. 각 계산 수행
    avg_x_dis_results[i] <- avg_x_dis(match_data1, c("X1", "X2", "X3"))
    avg_q_dis_results[i] <- avg_q_dis(match_data1, c("X1","X2","X3","Z1","Z2","Z3","Z4","Z5"))
    concor_d_results[i] <- concor_d(match_data1, method1)
  }
  
  # 4. 각 계산의 평균값 반환
  result <- list(
    avg_x_dis_mean = mean(avg_x_dis_results),
    avg_q_dis_mean = mean(avg_q_dis_results),
    concor_d_mean = mean(concor_d_results)
  )
  
  return(result)
}


result_1 <- process_matching1(data, n_iterations = 1000)
result_1


process_matching2 <- function(data, n_iterations = 1000) {
  
  # 결과를 저장할 벡터 초기화
  avg_x_dis_results <- numeric(n_iterations)
  avg_q_dis_results <- numeric(n_iterations)
  concor_d_results <- numeric(n_iterations)
  
  for (i in 1:n_iterations) {
    
    # 1. 데이터 섞기
    shuffled_data <- data[sample(nrow(data)), ]
    
    # 2. Matchit을 이용한 매칭
    method2 = matchit(group ~X1+X2+X3+Z1+Z2+Z3+Z4+Z5+ps,data=shuffled_data,method = "nearest",distance="mahalanobis")
    match_data2 = match.data(method2)
    
    # 3. 각 계산 수행
    avg_x_dis_results[i] <- avg_x_dis(match_data1, c("X1", "X2", "X3"))
    avg_q_dis_results[i] <- avg_q_dis(match_data1, c("X1","X2","X3","Z1","Z2","Z3","Z4","Z5"))
    concor_d_results[i] <- concor_d(match_data1, method1)
  }
  
  # 4. 각 계산의 평균값 반환
  result <- list(
    avg_x_dis_mean = mean(avg_x_dis_results),
    avg_q_dis_mean = mean(avg_q_dis_results),
    concor_d_mean = mean(concor_d_results)
  )
  
  return(result)
}


result_2 <- process_matching2(data1, n_iterations = 1000)
result_2

cbind(result_1star,result_1,result_2)