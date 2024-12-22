library(ggplot2)
library(tidyverse)
library(dplyr)
library(MatchIt)
library(readxl)
library(caret)
library(randomForest)
library(VIM)
library(mice)


final = read_csv("D:\\고려대 족보\\2021\\대학원\\결측자료분석(송주원)\\midterm\\final_drop1to9.csv")
complete = read_csv("D:\\고려대 족보\\2021\\대학원\\결측자료분석(송주원)\\midterm\\complete.csv")


set.seed(715)



#complete 전처리
#full에 대한 logistic regression
complete1 = complete #원본
#age categorized
complete$age = ifelse(complete$age<20,1,ifelse(complete$age >= 40,3,2)) 
#de1_dg replace
ind_8 = which(complete$DE1_dg==8)
complete$DE1_dg[ind_8] = 0

#NA를 대체..해야함 (nrow 기준 17개 차이남 -> randomsampling함)
complete[is.na(complete$age),]
complete[is.na(complete$DE1_dg),]['DE1_dg'] = sample(c(0,1),1,prob=c(0.5,0.5))

#income에 대한 대체
sum1=sum(complete[complete$age == 1, "incm5"] == 1,na.rm=TRUE)
sum2=sum(complete[complete$age == 1, "incm5"] == 2,na.rm=TRUE)
sum3=sum(complete[complete$age == 1, "incm5"] == 3,na.rm=TRUE)
sum4=sum(complete[complete$age == 1, "incm5"] == 4,na.rm=TRUE)
sum5=sum(complete[complete$age == 1, "incm5"] == 5,na.rm=TRUE)

#가중치 주고 뽑기(age=1)
total_sum = sum(sum1, sum2, sum3, sum4, sum5)
prob = c(sum1, sum2, sum3, sum4, sum5) / total_sum
n = nrow(complete[complete$age == 1 & is.na(complete$incm5), ])
complete[complete$age == 1 & is.na(complete$incm5), ]['incm5'] = sample(1:5,n,prob=prob,replace=TRUE)

#age=2
sum1=sum(complete[complete$age == 2, "incm5"] == 1,na.rm=TRUE)
sum2=sum(complete[complete$age == 2, "incm5"] == 2,na.rm=TRUE)
sum3=sum(complete[complete$age == 2, "incm5"] == 3,na.rm=TRUE)
sum4=sum(complete[complete$age == 2, "incm5"] == 4,na.rm=TRUE)
sum5=sum(complete[complete$age == 2, "incm5"] == 5,na.rm=TRUE)

#가중치 주고 뽑기(age=2)
total_sum = sum(sum1, sum2, sum3, sum4, sum5)
prob = c(sum1, sum2, sum3, sum4, sum5) / total_sum
n = nrow(complete[complete$age == 2 & is.na(complete$incm5), ])
complete[complete$age == 2 & is.na(complete$incm5), ]['incm5'] = sample(1:5,n,prob=prob,replace=TRUE)

#age=2
sum1=sum(complete[complete$age == 3, "incm5"] == 1,na.rm=TRUE)
sum2=sum(complete[complete$age == 3, "incm5"] == 2,na.rm=TRUE)
sum3=sum(complete[complete$age == 3, "incm5"] == 3,na.rm=TRUE)
sum4=sum(complete[complete$age == 3, "incm5"] == 4,na.rm=TRUE)
sum5=sum(complete[complete$age == 3, "incm5"] == 5,na.rm=TRUE)

#가중치 주고 뽑기(age=2)
total_sum = sum(sum1, sum2, sum3, sum4, sum5)
prob = c(sum1, sum2, sum3, sum4, sum5) / total_sum
n = nrow(complete[complete$age == 3 & is.na(complete$incm5), ])
complete[complete$age == 3 & is.na(complete$incm5), ]['incm5'] = sample(1:5,n,prob=prob,replace=TRUE)


#HE_anem 대체
sum1=sum(complete[complete$sex == 1, "HE_anem"] == 0,na.rm=TRUE)
sum2=sum(complete[complete$sex == 1, "HE_anem"] == 1,na.rm=TRUE)

#가중치 주고 뽑기(sex=1 : 남자)
total_sum = sum(sum1, sum2)
prob = c(sum1, sum2) / total_sum
n = nrow(complete[complete$sex == 1 & is.na(complete$HE_anem), ])
complete[complete$sex == 1 & is.na(complete$HE_anem), ]['HE_anem'] = sample(c(0,1),n,prob=prob,replace=TRUE)

#HE_anem 대체
sum1=sum(complete[complete$sex == 2, "HE_anem"] == 0,na.rm=TRUE)
sum2=sum(complete[complete$sex == 2, "HE_anem"] == 1,na.rm=TRUE)

#가중치 주고 뽑기(sex=2 : 여자)
total_sum = sum(sum1, sum2)
prob = c(sum1, sum2) / total_sum
n = nrow(complete[complete$sex == 2 & is.na(complete$HE_anem), ])
complete[complete$sex == 2 & is.na(complete$HE_anem), ]['HE_anem'] = sample(c(0,1),n,prob=prob,replace=TRUE)
complete$age = complete1$age

#################################################################################################################
simple_hotdeck <- function(data, ...){
  variable_names = c(...)
  for (variable_name in variable_names){
    data = hotdeck(data,variable=variable_name)
  }
  return(data)
}

complete = simple_hotdeck(complete,"HE_Ucrea","HE_Uph","HE_Ubld","HE_Unitr","HE_Uro","HE_Upro","HE_Usg","HE_rPLS")


family_hotdeck = function(data, var1,var2,var3){
  variables = c(var1,var2,var3)
  data$age_group = cut(
    data[["age"]],
    breaks=c(-Inf,20,59,Inf),
    labels=c("20대 미만","20~59","60세 이상")
  )
  data$group = interaction(data$age_group,data$sex,data$region)
  for(var in variables){
    data = hotdeck(data,variable=var, domain_var="group")
  }
  data = data %>%  dplyr::select(-age_group, -group)
  return(data)
}

other_hotdeck = function(data,var1,var2){
  variables=c(var1,var2)
  data$age_group = cut(
    data[["age"]],
    breaks=c(-Inf,20,59,Inf),
    labels=c("20대 미만","20~59","60세 이상")
  )
  data$group = interaction(data$age_group,data$incm5)
  for(var in variables){
    data = hotdeck(data,variable=var, domain_var="group")
  }
  data = data %>%  dplyr::select(-age_group, -group)
  return(data)
}

complete = family_hotdeck(complete,"HE_DMfh3","HE_DMfh2","HE_DMfh1")
complete = other_hotdeck(complete,"occp","edu")

imp_val = c("DE1_ag","HE_BMI","HE_HbA1c","HE_Uglu","HE_sbp","HE_dbp")

impute_and_replace <- function(data, vars) {
  for (var in vars) {
    na_indices <- which(is.na(data[[var]]))
    imputed_data <- mice(data[, vars], m = 1, method = 'pmm', printFlag = FALSE)
    imputed_values <- complete(imputed_data, 1)
    if (length(na_indices) > 0) {
      data[[var]][na_indices] <- imputed_values[[var]][na_indices]
    }
  } 
  return(data)
}

complete = impute_and_replace(complete,imp_val)
complete = complete[complete$HE_DMfh3 != 8,]
complete = complete[,c('HE_BMI', 'HE_HbA1c', 'HE_sbp', 'HE_dbp', 'DE1_ag','age', 'sex','HE_glu','HE_anem','HE_DMfh3')]
write.csv(complete, file = "D:\\고려대 족보\\2021\\대학원\\결측자료분석(송주원)\\final\\complete.csv", row.names = FALSE)


################################################################################################################################
complete = read_csv("D:\\고려대 족보\\2021\\대학원\\결측자료분석(송주원)\\final\\complete.csv")

complete1 = complete

complete$age = ifelse(complete$age<30,1,ifelse(complete$age >= 60,3,2))
complete$DE1_ag <- ifelse(
  complete$DE1_ag < 40, 4, 
  ifelse(
    complete$DE1_ag >= 40 & complete$DE1_ag <= 79, 3, 
    ifelse(
      complete$DE1_ag == 80, 2, 
      ifelse(complete$DE1_ag == 888, 1, NA)
    )
  )
)
complete$age = factor(complete$age)
complete$DE1_ag = factor(complete$DE1_ag)
z = model.matrix(~age+HE_anem+DE1_ag+HE_DMfh3, data=complete)
head(z)

#변수 구성이 좀 어려움 ... 바꾸면 되니까 머..
component = - 1.8 * z[, 2] - 1.2 * z[, 3] - 0.01 * z[, 4] + 0.45 * z[, 5] + 0.55 * z[,6] + 0.01 * z[,7] + 0.4*z[,8]
complete$propensity = exp(component)/(1+exp(component))

set.seed(715)
#사분위수 구하기
complete$quartile <- cut(complete$propensity,
                         breaks = quantile(complete$propensity, probs = seq(0, 1, 0.25)),
                         include.lowest = TRUE,
                         labels = 1:4)

#  그룹별 가중치 부여
weights <- 1:4  # 1분위수에 가장 낮은 가중치
complete$weight <- as.numeric(as.character(complete$quartile))
complete$weight <- weights[complete$weight]

# 인덱스 샘플링
#10%
mar_10<- sample(1:nrow(complete), size = 0.1*nrow(complete), prob = complete$weight, replace = FALSE)
complete_mar10 = complete1
complete_mar10$HE_glu[mar_10] = NA
sum(is.na(complete_mar10$HE_glu))
write.csv(complete_mar10, file = "D:\\고려대 족보\\2021\\대학원\\결측자료분석(송주원)\\final\\complete_mar10.csv", row.names = FALSE)

#40%
mar_40<- sample(1:nrow(complete), size = 0.4*nrow(complete), prob = complete$weight, replace = FALSE)
complete_mar40 = complete1
complete_mar40$HE_glu[mar_40] = NA
sum(is.na(complete_mar40$HE_glu))
write.csv(complete_mar40, file = "D:\\고려대 족보\\2021\\대학원\\결측자료분석(송주원)\\final\\complete_mar40.csv", row.names = FALSE)