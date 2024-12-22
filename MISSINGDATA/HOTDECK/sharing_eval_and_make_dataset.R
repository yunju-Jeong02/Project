# metrics function
calculate_imputation_metrics_10 <- function(data, true) {
  # 진짜 값
  Ytrue <- true
  
  # 대체된 값들
  Yimputed <- apply(data,2,mean)
  
  # 1. 편향(bias)
  estimated_means <- mean(Yimputed)
  bias <- mean(Ytrue) - estimated_means
  
  # 2. RMSE 계산
  miss = 0.1*length(true)
  rmse = mean(apply(data, 2, function(Yimp) {
    sqrt((1 / miss) * sum((Ytrue - Yimp)^2))
  }))
  
  
  # 5. 분산 계산
  D <- ncol(data)  # 대체값의 수
  Wd <- apply(data,2,var)  # 각 대체값의 분산
  B <- (1/4)*sum((apply(data,2,mean)-mean(apply(data,2,mean)))^2)  # 평균의 분산
  
  # 평균 내부 대체 분산
  W <- mean(Wd)
  
  # 총 분산
  total_variance <- W + (1 + 1/D) * B
  
  #fmi
  fmi = (1+1/D) * (B/total_variance)
  
  # 결과 리스트 생성
  results <- list(
    Bias = bias,
    RMSE = rmse,
    Variance = total_variance,
    FMI = fmi
  )
  
  return(results)
}

calculate_imputation_metrics_40 <- function(data, true) {
  # 진짜 값
  Ytrue <- true
  
  # 대체된 값들
  Yimputed <- apply(data,2,mean)
  
  # 1. 편향(bias)
  estimated_means <- mean(Yimputed)
  bias <- mean(Ytrue) - estimated_means
  
  # 2. RMSE 계산
  miss = 0.4*length(true)
  rmse = mean(apply(data, 2, function(Yimp) {
    sqrt((1 / miss) * sum((Ytrue - Yimp)^2))
  }))
  
  
  # 5. 분산 계산
  D <- ncol(data)  # 대체값의 수
  Wd <- apply(data,2,var)  # 각 대체값의 분산
  B <- (1/4)*sum((apply(data,2,mean)-mean(apply(data,2,mean)))^2)  # 평균의 분산
  
  # 평균 내부 대체 분산
  W <- mean(Wd)
  
  # 총 분산
  total_variance <- W + (1 + 1/D) * B
  
  #fmi
  fmi = (1+1/D) * (B/total_variance)
  
  # 결과 리스트 생성
  results <- list(
    Bias = bias,
    RMSE = rmse,
    Variance = total_variance,
    FMI = fmi
  )
  
  return(results)
}

calculate_imputation_metrics_10_median <- function(data, true) {
  # 진짜 값
  Ytrue <- true
  
  # 대체된 값들
  Yimputed <- apply(data,2,median)
  
  # 1. 편향(bias)
  estimated <- median(Yimputed)
  bias <- median(Ytrue) - estimated
  
  # 2. RMSE 계산
  miss = 0.1*length(true)
  rmse = mean(apply(data, 2, function(Yimp) {
    sqrt((1 / miss) * sum((Ytrue - Yimp)^2))
  }))
  
  
  # 5. 분산 계산
  D <- ncol(data)  # 대체값의 수
  Wd <- apply(data,2,var)  # 각 대체값의 분산
  B <- (1/4)*sum((apply(data,1,median)-median(apply(data,2,median)))^2)  # 평균의 분산
  
  # 평균 내부 대체 분산
  W <- mean(Wd)
  
  # 총 분산
  total_variance <- W + (1 + 1/D) * B
  
  #fmi
  fmi <- (1+1/D) * (B/total_variance)
  
  # 결과 리스트 생성
  results <- list(
    Bias = bias,
    RMSE = rmse,
    Variance = total_variance,
    FMI = fmi
  )
  
  return(results)
}

calculate_imputation_metrics_40_median <- function(data, true) {
  # 진짜 값
  Ytrue <- true
  
  # 대체된 값들
  Yimputed <- apply(data,2,mean)
  
  # 1. 편향(bias)
  estimated <- median(Yimputed)
  bias <- median(Ytrue) - estimated
  
  # 2. RMSE 계산
  miss = 0.4*length(true)
  rmse = mean(apply(data, 2, function(Yimp) {
    sqrt((1 / miss) * sum((Ytrue - Yimp)^2))
  }))
  
  
  # 5. 분산 계산
  D <- ncol(data)  # 대체값의 수
  Wd <- apply(data,2,var)  # 각 대체값의 분산
  B <- (1/4)*sum((apply(data,1,median)-median(apply(data,2,median)))^2)  # 평균의 분산
  
  # 평균 내부 대체 분산
  W <- mean(Wd)
  
  # 총 분산
  total_variance <- W + (1 + 1/D) * B
  
  #fmi
  fmi = (1+1/D) * (B/total_variance)
  
  # 결과 리스트 생성
  results <- list(
    Bias = bias,
    RMSE = rmse,
    Variance = total_variance,
    FMI = fmi
  )
  
  return(results)
}

# making dataframe

all_compare_10 = function(datasets,true_values){
  results_df = data.frame(
    Dataset = character(),
    Bias = numeric(),
    RMSE = numeric(),
    Variance = numeric(),
    FMI = numeric()
  )
  
  for(dataset_name in names(datasets)){
    dataset = datasets[[dataset_name]]
    metrics = calculate_imputation_metrics_10(dataset,true_values)
    results_df = rbind(results_df, data.frame(Dataset=dataset_name,Bias=metrics$Bias,RMSE=metrics$RMSE, Variance=metrics$Variance, FMI=metrics$FMI))
  }
  return(results_df)
}

all_compare_40 = function(datasets,true_values){
  results_df = data.frame(
    Dataset = character(),
    Bias = numeric(),
    RMSE = numeric(),
    Variance = numeric(),
    FMI = numeric()
  )
  
  for(dataset_name in names(datasets)){
    dataset = datasets[[dataset_name]]
    metrics = calculate_imputation_metrics_40(dataset,true_values)
    results_df = rbind(results_df, data.frame(Dataset=dataset_name,Bias=metrics$Bias,RMSE=metrics$RMSE, Variance=metrics$Variance, FMI=metrics$FMI))
  }
  return(results_df)
}

all_compare_10_med = function(datasets,true_values){
  results_df = data.frame(
    Dataset = character(),
    Bias = numeric(),
    RMSE = numeric(),
    Variance = numeric(),
    FMI = numeric()
  )
  
  for(dataset_name in names(datasets)){
    dataset = datasets[[dataset_name]]
    metrics = calculate_imputation_metrics_10_median(dataset,true_values)
    results_df = rbind(results_df, data.frame(Dataset=dataset_name,Bias=metrics$Bias,RMSE=metrics$RMSE, Variance=metrics$Variance, FMI=metrics$FMI))
  }
  return(results_df)
}

all_compare_40_med = function(datasets,true_values){
  results_df = data.frame(
    Dataset = character(),
    Bias = numeric(),
    RMSE = numeric(),
    Variance = numeric(),
    FMI = numeric()
  )
  
  for(dataset_name in names(datasets)){
    dataset = datasets[[dataset_name]]
    metrics = calculate_imputation_metrics_40_median(dataset,true_values)
    results_df = rbind(results_df, data.frame(Dataset=dataset_name,Bias=metrics$Bias,RMSE=metrics$RMSE, Variance=metrics$Variance, FMI=metrics$FMI))
  }
  return(results_df)
}