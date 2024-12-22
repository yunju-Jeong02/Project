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
    mutate(HE_glu_1 = ifelse(is.na(HE_glu), sample(HE_glu[is.na(HE_glu)==FALSE],1), HE_glu),HE_glu_2 = ifelse(is.na(HE_glu), sample(HE_glu[is.na(HE_glu)==FALSE],1), HE_glu),
           HE_glu_3 = ifelse(is.na(HE_glu), sample(HE_glu[is.na(HE_glu)==FALSE],1), HE_glu),HE_glu_4 = ifelse(is.na(HE_glu), sample(HE_glu[is.na(HE_glu)==FALSE],1), HE_glu),
           HE_glu_5 = ifelse(is.na(HE_glu), sample(HE_glu[is.na(HE_glu)==FALSE],1), HE_glu)) %>%
    ungroup()
  
  return(data)
}



metric = function(data1,data2){
  multi_dat = impute_match(data1)
  imp_dat = multi_dat[multi_dat$weights == 1.0,]
  no_dat = data2[is.na(data2$HE_glu)==FALSE,]
  no_dat = no_dat %>% mutate(HE_glu_1=HE_glu,HE_glu_2=HE_glu,HE_glu_3=HE_glu,HE_glu_4=HE_glu,HE_glu_5=HE_glu)
  imp_dat = imp_dat %>% dplyr::select(-c(id,HE_glu,weights,subclass,ps))
  no_dat = no_dat %>% dplyr::select(-HE_glu)
  
  final = rbind(no_dat,imp_dat)
  
  dataset = final[,c("HE_glu_1","HE_glu_2","HE_glu_3","HE_glu_4","HE_glu_5")]
  D = 5
  Yimputed = rowMeans(dataset)
  Wd = apply(dataset,2,var)
  W = mean(Wd)
  thetad = apply(dataset,2,mean)
  B = (1/4)*sum((thetad-mean(thetad))^2)
  total_var =  W + (1 + 1/D) * B
  fmi = (1+1/D) * (B/total_var)
  return(c(total_var,fmi))
}