install.packages('reshape2')
library(reshape2)
str(tips)
tips

unique(tips$sex)
idx <- which(tips[, 'sex'] == 'Female')
avg.female <- mean(tips[idx, 'tip'])
avg.female
idx <- which(tips[, 'sex'] == 'Male')
avg.male <- mean(tips[idx, 'tip'])
avg.male

unique(tips$smoker)
idx <- which(tips$smoker == 'Yes')
avg.smoker <- mean(tips[idx, 'tip'])
avg.smoker

idx <- which(tips$smoker == 'No')
avg.nonsmoker <- mean(tips[idx, 'tip'])
avg.nonsmoker

meanbycol.tip <- function(tips, colname){ }

meanbycol.tip <- function(colname) {
  value <- unique(tips[, colname]) # ① 값의 종류를 구함
  result <- list()
  for(i in 1:length(value)) { # ② 값의 종류별로 평균을 구함
    idx <- which(tips[,colname] == value[i]) # ②-1
    result[i] <- mean(tips[idx,'tip']) # ②-2
  }
  names(result) <- value # ③ 결과값에 이름을 붙임
  return(result)
}

source('myfunc.R')
meanbycol.tip('sex')

meanbycol.tip('smoker')
meanbycol.tip('size')
meanbycol.tip('day')
  
categorize.tip <- function(tips){
  tip_ratio <- tips$tip/tips$total_bill * 100
  class <- c()
  for(i in 1:nrow(tips)){
    if(tip_ratio[i] < 10){
      class[i] <- 1
    }else if(tip_ratio[i] < 15){
      class[i] <- 2
    }else if(tip_ratio[i] < 20){
      class[i] <- 3
    }else{
      class[i] <- 4
    }
  }
  tips.new <- cbind(tips, type = class, ratio = tip_ratio)
  return(tips.new)
}
  
source('myfunc.R')
tips.new <- categorize.tip(tips)
head(tips.new)
  
