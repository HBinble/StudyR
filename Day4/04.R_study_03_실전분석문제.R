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
