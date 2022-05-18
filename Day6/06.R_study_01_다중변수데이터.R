## 다중변수 데이터
# 데이터 확인
head(cars)

plot(cars$speed, cars$dist,
     main = '자동차 속도와 제동거리',
     xlab = '속도',
     ylab = '제동거리')

cor(cars$speed, cars$dist) # 속도와 제동거리의 상관관계(correlation coefficient)

class(state.x77)
st <- data.frame(state.x77)
class(st)
head(st)

plot(st)

cor(st$Life.Exp, st$Murder)
cor(st$Illiteracy, st$Murder)

cor(st$Income, st$Area)
cor(st$Income, st$HS.Grad)
cor(st$Income, st$Income)
cor(st$Area, st$Frost)

cor(st)


# LAB 경제지표 데이터 분석
str(longley)
head(longley)

df <- longley[,c('GNP', 'Unemployed', 'Armed.Forces',
                 'Population','Employed')]
df
plot(df)
cor(df)

# RData로 데이터셋 불러오기
getwd()
load('./result_sales_dt.RData')
# RData 저장
save(result_sales_dt, file = './result_dt.RData')

## 실전분석
install.packages('Ecdat')
library(Ecdat)
str(Hdma)

tbl <- table(Hdma$deny)
tbl      
tbl2 <- tbl / sum(tbl)
tbl2
names(tbl) <- c('승인', '거절')
tbl
names(tbl2) <- c('승인', '거절')
tbl2

barplot(tbl2, main = '주택담보대출 승인/거절 비율',
        col = c('green', 'red'),
        ylim = c(0,1), las = 1,
        ylab = '비율')

barplot(tbl, main = '주택담보대출 승인/거절 건수',
        col = c('green', 'red'),
        ylim = c(0,2500), las = 1,
        ylab = '건수')

hist(Hdma$lvr, main = 'LTV', col = rainbow(10),
     ylim = c(0,1200))

black.yn <- table(Hdma$black)
# 흑인 신청자중 거절 비율
black.deney <- sum(Hdma$black=='yes' & Hdma$deny=='yes') / 
  black.yn['yes']
# 비흑인 신청자중 거절 비율
non.black.deney <- sum(Hdma$black=='no' & Hdma$deny=='yes') / 
  black.yn['no']
cat('흑인, 비흑인 거절률 : ', black.deney, non.black.deney, '\n')

black.credit <- mean(Hdma$ccs[Hdma$black=='yes'])
non.black.credit <- mean(Hdma$ccs[Hdma$black=='no'])
cat('흑인, 비흑인 신용등급 : ', black.credit, non.black.credit, 
    '\n')

## dir 수입대비 보증금 비율, hir 수입대비 주택유지비용 비율
## ccs 고객 신용등급, mcs 대출 신용등급
df <- Hdma[,c('dir','hir','ccs','mcs')]
df
point.col <- c('green','red') 
plot(df, col= point.col[Hdma$deny])
cor(df)




