getwd()
setwd('./Day8./data')

install.packages("data table")
library(data.table)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("lubridate")
library(lubridate)
install.packages("stringr")
library(stringr)
install.packages("forecast")
library(forecast)
install.packages("randtests")
library(randtests)

load(file = './result_sales_dt.RData')

str(result_sales_dt)
unique(result_sales_dt$yyyymm)

glimpse(result_sales_dt, width=60)

# 쿼터별 매매량 계산
#qrt_cnt <- data.table명 [where, select, group by]
qrt_cnts <- result_sales_dt[,.N, yyyyqrt]
str(qrt_cnts)
head(qrt_cnts)
tail(qrt_cnts)


qrt_cnts <- result_sales_dt[yyyyqrt != '2015Q2',.N,yyyyqrt]

head(qrt_cnts)
tail(qrt_cnts)

ggplot(qrt_cnts, aes(x=yyyyqrt, y=N, group=1)) + 
  geom_line() + xlab('년도분기') + ylab('매매건수') +
  theme(axis.text.x=element_text(angle=90)) +
  stat_smooth(method = 'lm',
              col = 'Red',
              size = 2,
              span = 0.3)

region_cnts <- result_sales_dt[yyyyqrt !='2015Q2',.N,.(yyyyqrt,region)]

head(region_cnts)

ggplot(region_cnts, aes(yyyyqrt, N, group=region)) + 
  geom_line() + facet_wrap(~region, scales = 'free_y', ncol=3) +
  stat_smooth(method = 'lm') +
  theme(axis.text.x = element_blank())

## 시계열의 랜덤성 검정
# 월별 지역별 매매량
region_cnts <- result_sales_dt[,.N,.(yyyymm,region)]
head(region_cnts)

# 대표지역 추출
regions <- unique(region_cnts$region)
regions

# 각 지역별로 매매량의 랜덤성 검정 결과를 runs_p 변수에 추가
runs_p <- c()
for (reg in regions) {
  runs_p <- c(runs_p, runs.test(region_cnts[region %chin% reg,N])$p.value)
}
ggplot(data.table(regions, runs_p), 
       aes(x=regions, y=runs_p, group=1)) + 
  geom_line() + 
  geom_point() +
  ylab('P-value') + 
  xlab('지역')

# 시계열분할(서울지역)
seoul_cnts <- result_sales_dt[yyyymm != '201504' & 
                              region %chin% '서울',.N,.(yyyymm)]
tot_ts <- ts(seoul_cnts$N,start = c(2006,1), frequency = 12)
plot((stl(tot_ts, s.window = 'periodic')))

#부산지역
busan_cnts <- result_sales_dt[yyyymm != '201504' & 
                                region %chin% '부산',.N,.(yyyymm)]
tot_ts <- ts(busan_cnts$N,start = c(2006,1), frequency = 20)
plot((stl(tot_ts, s.window = 'periodic')))

arima_mdl <- auto.arima(tot_ts)
tsdiag(arima_mdl)

plot(forecast(arima_mdl,h=12))

# (보충) 통계분석 P-value
mpg <- as.data.frame(ggplot2::mpg)

library(dplyr)
mpg_diff <- mpg %>% 
  select(class, cty) %>%
  filter(class %in% c('compact', 'suv'))
head(mpg_diff)
str(mpg_diff)

table(mpg_diff$class)

t.test(data = mpg_diff, cty ~ class, var.equal = T)

# 일반 휘발유와 고급 휘발유의 가설 검정 (상관분석)

mpg_diff2 <- mpg %>% 
  select(fl, cty) %>% 
  filter(fl %in% c('r', 'p')) # r:regular p:premium

table(mpg_diff2$fl)

t.test(data = mpg_diff2, cty ~ fl, var.equal = T)

economics <- as.data.frame(ggplot2::economics)
cor.test(economics$unemploy, economics$pce)

# 상관행렬 히트맵
head(mtcars)

car_cor <- cor(mtcars) 
round(car_cor, 2) 

install.packages("corrplot")
library(corrplot)
# 히트맵 그리기
corrplot(car_cor)
# 숫자로 표현
corrplot(car_cor, method = 'number')

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(car_cor,
         method = "color", # 색깔로 표현
         col = col(200), # 색상 200 개 선정
         type = "lower", # 왼쪽 아래 행렬만 표시
         order = "hclust", # 유사한 상관계수끼리 군집화
         addCoef.col = "black", # 상관계수 색깔
         tl.col = "black", # 변수명 색깔
         tl.srt = 45, # 변수명 45 도 기울임
         diag = F) # 대각 행렬 제외
