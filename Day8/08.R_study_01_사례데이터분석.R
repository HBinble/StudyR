## 포브스 선정
install.packages('HSAUR')
library(HSAUR)
data('Forbes2000')
ds <- Forbes2000
ds[!complete.cases(ds),] # 결측값 확인

str(ds)

# 국가별 기업 통계
table(ds$country)
tmp <- sort(table(ds$country), decreasing=T)
top.10.contry <- tmp[1:10]
top.10.contry 
par(mar=c(8,4,4,2)) 
barplot(top.10.contry,
        main='기업수 상위 10개국',
        col=rainbow(10), 
        las=2)
par(mar=c(5,4,4,2))

# 업종별 기업 분포
table(ds$category)
tmp <- sort(table(ds$category), decreasing=T)
top.10.category <- tmp[1:10]
top.10.category 
par(mar=c(10,4,4,2))
barplot(top.10.category,
        main='기업수 상위 10개 업종',
        col='pink',
        las=2)
par(mar=c(5,4,4,2))

# 업종별 기업자산 분포
tmp <- ds[ds$category %in% names(top.10.category),]
levels(tmp$category)

tmp$category <- factor(tmp$category)
levels(tmp$category)

par(mar=c(10,4,4,2))
boxplot(assets~category, data = tmp,
        ylim = c(0,100),
        xlab = ' ',
        las=2
        )
par(mar=c(5,4,4,2))

# 기업 가치 상위 10대 기업
tmp <- ds[order(ds$marketvalue, decreasing = T),]
head(tmp$marketvalue)
tmp[1:10, c('name','country', 'category','marketvalue')]

# 한국 기업 정보
korea <- subset(ds, country=='South Korea')
str(korea)
korea[,c('rank', 'name', 'category', 'marketvalue')]

# 기업 가치와 타 변수와의 상관관계
tmp <- ds[,5:8]
str(tmp)
tmp <- tmp[complete.cases(tmp),] # 결측값 제거
plot(tmp, lower.panel=NULL) # 산점도
cor(tmp) 

## 대기오염 측정 데이터
getwd()
setwd('./Day8./data')
getwd()
files <- c('ds.2015.csv','ds.2016.csv','ds.2017.csv',
           'ds.2018.csv','ds.2019.csv')
ds <- NULL
ds
for (f in files) {
  tmp <- read.csv(f, header = T)
  ds <- rbind(ds, tmp)
  print(f)
}

str(ds)
unique(ds$loc)
unique(ds$mdate)
tail(ds$mdate)
range(ds$mdate)

# 열별 결측값 확인
for (i in 3:8) {
  cat(names(ds)[i], sum(is.na(ds[,i])), 
      sum(is.na(ds[,i]))/nrow(ds), '\n')
}

ds <- ds[,-8]
ds <- ds[complete.cases(ds),]
ds
str(ds)

# 그룹 정보 추가
mdate <- as.character(ds$mdate)
head(mdate)
ds$year <- as.numeric(substr(mdate, 1,4)) # 연도
ds$month <- as.numeric(substr(mdate, 5,6)) # 월
ds$hour <- as.numeric(substr(mdate, 9,10)) # 시간
ds$locname <- NA
ds$locname[ds$loc==111123] <- '서울' #도시
ds$locname[ds$loc==336111] <- '목포' #도시
ds$locname[ds$loc==632132] <- '강릉' #도시

head(ds)
str(ds)

# 세부설명
# loc의 코드 확인
unique(ds$loc)
# locname 열 추가
ds$locname <- NA
# loc에서 코드에 맞게끔 locname을 한글로 저장
ds$locname[ds$loc==111123] <- '서울' 
ds$locname[ds$loc==336111] <- '목포' 
ds$locname[ds$loc==632132] <- '강릉' 
unique(ds$locname)

boxplot(PM10~locname, data = ds,
        main = '미세먼지 농도 분포',
        ylim = c(1,100))

# 연도별, 지역별 PM10 농도 추이
library(ggplot2)
tmp.year <- aggregate(ds[,7], 
                      by=list(year=ds$year,loc=ds$locname), FUN='mean')
tmp.year$loc = as.factor(tmp.year$loc)
head(tmp.year)
ggplot(tmp.year, aes(x=year,y=x, colour=loc, group=loc))+
  geom_line( )+
  geom_point(size=6, shape=19, alpha=0.5)+
  ggtitle('연도별 PM10 농도 변화') + 
  labs(x = '연도', y = '농도') + 
  theme(plot.title = 
          element_text(size=16, 
                       face='bold',
                       color = 'darkblue',
                       hjust = 0.5),
        legend.position = 'bottom')

# 연도별, 지역별 PM10 농도 추이
library(ggplot2)
tmp.year <- aggregate(ds[,7], 
                      by=list(year=ds$year,loc=ds$locname), FUN='mean')
tmp.year$loc = as.factor(tmp.year$loc)
head(tmp.year)
ggplot(tmp.year, aes(x=year,y=x, colour=loc, group=loc))+
  geom_line( )+
  geom_point(size=6, shape=19, alpha=0.5)+
  ggtitle('연도별 PM10 농도 변화') + 
  labs(x = '연도', y = '농도') + 
  theme(plot.title = 
          element_text(size=16, 
                       face='bold',
                       color = 'darkblue',
                       hjust = 0.5),
        legend.position = 'bottom')

# 월별
tmp.month <- aggregate(ds[,7], 
                       by=list(month=ds$month,loc=ds$locname),
                       FUN='mean')
tmp.month$loc = as.factor(tmp.month$loc)
head(tmp.month)
ggplot(tmp.month, aes(x=month,y=x, colour=loc, group=loc))+
  geom_line( )+
  geom_point(size=3, shape=19, alpha=0.5)+
  ggtitle('월별 PM10 농도 변화')+
 ylab('농도')

# 시간대별
tmp.hour <- aggregate(ds[,7], 
                      by=list(hour=ds$hour,loc=ds$locname), FUN='mean')
tmp.hour$loc = as.factor(tmp.hour$loc)
head(tmp.hour)
ggplot(tmp.hour, aes(x=hour,y=x, colour=loc, group=loc))+
  geom_line( )+
  geom_point(size=3, shape=19, alpha=0.5)+
  ggtitle('시간별 PM10 농도 변화')+
 ylab('농도')

# 농도 상관분석
set.seed(1234)
plot(ds[sample(nrow(ds),5000),3:7], lower.panel=NULL)
cor(ds[,3:7])

# 미세먼지 최고점과 최저점
tmp.yml <- aggregate(ds[,7], 
                     by=list(year=ds$year,month=ds$month,
                             loc=ds$locname), FUN='mean')
# 가장 미세먼지가 많았던 달
idx <- which(tmp.yml$x==max(tmp.yml$x))
tmp.yml[idx,]
# 가장 미세먼지가 적었던 달
idx <- which(tmp.yml$x==min(tmp.yml$x))
tmp.yml[idx,]

