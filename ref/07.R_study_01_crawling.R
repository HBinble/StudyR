## 보배드림 크롤링
install.packages('rvest')
library(rvest)

#내가 수집하길 원하는 페이지 주소 
url <- 'https://www.bobaedream.co.kr/cyber/CyberCar.php?gubun=K&page=1' 
usedCar <- read_html(url)  #해당 url 페이지의 html tag를 가져와서 parsing함. 
usedCar

#parsing한 usedCar 에서 css가 'carinfo' 인 것을 찾아라. 
carInfos <- html_nodes(usedCar, css='.mode-cell.title')
head(carInfos)

#가장 첫번째 cafInfos로부터 정보 추출해보면, 
carInfos[1] %>% html_nodes('.tit.ellipsis') %>% html_text()

titles <- carInfos %>% html_nodes('.tit.ellipsis') %>% html_text()
head(titles)
length(titles)

carInfos[[2]]
carMainInfo <- carInfos %>% html_nodes('.tit.ellipsis.video') %>% html_text()
carMainInfo <-  gsub(' ', '', carMainInfo)
carMainInfo <-  gsub('\r\n', '', carMainInfo)
carMainInfo <-  gsub('\t', '', carMainInfo)

str(carMainInfo)

carDetailInfo <- carInfos %>% html_nodes('.data.clearfix') %>% html_text()
carDetailInfo

carDetailInfo <- gsub(' ', '', carDetailInfo)
carDetailInfo <- gsub('\r\n', '', carDetailInfo)
carDetailInfo <- gsub('\t', '', carDetailInfo)
carDetailInfo


## 영화 감상평 리뷰 가져오기
# https://movie.daum.net/moviedb/main?movieId=43104
install.packages('KoNLP')
install.packages('tm')
install.packages('qgraph')

library(rvest)
library(httr)
library(KoNLP)
library(stringr)
library(tm)
library(qgraph)