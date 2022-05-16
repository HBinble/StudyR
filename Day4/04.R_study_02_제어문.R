## 제어문
# if문
job.type <- 'A'

if (job.type == 'B') {
  bouns <- 200
} else {
  bonus <- 100
}

print(bonus)


a <- 10
b <- 20
if (a>5 & b>5) { # and &
  print(a+b)
}
if (a>5 | b>30) { # or |
  print(a*b)
}


a <- 10
b <- 20
if (a>b) {
  c <- a
} else {
  c <- b
}
print(c)

a <- 10
b <- 20
c <- ifelse(a>b, a, b)
print(c)
c

## 반복문(for문)
for(i in 1:5) {
  cat(i, '')
}
cat('\n')

for(y in 1:9) {
  cat('2 x', y,'=', 2*y,'\n')
}

## 구구단 - 파이썬과 비교(1:10(py) / 1:9(R))
for(x in 2:9) {
  for(y in 1:9) {
    cat(x,'x', y,'=', x*y,'\n')
  }
  cat('\n')
}

for (i in 1:20) {
  if(i %% 2 == 0) {
    cat(i,'')
  }
}

## iris 꽃잎 크기 분류
norow <- nrow(iris) # iris의 행의 수
norow
mylabel <- c() # 비어있는 벡터 선언
for(i in 1:norow) {
  if (iris$Petal.Length[i] <= 1.6) {
    mylabel[i] <- 'L'
  } else if (iris$Petal.Length[i] >= 5.1) {
    mylabel[i] <- 'H'
  } else {
    mylabel[i] <- 'M'
  }
}
print(mylabel) # 레이블 출력
str(mylabel)
newds <- data.frame(iris$Petal.Length, mylabel, iris$Species)
newds

write.csv(newds, './Day4/iris_petal.csv', row.names = F)

## 반복문(while문)
sum <- 0
i <- 1
while(i <= 100) {
  sum <- sum + i 
  i <- i + 1 
}
print(sum)

## 사용자 정의 함수

mymax <- function(x, y) {
  num.max <- x
  if (y > x) {
    num.max <- y
  }
  return(num.max)
}

max(1, 2)
mymax(1, 3)
max(1, 3, 5, 6, 7)
mymax(1, 3, 5, 8 ,9)

mymax(10,15)
a <- mymax(20,15)
b <- mymax(31,45)
print(a+b)

mydiv <- function(x, y=2) {
  result <- x/y
  return(result)
}
mydiv(10,3) 
mydiv(10) 

myfunc <- function(x,y) {
  val.sum <- x+y
  val.mul <- x*y
  return(list(sum=val.sum, mul=val.mul)) 
}
result <- myfunc(14,5)
s <- result$sum 
m <- result$mul 
cat('14+5 =', s, '\n')
cat('14*5 =', m, '\n') 

## 내장함수 apply() [apply(데이터셋, 행/열 방향 지정, 적용 함수)]
apply(iris[,1:4], 2, mean)
apply(iris[,1:4], 2, mode)
apply(iris[,1:4], 2, sum)
apply(iris[,1:4], 2, median)
apply(iris[,1:4], 2, max)

