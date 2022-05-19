## Oracle 연결
# 한글해결 연결 DBMSencoding = 'euc-kr', 'utf-8' 
# 1.RODBC 패키지 설치
# RJava, RJdbc, ROracle 등
install.packages('RODBC') # ROracle Open DataBase Connectivity(MS) (# Java DB Connectivity)
# 2.Library 연결
library(RODBC)

# 3.ODBC 데이터원본(64비트) 설정
conn1 = odbcConnect('SCOTT_DSN', uid = 'SCOTT', pwd = 'tiger', 
                    DBMSencoding = 'euc-kr',
                    believeNRows = F)

# 4.연결확인
summary(conn1)

# 5.쿼리실행
res <- sqlQuery(conn1,'SELECT * FROM dept')
str(res)

sqlQuery(conn1,'SELECT * FROM emp')

res <- sqlQuery(conn1,'SELECT * FROM memberTBL')

# 6.접속 종료 Close
odbcClose(conn1)
conn1
