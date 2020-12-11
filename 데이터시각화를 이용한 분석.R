movie <- read.csv("C:\\movie.csv",header = T)
audience_gender <- read.csv("C:\\audience_gender.csv",header = T)

### 두 자료의 순위가 달라, 영화 코드를 기준으로 병합한다. ###

# 두 자료를 영화 코드 순으로 정렬
movie <- movie[order(movie[3]),] 
audience_gender <- audience_gender[order(audience_gender[3]),]

# 두 자료를 병합
movie <- data.frame(movie, audience_gender[4][,1], audience_gender[5][,1])

#다시 영화 순위 순으로 정렬
movie <- movie[order(movie[1]),]

# factor값을 분석에 이용하기 위해 numeric로 바꾸는 함수
Trans_num <- function(i){
  x <- movie[i][,1]
  x <- as.character(x)
  x <- gsub( "%", "", x)
  x <- as.numeric(x)
  return(x)
}

# factor를 numeric으로 변환
for(i in c(11:15 ,34:35)){
  movie[i][,1] <- Trans_num(i)
}


### 1. 높은 평점 요인 분석 ###


## 1-(1) 장르별 분포 분석 ##
# 드라마/ 멜로 / 모험 / 스릴러 / 애니메이션 / 코미디 / 전쟁

drama <- length(grep("드라마",movie[4][,1])) 
melo <- length(grep("멜로",movie[4][,1])) 
advanture <- length(grep("모험",movie[4][,1])) 
thriller <- length(grep("스릴러",movie[4][,1]))
animation <- length(grep("애니메이션",movie[4][,1])) 
comedy <- length(grep("코미디",movie[4][,1])) 
war <- length(grep("전쟁",movie[4][,1])) 

Genre <- c(drama, melo, advanture, thriller, animation, comedy, war)

name <- c("drama", "melo", "advanture", "thriller", "ani", "comedy", "war")
barplot(Genre,col=ifelse(Genre>500,"red","grey"),xlab="genre", names.arg=name, main="Genre frequency")
# 평점순으로 나열한 2000개의 영화중 1096개의 영화가 드라마이다.


## 1-(2) 관람객 연령별 분포 ##

aud_10 <- mean(movie[11][,1], na.rm = T)
aud_20 <- mean(movie[12][,1], na.rm = T)
aud_30 <- mean(movie[13][,1], na.rm = T)
aud_40 <- mean(movie[14][,1], na.rm = T)
aud_50 <- mean(movie[15][,1], na.rm = T)

argument <- c(aud_10,aud_20,aud_30,aud_40,aud_50)
name <- c("aud_10","aud_20","aud_30","aud_40","aud_50")

barplot(argument, names.arg=name, col=ifelse(argument>20,"red","grey"), main="Ages frequency")
# 20,30대가 많이 보는 것을 알 수 있다.


## 1-(3)관람 등급별 분포 분석 ##

v_all <- length(grep("전체",movie[10][,1])) /2000
v_12 <- length(grep("12세",movie[10][,1])) /2000
v_15 <- length(grep("15세",movie[10][,1])) /2000
v_19 <- length(grep("청소년",movie[10][,1])) /2000

argument <- c(v_all,v_12,v_15,v_19)
name <- c("v_all","v_12","v_15","v_19")
barplot(argument, names.arg=name, xlab="연령",ylab="빈도수", col=ifelse(argument<0.15,"red","grey"), main="View Class frequency")
# 12세, 15세 이용가의 빈도가 높음을 알 수 있다.


## 1-(4) 개봉한 달에 따른 분포 ##

release <- gsub(" ","",movie[7][,1]) # 공백 제거
release <- substr(release, 6, 7) # month 추출

mean(table(release))

plot(table(release),col=ifelse(table(release)>164.25,"red","grey"),xlab="month", ylab="frequency",main="frequency per month")
abline(h=164.25,lty=2)

max(table(release))/min(table(release))
# 개봉한 달에 따라서, 영화의 수가 1,63배까지 차이가 난다 ( 12월과 6월 )
# 6월에 비해서 12월에 영화를 내는 것이 평점이 높을 가능성이 높을 것이다.



### 2. 재개봉한 영화의 국적 분석 ###

re <- grep("재개봉",movie[7][,1])
country <- (movie[5][re,1])

# 2000위 안에 드는 한국, 미국, 일본 영화
KR <- length(grep("한국",movie[5][,1])) / 2000
US <- length(grep("미국",movie[5][,1])) / 2000
JP <- length(grep("일본",movie[5][,1])) / 2000

# 재개봉한 한국, 미국, 일본 영화
re.KR <- length(grep("한국",country))/length(country)
re.US <- length(grep("미국",country))/length(country)
re.JP <- length(grep("일본",country))/length(country)

v1 <- c(KR, re.KR)
v2 <- c(US, re.US)
v3 <- c(JP, re.JP)
qty <- data.frame(KR=v1, US=v2, JP=v3)

barplot(as.matrix(qty),main="Release vs Re-Release", beside=T, ylim=c(0,1),col=rainbow(nrow(qty)))
legend(6.3,1, c("Release","Re-Release"), cex=0.8, fill=rainbow(nrow(qty)))
#재개봉은 해외영화를 위주로 함을 알 수 있다.