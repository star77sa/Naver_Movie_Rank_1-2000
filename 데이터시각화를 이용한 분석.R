movie <- read.csv("C:\\movie.csv",header = T)
audience_gender <- read.csv("C:\\audience_gender.csv",header = T)

### 두 자료의 순위가 달라, 영화 코드를 기준으로 병합한다.

# 두 자료를 코드 순으로 정렬
movie <- movie[order(movie[3]),] 
audience_gender <- audience_gender[order(audience_gender[3]),]

# 두 자료를 병합
movie <- data.frame(movie, audience_gender[4][,1], audience_gender[5][,1])

#다시 순위 순으로 정렬
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



#attach(movie)

##### 1. 개봉한 달에 따라서

#개봉한 달에 따른(계절에 따른)
release <- gsub(" ","",movie[7][,1]) # 공백제거
release <- substr(release, 6, 7) # 개월만 추출

# 분석시작
mean(table(release))
median(table(release))
plot(table(release),col=ifelse(table(release)>164.25,"blue","red"))
abline(h=164.25)

max(table(release))/min(table(release))
# 알수 있는 점 : 개봉한 달에 따라서, 영화의 수가 1.63배 까지 차이가 난다 ( 12월과 6월 )
# 12월의 경우엔 연말이라 많은 사람들이 영화를 보는것으로 예상이 된다.
# 6월에 비해서 12월에 영화를 내는 것이 흥행할 가능성이 높을 것이다.
# 그런데 평점이 높다고 흥행은 아니니... 좀 괜찮게 글 써보자. 


####### 장르분석
# 장르종류 : 드라마, SF, 판타지, 가족, 스릴러, 전쟁, 애니메이션, 코미디, 모험, 범죄, 미스터리,
# 공연실황, 멜로, 뮤지컬, 다큐멘터리, 
# 이렇게 다양한 장르가 있지만, 겹치는 장르는 제외하고 큰 장르로 나눠보면
# 드라마(가족) / 멜로 / 모험(SF,판타지) / 스릴러(범죄,미스터리) / 애니메이션 / 코미디(가족) / 전쟁
# 이렇게 6개 장르의 빈도수를 한번 살펴보겠다.

#subset(movie,movie[4][,1]%in%c("드라")) 
#?subset


drama <- length(grep("드라마",movie[4][,1])) 
melo <- length(grep("멜로",movie[4][,1])) 
advanture <- length(grep("모험",movie[4][,1])) 
thriller <- length(grep("스릴러",movie[4][,1]))
animation <- length(grep("애니메이션",movie[4][,1])) 
comedy <- length(grep("코미디",movie[4][,1])) 
war <- length(grep("전쟁",movie[4][,1])) 

Genre <- c(drama, melo, advanture, thriller, animation, comedy, war)
plot(Genre)  # plot 타입 바꿔주자

####################################################################

# 평점순으로 나열한 2000개의 영화중 1096개의 영화가 드라마이다.
# 그렇다면 이 드라마의 주요 관람층은 누구인가?
 
mean(movie[35][,1], na.rm=T)
mean(movie[35][grep("드라마",movie[4][,1]),1], na.rm=T)
## 남여차이는 딱히 없다. 그렇다면 연령별은?

#11 : 10대 , ~ 


aud_10 <- mean(movie[11][,1], na.rm = T)
aud_20 <- mean(movie[12][,1], na.rm = T)
aud_30 <- mean(movie[13][,1], na.rm = T)
aud_40 <- mean(movie[14][,1], na.rm = T)
aud_50 <- mean(movie[15][,1], na.rm = T)

plot(c(aud_10,aud_20,aud_30,aud_40,aud_50))
# 전체는 20,30대가 많이 보는 것을 알 수 있다...
# 드라마를 특정 성별, 나이대가 좋아하는지 비교해보았으나 전체장르와 유의미한 차이를 보이지는 않았다.



######## 유의미한 결과를 얻지 못한다... 드라마 장르랑 뭐랑 엮지 ? ..





######### 연령분석!!!!!!!!!!

v_all <- length(grep("전체",movie[10][,1])) 
v_12 <- length(grep("12세",movie[10][,1])) 
v_15 <- length(grep("15세",movie[10][,1])) 
v_19 <- length(grep("청소년",movie[10][,1]))


plot(c(v_all,v_12,v_15,v_19),xlab="연령가",ylab="빈도수")

v_all <- length((grep("전체",movie[10][,1])))

v_12 <- (grep("12세",movie[10][,1])) 
v_15 <- (grep("15세",movie[10][,1])) 
v_19 <- (grep("청소년",movie[10][,1]))

hist(c(v_all,v_12,v_15,v_19),xlab="연령가",ylab="빈도수")

########## 이거 그래프에 잘 나타나게 하는거 ...
#
#200 ㅣ
#100 ㅣ
#  0 ㅣ_ _ _ _ _ _ _ _ _
#     12세 15세 청소년
# 요런식으러 나오게 하는거 알아보기.
################################################# 12.11 대충 쓸만한거 분석한곳
summary(movie)

str(movie[7][,1])


plot(factor(movie[7][,1]), audience_score)

plot(factor(movie[7][,1]), movie[34][,1]) ## 개봉월별 남자관객 수
abline(h=50)


# movie[7] <- release # movie[7]= release

#16
#compare <- movie[,c(7,16)]
#pairs(compare, panel=panel.smooth)



## 
movie[10][,1] <- substr(movie[10][,1], 5, 6)
adult <- na.omit(movie[which(movie[10]=="청소"),])



plot(factor(movie[7][,1]), movie[34][,1]) ## 전체 영화, 개봉월별 남자관객 수
abline(h=50)
plot(factor(adult[7][,1]), adult[34][,1]) ## 청소년관람불가일때, 개봉월별 남자관객 수
abline(h=50)

### 네티즌 숫자.17 관람객숫자 26#
aud_Count <- movie[26][,1]
aud_Count <- as.character(aud_Count)
aud_Count <- as.numeric(aud_Count)

plot(factor(movie[7][,1]),aud_Count)




plot(factor(movie[7][,1]), movie[34][,1]) ## 개봉월별 



summary(adult)

##################### 17:네티즌카운트 10:뷰클래스



sd(na.omit(movie[34][,1]))

   