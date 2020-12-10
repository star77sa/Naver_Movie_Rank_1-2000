movie <- read.csv("C:\\movie.csv",header = T)
audience_gender <- read.csv("C:\\audience_gender.csv",header = T)

### 두 자료의 순위가 달라, 영화 코드를 기준으로 병합한다.

# 두 자료를 코드 순으로 정렬
movie <- movie[order(movie[3]),] 
audience_gender <- audience_gender[order(audience_gender[3]),]

# 남성관객, 여성관객수의 퍼센트를 이용하기 위해 factor -> integer으로 변환
male_percent <- vector(mode="integer", length=2000)
female_percent <- vector(mode="integer", length=2000)
for(i in 1:2000){
  male_percent[i] <- as.integer(substr(audience_gender[4][i,1], 1, 2))
  female_percent[i] <- as.integer(substr(audience_gender[5][i,1], 1, 2))
}

movie <- data.frame(movie, male_percent, female_percent)

# 다시 순위 순으로 정렬
movie <- movie[order(movie[1]),]

attach(movie)

######개봉한 달에 따른(계절에 따른) 네티즌 스코어 비교?
release <- gsub(" ","",movie[7][,1]) # 공백제거
release <- substr(release, 6, 7) # 개월만 추출
movie[7][,1] <- as.integer(release) # string 을 int로 변경


plot(factor(movie[7][,1]), audience_score)

# movie[7] <- release # movie[7]= release

#16
#compare <- movie[,c(7,16)]
#pairs(compare, panel=panel.smooth)



## 
movie[10][,1] <- substr(movie[10][,1], 5, 6)
adult <- na.omit(movie[which(movie[10]=="청소"),])

plot(factor(adult[7][,1]), adult[35][,1]) ## 청소년관람불가일때, 개봉월별 남자관객 수




summary(adult)





