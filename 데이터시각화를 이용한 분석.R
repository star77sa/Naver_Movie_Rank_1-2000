movie <- read.csv("C:\\movie.csv",header = T)
audience_gender <- read.csv("C:\\audience_gender.csv",header = T)

movie[3]

### 두 자료의 순위가 달라, 영화 코드를 기준으로 병합한다.

# 두 자료를 코드 순으로 정렬
movie <- movie[order(movie[3]),] 
audience_gender <- audience_gender[order(audience_gender[3]),]

audience_male <- audience_gender[4]
audience_female <- audience_gender[5]

movie <- cbind(movie, audience_male, audience_female)

# 다시 순위 순으로 정렬
movie <- movie[order(movie[1]),]



attach(movie)

#개봉한 달에 따른(계절에 따른) 네티즌 스코어 비교?

release <- gsub(" ","",release) # 공백제거
release <- substr(release, 6, 7) # 개월만 추출출
release <- as.integer(release) # string 을 int로 변경

movie[7] <- release
#16
compare <- movie[,c(7,16)]
pairs(compare, panel=panel.smooth)




view_class <- 





mean(as.integer(na.omit(unlist(movie[16]))))


# 5 6 
a <- "abcde"
a <- substr(a,2,3)
