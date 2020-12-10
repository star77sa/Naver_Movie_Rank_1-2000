movie <- read.csv("C:\\movie.csv",header = T)
audience_gender <- read.csv("C:\\audience_gender.csv",header = T)

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
release <- substr(release, 6, 7) # 개월만 추출
release <- as.integer(release) # string 을 int로 변경

movie[7] <- release # movie[7] = release
#16
#compare <- movie[,c(7,16)]
#pairs(compare, panel=panel.smooth)



## 
view_class <- substr(view_class, 5, 6)
movie[10] <- view_class # movie[10] = 

청소년 <- na.omit(movie[which(movie[10]=="청소"),])
attach(청소년)
summary(청소년)


mean(as.integer(unlist(청소년[34])))
mean(as.integer(unlist(청소년[35])))
length(unlist(청소년[35]))


audience_male

as.integer(unlist(청소년[34])) > as.integer(unlist(청소년[35]))
## 청소년 관람불가 영화가 남자가 여자보다 더 많은 영화 갯수




plot(view_class, unlist(movie[34]))
length(view_class)
length(movie[34])


# viewclass = 10

# 5 6 
a <- "abcde"
a <- substr(a,2,3)
