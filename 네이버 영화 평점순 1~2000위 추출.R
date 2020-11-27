library(rvest)

#임시로 40개 페이지에서 각 페이지당 50개의 제목 / 코드를 담을 리스트
titlelist=vector(mode="list",length=40)
codelist=vector(mode="list",length=40)

baseurl <- 'https://movie.naver.com/movie/sdb/rank/rmovie.nhn?sel=pnt&date=20201124&page='

for(page in 1:40){
  url <- paste0(baseurl,page)
  html <- read_html(url, encoding = "euc-kr")
  
  ## 제목 추출과정
  title <- html%>%
    html_nodes("td")%>%
    html_nodes("a")%>%
    html_attr("title")
  title <- title[c(TRUE,FALSE)][1:50]
  titlelist[page] <- list(title)
  
  ## 코드 추출 과정
  tables <- html%>%
    html_nodes("td")%>%
    html_nodes("a")%>%
    html_attr("href")
  url2 <- tables[c(TRUE,FALSE)][1:50]
  list(substr(url2, nchar(url2)-5, nchar(url2)))
  codelist[page] <- list(substr(url2, nchar(url2)-5, nchar(url2)))
}

# 2000개의 영화제목을 'title'에 벡터형태로 담는다.
title <- unlist(titlelist)

# 2000개의 영화코드를 'codes'에 벡터형태로 담는다.
codes <- unlist(codelist)
codes <- gsub("=","",codes)     #5자리 영화코드의 '='을 제거해준다.


################### 2차과정. 나머지 25개의 요소 추출 ################

#for문으로 2000번 반복. 하나의 벡터에 25개*2000개의 정보를 전부 담을 것임.

baseurl <- 'https://movie.naver.com/movie/bi/mi/point.nhn?code='

NAs <- rep("NA", 30)
infolist <- NULL

for (i in 1:2000){
  url <- paste0(baseurl,codes[i])
  html <- read_html(url, encoding = "utf-8")
  
  ##### genre, country, runtime, release, director, actor, view_class 추출 과정 : info_spec1 #####
  
  tables <- html%>%
    html_nodes("dd")%>%
    html_nodes("p")%>%
    html_nodes("span")%>%
    html_text()
  
  if(length(tables)==0) { tables[1:4] <- NA } # 정보가 없는경우 NA부여
  
  # genre, country, runtime, release
  
  tables <- gsub("\\n|\\t|\\r","",tables)
  tables[4] <- gsub("\\r","",tables[4])
  genre <-tables[1]
  country <- tables[2]
  runtime <- tables[3]
  release <- tables[4]
  
  
  # director, actor, view_class
  
  tables <- html%>%
    html_nodes("dd")%>%
    html_nodes("p")%>%
    html_text()
  
  if(length(tables)==3){
    tables[4] <- tables[3]
    tables[3] <- NA
    }
  
  tables <- gsub("\\n|\\t|\\r","",tables)
  director <- tables[2]
  actor <- tables[3]
  view_class <- tables[4]
  
  if(is.na(release)){
    temp=view_class
    view_class=actor
    actor=temp
  }
  
  info_spec1 <- c(genre,country,runtime,release,director,actor,view_class)
  
  ##### netizen, audience 추출 과정 : info_spec2, info_spec3 #####
  
  # netizen, audience의 _count와 _score
  
  tables <- html%>%
    html_nodes("div")%>%
    html_nodes("div")%>%
    html_nodes("div")%>%
    html_nodes("em")%>%
    html_text()
  
  if(length(tables)<10){tables <- c(tables, NAs)}
  
  aud_count <- tables[16] #audience_count
  aud_score <- paste(tables[17:20], collapse="")   #audience_score
  
  #예외케이스들이 꽤 많다..
  
  if(tables[21] == "기자 · 평론가 평점"){
    ntz_count <- tables[28]                         #netizen_count
    ntz_score <- paste(tables[29:32], collapse="")  #netizen_score
  }else if(tables[22] == "기자 · 평론가 평점"){
    ntz_count <- tables[29]                         #netizen_count
    ntz_score <- paste(tables[30:33], collapse="")  #netizen_score
    
    aud_count <- tables[17]                         #audience_count
    aud_score <- paste(tables[18:21], collapse="")  #audience_score
  }else if(tables[17] == "네티즌 평점"){
    ntz_count <- tables[19]                         #netizen_count
    ntz_score <- paste(tables[20:23], collapse="")  #netizen_score
    aud_count <- NA                                 #audience_count
    aud_score <- NA                                 #audience_score
    
  }else if(tables[17] == "기자 · 평론가 평점"){
    ntz_count <- tables[24]                         #netizen_count
    ntz_score <- paste(tables[25:28], collapse="")  #netizen_score
    aud_count <- NA                                 #audience_count
    aud_score <- NA                                 #audience_score
  }else{
    ntz_count <- tables[23]                         #netizen_count
    ntz_score <- paste(tables[24:27], collapse="")  #netizen_score
  }
  
  # netizen, audience의 성별 평점, 연령별 평점
  
  score <- html%>%
    html_nodes("div")%>%
    html_nodes("div")%>%
    html_nodes("div")%>%
    html_nodes("div")%>%
    html_nodes("div")%>%
    html_nodes("div")%>%
    html_nodes("div")%>%
    html_nodes("strong")%>%
    html_text()
  
  if(length(score)<10){score <- c(score, NAs)}
  
  #예외케이스
  
  if(score[1]=="네티즌 평점 도움말")
  {
    netizen <- score[5:18]
    audience <- NA
  }else if(score[12]=="네티즌 평점 도움말")
  {
    netizen <- score[26:39]
    audience <- score[48:61]
  }else{
    netizen <- score[25:38]
    audience <- score[47:60]
  }
  
  n_sex <- netizen[c(1,3)]  # netizen_성별 평점
  n_age <- netizen[5:14]    # netizen_연령별 평점
  n_age <- n_age[c(FALSE,TRUE)]
  netizen <- c(n_sex,n_age) #netizen_성별 + 연령별 평점
  
  a_sex <- audience[c(1,3)] # audience_성별 평점
  a_age <- audience[5:14]   # audience_연령별 평점
  a_age <- a_age[c(FALSE,TRUE)]
  audience <- c(a_sex, a_age) #audience_성별 + 연령별 평점
  
  
  info_spec2 <- c(ntz_score, ntz_count, netizen)  #netizen 9개요소
  info_spec2 <- gsub("NANANA",NA,info_spec2) 
  
  info_spec3 <- c(aud_score, aud_count, audience) #audience 9개요소
  info_spec3 <- gsub("NANANA",NA,info_spec3) 
  
  info <- c(info_spec1, info_spec2, info_spec3)
  infolist <- c(infolist, info) # 모든 정보를 누적해서 infolist에 담는다.
}

infomatrix <- matrix(infolist, ncol=25, byrow=TRUE) # 모든 정보가 담긴 infolist를 매트릭스로 만든다.

movie <- data.frame(cbind(unlist(titlelist),unlist(codelist),infomatrix)) # 최종적으로 데이터프레임으로 바꾸어준다.

col <- c("title", "code", "genre", "country", "runtime", "release", 
         "director", "actor", "view_class", 
         "netizen_score", "netizen_count", "ntz_male", "ntz_female", 
         "ntz_10", "ntz_20", "ntz_30", "ntz_40", "ntz_50",
         "audience_score", "audience_count", "audience_male", "audience_female", 
         "audience_10",	"audience_20",	"audience_30", "audience_40", "audience_50")
colnames(movie) <- col

write.csv(movie, file='movie2.csv') # 만들어진 데이터를 csv파일로 저장!!