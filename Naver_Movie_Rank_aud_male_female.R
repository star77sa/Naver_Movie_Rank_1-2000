# 준비단계

library(rvest)
library(RSelenium) 
shell("docker run -d -p 4445:4444 selenium/standalone-chrome")
remDr <- remoteDriver(remoteServerAddr = 'localhost', port = 4445, browserName = "chrome") 
remDr$open()


# 영화 제목과 코드 2,000개 추출

get_code <- function(date, page){
  base_url <- 'https://movie.naver.com/movie/sdb/rank/rmovie.nhn?sel=pnt&date='
  url <- paste0(base_url, date, "&page=", page)
  html <- read_html(url, encoding="euc-kr")
  
  href <- html %>%
    html_node("table") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  title <- html %>%
    html_node("table") %>%  
    html_nodes("a") %>%
    html_text()
  
  strs <- href[grep("\\?code=", href)]
  splited_str <- unlist(strsplit(strs, "="))
  code <- splited_str[c(FALSE, TRUE)]
  title <- title[c(TRUE, FALSE)]
  df <- data.frame(title=title, code=code)
  return(df)
}

date <- "20201124"
page <- 1:40
movie_code <- vector("list", length=length(page))

for(i in page) {
  movie_code[[i]] <- get_code(date, i)
}

movie_df <- do.call("rbind", movie_code)


# audience male / female 추출

get_detail <- function(title, code){
  base_url <- "https://movie.naver.com/movie/bi/mi/point.nhn?code="
  url <- paste0(base_url, code)
  
  remDr$navigate(url)
  remDr$getCurrentUrl()
  source <- remDr$getPageSource()[[1]]
  html <- read_html(source)
  percent <- html %>%
    html_nodes("tspan") %>%
    html_text()
  
  if(length(percent)<6){ # 정보가 없는경우. NA부여
    audience_male <- NA
    audience_female <- NA
  }else{
    audience_male <- percent[1]
    audience_female <- percent[2]
  }
  
  df <- data.frame(
    title <- title,
    code <- code,
    audience_male <- audience_male,
    audience_female <- audience_female
  )
}

detail <- vector("list", length=nrow(movie_df))

for(i in 1:nrow(movie_df)) {
  detail[[i]] <- get_detail(movie_df$title[i], movie_df$code[i])
}

detail_df <- do.call("rbind", detail)
colnames(detail_df) <- c("title","code","audience_male", "audience_female")

write.csv(detail_df, "c:/audience_gender_kks.csv")