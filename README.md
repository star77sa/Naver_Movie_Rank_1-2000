# Naver_Movie_Rank_1-2000

#### 네이버 영화 사이트에서, 평점별로 나열된(상영 끝난 영화 포함)영화를 1위부터 2000위 까지 추출하는 코드이다.

>link : https://movie.naver.com/movie/sdb/rank/rmovie.nhn?sel=pnt&date=20201126

#### 추출하는 정보 (27개 요소)
>제목, 영화의 코드번호, 제작 국가, 상영시간, 개봉일자, 감독, 출연배우, 상영 등급,

>네티즌의 성별·연령별 평점, 관람객의 성별·연령별 평점

#### 코드 실행 후 저장된 csv파일
><img src="https://user-images.githubusercontent.com/73769046/100460656-e7017b00-310a-11eb-8d27-bd550547bc65.PNG" width="700" height="370">


### 진행과정

##### 2020-11-25

>movie code 추출과정에서 5자리와 6자리의 코드가 존재하여 구분하여 추출하려했다가, 6자리를 추출한 뒤 gsub함수를 이용하여 텍스트처리.

>movie code와 title 추출 완료

>movie code, title 2000개 요소는 각각 벡터로 놔둔 뒤, 25개 요소 추출 뒤에 합치자.

##### 2020-11-26

>인코딩 오류가 발생했다. 홈페이지마다 encoding = "euc-kr"이 되는 곳도 있고, 아닌 곳도 있는듯

>encoding = "utf-8"로 바꾸어서 해결했다.

>25개 요소도 각각 2000개씩 따로 뽑아서 합쳐야 하나 고민하다가, 25개 요소는 바로 matrix로 만들고, 앞에서 구한 title과 code를 후에 합치기로 함.

>25개 요소 전부 추출을 완료하였으나, 영화별로 html코드 구성이 달라서 codes[1]에서 되던게 codes[12]에서는 이상하게 나오는 등 문제가 생김

>범용적으로 쓸 수 있도록 수정하자

##### 2020-11-27

>if문을 이용하며 다양한 예외처리들을 했다. 조금 비효율적인 측면이 있어서 더 범용적으로 적용되는 코드를 고민해봐야겠다.

>추출된 정보들을 어떻게 합칠까 고민했는데, 처음엔 rbind로 매번 1행씩 추가하였더니 반복을 2000번이나 하다보니 시간이 엄청나게 들었다.

>그래서 'infolist'라는 하나의 벡터에 codes[1]의 정보, code[2]의 정보, ... , code[2000]의 정보를 모두 담은 뒤에 

>matrix(infolist, ncol=25, byrow=TRUE)를 사용하여 한 행에 25개의 정보를 담은 행렬로 변경시켜주었다. (2000x25)

>그 뒤에, 앞전에 구한 title, code정보를 위의 행렬과 합쳐 데이터프레임으로 변경시킨 뒤, write.csv를 써서 csv파일로 저장시켜주었다.

>조금 더 시간을 단축시킬 수 있는 코드를 고민해보자!


#### 코드 실행 후 저장된 csv파일
><img src="https://user-images.githubusercontent.com/73769046/100460656-e7017b00-310a-11eb-8d27-bd550547bc65.PNG" width="700" height="370">

><img src="https://user-images.githubusercontent.com/73769046/101238072-0ad34b00-3721-11eb-85a4-43ce73cba0d5.JPG" width="700" height="370">
