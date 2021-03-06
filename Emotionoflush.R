library(stringr)
library(wordcloud)
library(data.table)
library(wordcloud2)
library(RSelenium)
library(rvest)
library(httr) #HTML처리
library(stringr)
library(dplyr)
#크롤링
ch=wdman::chrome(port=4567L)
remDr=remoteDriver(port=4567L, browserName='chrome')
remDr$open()
remDr$navigate("html-code")
All_review=c()

for(i in 1:100) {  #페이지 수만큼 반복하여 review긁어오기 // 각페이지의 리뷰 수는 10개이다.
  a=i #페이지 번호
  first="javascript:goAjaxPage('page=" #i를 변수로 받기 위해서 문장을 분해 i자리 앞쪽
  secon="&goodsNo=97&activeSno=&sort=&_=1620905746789')" #i자리 뒤쪽
  script=paste(first,a,secon,sep='') # a는 자동적으로 i를 받아 변하고 paste를 통해 한문장이 됨.
  pagemove <- remDr$executeScript(script, args = 1:2) #args 요 부분 의미는 잘 모르겠음.
  source<-remDr$getPageSource()[[1]] #페이지 소스 가져오기
  main <- read_html(source, encoding = "UTF-8")
  mainfo<-html_nodes(main ,css ='.text-container')
  
  
  review=mainfo%>%html_text() #텍스트만 가져오기
  All_review=c(All_review,review) #텍스트 저장
}
All_review


reviews <- gsub("더보기", " ", All_review)
reviews <- gsub("\n\t\t\t\t(.*)\n\t\t\t", "", reviews)
reviews <- gsub("닫기", " ", reviews)
#reviews <- gsub("추천", " ", reviews)
#reviews <- gsub("너무", " ", reviews)
reviews <- gsub("피부가", " ", reviews)
reviews <- gsub("js-pr-contents-desc img", "", reviews)
reviews <- gsub("max-width:80%", "", reviews)
reviews <- gsub("\t|\n", "", reviews) 
reviews <- gsub("              ", " ", reviews) 
reviews <- gsub("<U+00A0>", " ", reviews) 
reviews <- gsub("배송", " ", reviews) 
reviews <- gsub("빠르고", " ", reviews) 
reviews <- gsub("       "," ",reviews)
reviews <- gsub("."," ",reviews,fixed=TRUE)
#reviews <- gsub("!","",reviews,fixed=TRUE)
#reviews <- gsub("?","",reviews,fixed=TRUE)

reviews

remDr$close

#분석부분
pos.dic <- read.table("pos_pol_word.txt", header=TRUE, stringsAsFactors=FALSE)
head(pos.dic)
neg.dic <- read.table("neg_pol_word.txt", header=TRUE, stringsAsFactors=FALSE)
head(neg.dic)



predict <- NULL
m <- length(reviews)

for(i in 1:m) {
  if (i %% 100 == 0) {
    txt <- paste(i, "/", m)
    print(txt)
  } 
  
  words <- str_split(reviews[i],'\\s+') 
  words <- unlist(words)
  words <- gsub("[^A-Za-z0-9ㄱ-힣]","",words) 
  
  x <- intersect(words, pos.dic[,1]) 
  pos.cnt <- length(x)
  x <- intersect(words, neg.dic[,1]) 
  neg.cnt <- length(x)
  
  if (pos.cnt > neg.cnt) {
    predict <- c(predict, 1)
  } else if (pos.cnt < neg.cnt) {
    predict <- c(predict, 0)
  } else {
    predict <- c(predict, -1)
  }
}
predict

circle <-table(predict)
circle<-circle[2:3]
circle
names(circle) <-(c("Neg","Pos"))
pct <- round(circle/sum(circle)*100,2)
label <- paste(names(circle), "\n", pct, "%")
title<-c("마스크 오브 매그너민티")
pie(circle, labels=label,col=c("white","skyblue"),main=title)

p <- length(predict[predict == 1])
n <- length(predict[predict == 0])

polarity <- (p - n) / (p + n)
polarity

##------------------글자빈도 출력

word = strsplit(reviews ,split=" ") %>% unlist()
rm_obj=which(!str_detect(word,"."))
word2=word[-rm_obj]
word_table=table(word2) %>% sort(decreasing=TRUE) 
word_df=word_table %>% as.data.frame()
head(word_df,10)

wordcloud2(data=word_df,fontFamily = '나눔바른고딕')

par(mar=c(1,1,1,1), mfrow=c(1,2))

df <- head(word_df, n=200)
wordcloud(df[,1],freq=df[,2],
          scale=c(3, 0.5),
          rot.per=0.25,
          random.order=F, random.color=T, 
          colors=rainbow(5))

df <- head(neg.dic, n=200)
wordcloud(df[,1],freq=df[,2],
          scale=c(3, 0.5),
          rot.per=0.25,
          random.order=F, random.color=T, 
          colors=rainbow(5))

plot.new()  

