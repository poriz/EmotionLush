library(stringr)
library(wordcloud)
library(data.table)
library(wordcloud2)
library(RSelenium)
library(rvest)
library(httr) #HTMLÃ³¸®
library(stringr)
library(dplyr)
#Å©·Ñ¸µ
ch=wdman::chrome(port=4567L)
remDr=remoteDriver(port=4567L, browserName='chrome')
remDr$open()
remDr$navigate("html-code")
All_review=c()

for(i in 1:100) {  #ÆäÀÌÁö ¼ö¸¸Å­ ¹İº¹ÇÏ¿© review±Ü¾î¿À±â // °¢ÆäÀÌÁöÀÇ ¸®ºä ¼ö´Â 10°³ÀÌ´Ù.
  a=i #ÆäÀÌÁö ¹øÈ£
  first="javascript:goAjaxPage('page=" #i¸¦ º¯¼ö·Î ¹Ş±â À§ÇØ¼­ ¹®ÀåÀ» ºĞÇØ iÀÚ¸® ¾ÕÂÊ
  secon="&goodsNo=97&activeSno=&sort=&_=1620905746789')" #iÀÚ¸® µÚÂÊ
  script=paste(first,a,secon,sep='') # a´Â ÀÚµ¿ÀûÀ¸·Î i¸¦ ¹Ş¾Æ º¯ÇÏ°í paste¸¦ ÅëÇØ ÇÑ¹®ÀåÀÌ µÊ.
  pagemove <- remDr$executeScript(script, args = 1:2) #args ¿ä ºÎºĞ ÀÇ¹Ì´Â Àß ¸ğ¸£°ÚÀ½.
  source<-remDr$getPageSource()[[1]] #ÆäÀÌÁö ¼Ò½º °¡Á®¿À±â
  main <- read_html(source, encoding = "UTF-8")
  mainfo<-html_nodes(main ,css ='.text-container')
  
  
  review=mainfo%>%html_text() #ÅØ½ºÆ®¸¸ °¡Á®¿À±â
  All_review=c(All_review,review) #ÅØ½ºÆ® ÀúÀå
}
All_review


reviews <- gsub("´õº¸±â", " ", All_review)
reviews <- gsub("\n\t\t\t\t(.*)\n\t\t\t", "", reviews)
reviews <- gsub("´İ±â", " ", reviews)
#reviews <- gsub("ÃßÃµ", " ", reviews)
#reviews <- gsub("³Ê¹«", " ", reviews)
reviews <- gsub("ÇÇºÎ°¡", " ", reviews)
reviews <- gsub("js-pr-contents-desc img", "", reviews)
reviews <- gsub("max-width:80%", "", reviews)
reviews <- gsub("\t|\n", "", reviews) 
reviews <- gsub("              ", " ", reviews) 
reviews <- gsub("<U+00A0>", " ", reviews) 
reviews <- gsub("¹è¼Û", " ", reviews) 
reviews <- gsub("ºü¸£°í", " ", reviews) 
reviews <- gsub("       "," ",reviews)
reviews <- gsub("."," ",reviews,fixed=TRUE)
#reviews <- gsub("!","",reviews,fixed=TRUE)
#reviews <- gsub("?","",reviews,fixed=TRUE)

reviews

remDr$close

#ºĞ¼®ºÎºĞ
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
  words <- gsub("[^A-Za-z0-9¤¡-ÆR]","",words) 
  
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
title<-c("¸¶½ºÅ© ¿Àºê ¸Å±×³Ê¹ÎÆ¼")
pie(circle, labels=label,col=c("white","skyblue"),main=title)

p <- length(predict[predict == 1])
n <- length(predict[predict == 0])

polarity <- (p - n) / (p + n)
polarity

##------------------±ÛÀÚºóµµ Ãâ·Â

word = strsplit(reviews ,split=" ") %>% unlist()
rm_obj=which(!str_detect(word,"."))
word2=word[-rm_obj]
word_table=table(word2) %>% sort(decreasing=TRUE) 
word_df=word_table %>% as.data.frame()
head(word_df,10)

wordcloud2(data=word_df,fontFamily = '³ª´®¹Ù¸¥°íµñ')

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

