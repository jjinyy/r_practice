################################################################
## 영화의 리뷰를 분석하여 워드 클라우드 그리기
################################################################
setwd("c:\\a_temp")
install.packages("arules")
library(arules)

install.packages("igraph")
library(igraph)

install.packages("combinat")
library(combinat)

install.packages("KoNLP")
library(KoNLP)

install.packages("arulesViz")
library(arulesViz)

install.packages("wordcloud")
library(wordcloud)

install.packages("wordcloud2")
library(wordcloud2)


data1 <- readLines("영화_곡성.txt")

data1

tran1 <- Map(extractNoun, data1)
tran1
tran11 <- unique(tran1)
tran2 <- sapply(tran11, unique) # 각 리스트안에서 중복된 단어들 제거하기
tran2

tran3 <- rapply(tran2, function(x) gsub("리뷰", "", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("영화", "", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("평점", "", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("내용", "", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("제외", "", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("ㅋㅋㅋ", "", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("ㄱㄱㄱ", "", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("유아", "유아인", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("진짜", "", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("로코", "", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("완전", "", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("세포", "", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("연애", "연애세포", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3

tran4 <- sapply(tran3, function(x) {Filter(function(y) {nchar(y) <= 6 && nchar(y) > 1},x)} ) # 글자수로 제거하기, 2글자 이상 6글자 이하만 출력
tran4


write(unlist(tran4),"주_2.txt") 
data4 <- read.table("주_2.txt")
data4
nrow(data4) 

wordcount <- table(data4) 
wordcount
wordcount <- Filter(function(x) {nchar(x) <= 10} ,wordcount)
head(sort(wordcount, decreasing=T),100)

txt <- readLines("영화gsub.txt")
txt
cnt_txt <- length(txt)
cnt_txt
for( i in 1:cnt_txt) {
     # data3 <-gsub((txt[i]),"",data4)  
      tran3 <- rapply(tran3, function(x) gsub((txt[i]),"", x), how = "replace")   
      }
tran3
#data3 <- Filter(function(x) {nchar(x) >= 2} ,data3)
data3 <- sapply(tran3, function(x) {Filter(function(y) { nchar(y) >=2},x)} )
write(unlist(data3),"곡성_2.txt") 
data4 <- read.table("곡성_2.txt")
data4
nrow(data4) 

#아래 과정이 필터링이 완료된 단어들을 언급 빈도수로 집계하는 과정입니다.
wordcount <- table(data4) 
wordcount
head(sort(wordcount, decreasing=T),100)

library(RColorBrewer) 
palete <- brewer.pal(7,"Set2") 
wordcloud(names(wordcount),freq=wordcount,scale=c(5,1),rot.per=0.25,min.freq=2,
random.order=F,random.color=T,colors=palete)
legend(0.3,1 ,"영화 댓글 분석 - 곡성  ",cex=1.2,fill=NA,border=NA,bg="white" ,
       text.col="red",text.font=2,box.col="red")
savePlot("영화_곡성.png",type="png")

wordcount2 <- head(sort(wordcount, decreasing=T),100)
wordcloud2(wordcount2,gridSize=1,size=0.5,shape="diamond")

#################################################################
####### 영화 곡성 단어 연관 네트워크 그리기 #######
#################################################################

dev.new( )
data1 <- readLines("영화_곡성.txt")

data1

tran1 <- Map(extractNoun, data1)
tran1
tran11 <- unique(tran1)
tran2 <- sapply(tran11, unique) # 각 리스트안에서 중복된 단어들 제거하기
tran2

tran3 <- rapply(tran2, function(x) gsub("리뷰", "", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("영화", "", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("평점", "", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("내용", "", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("제외", "", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("ㅋㅋㅋ", "", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("ㄱㄱㄱ", "", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("유아", "유아인", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("진짜", "", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("로코", "", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("완전", "", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("세포", "", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3 <- rapply(tran3, function(x) gsub("연애", "연애세포", x), how = "replace") # 각 리스트에서 특정 단어 제거하기
tran3

txt <- readLines("영화gsub.txt")
 txt
 cnt_txt <- length(txt)
 cnt_txt
 for( i in 1:cnt_txt) {
     # data3 <-gsub((txt[i]),"",data4)  
      tran3 <- rapply(tran3, function(x) gsub((txt[i]),"", x), how = "replace")   
      }
 tran3

tran4 <- sapply(tran3, function(x) {Filter(function(y) {nchar(y) <= 6 && nchar(y) > 1 && is.hangul(y)},x)} ) # 글자수로 제거하기, 2글자 이상 6글자 이하만 출력
tran4

names(tran4) <- paste("List_", 1:length(tran4), sep="")
tran4

tran4 <- sapply(tran4, unique) # 각 리스트안에서 중복된 단어들 제거하기
wordtran <- as(tran4, "transactions")
wordtran
class(wordtran)
inspect(wordtran)

#co-occurance table 
wordtab <- crossTable(wordtran)
wordtab

ares <- apriori(wordtran, parameter=list(supp=0.01, conf=0.03))# support : 쌍 출현 확률 , #confidence : 조건부 확률
summary(ares)

inspect(ares)

rule1 <- labels(ares, ruleSep=" ") # 위의 ares 변수의 결과에서 원인과 결과 조건을 하나의 항목으로 합치는 작업. 경우의 수를 만드는 것임
rule1
rule2 <- sapply(rule1, strsplit, " ",  USE.NAMES=F) # 위 rule1 은 벡터인데 list 형태로 변환. list가 아니면 에러가 나기 때문임
rule2
rulematrix <- do.call("rbind", rule1) # 여기서 list가 아니라고 에러가 남. 그래서 위 작업 rule2 에서 list 형태로 변환함

rulematrix <- do.call("rbind", rule2)
rulematrix

edgelist <- graph.adjacency(wordtab,weighted=T,mode="undirected") # graph 형으로 데이터 변환하기
edgelist

degree(edgelist)
dim(edgelist)
edge1 <- graph.edgelist(rulematrix[-c(1:3),],directed=F)# 에지 갯수가 1-3 개 인 것들은 제외하고 그래프를 그림
edge1

plot.igraph(edge1, vertex.label=V(edge1)$name, vertex.label.cex=0.8, vertex.size=20, layout=layout.fruchterman.reingold.grid)
edge2 <- simplify(edge1)
edge2
plot(edge2)
#legend(-0.7,1.3 ,"영화 리뷰 분석 - 곡성   ",cex=1.2,fill=NA,border=NA,bg="white" ,
#       text.col="red",text.font=2,box.col="blue")
#savePlot("영화-곡성_연관단어분석_가중치없음.png",type="png")


# 언급된 횟수만큼 가중치를 주어서 그래프 그리기
dev.new( )
degree(edge2) # 이 값은 E(edge2) 명령을 수행해서 결과를 비교하면 알수 있음.

V(edge2)$degree <- degree(edge2) 
V(edge2)$label.cex <-3*(V(edge2)$degree / max(V(edge2)$degree))
V(edge2)$size <- 1*(V(edge2)$degree / max(V(edge2)$degree))
E(edge2)$width <- 2*(E(edge2)$weight / max(E(edge2)$weight)) # <- 경고 나오는데 경고 해결하기
plot(edge2)
legend(-0.7,1.3 ,"영화 리뷰 분석 - 곡성 -가중치반영   ",cex=1.2,fill=NA,border=NA,bg="white" ,
       text.col="red",text.font=2,box.col="blue")
#savePlot("영화_곡성_연관단어분석_가중치반영.png",type="png")

# 특정 빈도 이하의 엣지를 제거하기
#dev.new( )
#V(edge2)$size <- degree(edge2)
#V(edge2)$size
#removeedge <- V(edge2)[degree(edge2) < 2 ]
#edge2 <- delete.vertices(edge2,removeedge)
#head(sort(closeness(edge2),decreasing=T))
#head(sort(betweenness(edge2),decreasing=T))
#plot(edge2)
#legend(-0.7,1.3 ,"영화 리뷰 분석 - 주토피아-가중치반영   ",cex=0.8,fill=NA,border=NA,bg="white" ,
#       text.col="red",text.font=2,box.col="red")

# 특정 빈도별로 색상 다르게 지정하기
dev.new( )
V(edge2)$color <- ifelse(V(edge2)$degree >= 2,"red","green")
V(edge2)$label.color <- ifelse(V(edge2)$degree >= 2,"blue","red")
V(edge2)$degree
plot(edge2)
legend(-0.7,1.3 ,"영화 리뷰 분석 - 곡성 -가중치반영   ",cex=1.2,fill=NA,border=NA,bg="white" ,
       text.col="red",text.font=2,box.col="red")

savePlot("영화_곡성_연관단어분석_가중치반영.png",type="png")

#dev.new()
#plot(ares, method = "grouped")

dev.new()
plot(ares, method = "graph", control = list(type = "items"))