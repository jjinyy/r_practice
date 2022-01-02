setwd("c:\\a_temp")  
 #install.packages("KoNLP") 
 #install.packages("wordcloud") 
 #install.packages("stringr")
 library(stringr)
 library(KoNLP)  
 library(wordcloud)
 useSejongDic() 
 library(arules)
 library(igraph)
 library(arulesViz)


 install.packages("doParallel")
 library(doParallel)
 registerDoParallel(cores=4)  

 data1 <- readLines("다산콜센터상담내역_연습.txt")
 data1
 options(max.print=5000000)
 
#############################################
##> tran1 <- Map(extractNoun, data1)
##Error in `Encoding<-`(`*tmp*`, value = "UTF-8") : 

data2 <- Filter(function(x) {nchar(x) <= 200} ,data1)
data2

#data2 <- str_replace_all(data1,"[^[:alpha:][:blank:]]","")
#data2

tran1 <- Map(extractNoun, data2)
tran1
tran11 <- unique(tran1)
tran2 <- sapply(tran11, unique) # 각 리스트안에서 중복된 단어들 제거하기
tran2

tran3 <- rapply(tran2, function(x) gsub("민원", "", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("접수", "", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("한지", "", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("가능", "가능한지", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("원전", "서울시 민원 전화", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("자동이체", "수도요금자동이체", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("수도요금", "수도요금자동이체", x), how = "replace")
tran3

tran4 <- sapply(tran3, function(x) {Filter(function(y) {nchar(y) <= 6 && nchar(y) > 1 },x)} ) # 글자수로 제거하기, 2글자 이상 6글자 이하만 출력
tran4


write(unlist(tran4),"다산_2.txt") 
data4 <- read.table("다산_2.txt")
data4
nrow(data4) 
wordcount <- table(data4) 
wordcount
wordcount <- Filter(function(x) {nchar(x) <= 10} ,wordcount)
head(sort(wordcount, decreasing=T),100)

txt <- readLines("다산콜gsub.txt")
 txt
 cnt_txt <- length(txt)
 cnt_txt
 for( i in 1:cnt_txt) {
      tran3 <- rapply(tran3, function(x) gsub((txt[i]),"", x), how = "replace")   
      }
 tran3

data3 <- sapply(tran3, function(x) {Filter(function(y) { nchar(y) >=2 },x)} )
 write(unlist(data3),"다산_2.txt") 
 data4 <- read.table("다산_2.txt")
 data4
 nrow(data4) 


 wordcount <- table(data4) 
 wordcount
 head(sort(wordcount, decreasing=T),100)
 
library(RColorBrewer) 
 palete <- brewer.pal(7,"Set2")
 wordcloud(names(wordcount),freq=wordcount,scale=c(5,0.8),rot.per=0.25,min.freq=100,
 random.order=F,random.color=T,colors=palete)
 legend(0.2,1 ,"서울시 다산 콜센터 문의 사항 분석        ",cex=1.3,fill=NA,
 border=NA,bg="white" , text.col="red",text.font=3,box.col="red")


tran4 <- sapply(tran4, unique)
wordtran <- as(tran4, "transactions")
wordtran
class(wordtran)
inspect(wordtran)

#co-occurance table 
wordtab <- crossTable(wordtran)
wordtab

ares <- apriori(wordtran, parameter=list(supp=0.01, conf=0.01))
summary(ares)

inspect(ares)

rule1 <- labels(ares, ruleSep=" ")
rule1
rule2 <- sapply(rule1, strsplit, " ",  USE.NAMES=F)
rule2
rulematrix <- do.call("rbind", rule1)
rulematrix <- do.call("rbind", rule2)
rulematrix

edgelist <- graph.adjacency(wordtab,weighted=T,mode="undirected")
edgelist

degree(edgelist)
dim(edgelist)
edge1 <- graph.edgelist(rulematrix[-c(1:104),],directed=F)
edge1

plot.igraph(edge1, vertex.label=V(edge1)$name, vertex.label.cex=0.8, vertex.size=20, layout=layout.fruchterman.reingold.grid)
edge2 <- simplify(edge1)
edge2
plot(edge2)


dev.new( )
degree(edge2)

V(edge2)$degree <- degree(edge2) 
V(edge2)$label.cex <-3*(V(edge2)$degree / max(V(edge2)$degree))
V(edge2)$size <- 1*(V(edge2)$degree / max(V(edge2)$degree))
E(edge2)$width <- 2*(E(edge2)$weight / max(E(edge2)$weight)) 
plot(edge2)
legend(-0.7,1.3 ,"다산콜센터 상담내역 분석    ",cex=1.2,fill=NA,border=NA,bg="white" ,
       text.col="red",text.font=2,box.col="blue")


# 특정 빈도 이하의 엣지를 제거하기
#dev.new( )
#V(edge2)$size <- degree(edge2)
#V(edge2)$size
#removeedge <- V(edge2)[degree(edge2) < 2 ]
#edge2 <- delete.vertices(edge2,removeedge)
#head(sort(closeness(edge2),decreasing=T))
#head(sort(betweenness(edge2),decreasing=T))
#plot(edge2)
#legend(-0.7,1.3 ,"다산콜센터 상담내역 분석   ",cex=0.8,fill=NA,border=NA,bg="white" ,
#       text.col="red",text.font=2,box.col="red")

dev.new( )
V(edge2)$color <- ifelse(V(edge2)$degree >= 2,"red","green")
V(edge2)$label.color <- ifelse(V(edge2)$degree >= 2,"blue","red")
V(edge2)$degree
plot(edge2)
legend(-0.7,1.3 ,"다산콜센터 상담내역 분석   ",cex=1.2,fill=NA,border=NA,bg="white" ,
       text.col="red",text.font=2,box.col="red")

#dev.new()
#plot(ares, method = "grouped")

dev.new()
plot(ares, method = "graph", control = list(type = "items"))

####################################################################33
############# 그래프로 표시하기 ####################################
###############################################################
tran3 <- rapply(tran2, function(x) gsub("납부", "", x), how = "replace") 
tran3 <- rapply(tran2, function(x) gsub("과태료", "과태료납부", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("접수", "", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("한지", "", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("가능", "가능한지", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("원전", "서울시 민원 전화", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("자동이체", "수도요금자동이체", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("수도요금", "수도요금자동이체", x), how = "replace")
tran3

tran4 <- sapply(tran3, function(x) {Filter(function(y) {nchar(y) <= 6 && nchar(y) > 1 },x)} )
tran4


write(unlist(tran4),"다산_2.txt") 
data4 <- read.table("다산_2.txt")
data4
nrow(data4) 
wordcount <- table(data4) 
wordcount
wordcount <- Filter(function(x) {nchar(x) <= 10} ,wordcount)
head(sort(wordcount, decreasing=T),100)

txt <- readLines("다산콜gsub.txt")
 txt
 cnt_txt <- length(txt)
 cnt_txt
 for( i in 1:cnt_txt) {
      tran3 <- rapply(tran3, function(x) gsub((txt[i]),"", x), how = "replace")   
      }
 tran3

data3 <- sapply(tran3, function(x) {Filter(function(y) { nchar(y) >=2 },x)} )
 write(unlist(data3),"다산_2.txt") 
 data4 <- read.table("다산_2.txt")
 data4
 nrow(data4) 

 wordcount <- table(data4) 
 wordcount
 head(sort(wordcount, decreasing=T),100)

final <- head(sort(wordcount, decreasing=T),15)
bp <- barplot(final,main = "다산 콜센터 문의 TOP 15 - 건별 출력", 
      col = rainbow(10), cex.names=0.7, las = 2,ylim=c(0,350))
text(x = bp, y = final*1.05, labels = paste(final,"건"), col = "black", cex = 0.7)

dev.new( )
bp <- barplot(final,  main = "다산 콜센터 문의 TOP 15 - 비율별 출력", 
      col = rainbow(10), cex.names=0.7,  las = 2 , ylim=c(0,350))
pct <- round(final/sum(final) * 100 ,1)
text(x = bp, y = final*1.05, labels = paste(pct,"%"), col = "black", cex = 0.7)


install.packages("wordcloud2")
library(wordcloud2)
wordcloud2(wordcount,size=0.5,shape="diamond")

wordcount2 <- head(sort(wordcount, decreasing=T),100)
wordcloud2(wordcount2,gridSize=1,size=0.5,shape="diamond")
 



