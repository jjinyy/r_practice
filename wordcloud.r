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

 data1 <- readLines("�ٻ��ݼ��ͻ�㳻��_����.txt")
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
tran2 <- sapply(tran11, unique) # �� ����Ʈ�ȿ��� �ߺ��� �ܾ�� �����ϱ�
tran2

tran3 <- rapply(tran2, function(x) gsub("�ο�", "", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("����", "", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("����", "", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("����", "��������", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("����", "����� �ο� ��ȭ", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("�ڵ���ü", "��������ڵ���ü", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("�������", "��������ڵ���ü", x), how = "replace")
tran3

tran4 <- sapply(tran3, function(x) {Filter(function(y) {nchar(y) <= 6 && nchar(y) > 1 },x)} ) # ���ڼ��� �����ϱ�, 2���� �̻� 6���� ���ϸ� ���
tran4


write(unlist(tran4),"�ٻ�_2.txt") 
data4 <- read.table("�ٻ�_2.txt")
data4
nrow(data4) 
wordcount <- table(data4) 
wordcount
wordcount <- Filter(function(x) {nchar(x) <= 10} ,wordcount)
head(sort(wordcount, decreasing=T),100)

txt <- readLines("�ٻ���gsub.txt")
 txt
 cnt_txt <- length(txt)
 cnt_txt
 for( i in 1:cnt_txt) {
      tran3 <- rapply(tran3, function(x) gsub((txt[i]),"", x), how = "replace")   
      }
 tran3

data3 <- sapply(tran3, function(x) {Filter(function(y) { nchar(y) >=2 },x)} )
 write(unlist(data3),"�ٻ�_2.txt") 
 data4 <- read.table("�ٻ�_2.txt")
 data4
 nrow(data4) 


 wordcount <- table(data4) 
 wordcount
 head(sort(wordcount, decreasing=T),100)
 
library(RColorBrewer) 
 palete <- brewer.pal(7,"Set2")
 wordcloud(names(wordcount),freq=wordcount,scale=c(5,0.8),rot.per=0.25,min.freq=100,
 random.order=F,random.color=T,colors=palete)
 legend(0.2,1 ,"����� �ٻ� �ݼ��� ���� ���� �м�        ",cex=1.3,fill=NA,
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
legend(-0.7,1.3 ,"�ٻ��ݼ��� ��㳻�� �м�    ",cex=1.2,fill=NA,border=NA,bg="white" ,
       text.col="red",text.font=2,box.col="blue")


# Ư�� �� ������ ������ �����ϱ�
#dev.new( )
#V(edge2)$size <- degree(edge2)
#V(edge2)$size
#removeedge <- V(edge2)[degree(edge2) < 2 ]
#edge2 <- delete.vertices(edge2,removeedge)
#head(sort(closeness(edge2),decreasing=T))
#head(sort(betweenness(edge2),decreasing=T))
#plot(edge2)
#legend(-0.7,1.3 ,"�ٻ��ݼ��� ��㳻�� �м�   ",cex=0.8,fill=NA,border=NA,bg="white" ,
#       text.col="red",text.font=2,box.col="red")

dev.new( )
V(edge2)$color <- ifelse(V(edge2)$degree >= 2,"red","green")
V(edge2)$label.color <- ifelse(V(edge2)$degree >= 2,"blue","red")
V(edge2)$degree
plot(edge2)
legend(-0.7,1.3 ,"�ٻ��ݼ��� ��㳻�� �м�   ",cex=1.2,fill=NA,border=NA,bg="white" ,
       text.col="red",text.font=2,box.col="red")

#dev.new()
#plot(ares, method = "grouped")

dev.new()
plot(ares, method = "graph", control = list(type = "items"))

####################################################################33
############# �׷����� ǥ���ϱ� ####################################
###############################################################
tran3 <- rapply(tran2, function(x) gsub("����", "", x), how = "replace") 
tran3 <- rapply(tran2, function(x) gsub("���·�", "���·ᳳ��", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("����", "", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("����", "", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("����", "��������", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("����", "����� �ο� ��ȭ", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("�ڵ���ü", "��������ڵ���ü", x), how = "replace") 
tran3 <- rapply(tran3, function(x) gsub("�������", "��������ڵ���ü", x), how = "replace")
tran3

tran4 <- sapply(tran3, function(x) {Filter(function(y) {nchar(y) <= 6 && nchar(y) > 1 },x)} )
tran4


write(unlist(tran4),"�ٻ�_2.txt") 
data4 <- read.table("�ٻ�_2.txt")
data4
nrow(data4) 
wordcount <- table(data4) 
wordcount
wordcount <- Filter(function(x) {nchar(x) <= 10} ,wordcount)
head(sort(wordcount, decreasing=T),100)

txt <- readLines("�ٻ���gsub.txt")
 txt
 cnt_txt <- length(txt)
 cnt_txt
 for( i in 1:cnt_txt) {
      tran3 <- rapply(tran3, function(x) gsub((txt[i]),"", x), how = "replace")   
      }
 tran3

data3 <- sapply(tran3, function(x) {Filter(function(y) { nchar(y) >=2 },x)} )
 write(unlist(data3),"�ٻ�_2.txt") 
 data4 <- read.table("�ٻ�_2.txt")
 data4
 nrow(data4) 

 wordcount <- table(data4) 
 wordcount
 head(sort(wordcount, decreasing=T),100)

final <- head(sort(wordcount, decreasing=T),15)
bp <- barplot(final,main = "�ٻ� �ݼ��� ���� TOP 15 - �Ǻ� ���", 
      col = rainbow(10), cex.names=0.7, las = 2,ylim=c(0,350))
text(x = bp, y = final*1.05, labels = paste(final,"��"), col = "black", cex = 0.7)

dev.new( )
bp <- barplot(final,  main = "�ٻ� �ݼ��� ���� TOP 15 - ������ ���", 
      col = rainbow(10), cex.names=0.7,  las = 2 , ylim=c(0,350))
pct <- round(final/sum(final) * 100 ,1)
text(x = bp, y = final*1.05, labels = paste(pct,"%"), col = "black", cex = 0.7)


install.packages("wordcloud2")
library(wordcloud2)
wordcloud2(wordcount,size=0.5,shape="diamond")

wordcount2 <- head(sort(wordcount, decreasing=T),100)
wordcloud2(wordcount2,gridSize=1,size=0.5,shape="diamond")
 



