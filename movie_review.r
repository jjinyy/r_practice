################################################################
## ��ȭ�� ���並 �м��Ͽ� ���� Ŭ���� �׸���
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


data1 <- readLines("��ȭ_�.txt")

data1

tran1 <- Map(extractNoun, data1)
tran1
tran11 <- unique(tran1)
tran2 <- sapply(tran11, unique) # �� ����Ʈ�ȿ��� �ߺ��� �ܾ�� �����ϱ�
tran2

tran3 <- rapply(tran2, function(x) gsub("����", "", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("��ȭ", "", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("����", "", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("����", "", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("����", "", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("������", "", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("������", "", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("����", "������", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("��¥", "", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("����", "", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("����", "", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("����", "", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("����", "���ּ���", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3

tran4 <- sapply(tran3, function(x) {Filter(function(y) {nchar(y) <= 6 && nchar(y) > 1},x)} ) # ���ڼ��� �����ϱ�, 2���� �̻� 6���� ���ϸ� ���
tran4


write(unlist(tran4),"��_2.txt") 
data4 <- read.table("��_2.txt")
data4
nrow(data4) 

wordcount <- table(data4) 
wordcount
wordcount <- Filter(function(x) {nchar(x) <= 10} ,wordcount)
head(sort(wordcount, decreasing=T),100)

txt <- readLines("��ȭgsub.txt")
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
write(unlist(data3),"�_2.txt") 
data4 <- read.table("�_2.txt")
data4
nrow(data4) 

#�Ʒ� ������ ���͸��� �Ϸ�� �ܾ���� ��� �󵵼��� �����ϴ� �����Դϴ�.
wordcount <- table(data4) 
wordcount
head(sort(wordcount, decreasing=T),100)

library(RColorBrewer) 
palete <- brewer.pal(7,"Set2") 
wordcloud(names(wordcount),freq=wordcount,scale=c(5,1),rot.per=0.25,min.freq=2,
random.order=F,random.color=T,colors=palete)
legend(0.3,1 ,"��ȭ ��� �м� - �  ",cex=1.2,fill=NA,border=NA,bg="white" ,
       text.col="red",text.font=2,box.col="red")
savePlot("��ȭ_�.png",type="png")

wordcount2 <- head(sort(wordcount, decreasing=T),100)
wordcloud2(wordcount2,gridSize=1,size=0.5,shape="diamond")

#################################################################
####### ��ȭ � �ܾ� ���� ��Ʈ��ũ �׸��� #######
#################################################################

dev.new( )
data1 <- readLines("��ȭ_�.txt")

data1

tran1 <- Map(extractNoun, data1)
tran1
tran11 <- unique(tran1)
tran2 <- sapply(tran11, unique) # �� ����Ʈ�ȿ��� �ߺ��� �ܾ�� �����ϱ�
tran2

tran3 <- rapply(tran2, function(x) gsub("����", "", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("��ȭ", "", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("����", "", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("����", "", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("����", "", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("������", "", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("������", "", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("����", "������", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("��¥", "", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("����", "", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("����", "", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("����", "", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3 <- rapply(tran3, function(x) gsub("����", "���ּ���", x), how = "replace") # �� ����Ʈ���� Ư�� �ܾ� �����ϱ�
tran3

txt <- readLines("��ȭgsub.txt")
 txt
 cnt_txt <- length(txt)
 cnt_txt
 for( i in 1:cnt_txt) {
     # data3 <-gsub((txt[i]),"",data4)  
      tran3 <- rapply(tran3, function(x) gsub((txt[i]),"", x), how = "replace")   
      }
 tran3

tran4 <- sapply(tran3, function(x) {Filter(function(y) {nchar(y) <= 6 && nchar(y) > 1 && is.hangul(y)},x)} ) # ���ڼ��� �����ϱ�, 2���� �̻� 6���� ���ϸ� ���
tran4

names(tran4) <- paste("List_", 1:length(tran4), sep="")
tran4

tran4 <- sapply(tran4, unique) # �� ����Ʈ�ȿ��� �ߺ��� �ܾ�� �����ϱ�
wordtran <- as(tran4, "transactions")
wordtran
class(wordtran)
inspect(wordtran)

#co-occurance table 
wordtab <- crossTable(wordtran)
wordtab

ares <- apriori(wordtran, parameter=list(supp=0.01, conf=0.03))# support : �� ���� Ȯ�� , #confidence : ���Ǻ� Ȯ��
summary(ares)

inspect(ares)

rule1 <- labels(ares, ruleSep=" ") # ���� ares ������ ������� ���ΰ� ��� ������ �ϳ��� �׸����� ��ġ�� �۾�. ����� ���� ����� ����
rule1
rule2 <- sapply(rule1, strsplit, " ",  USE.NAMES=F) # �� rule1 �� �����ε� list ���·� ��ȯ. list�� �ƴϸ� ������ ���� ������
rule2
rulematrix <- do.call("rbind", rule1) # ���⼭ list�� �ƴ϶�� ������ ��. �׷��� �� �۾� rule2 ���� list ���·� ��ȯ��

rulematrix <- do.call("rbind", rule2)
rulematrix

edgelist <- graph.adjacency(wordtab,weighted=T,mode="undirected") # graph ������ ������ ��ȯ�ϱ�
edgelist

degree(edgelist)
dim(edgelist)
edge1 <- graph.edgelist(rulematrix[-c(1:3),],directed=F)# ���� ������ 1-3 �� �� �͵��� �����ϰ� �׷����� �׸�
edge1

plot.igraph(edge1, vertex.label=V(edge1)$name, vertex.label.cex=0.8, vertex.size=20, layout=layout.fruchterman.reingold.grid)
edge2 <- simplify(edge1)
edge2
plot(edge2)
#legend(-0.7,1.3 ,"��ȭ ���� �м� - �   ",cex=1.2,fill=NA,border=NA,bg="white" ,
#       text.col="red",text.font=2,box.col="blue")
#savePlot("��ȭ-�_�����ܾ�м�_����ġ����.png",type="png")


# ��޵� Ƚ����ŭ ����ġ�� �־ �׷��� �׸���
dev.new( )
degree(edge2) # �� ���� E(edge2) ����� �����ؼ� ����� ���ϸ� �˼� ����.

V(edge2)$degree <- degree(edge2) 
V(edge2)$label.cex <-3*(V(edge2)$degree / max(V(edge2)$degree))
V(edge2)$size <- 1*(V(edge2)$degree / max(V(edge2)$degree))
E(edge2)$width <- 2*(E(edge2)$weight / max(E(edge2)$weight)) # <- ��� �����µ� ��� �ذ��ϱ�
plot(edge2)
legend(-0.7,1.3 ,"��ȭ ���� �м� - � -����ġ�ݿ�   ",cex=1.2,fill=NA,border=NA,bg="white" ,
       text.col="red",text.font=2,box.col="blue")
#savePlot("��ȭ_�_�����ܾ�м�_����ġ�ݿ�.png",type="png")

# Ư�� �� ������ ������ �����ϱ�
#dev.new( )
#V(edge2)$size <- degree(edge2)
#V(edge2)$size
#removeedge <- V(edge2)[degree(edge2) < 2 ]
#edge2 <- delete.vertices(edge2,removeedge)
#head(sort(closeness(edge2),decreasing=T))
#head(sort(betweenness(edge2),decreasing=T))
#plot(edge2)
#legend(-0.7,1.3 ,"��ȭ ���� �м� - �����Ǿ�-����ġ�ݿ�   ",cex=0.8,fill=NA,border=NA,bg="white" ,
#       text.col="red",text.font=2,box.col="red")

# Ư�� �󵵺��� ���� �ٸ��� �����ϱ�
dev.new( )
V(edge2)$color <- ifelse(V(edge2)$degree >= 2,"red","green")
V(edge2)$label.color <- ifelse(V(edge2)$degree >= 2,"blue","red")
V(edge2)$degree
plot(edge2)
legend(-0.7,1.3 ,"��ȭ ���� �м� - � -����ġ�ݿ�   ",cex=1.2,fill=NA,border=NA,bg="white" ,
       text.col="red",text.font=2,box.col="red")

savePlot("��ȭ_�_�����ܾ�м�_����ġ�ݿ�.png",type="png")

#dev.new()
#plot(ares, method = "grouped")

dev.new()
plot(ares, method = "graph", control = list(type = "items"))