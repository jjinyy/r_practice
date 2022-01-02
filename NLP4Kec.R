#NLP4kec_대통령연설문비교
#a
Sys.getenv()
Sys.setenv(JAVA_HOME="C:/Users/jjjiny/corretto/jdk11.0.11_9")
library(rJava)
.jinit()
.jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
#install.packages("rJava")
library(rJava)

#b
#setwd("C:/Users/jjjiny/Desktop/yonseiR")
getwd()
#install.packages("./Data/NLP4kec_1.4.0.zip" , repos=NULL, type="win.binary")
library(NLP4kec)
library(tm) # text mining package

#c
stopWordDic <- read.csv("./Data/dictionary/stopword_ko.csv")
synonymDic <- read.csv("./Data/dictionary/synonym.csv")

#d
data <- read.csv("./Data/raw_data/speech.csv")
class(data)
result = file_parser_r(path = data$content
                           ,language = "ko"
                           ,korDicPath = "./Data/dictionary/user_dictionary.txt")
result

#e
#동의어 처리
synonym_processing(parsedVector = result, synonymDic = synonymDic)

# Corpus 생성
corp = VCorpus(VectorSource(result))


# 특수문자 제거
corp = tm_map(corp, removePunctuation)

# 숫자삭제
corp = tm_map(corp, removeNumbers)

#소문자로 변경
corp <- tm_map(corp, tolower)

# 불용어 제거
corp = tm_map(corp, removeWords, stopWordDic$stopword)

#텍스트 문서 형식으로 변환
corp <- tm_map(corp, PlainTextDocument)
corp

# DTM, TDM 생성
dtm <- DocumentTermMatrix(corp, control = list(wordLengths = c(2, Inf)))
tdm <- TermDocumentMatrix(corp, control = list(wordLengths=c(2, Inf)))

# 양 옆 space 제거 및 한글자 단어 제외
colnames(dtm) <- trimws(colnames(dtm))
dtm <- dtm[, nchar(colnames(dtm)) > 1]

# h. Sparse Terms 삭제 (값이 작아질 수록 term수가 줄어드는데, 0.7로 set.)
dtm <- removeSparseTerms(dtm, as.numeric(0.7))

#i. DTM, TDM을 데이터프레임 형식으로 저장
dtm_df <- as.data.frame(as.matrix(dtm))
tdm_df <- as.data.frame(as.matrix(tdm))

#j. 단어 발생 빈도를 구하고, 문재인 대통령이 많이 쓴 단어 20개 추출
freq <- colSums(as.matrix(dtm))
freq

moon <- dtm[4,]
moonFreq <- colSums(as.matrix(moon))
moonFreq
moonFreq[head(order(-moonFreq), 20)]

# k. 단어빈도정보 Data set
df_word <- as.data.frame(freq, stringsAsFactors = F)
class(df_word) # data.frame
dim(df_word)
summary(df_word)
df_word

# l. 문재인 대통령이 사용한 상위 20개 단어로 bar chart로 제시
wordDfMoon <- data.frame(word = names(moonFreq), freq = moonFreq)
library(ggplot2)
library(dplyr)
ggplot(head(arrange(wordDfMoon, -freq), 20), aes(x=reorder(word,-freq), y=freq)) + geom_bar(stat = "identity")

# m. 문재인 대통령 연설문 Tree map 생성
#install.packages("treemap")
library(treemap)
pal <- RColorBrewer::brewer.pal(9, "Set1")
treemap(wordDfMoon,                    # 대상 데이터 설정
        title = "Word Tree Map",
        index = c("word"),        # 박스 안에 들어갈 변수 설정
        vSize = "freq",            # 박스 크기 기준
        fontsize.labels = 12,     # 폰트 크기 설정
        palette = pal,             # 위에서 만든 팔레트 정보 입력
        border.col = "white")    # 경계선 색깔 설정


# n. 문재인, 박근혜 대통령 연설문을 word cloud로 비교.
#install.packages("wordcloud")

park <- dtm[3,]
parkFreq <- colSums(as.matrix(park))
parkFreq
wordDfPark <- data.frame(word = names(parkFreq), freq = parkFreq)

library(wordcloud)

df_president = cbind(wordDfMoon, wordDfPark)

names(df_president) <- c("moon1","moon2","park1","park2")
names(df_president)

par(mfrow=c(2,1))
blues <- brewer.pal(8, "Blues")[-(1:2)]
wordcloud(df_president$moon1, df_president$moon2, max.words=100, colors=blues)

reds <- brewer.pal(8, "Reds")[-(1:2)]
wordcloud(df_president$park1, df_president$park2, max.words=100, colors=reds)



# o. 문재인과 박근혜 대통령이 사용한 공통단어 피라미드 그래프 생성.
#install.packages("plotrix")
library(dplyr)
library(plotrix)

par(mfrow=c(1,1))

common_words_25 <- df_president %>% 
  mutate(label = rownames(df_president)) %>% 
  dplyr::filter(moon1 > 0 & park1 >0) %>% 
  mutate(diff = abs(df_president$moon2 - df_president$park2)) %>% 
  arrange(desc(diff)) %>% slice(1:25)

plotrix::pyramid.plot(common_words_25$moon2, common_words_25$park2,
                      labels = common_words_25$label, gap = 8,
                      top.labels = c("문재인", "Words", "박근혜"),
                      main = "공통단어", laxlab = NULL, 
                      raxlab = NULL, unit = NULL)
