library(KoNLP)
library("reader")
library(wordcloud2)


# answer to Q1.a===============================================================
# 추미애 전 장관 2021년 신년사 화일 읽어들이기
choo_file <- file("./Data/추미애2021년 신년사.txt", blocking=F, "rt", encoding = "UTF-8")

# 파일 내 텍스틑를 line별로 읽어들이기
choo_text <- readLines(choo_file)

# 라인별로 읽어드린 텍스트 화일을 살펴보니, 텍스트가 존재하지 않는 element가 있음
# print(choo_text)

# 그러므로, empty line을 제거함
NoEmptyLine_choo_txt <- choo_text[choo_text != ""]

# 결과 확인; => 모든 empty line이 제거된 것을 확인.
# print(NoEmptyLine_choo_txt) 

# 텍스트 내 영어나 한글이 아닌, 즉 텍스트가 아닌 기호는 모두 white space로 대체.
# 제거하는 것도 방법이나, 제거 시 전,후 단어가 하나로 합쳐지는 경우가 발생할 수 있어 white space로 대체.
TextOnly_choo_txt <- gsub("[^A-Za-z가-힣]", " ", NoEmptyLine_choo_txt)
# print(TextOnly_choo_txt) 

# 텍스트 내 둘 이상의 white space가 있으면 하나의 space로 압축
SingleSpace_choo_txt <- gsub("  *", " ", TextOnly_choo_txt)
# print(SingleSpace_choo_txt)

# 텍스트 중간에 한글자 단어가 있으면 하나의 white space로 대체,
# 한 글자 단어는 의미가 없거나, 불완전 명사이므로 while space로 제거.
NoOneChar_choo_txt <- gsub(" . ", " ", SingleSpace_choo_txt)
# print(NoOneChar_choo_txt)

# 텍스트 마지막 문자열이 한 글자이면 하나의 white space로 대체
# 앞 코드가 문장의 맨 마지막 한글자는 제거하지 못하므로 여기서 while space로 대체.
NoOneChar_choo_txt <- gsub(" .$", " ", NoOneChar_choo_txt)
# print(NoOneChar_choo_txt)

# 결과를 확인해 보니 아직 한글자 단어가 존재.
# 다시 텍스트 내 한글자 단어가 있으면 white space로 대체
NoOneChar_choo_txt <- gsub(" . ", " ", NoOneChar_choo_txt)
# print(NoOneChar_choo_txt)

# 원소 마지막 문자열이 white space 인 경우 제거
NoOneChar_choo_txt <- gsub(" $", "", NoOneChar_choo_txt)
# print(NoOneChar_choo_txt)

# 텍스트 내 한글자만 존재하는 원소를 제거하면 empty line이 되었을 것이므로 다시 공백 line 제거.
Final_choo_txt <- NoOneChar_choo_txt[NoOneChar_choo_txt != ""]
# print(Final_choo_txt)

# 사전은 세종 사전 사용.
useSejongDic()

# 사전에 없는 새로운 사용자 용어사전을 만듬
user_d <- data.frame(v1=c("코로나19","신축년","형사사법시스템","공수처","주무부처","N번방"," 스토킹처벌법","법무부장관"),
                     v2=c("ncn","ncn","ncn","ncn","ncn","ncn","ncn","ncn"))
buildDictionary(ext_dic = "sejong", user_dic = user_d, replace_usr_dic = F)

# 명사를 추출하고, unlist 함.
choo_nouns = sapply(Final_choo_txt, extractNoun, USE.NAMES=F)
unlisted_choo_nouns = unlist(choo_nouns)

# 두 글자 이상만
final_ch_nouns <- unlisted_choo_nouns[nchar(unlisted_choo_nouns) >= 2]
# print(final_ch_nouns)

# 텍스트 마다 필터링이 필요하므로 함수로 정의
filtering <- function(nouns){
  idx = (1:length(nouns))[nouns==""]
  idx = (1:length(nouns))[nouns==" "]
  idx = c(idx,(1:length(nouns))[nouns=="밝았습니"])
  idx = c(idx,(1:length(nouns))[nouns=="집중해주시길"])
  idx = c(idx,(1:length(nouns))[nouns=="올립니"])
  idx = c(idx,(1:length(nouns))[nouns=="합니"])
  idx = c(idx,(1:length(nouns))[nouns=="바랍니"])
  idx = c(idx,(1:length(nouns))[nouns=="있습니"])
  idx = c(idx,(1:length(nouns))[nouns=="기원합니"])
  idx = c(idx,(1:length(nouns))[nouns=="감사합니"])
  idx = c(idx,(1:length(nouns))[nouns=="그동안"])
  idx = c(idx,(1:length(nouns))[nouns=="여러분"])
  idx = c(idx,(1:length(nouns))[nouns=="누구"])
  idx = c(idx,(1:length(nouns))[nouns=="하시"])
  idx = c(idx,(1:length(nouns))[nouns=="않길"])
  idx = c(idx,(1:length(nouns))[nouns=="첫째"])
  idx = c(idx,(1:length(nouns))[nouns=="둘째"])
  idx = c(idx,(1:length(nouns))[nouns=="셌째"])
  idx = c(idx,(1:length(nouns))[nouns=="넷째"])
  nouns = nouns [-idx]
  return(nouns)
}

filtered_ch_nouns <- filtering(final_ch_nouns)
# print(filtered_ch_nouns)

choo_wc = table(filtered_ch_nouns)
choo_wc = sort(choo_wc, decreasing=T)

# head(choo_wc, 10)
wordcloud2(choo_wc, size = 1.5, color = "random-dark", rotateRatio = 0, fontWeight = "bold")



# answer to Q1.b===============================================================
# 윤석윤 전 검찰총장 2021년 신년사 화일 읽어들이기
yoon_file <- file("./Data/윤석열2021년신년사.txt", blocking=F, "rt", encoding = "UTF-8")

# 파일 내 텍스틑를 line별로 읽어들이기
yoon_text <- readLines(yoon_file)

# 라인별로 읽어드린 텍스트 화일을 살펴보니, 텍스트가 존재하지 않는 원소가 있음
# print(yoon_text)

# 그러므로, empty line을 제거함
NoEmptyLine_yoon_txt <- yoon_text[yoon_text != ""]
# print(NoEmptyLine_yoon_txt) 

# 텍스트 내 영어나 한글이 아닌, 즉 텍스트가 아닌 기호는 모두 white space로 대체.
TextOnly_yoon_txt <- gsub("[^A-Za-z가-힣]", " ", NoEmptyLine_yoon_txt)
# print(TextOnly_yoon_txt) 

# 텍스트 내 둘 이상의 white space가 있으면 하나의 space로 압축
SingleSpace_yoon_txt <- gsub("  *", " ", TextOnly_yoon_txt)
# print(SingleSpace_yoon_txt)

# 텍스트 중간에 한글자 단어가 있으면 하나의 white space로 대체
NoOneChar_yoon_txt <- gsub(" . ", " ", SingleSpace_yoon_txt)
# print(NoOneChar_yoon_txt)

# 텍스트 마지막 문자열이 한 글자이면 하나의 white space로 대체
NoOneChar_yoon_txt <- gsub(" .$", " ", NoOneChar_yoon_txt)
# print(NoOneChar_yoon_txt)

# 다시 텍스트 내 한글자 단어가 있으면 white space로 대체
NoOneChar_yoon_txt <- gsub(" . ", " ", NoOneChar_yoon_txt)
# print(NoOneChar_yoon_txt)

# 원소 마지막 문자열이 white space 인 경우 제거
NoOneChar_yoon_txt <- gsub(" $", "", NoOneChar_yoon_txt)
# print(NoOneChar_yoon_txt)

# 텍스트 내 한글자만 존재하는 원소를 제거하면 empty line이 되었을 것이므로 다시 제거.
Final_yoon_txt <- NoOneChar_yoon_txt[NoOneChar_yoon_txt != ""]
# print(Final_yoon_txt)

# 사전은 세종 사전 사용.
useSejongDic()

# 사전에 없는 새로운 사용자 용어사전을 만듬
user_d <- data.frame(v1=c("코로나19","신축년","형사사법시스템","공수처","주무부처","N번방"," 스토킹처벌법","법무부장관",
                          "팬데믹", "코로나", "비상상황", "여러분"),
                     v2=c("ncn","ncn","ncn","ncn","ncn","ncn","ncn","ncn","ncn","ncn","ncn","ncn"))
buildDictionary(ext_dic = "sejong", user_dic = user_d, replace_usr_dic = F)

# 명사를 추출하고, unlist 함.
yoon_nouns = sapply(Final_yoon_txt, extractNoun, USE.NAMES=F)
unlisted_yoon_nouns = unlist(yoon_nouns)

# 두 글자 이상만
final_yn_nouns <- unlisted_yoon_nouns[nchar(unlisted_yoon_nouns) >= 2]
# print(final_yn_nouns)


# 텍스트 마다 필러링이 필요하므로 함수로 정의
filtering_yoon <- function(nouns){
  idx = (1:length(nouns))[nouns==""]
  idx = (1:length(nouns))[nouns==" "]
  idx = c(idx,(1:length(nouns))[nouns=="상태입니"])
  idx = c(idx,(1:length(nouns))[nouns=="마비됩니"])
  idx = c(idx,(1:length(nouns))[nouns=="상태입니"])
  idx = c(idx,(1:length(nouns))[nouns=="감사합니"])
  idx = c(idx,(1:length(nouns))[nouns=="감사드립니"])
  idx = c(idx,(1:length(nouns))[nouns=="말합니"])
  idx = c(idx,(1:length(nouns))[nouns=="있습니"])
  idx = c(idx,(1:length(nouns))[nouns=="합니"])
  idx = c(idx,(1:length(nouns))[nouns=="됩니"])
  idx = c(idx,(1:length(nouns))[nouns=="시행됩니"])
  idx = c(idx,(1:length(nouns))[nouns=="바랍니"])
  idx = c(idx,(1:length(nouns))[nouns=="아닙니"])
  idx = c(idx,(1:length(nouns))[nouns=="보장입니"])
  idx = c(idx,(1:length(nouns))[nouns=="중요합니"])
  idx = c(idx,(1:length(nouns))[nouns=="밝았습니"])
  idx = c(idx,(1:length(nouns))[nouns=="왔습니"])
  idx = c(idx,(1:length(nouns))[nouns=="것입니"])
  idx = c(idx,(1:length(nouns))[nouns=="않습니"]) 
  idx = c(idx,(1:length(nouns))[nouns=="무엇인가에"])
  idx = c(idx,(1:length(nouns))[nouns=="있었"])
  idx = c(idx,(1:length(nouns))[nouns=="겪었"])
  idx = c(idx,(1:length(nouns))[nouns=="보장하"])
  idx = c(idx,(1:length(nouns))[nouns=="가져오"])
  idx = c(idx,(1:length(nouns))[nouns=="않으"])
  idx = c(idx,(1:length(nouns))[nouns=="방식으"])
  idx = c(idx,(1:length(nouns))[nouns=="생각으"])
  idx = c(idx,(1:length(nouns))[nouns=="개정안으"])
  idx = c(idx,(1:length(nouns))[nouns=="개정만으"])
  idx = c(idx,(1:length(nouns))[nouns=="있으므"])
  idx = c(idx,(1:length(nouns))[nouns=="하므"])
  idx = c(idx,(1:length(nouns))[nouns=="어려우므"])
  idx = c(idx,(1:length(nouns))[nouns=="있으므로"])
  idx = c(idx,(1:length(nouns))[nouns=="발견되거"])
  idx = c(idx,(1:length(nouns))[nouns=="보장되도"])
  idx = c(idx,(1:length(nouns))[nouns=="되돌아보"])
  idx = c(idx,(1:length(nouns))[nouns=="판단되"])
  idx = c(idx,(1:length(nouns))[nouns=="바라보"])
  idx = c(idx,(1:length(nouns))[nouns=="다해주"])
  idx = c(idx,(1:length(nouns))[nouns=="연결되"])
  nouns = nouns [-idx]
  return(nouns)
}


filtered_yn_nouns <- filtering_yoon(final_yn_nouns)
# print(filtered_yn_nouns)

yoon_wc = table(filtered_yn_nouns)
yoon_wc = sort(yoon_wc, decreasing=T)

head(choo_wc, 10)
wordcloud2(yoon_wc, size = 1.5, color = "random-dark", rotateRatio = 0, fontWeight = "bold")




# answer to Q2.a===============================================================
library(KoNLP)
library("reader")
library(wordcloud2)

# 추미애 전 장관 2021년 신년사 화일 읽어들이기
choo_file <- file("./Data/추미애2021년 신년사.txt", blocking=F, "rt", encoding = "UTF-8")

# 파일 내 텍스틑를 line별로 읽어들이기
choo_text <- readLines(choo_file)
print(choo_text)

# 그러나, 이번에 전처리 순서를 바꾸어 코드를 더 간결하게 시도해 봄.
# 텍스트 내 영어나 한글이 아닌, 즉 텍스트가 아닌 기호는 모두 제거.
TextOnly_choo_txt <- gsub("[^A-Za-z가-힣]", " ", choo_text)
print(TextOnly_choo_txt)

# 텍스트 내 둘 이상의 white space가 있으면 하나의 space로 압축
SingleSpace_choo_txt <- gsub("  *", " ", TextOnly_choo_txt)
print(SingleSpace_choo_txt)

# 텍스트 중간에 한글자 단어가 있으면 하나의 white space로 대체,
NoOneChar_choo_txt <- gsub(" . ", " ", SingleSpace_choo_txt)
print(NoOneChar_choo_txt)

# 원소 마지막 문자열이 white space 인 경우 제거
NoOneChar_choo_txt <- gsub(" $", "", NoOneChar_choo_txt)
print(NoOneChar_choo_txt)

# 텍스트 중간에 한글자 단어가 있으면 하나의 white space로 대체,
NoOneChar_choo_txt <- gsub(" . ", " ", NoOneChar_choo_txt)
print(NoOneChar_choo_txt)

# 텍스트 내 공백 line 제거.
NoEmptyLine_choo_txt <- NoOneChar_choo_txt[NoOneChar_choo_txt != ""]
print(NoEmptyLine_choo_txt)

# 사전은 NIA 사전 사용.
useNIADic()

# 사전에 없는 새로운 사용자 용어사전을 만듬
user_d <- data.frame(v1=c("코로나19","신축년","형사사법시스템","공수처","주무부처","N번방"," 스토킹처벌법","법무부장관"),
                     v2=c("ncn","ncn","ncn","ncn","ncn","ncn","ncn","ncn"))
buildDictionary(ext_dic = "useNIA", user_dic = user_d, replace_usr_dic = F)

# 명사를 추출하고, unlist 함.
choo_nouns = sapply(NoEmptyLine_choo_txt, extractNoun, USE.NAMES=F)
unlisted_choo_nouns = unlist(choo_nouns)

# 두 글자 이상만
final_ch_nouns <- unlisted_choo_nouns[nchar(unlisted_choo_nouns) >= 2]
print(final_ch_nouns)

# 텍스트 마다 필터링이 필요하므로 함수로 정의
filtering <- function(nouns){
  idx = (1:length(nouns))[nouns==""]
  idx = (1:length(nouns))[nouns==" "]
  idx = c(idx,(1:length(nouns))[nouns=="밝았습니"])
  idx = c(idx,(1:length(nouns))[nouns=="집중해주시길"])
  idx = c(idx,(1:length(nouns))[nouns=="올립니"])
  idx = c(idx,(1:length(nouns))[nouns=="합니"])
  idx = c(idx,(1:length(nouns))[nouns=="바랍니"])
  idx = c(idx,(1:length(nouns))[nouns=="있습니"])
  idx = c(idx,(1:length(nouns))[nouns=="기원합니"])
  idx = c(idx,(1:length(nouns))[nouns=="감사합니"])
  idx = c(idx,(1:length(nouns))[nouns=="그동안"])
  idx = c(idx,(1:length(nouns))[nouns=="여러분"])
  idx = c(idx,(1:length(nouns))[nouns=="누구"])
  idx = c(idx,(1:length(nouns))[nouns=="하시"])
  idx = c(idx,(1:length(nouns))[nouns=="않길"])
  idx = c(idx,(1:length(nouns))[nouns=="첫째"])
  idx = c(idx,(1:length(nouns))[nouns=="둘째"])
  idx = c(idx,(1:length(nouns))[nouns=="셌째"])
  idx = c(idx,(1:length(nouns))[nouns=="넷째"])
  nouns = nouns [-idx]
  return(nouns)
}

filtered_ch_nouns <- filtering(final_ch_nouns)
print(filtered_ch_nouns)

choo_wc = table(filtered_ch_nouns)
choo_wc = sort(choo_wc, decreasing=T)

# head(choo_wc, 10)
wordcloud2(choo_wc, size = 1.5, color = "random-dark", rotateRatio = 0, fontWeight = "bold")



# answer to Q2.b===============================================================

# 윤석윤 전 검찰총장 2021년 신년사 화일 읽어들이기
yoon_file <- file("./Data/윤석열2021년신년사.txt", blocking=F, "rt", encoding = "UTF-8")

# 파일 내 텍스틑를 line별로 읽어들이기
yoon_text <- readLines(yoon_file)

# 라인별로 읽어드린 텍스트 화일을 살펴보니, 텍스트가 존재하지 않는 원소가 있음
# print(yoon_text)

# 그러므로, empty line을 제거함
NoEmptyLine_yoon_txt <- yoon_text[yoon_text != ""]
# print(NoEmptyLine_yoon_txt) 

# 텍스트 내 영어나 한글이 아닌, 즉 텍스트가 아닌 기호는 모두 white space로 대체.
TextOnly_yoon_txt <- gsub("[^A-Za-z가-힣]", " ", NoEmptyLine_yoon_txt)
# print(TextOnly_yoon_txt) 

# 텍스트 내 둘 이상의 white space가 있으면 하나의 space로 압축
SingleSpace_yoon_txt <- gsub("  *", " ", TextOnly_yoon_txt)
# print(SingleSpace_yoon_txt)

# 텍스트 중간에 한글자 단어가 있으면 하나의 white space로 대체
NoOneChar_yoon_txt <- gsub(" . ", " ", SingleSpace_yoon_txt)
# print(NoOneChar_yoon_txt)

# 텍스트 마지막 문자열이 한 글자이면 하나의 white space로 대체
NoOneChar_yoon_txt <- gsub(" .$", " ", NoOneChar_yoon_txt)
# print(NoOneChar_yoon_txt)

# 다시 텍스트 내 한글자 단어가 있으면 white space로 대체
NoOneChar_yoon_txt <- gsub(" . ", " ", NoOneChar_yoon_txt)
# print(NoOneChar_yoon_txt)

# 원소 마지막 문자열이 white space 인 경우 제거
NoOneChar_yoon_txt <- gsub(" $", "", NoOneChar_yoon_txt)
# print(NoOneChar_yoon_txt)

# 텍스트 내 한글자만 존재하는 원소를 제거하면 empty line이 되었을 것이므로 다시 제거.
Final_yoon_txt <- NoOneChar_yoon_txt[NoOneChar_yoon_txt != ""]
# print(Final_yoon_txt)

# 사전은 세종 사전 사용.
useSejongDic()

# 사전에 없는 새로운 사용자 용어사전을 만듬
user_d <- data.frame(v1=c("코로나19","신축년","형사사법시스템","공수처","주무부처","N번방"," 스토킹처벌법","법무부장관",
                          "팬데믹", "코로나", "비상상황", "여러분"),
                     v2=c("ncn","ncn","ncn","ncn","ncn","ncn","ncn","ncn","ncn","ncn","ncn","ncn"))
buildDictionary(ext_dic = "sejong", user_dic = user_d, replace_usr_dic = F)

# 명사를 추출하고, unlist 함.
yoon_nouns = sapply(Final_yoon_txt, extractNoun, USE.NAMES=F)
unlisted_yoon_nouns = unlist(yoon_nouns)

# 두 글자 이상만
final_yn_nouns <- unlisted_yoon_nouns[nchar(unlisted_yoon_nouns) >= 2]
# print(final_yn_nouns)


# 텍스트 마다 필러링이 필요하므로 함수로 정의
filtering_yoon <- function(nouns){
  idx = (1:length(nouns))[nouns==""]
  idx = (1:length(nouns))[nouns==" "]
  idx = c(idx,(1:length(nouns))[nouns=="상태입니"])
  idx = c(idx,(1:length(nouns))[nouns=="마비됩니"])
  idx = c(idx,(1:length(nouns))[nouns=="상태입니"])
  idx = c(idx,(1:length(nouns))[nouns=="감사합니"])
  idx = c(idx,(1:length(nouns))[nouns=="감사드립니"])
  idx = c(idx,(1:length(nouns))[nouns=="말합니"])
  idx = c(idx,(1:length(nouns))[nouns=="있습니"])
  idx = c(idx,(1:length(nouns))[nouns=="합니"])
  idx = c(idx,(1:length(nouns))[nouns=="됩니"])
  idx = c(idx,(1:length(nouns))[nouns=="시행됩니"])
  idx = c(idx,(1:length(nouns))[nouns=="바랍니"])
  idx = c(idx,(1:length(nouns))[nouns=="아닙니"])
  idx = c(idx,(1:length(nouns))[nouns=="보장입니"])
  idx = c(idx,(1:length(nouns))[nouns=="중요합니"])
  idx = c(idx,(1:length(nouns))[nouns=="밝았습니"])
  idx = c(idx,(1:length(nouns))[nouns=="왔습니"])
  idx = c(idx,(1:length(nouns))[nouns=="것입니"])
  idx = c(idx,(1:length(nouns))[nouns=="않습니"]) 
  idx = c(idx,(1:length(nouns))[nouns=="무엇인가에"])
  idx = c(idx,(1:length(nouns))[nouns=="있었"])
  idx = c(idx,(1:length(nouns))[nouns=="겪었"])
  idx = c(idx,(1:length(nouns))[nouns=="보장하"])
  idx = c(idx,(1:length(nouns))[nouns=="가져오"])
  idx = c(idx,(1:length(nouns))[nouns=="않으"])
  idx = c(idx,(1:length(nouns))[nouns=="방식으"])
  idx = c(idx,(1:length(nouns))[nouns=="생각으"])
  idx = c(idx,(1:length(nouns))[nouns=="개정안으"])
  idx = c(idx,(1:length(nouns))[nouns=="개정만으"])
  idx = c(idx,(1:length(nouns))[nouns=="있으므"])
  idx = c(idx,(1:length(nouns))[nouns=="하므"])
  idx = c(idx,(1:length(nouns))[nouns=="어려우므"])
  idx = c(idx,(1:length(nouns))[nouns=="있으므로"])
  idx = c(idx,(1:length(nouns))[nouns=="발견되거"])
  idx = c(idx,(1:length(nouns))[nouns=="보장되도"])
  idx = c(idx,(1:length(nouns))[nouns=="되돌아보"])
  idx = c(idx,(1:length(nouns))[nouns=="판단되"])
  idx = c(idx,(1:length(nouns))[nouns=="바라보"])
  idx = c(idx,(1:length(nouns))[nouns=="다해주"])
  idx = c(idx,(1:length(nouns))[nouns=="연결되"])
  nouns = nouns [-idx]
  return(nouns)
}


filtered_yn_nouns <- filtering_yoon(final_yn_nouns)
# print(filtered_yn_nouns)

yoon_wc = table(filtered_yn_nouns)
yoon_wc = sort(yoon_wc, decreasing=T)

head(choo_wc, 10)
wordcloud2(yoon_wc, size = 1.5, color = "random-dark", rotateRatio = 0, fontWeight = "bold")


# 결론: NIA 사전에 등록된 단어 수가 세종 단어 대비 약 3배에 가까우나, 결과는 유사함. 그러므로 사전의 단어 수에 영향을 크게 받지 않음.
# 사전보다는 사용자 정의 단어집, 불용어 사전이 전처리 과정 및 텍스트분석에는 더욱 중요.
# 추미애 전 장관보다는 윤석열 전 총장 사용 단어 수가 약 2배가량으로 사용 단어가 풍부함.