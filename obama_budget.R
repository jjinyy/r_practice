
#install.packages("treemap")
library(xtable)
library(dplyr)
library(treemap)
#setwd("C:/Users/jjjiny/Downloads")
#getwd()
outlays <- read.csv("BUDGET-2017-DB-2.csv",stringsAsFactors = FALSE)
outlays$X2017 <- gsub(",","", outlays$X2017)
outlays$X2017 <- as.numeric(outlays$X2017)

spending <- outlays %>% select(Agency.Name, Bureau.Name, Account.Name, X2017) %>%
  group_by(Agency.Name, Bureau.Name, Account.Name) %>%
  summarize(X2017 = sum(X2017, na.rm=FALSE)) %>%
  filter(X2017 > 0)

#그래프1
treemap(spending,
        index=c("Agency.Name","Bureau.Name","Account.Name"),
        vSize = "X2017",
        type="index",
        palette = "Reds",
        title="Spending in President Obama's 2017 Budget",
        fontsize.title = 14
)

#그래프2
receipts <- outlays %>% select(Agency.Name, Bureau.Name, Account.Name, X2017) %>%
  group_by(Agency.Name, Bureau.Name, Account.Name) %>%
  summarize(X2017 = sum(X2017, na.rm=FALSE)) %>%
  filter(X2017 < 0) %>%
  mutate(X2017 = abs(X2017))
treemap(receipts, #Your data frame object
        index=c("Agency.Name","Bureau.Name","Account.Name"),
        vSize = "X2017",
        type="index",
        palette = "Greens",
        title="Receipts in President Obama's 2017 Budget",
        fontsize.title = 14
)

