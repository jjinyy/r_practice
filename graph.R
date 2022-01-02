#1
#install.packages("ggplot2")
library(ggplot2)                             
midwest <- read.csv("https://goo.gl/G1K41K")
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(0, 0.1)) + 
  ylim(c(0, 500000)) + 
  labs(subtitle="Area Vs Population", 
       y="Population", 
       x="Area", 
       title="Scatterplot", 
       caption = "Source: midwest")

plot(gg)

#2
#install.packages("ggpubr")
library(ggpubr)
data("mtcars")
dfm <- mtcars
dfm$cyl <- as.factor(dfm$cyl)
dfm$name <- rownames(dfm)
head(dfm[, c("name", "wt", "mpg", "cyl")])
ggdotchart(dfm, x = "name", y = "mpg",
           color = "cyl",
           palette = c("#00AFBB", "#E7B800", "#FC4E07"),
           sorting = "ascending",
           add = "segments",
           add.params = list(color = "lightgray", size = 2),
           group = "cyl",
           dot.size = 6,
           ggtheme = theme_pubr()
)+
  geom_hline(yintercept = 0, linetype = 2, color = "lightgray")

#3
library(tidyr)
library(dplyr)
data(economics)
df <- economics %>%
  select(date, psavert, uempmed) %>%
  gather(key = "variable", value = "value", -date)
ggplot(df, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

#4
#install.packages("plotly")
library(plotly)
data1 <- rnorm(100, mean = 10)
data2 <- rnorm(100, mean = 0)
data3 <- rnorm(100, mean = -10)
x <- c(1:100)

data <- data.frame(x, data1, data2, data3)

fig <- plot_ly(data, x = ~x)
fig <- fig %>% add_trace(y = ~data1, name = 'data 1',mode = 'lines')
fig <- fig %>% add_trace(y = ~data2, name = 'data 2', mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~data3, name = 'data 3', mode = 'markers')
fig