
install.packages("gapminder")
library(gapminder) 
library(dplyr)
glimpse(gapminder)

gapminder[, c("country", "year", "lifeExp")]

gapminder[gapminder$country == "Korea, Rep.", c("year", "lifeExp","pop")]

summarise(group_by(gapminder, continent, country), lifeExp_avg = mean(lifeExp), pop_avg = mean(pop))

gapminder %>% group_by(continent, country) %>% summarise(lifeExp_avg = mean(lifeExp), pop_avg = mean(pop))

temp1 = filter(gapminder, country == "Korea, Dem. Rep.")  
temp1 = filter(gapminder, country == "Korea, Rep.")      
temp2 = select(temp1, country, year, lifeExp)  
temp3 = apply(temp2[ , c("lifeExp")], 2, mean)
temp3

subset(gapminder, year < 1990 & country == "Korea, Rep.", select = c(country, lifeExp)) %>% summarize(avg_lifeExp = mean(lifeExp))
subset(gapminder, year < 1990 & country == "Korea, Dem. Rep.", select = c(country, lifeExp)) %>% summarize(avg_lifeExp = mean(lifeExp))

subset(gapminder, year > 1990 & country == "Korea, Rep.", select = c(country, lifeExp)) %>% summarize(avg_lifeExp = mean(lifeExp))
subset(gapminder, year > 1990 & country == "Korea, Dem. Rep.", select = c(country, lifeExp)) %>% summarize(avg_lifeExp = mean(lifeExp))

# gapminder %>% filter(country == "Korea, Rep.") %>% select(country, year, lifeExp) %>% summarise(lifeExp_avg = mean(lifeExp))
# gapminder %>% filter(country == "Korea, Dem. Rep.") %>% select(country, year, lifeExp) %>% summarise(lifeExp_avg = mean(lifeExp))
