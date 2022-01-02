# 1인당 전기 생산량 데이터 파일: electricity_generation_per_person.csv 가 업로드된 data에 포함되어 있습니다.
# 화일을 읽어들이면, 총 65개 국가에 대해 33년 (1985-2016) 동안 1인당 전기 생산량이 기록되어 있지만, column 이름이 되는 
# [연도] 속성에 X 문자가 의도치 않게 추가되어 있음을 알 수 있습니다. 텍스트 인코딩 문제로 가끔 이런 일이 발생합니다.

# Q1.[10점] names() 와 substr 수를 사용하면 X문자를 제거하고,
# View() 함수룰 사용해 정리된 결과를 확인할 수 있는 코드를 제시하십시오.

getwd()
elec_gen = read.csv(".\\Data\\electricity_generation_per_person.csv", header = TRUE, sep = ",")
View(elec_gen)
names(elec_gen)
names(elec_gen)[2:33] = substr(names(elec_gen)[2:33], 2, nchar(names(elec_gen)[2:33]))
#names(elec_gen) = substr(names(elec_gen), 2, nchar(names(elec_gen)))
names(elec_gen)

# 같은 방법으로 electricity_use_per_person.csv 파일을 읽어들이면,
# 총 138개국의 56년동안 (1960-2014) 1인당 전기사용량이 기록되어 있음을 알 수 있습니다.

# Q2.[10점] names() 와 substr 수를 사용하면 불필요한 문자를 제거하고,
# View() 함수룰 사용해 정리된 결과를 확인할 수 있도록 코딩하십시오.

elec_use = read.csv(".\\Data\\electricity_use_per_person.csv", header = TRUE, sep = ",")
names(elec_use)
names(elec_use)[2:56] = substr(names(elec_use)[2:56], 2, nchar(names(elec_use)[2:56]))
names(elec_use)
View(elec_use)

# 위 데이터 프레임은 이제까지 다루었던 일반 데이터프레임 구성, 즉 속성 하나를 열 하나에 배치하고, 행 하나가 
# 샘플하나로 채워진 구성이 아닙니다. 각 측정 연도 값이 열의 이름이 되어 여러 해에 걸친 값이 하나의 행을 구성하고 있습니다.
# 국가명, 연도, 전기사용량과 소비량을 각각 하나의 열에 대응시키는 변형이 필요합니다.
# "tidyr" package를 사용해, gather() 함수를 사용하면 "year"를 key에, 측정값의 속성이름을 value에
# 지정하는 새로운 데이터 프레임을 구성할 수 있습니다.

# Q3.[20점] elec_gen_df, elec_use_df 데이터 프레임을 구성하는 코드를 제시하고,
# 두 데이터 프레임을 공통 column과 row names를 기반으로 merge 하는 코드도 제시하시오.

#install.packages("tidyverse")
library(tidyr)
elec_gen_df = gather(elec_gen, -country, key = "year", value = "ElectricityGeneration")
elec_use_df = gather(elec_use, -country, key  = "year", value = "ElectricityUse")
elec_gen_use = merge(elec_gen_df, elec_use_df)
View(elec_gen_use)

# Q4.[20점] gapminder 사이트에서 Home > Resources > Data [download the data] > Education > Literacy 에 있는
# 5 종류의 관측 데이터 set과 인터넷에서 literacy_rate_adult_female_percent_of_females_ages_15_above.csv
# (1975-2011)를 찾아, 총 6 종류의 관측 데이터 set을 읽어들이는 코드를 제시하시오.
total_15_24 = read.csv(".\\Data\\literacy_rate_youth_total_percent_of_people_ages_15_24.csv", header = TRUE, sep = ",")
male_15_24 = read.csv(".\\Data\\literacy_rate_youth_male_percent_of_males_ages_15_24.csv", header = TRUE, sep = ",")
female_15_24 = read.csv(".\\Data\\literacy_rate_youth_female_percent_of_females_ages_15_24.csv", header = TRUE, sep = ",")
total_15_above = read.csv(".\\Data\\literacy_rate_adult_total_percent_of_people_ages_15_and_above.csv", header = TRUE, sep = ",")
male_15_above = read.csv(".\\Data\\literacy_rate_adult_male_percent_of_males_ages_15_and_above.csv", header = TRUE, sep = ",")
female_15_above = read.csv(".\\Data\\literacy_rate_adult_female_percent_of_females_ages_15_above.csv", header = TRUE, sep = ",")
#names(total_15_24)
#View(total_15_24)

# Q5.[40점] 결측치가 있는 year는 제외하고, 국가별, 연도별 "adult_female", "adult_male",
# "adult_total", "youth_female", "youth_male", "youth_total" 순서로 literacy 를 보여주는 코드를 제시하시오.
# 최종 결과에 불필요한 문자나 문자열이 있다면 제거되어야 합니다. 최종 결과는 543 obs. of 8 variables가 도출됩니다. 

female_15_24_df = gather(female_15_24, -country, key = "year", value = "adult_female")
male_15_24_df = gather(male_15_24, -country, key = "year", value = "adult_male")
total_15_24_df = gather(total_15_24, -country, key = "year", value = "adult_total")
female_15_above_df = gather(female_15_above, -country, key = "year", value = "youth_female")
male_15_above_df = gather(male_15_above, -country, key = "year", value = "youth_male")
total_15_above_df = gather(total_15_above, -country, key = "year", value = "youth_total")

merge_rate1 = merge(female_15_24_df, male_15_24_df)
merge_rate2 = merge(total_15_24_df,female_15_above_df)
merge_rate3 = merge(male_15_above_df,total_15_above_df)
merge_rate4 = merge(merge_rate1, merge_rate2)
merge_rate5 = merge(merge_rate4, merge_rate3)
#View(merge_rate5)
merge_rate <- na.omit(merge_rate5)
View(merge_rate)

