library(readxl)
library(tidyverse)

sheets <- excel_sheets("~/Downloads/2025term34.xlsx")
print(sheets)
data <- read_excel("~/Downloads/2025term34.xlsx", sheet = "1.5_p.21")
data <- data[-c(0:3),]
view(data)

data_clean <- data %>%
  slice(5:25) %>%
  rename(구분 = 1)

colnames(data_clean) <- c("구분", "비고",
                          "1분위_금액", "1분위_증감률",
                          "2분위_금액", "2분위_증감률",
                          "3분위_금액", "3분위_증감률",
                          "4분위_금액", "4분위_증감률",
                          "5분위_금액", "5분위_증감률"
                          )

data_clean <- data_clean %>%
  select(구분, contains("금액")) %>%
  mutate(across(-구분, as.numeric))


data_long <- data_clean %>%
  pivot_longer(cols = -구분,
               names_to = "분위",
               values_to = "지출액") %>%
  mutate(분위 = str_remove(분위, "_금액"))

data_long <- data_long %>%
  mutate(지출액 = as.numeric(지출액))

view(data_long)

story_data <- data_long %>%
  filter(구분 %in% c("주거 · 수도 · 광열", "오락 · 문화", "교   육"))

view(story_data)

ggplot(story_data, aes(x = 구분, y = 지출액, fill = 분위)) +
  geom_col(position = "dodge") + coord_flip() + theme_classic() +
  theme(text = element_text(family = "AppleGothic"))

target_categories <- c("식료품 · 비주류음료", "주거 · 수도 · 광열", "의류 · 신발", "오락 · 문화", "교   육")
total_spending <- data_long %>%
  filter(구분 == "소비지출") %>%
  select(분위, 전체지출 = 지출액)

df_ratio <- data_long %>%
  filter(구분 %in% target_categories) %>%
  left_join(total_spending, by = "분위") %>%
  mutate(비율 = (지출액 / 전체지출) * 100)

ggplot(df_ration, aes(x = 분위, y = 비율, fill = 구분)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() + theme(text = element_text(family = "AppleGothic"))
