library(tidyverse)
library(readxl)
library(ggplot2)
library(janitor)
library(scales)
#library(openxlsx)

raw_data <- read_excel("data.xlsx", sheet = "Scopus", range = "A1:R1102", na = "") %>% clean_names()
raw_data %>% glimpse()

raw_data %>% count(field, sort = TRUE, ) %>% tally(n)

df <-
  raw_data %>% 
  mutate(discipline = case_when(field == "ACCOUNTING" ~ "Accounting",
                                field == "FINANCE" ~ "Finance",
                                field == "INFO MAN" ~ "Information Systems",
                                field == "MARKETING" ~ "Marketing",
                                str_detect(field, "OPERATION") ~ "Operations",
                                (field == "ENTREPRENEURSHIP" | field == "IB") ~ "Entrepreneurship and IB",
                                (field == "HR" | field == "ORGANISATION STUDIES" | field == "MANAGEMENT") ~ "Management",
                                (field == "STRATEGY" | field == "INNOVATION") ~ "Strategy and Innovation",
                                .default = NA)) 

df %>% filter(is.na(discipline)) %>% count(discipline, sort = TRUE)
df %>% count(discipline, sort = TRUE)

df %>% count(discipline, included) %>% spread(key = included, value = n) %>% 
  mutate(percentage = percent(Yes / (No + Yes), accuracy = 0.1))

df %>% distinct(discipline, field) %>% arrange(discipline)

df %>% distinct(source_title, discipline, rating) %>% count(discipline, rating) %>% 
  spread(key = rating, value = n) %>% arrange(discipline)

#http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization
df %>% filter(year != "2023") %>% 
  group_by(discipline) %>% 
  count(year, sort = TRUE) %>%
  ggplot(aes(x=year, y=n, group=discipline)) +
  geom_line(aes(linetype=discipline, color=discipline)) +
  theme(legend.position="bottom")





predf <- clean_names(csv)
predf <- predf %>% filter(orig_term %in% semesters)

df <- predf %>% 
  pivot_longer(cols = c('x20131s':'x20223s'))

df <-
  df %>% 
  mutate(name = str_remove(name, pattern = 'x')) %>% 
  mutate(name = str_remove(name, pattern = 's')) %>% 
  separate(col = name, into = c('year', 'semester'), sep = 4) %>% 
  group_by(record_nbr) %>% 
  mutate(test = ! is.na(value)) %>% 
  mutate(cum_test = cumsum(test)) %>% 
  mutate(max_test = max(cum_test)) %>% 
  filter(cum_test > 0) %>% 
  filter(! ((cum_test == max_test) & (test == FALSE)) ) %>% 
  ungroup() %>% 
  select(-test, -cum_test, -max_test) %>% 
  group_by(record_nbr) %>% 
  mutate(semester_seq = 1:length(value)) %>% 
  pivot_wider(id_cols = c(record_nbr, orig_term, degree_term), names_from = semester_seq, values_from = value) %>% 
  ungroup()

glimpse(df)

load(file="updateDF.RData")

glimpse(df_test)
df <- df_test %>% ungroup() %>% mutate(degree_year = str_sub(degree_term, 1, 4)) %>% 
  mutate(corrected_first_semester = str_trim(`1`, side = "both"), 
         corrected_end_major = str_trim(end_major, side = "both"))

df %>% filter(degree_term != "not graduated", end_major == "ISDS") %>% 
  count(degree_year, sort = FALSE) %>% View()

df %>% filter(degree_term != "not graduated", corrected_end_major == "ISDS") %>% 
  count(corrected_first_semester, sort = TRUE) %>% 
  mutate(percent = n / sum(n) * 100) %>% View()

df %>% filter(degree_term != "not graduated", corrected_end_major == "ISDS") %>% 
  group_by(degree_year) %>% 
  count(corrected_first_semester, sort = TRUE) %>% 
  slice_max(n, n = 3) %>% View()

df <- read_excel("BS ISDS Grads_2017-2021_Nanine UPDATES.xlsx", sheet = "Sheet1", range = "A1:R343", 
                 col_types = c(rep("text", 3), "date", rep("text", 14)), na = "") %>% clean_names()
df %>% glimpse()


df %>% count(company, sort = TRUE, ) %>% filter(!is.na(company)) %>% writeData(wb, sheet = 1, .)

df %>% count(first_company, sort = TRUE) %>% filter(!is.na(first_company)) %>% writeData(wb, sheet = 2, .)

df %>% count(company, current_role, sort = TRUE) %>% filter(!is.na(company)) %>% writeData(wb, sheet = 3, .)

df %>% count(first_company, first_role, sort = TRUE) %>% filter(!is.na(first_company)) %>% writeData(wb, sheet = 4, .)

saveWorkbook(wb, "recrutiers.xlsx", overwrite = TRUE)