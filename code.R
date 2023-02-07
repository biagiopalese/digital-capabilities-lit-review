library(tidyverse)
library(readxl)
library(ggplot2)
library(janitor)
library(scales)
#library(openxlsx)

raw_scopus <- read_excel("data.xlsx", sheet = "Scopus", range = "A1:R1102", na = "") %>% clean_names()
raw_scopus %>% glimpse()
raw_scopus %>% count(field, sort = TRUE, ) %>% tally(n)

raw_classified <- read_excel("data.xlsx", sheet = "Definition classification", range = "A1:U681", na = "") %>% 
  clean_names() %>% mutate(article_number = str_sub(string = id, start = 1, end = 6))
raw_classified %>% glimpse()
raw_classified %>% distinct(article_number) %>% tally()
raw_classified %>% nrow()
raw_classified %>% count(str_to_lower(explicit_definition_y_n)) %>% 
  filter(`str_to_lower(explicit_definition_y_n)` == "y") %>% pull(n)
raw_classified %>% select(article_number, type_of_construct) %>% filter(type_of_construct != "Excluded") %>% 
  distinct(article_number) %>% tally() %>% pull(n)

constructs <- c("Organizational capability", "IT capability",
                "IT-enabled capability", "Digital capability", "Excluded")

# constructs <- c("Organizational capabilities", "IT capabilities", 
#                 "IT-enabled capabilities", "Digital capabilities", "Excluded")

df <-
  raw_scopus %>% 
  mutate(discipline = case_when(field == "ACCOUNTING" ~ "Accounting",
                                field == "FINANCE" ~ "Finance",
                                field == "INFO MAN" ~ "Information Systems",
                                field == "MARKETING" ~ "Marketing",
                                str_detect(field, "OPERATION") ~ "Operations",
                                (field == "ENTREPRENEURSHIP" | field == "IB") ~ "Entrepreneurship and IB",
                                (field == "HR" | field == "ORGANISATION STUDIES" | field == "MANAGEMENT") ~ "Management",
                                (field == "STRATEGY" | field == "INNOVATION") ~ "Strategy and Innovation",
                                .default = NA)) 

df_classified <-
  raw_classified %>% 
  mutate(discipline = case_when(field == "ACCOUNTING" ~ "Accounting",
                                field == "FINANCE" ~ "Finance",
                                field == "INFO MAN" ~ "Information Systems",
                                field == "MARKETING" ~ "Marketing",
                                str_detect(field, "OPERATION") ~ "Operations",
                                (field == "ENTREPRENEURSHIP" | field == "IB") ~ "Entrepreneurship and IB",
                                (field == "HR" | field == "ORGANISATION STUDIES" | field == "MANAGEMENT") ~ "Management",
                                (field == "STRATEGY" | field == "INNOVATION") ~ "Strategy and Innovation",
                                .default = NA)) %>% 
           mutate(construct = factor(type_of_construct, levels = constructs, ordered = TRUE), .keep = "all")

df %>% filter(is.na(discipline)) %>% count(discipline, sort = TRUE)
df %>% count(discipline, sort = TRUE)

df %>% count(discipline, included) %>% spread(key = included, value = n) %>% 
  mutate(percentage = percent(Yes / (No + Yes), accuracy = 0.1))

df %>% distinct(discipline, field) %>% arrange(discipline)

#Articles and Journals
df %>% distinct(source_title, discipline, rating) %>% count(discipline, rating) %>% 
  spread(key = rating, value = n) %>% arrange(discipline)

#http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization
df %>% filter(year != "2023") %>% 
  group_by(discipline) %>% 
  count(year, sort = TRUE) %>%
  ggplot(aes(x=year, y=n, group=discipline)) +
  geom_line(aes(linetype=discipline, color=discipline)) +
  theme(legend.position="bottom")

df_classified %>% select(article_number, construct) %>% filter(construct != "Excluded") %>% 
  distinct(article_number) %>% tally() %>% pull(n)

df_classified %>% filter(construct != "Excluded") %>% select(article_number, discipline) %>% 
  distinct(article_number, discipline) %>% count(discipline) %>% arrange(discipline)

#Constructs
df_classified %>% count(construct, name = "Frequency") %>% 
  arrange(construct)

df_classified %>% filter(construct != "Excluded") %>% tally() %>% pull(n)

df_classified %>% filter(construct != "Excluded") %>% select(construct, discipline) %>% 
  count(discipline) %>% arrange(discipline)

df_classified %>% filter(year != "2023") %>% 
  select(year, construct, discipline) %>% 
  group_by(discipline) %>% 
  count(year, sort = TRUE) %>%
  ggplot(aes(x=year, y=n, group=discipline)) +
  geom_line(aes(linetype=discipline, color=discipline)) +
  theme(legend.position="top") +
  theme(legend.title=element_blank())

df_classified %>% select(year, construct, discipline) %>% 
  filter(year != "2023") %>% 
  filter(construct != "Excluded") %>%
  group_by(construct) %>% 
  count(year, sort = TRUE) %>%
  ggplot(aes(x=year, y=n, group=construct)) +
  geom_line(aes(linetype=construct, color=construct)) +
  theme(legend.position="top") +
  theme(legend.title=element_blank())

#Article construct names versus our categorization
df_mutated <- df_classified %>% 
  mutate(construct_name_clean= str_replace_all(construct_name,  
                                               c("capabilities"= "capability","capabilites"= "capability",
                                                 "competencies"= "competence", "infrastructures"= "infrastructure",
                                                 "abilities"= "ability", "analytics"= "analytic", "IT" = "InformationTechnology", 
                                                 "IS"= "InformationSystems", "spannig" ="spanning")), 
         construct_def_clean= str_replace_all(construct_definition,  c("capabilities"= "capability","capabilites"= "capability", 
                                                                       "competencies"= "competence", 
                                                                       "infrastructures"= "infrastructure",
                                                                       "abilities"= "ability", "analytics"= "analytic", 
                                                                       "IT" = "InformationTechnology", "IS"= 
                                                                         "InformationSystems","spannig" ="spanning"))) %>%
  mutate(construct_name_clean= str_replace_all(construct_name_clean, "[:digit:]", ""), 
         construct_def_clean= str_replace_all(construct_def_clean, "[:digit:]", "")) %>% 
  mutate(construct_name_clean= str_to_lower(construct_name_clean, locale = "en"), 
         construct_def_clean=str_to_lower(construct_def_clean, locale = "en")) 

df_classified %>% group_by(construct) %>% tally()
df_mutated %>% group_by(construct) %>% tally()
df_mutated %>% select(construct_name_clean) %>% filter(is.na(construct_name_clean))

##Number of constructs in each classification
df_mutated %>% filter(construct != "Excluded") %>% 
  group_by(construct) %>% 
  count(construct_name_clean) %>% tally()

##Number of constructs in each classification per discipline
df_mutated %>% filter(construct != "Excluded") %>% 
  group_by(construct, discipline) %>% 
  count(construct_name_clean) %>% tally()

##Number of constructs in each classification per discipline
df_mutated %>% filter(construct != "Excluded") %>% 
  group_by(construct_name_clean, discipline) %>% tally() %>% arrange(desc(n)) %>% 
  spread(key = discipline, value = n) %>% View()
  mutate(percentage = percent(Yes / (No + Yes), accuracy = 0.1))

##Number of constructs in each discipline
  df_mutated %>% filter(construct != "Excluded") %>% 
  group_by(discipline, construct, construct_name_clean) %>% tally() %>% arrange(desc(n), .by_group = TRUE)

  df_mutated %>% filter(construct != "Excluded") %>% 
  group_by(construct_name_clean, construct, discipline) %>% tally() %>% arrange(desc(n), .by_group = TRUE) %>% View()



##Biagio's bigram analyis
library(tidytext)
library(yardstick)
library(igraph)
library(ggraph)

df_mutated <- df_classified %>% 
  mutate(construct_name_clean= str_replace_all(construct_name,  
                                               c("capabilities"= "capability","capabilites"= "capability",
                                                 "competencies"= "competence", "infrastructures"= "infrastructure",
                                                 "abilities"= "ability", "analytics"= "analytic", "IT" = "InformationTechnology", 
                                                 "IS"= "InformationSystems", "spannig" ="spanning")), 
         construct_def_clean= str_replace_all(construct_definition,  c("capabilities"= "capability","capabilites"= "capability", 
                                                                       "competencies"= "competence", 
                                                                       "infrastructures"= "infrastructure",
                                                                       "abilities"= "ability", "analytics"= "analytic", 
                                                                       "IT" = "InformationTechnology", "IS"= 
                                                                         "InformationSystems","spannig" ="spanning"))) %>%
  mutate(construct_name_clean= str_replace_all(construct_name_clean, "[:digit:]", ""), 
         construct_def_clean= str_replace_all(construct_def_clean, "[:digit:]", "")) %>% 
  mutate(construct_name_clean= str_to_lower(construct_name_clean, locale = "en"), 
         construct_def_clean=str_to_lower(construct_def_clean, locale = "en")) 


#Create the bigrams
df_bigrams <- df_mutated %>%
  unnest_tokens(bigram, construct_name_clean, token = "ngrams", n = 2)%>%
  filter(!is.na(bigram))

df_bigrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- df_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_tf_idf <- bigrams_united %>%
  count(construct_name, bigram) %>%
  bind_tf_idf(bigram, construct_name, n) %>%
  arrange(desc(tf_idf))

bigram_graph <- bigram_counts %>%
  filter(n > 1) %>%
  graph_from_data_frame()


#Create bigram graph
set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.1, 'inches')) +
  geom_node_point(color = "lightblue", size = 2) +
  geom_node_text(aes(label = name), vjust = 0.5, hjust = 0.5, check_overlap = T) +
  theme_void()# repeat analysis on the definitions

bigrams_united

bigram_counts

bigram_graph

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.1, 'inches')) +
  geom_node_point(color = "red", size = 2) +
  geom_node_text(aes(label = name), vjust = 0.5, hjust = 0.5, check_overlap = T) +
  theme_void()# repeat analysis on the definitions



