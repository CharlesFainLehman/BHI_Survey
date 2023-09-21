library(tidyverse)
library(stargazer)
library(spatstat.geom)

bhi <- read.csv("dat/BHI_Responses_With_Weights_and_BHI_categories.csv")

black_n = 1050
non_black_n = 529

bhi %>%
  select(wt, Black, IDed.BHI, BHI.Believer, starts_with("QHit")) %>%
  pivot_longer(cols = starts_with("QHit"), names_to = "Question", values_to = "Response") %>%
  mutate(Response = ifelse(Response %in% c("Somewhat support", "Strongly support"), "Support", "Do Not Support"),
         Question = case_when(Question == "QHit1" ~ "Protest",
                              Question == "QHit2" ~ "Drunk",
                              Question == "QHit3" ~ "Beating",
                              Question == "QHit4" ~ "Break in")) %>%
  pivot_longer(cols = c(IDed.BHI, BHI.Believer), names_to = "Identification", values_to = "ID_Response") %>%
  mutate(ID_Response = ifelse(ID_Response == T, "Yes", "No"),
         Identification = ifelse(Identification == "IDed.BHI", "Self-IDed BHI", "BHI Believer")) %>%
  group_by(Black, Identification, ID_Response, Question, Response) %>%
  summarise(wtd.n = sum(wt)) %>%
  mutate(p = wtd.n/sum(wtd.n),
         se = ifelse(Black == "Black", 1.96 * sqrt((p * (1-p))/black_n), 1.96 * sqrt((p * (1-p))/non_black_n))) %>%
  filter(Response == "Support") %>%
  ggplot(aes(x=Question, y = p, color = ID_Response)) + 
  geom_point(position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = p - se, ymax = p + se), width = 0.1, position = position_dodge(width = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(cols = vars(Black), rows = vars(Identification)) +
  theme(panel.border = element_rect(color = '#aeb0b7', fill = NA)) + 
  labs(y = "Share Somewhat/Strongly Support", color = "Identified in Group")

ggsave("img/plot6.png", width = 8, height = 5)

bhi %>%
  filter(Black == "Black") %>%
  mutate(across(starts_with("QHit"), ~ ifelse(.x %in% c("Strongly support", "Somewhat support"), 1, 0)),
         HitTotal = QHit1 + QHit2 + QHit3 + QHit4) %>%
  lm(HitTotal ~ BHI.Believer, data = ., weights = wt) %>%
  summary()
  

bhi %>%
  select(wt, Black, IDed.BHI, BHI.Believer, starts_with("QVio")) %>%
  pivot_longer(cols = starts_with("QVio"), names_to = "Question", values_to = "Response") %>%
  mutate(Response = ifelse(Response %in% c("Somewhat support", "Strongly support"), "Support", "Do Not Support"),
         Question = case_when(Question == "QVio1" ~ "Advance Goals",
                              Question == "QVio2" ~ "Rebellion",
                              Question == "QVio3" ~ "Discrimination")) %>%
  pivot_longer(cols = c(IDed.BHI, BHI.Believer), names_to = "Identification", values_to = "ID_Response") %>%
  mutate(ID_Response = ifelse(ID_Response == T, "Yes", "No"),
         Identification = ifelse(Identification == "IDed.BHI", "Self-IDed BHI", "BHI Believer")) %>%
  group_by(Black, Identification, ID_Response, Question, Response) %>%
  summarise(wtd.n = sum(wt)) %>%
  mutate(p = wtd.n/sum(wtd.n),
         se = ifelse(Black == "Black", 1.96 * sqrt((p * (1-p))/black_n), 1.96 * sqrt((p * (1-p))/non_black_n))) %>%
  filter(Response == "Support") %>%
  ggplot(aes(x=Question, y = p, color = ID_Response)) + 
  geom_point(position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = p - se, ymax = p + se), width = 0.1, position = position_dodge(width = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(cols = vars(Black), rows = vars(Identification)) +
  theme(panel.border = element_rect(color = '#aeb0b7', fill = NA)) + 
  labs(y = "Share Somewhat/Strongly Support", color = "Identified in Group") 

ggsave("img/plot7.png", width = 8, height = 5)

bhi %>%
  filter(Black == "Black") %>%
  lm(IDed.BHI ~ QDemo9, data = ., weights = wt) %>%
  summary()