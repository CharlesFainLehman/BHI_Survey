library(tidyverse)
library(stargazer)
library(spatstat.geom)

bhi <- read.csv("dat/BHI_Responses_With_Weights_and_BHI_categories.csv")

#Figure 7
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
  summarise(wtd.n = sum(wt), 
            n = n()) %>%
  mutate(p = wtd.n/sum(wtd.n),
         se = sqrt((p * (1-p))/n)) %>%
  filter(Response == "Support") %>%
  ggplot(aes(x=Question, y = p)) + 
  geom_col(position = position_dodge(width = 0.5), width = 0.5, aes(fill = ID_Response)) + 
  geom_errorbar(aes(ymin = p - 1.96 * se, ymax = p + 1.96 * se, group = ID_Response), width = 0.25, position = position_dodge(width = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(cols = vars(Black), rows = vars(Identification)) +
  theme(panel.border = element_rect(color = '#aeb0b7', fill = NA)) + 
  labs(y = "Share Somewhat/Strongly Support", fill = "Identified in Group")

ggsave("img/plot7.png", width = 8, height = 5)

#Effect of BHI on total number of Yeses in the interpersonal violence Qs
bhi %>%
  filter(Black == "Black") %>%
  mutate(across(starts_with("QHit"), ~ ifelse(.x %in% c("Strongly support", "Somewhat support"), 1, 0)),
         HitTotal = QHit1 + QHit2 + QHit3 + QHit4) %>%
  lm(HitTotal ~ BHI.Believer, data = ., weights = wt) %>%
  summary()
  
#Figure 8
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
  summarise(wtd.n = sum(wt),
            n = n()) %>%
  mutate(p = wtd.n/sum(wtd.n),
         se = sqrt((p * (1-p))/n)) %>%
  filter(Response == "Support") %>%
  ggplot(aes(x=Question, y = p)) + 
  geom_col(position = position_dodge(width = 0.5), width = 0.5, aes(fill = ID_Response)) + 
  geom_errorbar(aes(ymin = p - 1.96  * se, ymax = p + 1.96 * se, group = ID_Response), width = 0.25, position = position_dodge(width = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(cols = vars(Black), rows = vars(Identification)) +
  theme(panel.border = element_rect(color = '#aeb0b7', fill = NA)) + 
  labs(y = "Share Somewhat/Strongly Support", fill = "Identified in Group") 

ggsave("img/plot8.png", width = 8, height = 5)