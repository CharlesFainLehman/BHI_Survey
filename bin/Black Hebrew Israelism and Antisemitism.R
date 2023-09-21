library(tidyverse)
library(stargazer)
library(spatstat.geom)

bhi <- read.csv("dat/BHI_Responses_With_Weights_and_BHI_categories.csv")

black_qjews <- bhi %>%
  filter(Black == "Black") %>%
  select(QFeelings_6, starts_with("QJew"), wt) %>%
  mutate(across(starts_with("QJew"),
                ~ case_when(.x %in% c("Somewhat agree", "Strongly agree") ~ 1,
                            is.na(.x) ~ NA,
                            T ~ 0)),
         QFeelings_6 = QFeelings_6/100)

non_black_qjews <- bhi %>%
  filter(Black == "Non-black") %>%
  select(QFeelings_6, starts_with("QJew"), wt) %>%
  mutate(across(starts_with("QJew"),
                ~ case_when(.x %in% c("Somewhat agree", "Strongly agree") ~ 1,
                            is.na(.x) ~ NA,
                            T ~ 0)),
         QFeelings_6 = QFeelings_6/100)

#This is used to give the values in table 3, but is not itself table 3
stargazer(lm(QJew1 ~ QFeelings_6, data = filter(black_qjews, !is.na(QJew1)), weights = wt),
          lm(QJew2 ~ QFeelings_6, data = filter(black_qjews, !is.na(QJew2)), weights = wt),
          lm(QJew3 ~ QFeelings_6, data = filter(black_qjews, !is.na(QJew3)), weights = wt),
          lm(QJew4 ~ QFeelings_6, data = filter(black_qjews, !is.na(QJew4)), weights = wt),
          lm(QJew5 ~ QFeelings_6, data = filter(black_qjews, !is.na(QJew5)), weights = wt),
          lm(QJew6 ~ QFeelings_6, data = filter(black_qjews, !is.na(QJew6)), weights = wt),
          lm(QJew1 ~ QFeelings_6, data = filter(non_black_qjews, !is.na(QJew1)), weights = wt),
          lm(QJew2 ~ QFeelings_6, data = filter(non_black_qjews, !is.na(QJew2)), weights = wt),
          lm(QJew3 ~ QFeelings_6, data = filter(non_black_qjews, !is.na(QJew3)), weights = wt),
          lm(QJew4 ~ QFeelings_6, data = filter(non_black_qjews, !is.na(QJew4)), weights = wt),
          lm(QJew5 ~ QFeelings_6, data = filter(non_black_qjews, !is.na(QJew5)), weights = wt),
          lm(QJew6 ~ QFeelings_6, data = filter(non_black_qjews, !is.na(QJew6)), weights = wt),
          type = 'html', out = 'dat/table3.html', report=("vcsp"))

#Deprecated
bhi %>%
  select(Black, BHI.Believer, IDed.BHI, starts_with("QFeelings"), wt) %>%
  pivot_longer(cols = starts_with("QFeelings"), names_to = "Object") %>%
  mutate(Object = case_when(Object == "QFeelings_1" ~ "African Americans",
                          Object == "QFeelings_2" ~ "Whites",
                          Object == "QFeelings_3" ~ "Hispanics",
                          Object == "QFeelings_4" ~ "Asians",
                          Object == "QFeelings_5" ~ "Christians",
                          Object == "QFeelings_6" ~ "Jews",
                          Object == "QFeelings_7" ~ "Muslims",
                          Object == "QFeelings_8" ~ "BHIs")) %>%
  pivot_longer(cols = c(BHI.Believer, IDed.BHI), names_to = "BHI.Category", values_to = "Agree") %>%
  select(Black, BHI.Category, Agree, Object, value, wt) %>%
  mutate(Agree = case_when(BHI.Category == "BHI.Believer" & Agree == F ~ "No",
                           BHI.Category == "BHI.Believer" & Agree == T ~ "Yes",
                           BHI.Category == "IDed.BHI" & Agree == F ~ "No",
                           BHI.Category == "IDed.BHI" & Agree == T ~ "Yes"
                           )) %>%
  ggplot(aes(x=Object, color = Agree, y = value)) + 
  geom_boxplot(aes(weight = wt)) +
  facet_grid(cols = vars(Black), rows = vars(BHI.Category)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#These areis used to give the values in table 4, but are not themselves table 4

stargazer(
  lm(formula = QFeelings_1 ~ BHI.Believer, data = filter(bhi, Black == "Black"), weights = wt),
  lm(formula = QFeelings_2 ~ BHI.Believer, data = filter(bhi, Black == "Black"), weights = wt),
  lm(formula = QFeelings_3 ~ BHI.Believer, data = filter(bhi, Black == "Black"), weights = wt),
  lm(formula = QFeelings_4 ~ BHI.Believer, data = filter(bhi, Black == "Black"), weights = wt),
  lm(formula = QFeelings_5 ~ BHI.Believer, data = filter(bhi, Black == "Black"), weights = wt),
  lm(formula = QFeelings_6 ~ BHI.Believer, data = filter(bhi, Black == "Black"), weights = wt),
  lm(formula = QFeelings_7 ~ BHI.Believer, data = filter(bhi, Black == "Black"), weights = wt),
  lm(formula = QFeelings_8 ~ BHI.Believer, data = filter(bhi, Black == "Black"), weights = wt),
  lm(formula = QFeelings_1 ~ IDed.BHI, data = filter(bhi, Black == "Black"), weights = wt),
  lm(formula = QFeelings_2 ~ IDed.BHI, data = filter(bhi, Black == "Black"), weights = wt),
  lm(formula = QFeelings_3 ~ IDed.BHI, data = filter(bhi, Black == "Black"), weights = wt),
  lm(formula = QFeelings_4 ~ IDed.BHI, data = filter(bhi, Black == "Black"), weights = wt),
  lm(formula = QFeelings_5 ~ IDed.BHI, data = filter(bhi, Black == "Black"), weights = wt),
  lm(formula = QFeelings_6 ~ IDed.BHI, data = filter(bhi, Black == "Black"), weights = wt),
  lm(formula = QFeelings_7 ~ IDed.BHI, data = filter(bhi, Black == "Black"), weights = wt),
  lm(formula = QFeelings_8 ~ IDed.BHI, data = filter(bhi, Black == "Black"), weights = wt),
  out = "dat/table4_black.html", type = 'html', report=("vcsp")
)

stargazer(
  lm(formula = QFeelings_1 ~ BHI.Believer, data = filter(bhi, Black == "Non-black"), weights = wt),
  lm(formula = QFeelings_2 ~ BHI.Believer, data = filter(bhi, Black == "Non-black"), weights = wt),
  lm(formula = QFeelings_3 ~ BHI.Believer, data = filter(bhi, Black == "Non-black"), weights = wt),
  lm(formula = QFeelings_4 ~ BHI.Believer, data = filter(bhi, Black == "Non-black"), weights = wt),
  lm(formula = QFeelings_5 ~ BHI.Believer, data = filter(bhi, Black == "Non-black"), weights = wt),
  lm(formula = QFeelings_6 ~ BHI.Believer, data = filter(bhi, Black == "Non-black"), weights = wt),
  lm(formula = QFeelings_7 ~ BHI.Believer, data = filter(bhi, Black == "Non-black"), weights = wt),
  lm(formula = QFeelings_8 ~ BHI.Believer, data = filter(bhi, Black == "Non-black"), weights = wt),
  lm(formula = QFeelings_1 ~ IDed.BHI, data = filter(bhi, Black == "Non-black"), weights = wt),
  lm(formula = QFeelings_2 ~ IDed.BHI, data = filter(bhi, Black == "Non-black"), weights = wt),
  lm(formula = QFeelings_3 ~ IDed.BHI, data = filter(bhi, Black == "Non-black"), weights = wt),
  lm(formula = QFeelings_4 ~ IDed.BHI, data = filter(bhi, Black == "Non-black"), weights = wt),
  lm(formula = QFeelings_5 ~ IDed.BHI, data = filter(bhi, Black == "Non-black"), weights = wt),
  lm(formula = QFeelings_6 ~ IDed.BHI, data = filter(bhi, Black == "Non-black"), weights = wt),
  lm(formula = QFeelings_7 ~ IDed.BHI, data = filter(bhi, Black == "Non-black"), weights = wt),
  lm(formula = QFeelings_8 ~ IDed.BHI, data = filter(bhi, Black == "Non-black"), weights = wt),
  out = "dat/table4_nonblack.html", type = 'html', report=("vcsp")
)

#Figure 4
bhi %>%
  select(Black, BHI.Believer, IDed.BHI, starts_with('QJew'), wt) %>%
  mutate(across(starts_with("QJew"),
                ~ case_when(.x %in% c("Somewhat agree", "Strongly agree") ~ "Agree",
                            is.na(.x) ~ NA,
                            T ~ "Disagree"))) %>%
  pivot_longer(cols = c(BHI.Believer, IDed.BHI), names_to = "Measure", values_to = "Measure_Response") %>% 
  pivot_longer(cols = starts_with("QJew"), names_to = "Question", values_to = "Response") %>%
  group_by(Black, Measure, Measure_Response, Question, Response) %>%
  summarise(wtd.n = sum(wt),
            n = n(),
            .groups = 'drop') %>%
  drop_na() %>%
  group_by(Black, Measure, Question) %>%
  mutate(n_respondents = sum(n)) %>%
  ungroup() %>%
  group_by(Black, Measure, Measure_Response, Question) %>%
  mutate(p = wtd.n/sum(wtd.n),
         se = 1.96 * sqrt((p * (1-p))/n_respondents)) %>%
  filter(Response == "Agree") %>%
  #Relabeling
  mutate(Measure = ifelse(Measure == "BHI.Believer", "BHI Believer", "Self-IDed BHI"),
         Measure_Response = ifelse(Measure_Response == T, "Yes", "No"),
         Question = factor(case_when(Question == "QJew1" ~ "Loyalty",
                              Question == "QJew2" ~ "Boycott",
                              Question == "QJew3" ~ "Power",
                              Question == "QJew4" ~ "Slavery",
                              Question == "QJew5" ~ "Esau",
                              Question == "QJew6" ~ "Satan"),
                           levels = c("Loyalty", "Boycott", "Power", "Slavery", "Esau", "Satan"))) %>%
  ggplot(aes(x=Question, y=p, color = Measure_Response, group = Black)) + 
  geom_point(position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = p - se, ymax = p + se, group = Black), position = position_dodge(width = 0.5), width = 0.1) +
  facet_grid(cols = vars(Black), rows = vars(Measure)) +
  scale_y_continuous(labels = scales::percent) + 
  labs(color = "Identified in Group") + 
  theme(axis.title.y = element_blank(),
        panel.border = element_rect(color = '#aeb0b7', fill = NA))

ggsave("img/plot4.png", width = 8, height = 5)

#count of questions regression

bhi %>%
  select(Black, BHI.Believer, IDed.BHI, starts_with("QJew"), wt) %>%
  mutate(across(starts_with("QJew"),
                ~ case_when(.x %in% c("Somewhat agree", "Strongly agree") ~ 1,
                            is.na(.x) ~ NA,
                            T ~ 0))) %>%
  rowwise() %>%
  mutate(Affirmative_Answers = sum(QJew1, QJew2, QJew3, QJew4, QJew5, QJew6, na.rm = T)) -> bhi_count_data
 
#Average yeses
filter(bhi_count_data, Black == "Black") %>% ungroup() %>% summarise(mean(Affirmative_Answers))
filter(bhi_count_data, Black == "Non-black") %>% ungroup() %>% summarise(mean(Affirmative_Answers))

stargazer(lm(Affirmative_Answers ~ BHI.Believer, data = filter(bhi_count_data, Black == "Black"), weights = wt),
          lm(Affirmative_Answers ~ IDed.BHI, data = filter(bhi_count_data, Black == "Black"), weights = wt),
          lm(Affirmative_Answers ~ BHI.Believer, data = filter(bhi_count_data, Black == "Non-black"), weights = wt),
          lm(Affirmative_Answers ~ IDed.BHI, data = filter(bhi_count_data, Black == "Non-black"), weights = wt),
             type = 'text')

black_n = 1050
non_black_n = 529

bhi %>%
  select(Black, BHI.Believer, IDed.BHI, QBHI4, wt) %>%
  mutate(Jews_Not_Descended = QBHI4 %in% c("Somewhat disagree", "Strongly disagree")) %>%
  pivot_longer(cols = c(BHI.Believer, IDed.BHI), names_to = "Measure", values_to = "Measure_Response") %>%
  group_by(Black, Measure, Measure_Response, Jews_Not_Descended) %>%
  summarise(n = sum(wt)) %>%
  mutate(p = n/sum(n),
         se = ifelse(Black == "Black", 1.96 * sqrt((p * (1-p))/black_n), 1.96 * sqrt((p * (1-p))/non_black_n))) %>%
  filter(Jews_Not_Descended == T) %>%
  #Relabeling
  mutate(Measure = ifelse(Measure == "BHI.Believer", "BHI Believer", "Self-IDed BHI"),
         Measure_Response = ifelse(Measure_Response == T, "Yes", "No")) %>%
  ggplot(aes(x=Measure_Response, y=p, color = Black)) + 
  geom_point(position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = p - se, ymax = p + se), position = position_dodge(width = 0.5), width = 0.1) +
  facet_wrap(~Measure) +
  scale_y_continuous(labels = scales::percent) + 
  labs(shape = "Identified in Group", color = "") + 
  theme(axis.title.y = element_blank(),
        panel.border = element_rect(color = '#aeb0b7', fill = NA)) + 
  labs(x = "Identified in Group")

ggsave("img/plot5.png", width = 8, height = 5)