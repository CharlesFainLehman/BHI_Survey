library(tidyverse)

bhi <- read.csv('dat/BHI_Responses_With_Weights.csv') %>%
  #Simplifying the black category
  mutate(Black = ifelse(QScreen1 == "Black or African American", "Black", "Nonblack"))

#Figure 1
bhi %>%
  select(Black, QBHI2, QBHI3, QBHI4, wt) %>%
  pivot_longer(cols = starts_with('Q')) %>%
  mutate(name = case_when(name == "QBHI2" ~ "Black Americans",
                          name == "QBHI3" ~ "Other Groups",
                          name == "QBHI4" ~ "Jews")) %>%
  group_by(Black, name, value) %>%
  summarise(wtd.n = sum(wt),
            n = n()) %>%
  mutate(p = wtd.n/sum(wtd.n),
         n = sum(n),
         se = sqrt((p * (1-p))/n),
         value = factor(value, levels = c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree"))) %>%
  ggplot(aes(x=value, y=p)) + 
  geom_col(position = position_dodge(width = 0.5), width = 0.5, aes(fill = Black)) + 
  geom_errorbar(aes(ymin = p - 1.96 * se, ymax = p + 1.96 * se, group = Black), width = 0.25, position = position_dodge(width = 0.5)) +
  facet_wrap(~name) + 
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank(),
        legend.title = element_blank(),
        panel.border = element_rect(color = '#aeb0b7', fill = NA)) + 
  labs(title = "_______ are directly descended from the ancient Israelites")

ggsave("img/plot1.png", width = 8, height = 5)

#Figure 2
bhi %>%
  select(Black, QBHI1, QBHI2, QBHI4, wt) %>%
  pivot_longer(cols = c(QBHI2, QBHI4)) %>%
  mutate(name = case_when(name == "QBHI2" ~ "Black Americans",
                          name == "QBHI4" ~ "Jews"),
         Biblit = ifelse(!QBHI1 %in% c("Not familiar at all", "Only slightly familiar"), "Familiar", "Not familiar")) %>%
  group_by(Black, name, Biblit, value) %>%
  summarise(wtd.n = sum(wt),
            n = n()) %>%
  mutate(p = wtd.n/sum(wtd.n),
         n = sum(n),
         se = sqrt((p * (1-p))/n),
         value = factor(value,
                           levels = c("Strongly agree",
                                      "Somewhat agree",
                                      "Neither agree nor disagree",
                                      "Somewhat disagree",
                                      "Strongly disagree"))) %>% 
  ggplot(aes(x=value, y=p)) + 
  geom_col(position = position_dodge(width = 0.5), aes(fill = Black), width = 0.5) + 
  geom_errorbar(aes(ymin = p - 1.96 * se, ymax = p + 1.96 * se, group = Black), width = 0.25, position = position_dodge(width = 0.5)) + 
  facet_grid(rows = vars(Biblit), cols = vars(name)) + 
  scale_y_continuous(labels = scales::percent) + 
  theme(legend.title = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = '#aeb0b7', fill = NA)) + 
  labs(title = "_______ are directly descended from the ancient Israelites")

ggsave("img/plot2.png", width = 8, height = 5)

#Figure 3
bhi %>%
  mutate(QDemo1 = ifelse(QDemo1 == "Yes", "BHI IDed", "Not BHI IDed")) %>%
  group_by(Black, QDemo1, QBHI2) %>%
  summarise(wtd.n = sum(wt),
            n = n()) %>%
  mutate(p = wtd.n/sum(wtd.n),
         n = sum(n),
         se = sqrt((p * (1-p))/n),
         QBHI2 = factor(QBHI2,
                        levels = c("Strongly agree",
                                   "Somewhat agree",
                                   "Neither agree nor disagree",
                                   "Somewhat disagree",
                                   "Strongly disagree"))) %>%
  ggplot(aes(x=QBHI2, y=p)) +
  geom_col(position = position_dodge(width = 0.5), width = 0.5, aes(fill = QDemo1)) + 
  geom_errorbar(aes(ymin = p - 1.96 * se, ymax = p + 1.96 * se, group = QDemo1), width = 0.25, position = position_dodge(width = 0.5)) + 
  facet_wrap(~Black) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = '#aeb0b7', fill = NA)) + 
  labs(title = "Black Americans are directly descended from the ancient Israelites")

ggsave("img/plot3.png", width = 8, height = 5)

#Table 2
bhi %>%
  mutate(IDed.BHI = !QBHI1 %in% c("Not familiar at all", "Slightly familiar") & QBHI2 %in% c("Strongly agree", "Somewhat agree") & QDemo1 == "Yes",
         BHI.Believer = !QBHI1 %in% c("Not familiar at all", "Slightly familiar") & QBHI2 %in% c("Strongly agree", "Somewhat agree")) %>%
  select(Black, wt, IDed.BHI, BHI.Believer) %>%
  pivot_longer(cols = c(IDed.BHI, BHI.Believer)) %>%
  group_by(Black, name, value) %>%
  summarise(wtd.n = sum(wt),
            n = n()) %>%
  mutate(p = wtd.n/sum(wtd.n),
         n = sum(n),
         se = 1.96 * sqrt((p * (1-p))/n),
         min = p - se,
         max = p + se) %>%
  filter(value == T) 

#Create new data with the BHI group categorizations
bhi %>%
  mutate(IDed.BHI = !QBHI1 %in% c("Not familiar at all", "Slightly familiar") & QBHI2 %in% c("Strongly agree", "Somewhat agree") & QDemo1 == "Yes",
         BHI.Believer = !QBHI1 %in% c("Not familiar at all", "Slightly familiar") & QBHI2 %in% c("Strongly agree", "Somewhat agree")) %>%
  write.csv("dat/BHI_Responses_With_Weights_and_BHI_categories.csv", row.names = F)