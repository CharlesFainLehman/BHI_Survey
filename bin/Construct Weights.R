#See this guide for details
#https://sdaza.com/blog/2012/raking/

library(anesrake)
library(tidyverse)
library(readxl)

bhi <- read_excel("dat/Black+Hebrew+Israelism+Survey_September+11,+2023_15.46.xlsx") %>%
  #drop the top row, which contains the question text
  slice_tail(n = nrow(.) - 1) %>%
  #create a unique id for each row
  mutate(id = 1:n()) 

#construct weights for black subsample

#identify subsample and pull out weight-relevant cols
bhi_black_subsample <- filter(bhi, QScreen1 == "Black or African American") %>%
  select(id, QDemo3, QScreen2, QDemo9, QDemo2) %>%
  rename(Sex = QDemo3, Hispanic = QScreen2, Education = QDemo9, Age = QDemo2) %>%
  #I drop sex non-respondents
  filter(Sex %in% c("Male", "Female")) %>%
  #these need to be factors for anesrake to work
  mutate(Sex = factor(Sex),
         Hispanic = factor(Hispanic),
         Education = factor(Education),
         Age = as.numeric(Age),
         Age_Category = cut(Age, breaks = c(17, 27, 37, 47, 57, 67, 77, 87, 97, 107)))

#black sample targets
b_targets = list(
  Sex = c(.471, .529),
  Hispanic = c(.018, .982),
  Education = c(.121, .322, .248, .083, .141, .077, .008),
  Age_Category = c(.194, .197, .168, .162, .151, .087, .033, .009, .000001)
)

names(b_targets$Sex) <- c("Male", "Female")
names(b_targets$Hispanic) <- c("Yes", "No")
names(b_targets$Education) = c("Less than high school", "High school graduate", "Some college", "2 year degree", "4 year degree", "Masters/Professional degree", "Doctorate")
names(b_targets$Age_Category) <- levels(bhi_black_subsample$Age_Category)

black_weights <- anesrake(inputter = b_targets, dataframe = as.data.frame(bhi_black_subsample), caseid = bhi_black_subsample$id, choosemethod = 'total', iterate = T, force1 = T)

#construct weights for non-black subsample

bhi_non_black_subsample <- filter(bhi, QScreen1 != "Black or African American") %>%
  select(id, QDemo3, QScreen1, QScreen2, QDemo9, QDemo2) %>%
  rename(Sex = QDemo3, Race = QScreen1, Hispanic = QScreen2, Education = QDemo9, Age = QDemo2) %>%
  filter(Sex %in% c("Male", "Female")) %>%
  mutate(Sex = factor(Sex),
         Hispanic = factor(Hispanic),
         Education = factor(Education),
         Race = factor(Race),
         Age = as.numeric(Age),
         Age_Category = cut(Age, breaks = c(17, 27, 37, 47, 57, 67, 77, 87, 97, 107)))

nb_targets = list(
  Sex = c(.493, .507),
  Race = c(.823, .079, .012, .086),
  Hispanic = c(.189, .811),
  Education = c(.105, .266, .206, .084, .21, .113, .015),
  Age_Category = c(.164, .173, .162, .16, .163, .114, .05, .014, .000001)
)

names(nb_targets$Sex) <- c("Male", "Female")
names(nb_targets$Race) <- c("White", "Asian or Pacific Islander", "American Indian or Alaska Native", "Other")
names(nb_targets$Hispanic) <- c("Yes", "No")
names(nb_targets$Education) = c("Less than high school", "High school graduate", "Some college", "2 year degree", "4 year degree", "Masters/Professional degree", "Doctorate")
names(nb_targets$Age_Category) <- levels(bhi_non_black_subsample$Age_Category)

non_black_weights <- anesrake(inputter = nb_targets, dataframe = as.data.frame(bhi_non_black_subsample), caseid = bhi_non_black_subsample$id, choosemethod = 'total', iterate = T, force1 = T)

overall_weights <- bind_rows(
  data.frame(id = black_weights$caseid, wt = black_weights$weightvec),
  data.frame(id = non_black_weights$caseid, wt = non_black_weights$weightvec))

merge(bhi, overall_weights, by = 'id', all.y = F) %>%
  write_excel_csv("dat/BHI_Responses_With_Weights.csv")