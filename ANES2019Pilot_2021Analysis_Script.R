#### Heading ####
#### "Racialized Economics in 2019,"
#### by Andrew Infantino.
#### Programmed in R 3.6.2.
#### Completed May 12th, 2021.

#### Load Data
# anes_pilot_2019_csv.zip must be in working directory for code to function properly:
library(tidyverse)
unzip(zipfile = "anes_pilot_2019_csv.zip", list = FALSE)
data <- read.csv("anes_pilot_2019.csv")

#### Racial Resentment and Economic Evaluations ####
# Focus items:
# [rr3] (1-5: Strongly agree-Strongly disagree) "Over the past few years, blacks have gotten less than they deserve."
# [econnow] (1-5: Much better-Much worse) "Now thinking about the economy in the country as a whole, would you say that as compared to one year ago, the nation's economy is now better, about the same, or worse?"
# [confecon] (1-5: Not worried-Extremely worried) "Overall, how worried are you about the national economy?"

# Eliminate non-applicable responses:
data1 <- data %>%
  filter(rr3 %in% 1:5, econnow %in% 1:5, confecon %in% 1:5)

# Regression coefficients:
data1 %>%                                        # Resentment and economic change
  summarize(R = cor(rr3, econnow),               # -0.4600833
            p = cor.test(rr3, econnow)$p.value)  # ****
data1 %>%                                        # Resentment and economic worry
  summarize(R = cor(rr3, confecon),              # -0.3956359
            p = cor.test(rr3, confecon)$p.value) # ****

#### Group by Race ####
# 1: White -0.483 ****
# 2: Black -0.0963 ^
# 3: Hispanic -0.287 ****
# 4: Asian -0.250 *
# 5: Native American -0.558 ****
# 6: Middle Eastern -0.600 ****
data2 <- data1 %>%
  filter(race %in% 1:6)
data2 %>% 
  group_by(race) %>%
  summarize(R = cor(rr3, econnow), p = cor.test(rr3, econnow)$p.value)

#### Group by Ideology ####
# 1-3: Liberal -0.0761 *
# 4: Moderate -0.216 ****
# 5-7: Conservative -0.364 ****
data3 <- data1 %>%
  filter(lcself %in% 1:7)
data3 %>%
  mutate(Ideology = ifelse(lcself %in% 1:3, "Liberal",
                           ifelse(lcself == 4, "Moderate", "Conservative"))) %>%
  group_by(Ideology) %>%
  summarize(R = cor(rr3, econnow), p = cor.test(rr3, econnow)$p.value)

#### Group by Party ####
data4 <- data1 %>%
  mutate(Party = ifelse(pid1d==1|pid1r==1|pidlean==2, "Democrat/Leaner",   # -0.0830**
                 ifelse(pid1d==2|pid1r==2|pidlean==1, "Republican/Leaner", # -0.270****
                                              "Independent/Third Party"))) # -0.345****
data4 %>%
  group_by(Party) %>%
  summarize(R = cor(rr3, econnow), p = cor.test(rr3, econnow)$p.value)
