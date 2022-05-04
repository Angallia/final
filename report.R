dataset = read.csv("https://raw.githubusercontent.com/Angallia/final/main/data%20grapes%20wall%20csv.csv")


# Hypothesis 1
library("dplyr")
dataset.h1 = subset(dataset, Treatment != "No Net")

dataset.h1.factor = dataset.h1 %>%
  mutate_at(vars(Treatment),
            list(factor))

hyp_1_anov = aov(SLF~Treatment, data = dataset.h1.factor)
summary(hyp_1_anov)
#             Df Sum Sq Mean Sq F value Pr(>F)
#Treatment     1    499   498.9   2.026  0.155
#Residuals   766 188666   246.3   

#Hypothesis is true- 

# Hypothesis 2
dataset.h2 = subset(dataset, Treatment != "Treated Net")

dataset.h2.factor = dataset.h2 %>%
  mutate_at(vars(Treatment),
            list(factor))

hyp_2_anov = aov(SLF~Treatment, data = dataset.h2.factor)
summary(hyp_2_anov)
#             Df Sum Sq Mean Sq F value Pr(>F)  
#Treatment     1   1081  1080.6   5.794 0.0163 *
#  Residuals   766 142864   186.5   

