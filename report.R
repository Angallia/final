dataset = read.csv("https://raw.githubusercontent.com/Angallia/final/main/data%20grapes%20wall%20csv.csv")
#This final script was designed to be uploaded to GitHub for grading- originally intended to be a markdown
#file, I couldn't make it work ): this was modified from using local files, hopefully 
#it will work with github and there will be no errors


#I decided to examine this data as two hypotheses: the data has three "treatments", the control
#and 2 parts of the barrier. The idea of the project is to determine if a barrier is effective
# at excluding SLF from accumulating in vineyard borders, and if it is, to further examine if treated netting
#reduces # of SLF more than untreated netting.

# Hypothesis 1
library("dplyr")
dataset.h1 = subset(dataset, Treatment != "No Net")

dataset.h1.factor = dataset.h1 %>%
  mutate_at(vars(Treatment),
            list(factor))

#this is ANOVA among the treatments excluding the control

hyp_1_anov = aov(SLF~Treatment, data = dataset.h1.factor)
summary(hyp_1_anov)
#             Df Sum Sq Mean Sq F value Pr(>F)
#Treatment     1    499   498.9   2.026  0.155
#Residuals   766 188666   246.3   


#P = .155


#Hypothesis is true- 

# Hypothesis 2
dataset.h2 = subset(dataset, Treatment != "Treated Net")

dataset.h2.factor = dataset.h2 %>%
  mutate_at(vars(Treatment),
            list(factor))

#ANOVA for untreated barrier against the control, excluding treated net

hyp_2_anov = aov(SLF~Treatment, data = dataset.h2.factor)
summary(hyp_2_anov)
#             Df Sum Sq Mean Sq F value Pr(>F)  
#Treatment     1   1081  1080.6   5.794 0.0163 *
#  Residuals   766 142864   186.5   

#P = .0163
#Low P value is strong evidence against null hypothesis

