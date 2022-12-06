library(haven)      # Read and write Stata, SPSS, or SAS data file 
library(lavaan)     # LAtent VAriable ANalysis    # Diagrams of models estimated with lavaan
library(ltm)
library(dplyr)
library(tidyverse)
library(car)
library(ggpubr)
library(mediation)

data_thesis <- read.csv("C:/Users/frian/thesis_francisco_riano_ma/final_data.csv")
#data_thesis <- read.csv("C:/Users/frian/OneDrive/Documentos - copia/tilburg/Lessons/test_1.csv")
head(data_thesis$condition)
head(data_thesis$type2)
head(data_thesis$items_d2)
dim(data_thesis)

#data cleaning and processing

#step1

data_thesis %>% 
  group_by(items_d, type) %>% 
  summarize(wtp_avg = mean(wtp), wtp = sum(wtp), po_avg = mean(PO), po = sum(PO), amt = n())

head(data_thesis$items_d)

data_thesis <- data_thesis %>% 
  mutate(items_d2 = case_when(items_d == "high" ~ 1,
                              TRUE ~ 0))


data_thesis <- data_thesis %>% 
  mutate(type_2 = case_when(type == "hedonic" ~ 1,
                              TRUE ~ 0))

data_thesis <- data_thesis %>% 
  mutate(gender_ = case_when(Q91 == "Non-binary / third gender" ~ 1,
                             Q91 == "Male" ~ 2,
                             Q91 == "Female" ~ 3))




data_thesis2 <- data_thesis %>% 
  dplyr::select(wtp:type_2)

data_thesis2 <- data_thesis2 %>% 
  mutate(wtp_ = as.numeric(wtp))

data_thesis2 <- data_thesis2 %>% 
  mutate(favorite_means_tr = Q93) %>% 
  mutate(knowledge_cars = Q112_1) %>% 
  mutate(gender = Q91) %>% 
  mutate(age_rg = Q92) %>% 
  dplyr::select(-Q93, -Q112_1, -Q91, -Q92, wtp)



write.csv(data_thesis, "C:/Users/frian/OneDrive/Documentos - copia/tilburg/Lessons/test_1.csv")


sum(is.na(data_thesis2))


process(data = data_thesis2, y = "wtp_", x = "items_d2" , m = "PO", w = "type_2", model = 8, 
        cov = c("favorite_means_tr", "knowledge_cars", "gender", "age_rg"), center = 2, moments = 1, modelbt = 1, boot = 1000, seed = 19421)
 
View(mtcars)
process(mtcars, y = "mpg", x = "hp", m = "wt", model = 4)                                                                                           
    


class(mtcars$mpg)
class(mtcars$hp)
class(mtcars$wt)
     
class(data_thesis$wtp_)
class(data_thesis$PO)
class(data_thesis2$type_2)
class(data_thesis2$items_d2)

summarize(data_thesis)


data_thesis2 %>% 
  group_by(condition) %>% 
  summarize(dw = mean(PO), n = n(), mad = mean(wtp))


#visualizations


  #direct effect on wtp

boxplot(wtp ~ items_d ,
        data = data_thesis,
        main = "WTP by Level of customization",
        xlab = "Level of customization",
        ylab = "Willingness to pay",
        col = "steelblue",
        border = "black")



  boxplot(wtp ~ type ,
        data = data_thesis,
        main = "WTP by type of features",
        xlab = "Type of features used to customize",
        ylab = "Willingness to pay",
        col = "steelblue",
        border = "black")


  #effect on PO

boxplot(PO ~ items_d ,
        data = data_thesis,
        main = "PO by level of customization",
        xlab = "Type of features used to customize",
        ylab = "Psychological ownership",
        col = "steelblue",
        border = "black")



boxplot(PO ~ condition,
        data = data_thesis,
        main = "PO by type of features",
        xlab = "Type of features used to customize",
        ylab = "Psychological ownership",
        col = "steelblue",
        border = "black")



data_thesis %>% 
  group_by(items_d, type) %>% 
  summarize(wtp_avg = mean(wtp)) %>% 
  ggplot(aes(x = items_d, y = wtp_avg, fill = type))+
  geom_col(position = "dodge")

data_thesis %>% 
  group_by(items_d, type) %>% 
  summarize(po_avg = mean(PO)) %>% 
  ggplot(aes(x = items_d, y = po_avg, fill = type))+
  geom_col(position = "dodge")



#randomization


evaltot_aov1 <- lm(gender_ ~ items ,data_thesis)
Anova(evaltot_aov1, type=3)


#Cronbach's alpha
data_thesis3 <- data_thesis2 %>% 
  dplyr::select(Q62_1,Q62_2,Q62_3,Q62_4)

data_thesis3 <- data_thesis2 %>% 
  dplyr::select(Q62_1_,Q62_2_,Q62_3_,Q62_4_)

 cronbach.alpha(data_thesis3)
  
 
 
#assumptions
 
leveneTest(wtp ~ items_d, data_thesis2, center=mean)
leveneTest(PO ~ items_d, data_thesis2, center=mean)
shapiro.test(data_thesis2$wtp)
shapiro.test(data_thesis2$PO)
  
  
  