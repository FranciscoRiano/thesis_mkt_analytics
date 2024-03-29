library(ltm)
library(dplyr)
library(tidyverse)
library(car)
library(ggpubr)
library(semPlot)

data_thesis <- read.csv("C:/Users/frian/thesis_francisco_riano_ma/final_data.csv")
#data_thesis <- read.csv("C:/Users/frian/OneDrive/Documentos - copia/tilburg/Lessons/test_1.csv")
head(data_thesis$condition)
head(data_thesis$type2)
head(data_thesis$items_d2)
summary(data_thesis)

#data cleaning and processing
#step1

data_thesis %>% 
  group_by(condition) %>% 
  summarize( amt = n(), wtp_avg = mean(wtp), sd_wtp = sd(wtp), po_avg = mean(PO), po_sd = sd(PO))

head(data_thesis$items_d)

data_thesis <- data_thesis %>% 
  mutate(items_d2 = case_when(items_d == "high" ~ 0,
                              TRUE ~ 1))


data_thesis <- data_thesis %>% 
  mutate(type_2 = case_when(type == "hedonic" ~ 0,
                              TRUE ~ 1))

data_thesis <- data_thesis %>% 
  mutate(gender_ = case_when(Q91 == "Male" ~ 0,
                             Q91 == "Female" ~ 1))


data_thesis2 <- data_thesis %>% 
  dplyr::select(wtp:gender_) %>% 
  mutate(wtp_ = as.numeric(wtp)) %>% 
  mutate(favorite_means_tr = Q93) %>% 
  mutate(knowledge_cars = Q112_1) %>% 
  mutate(age_rg = Q92) %>% 
  dplyr::select(-Q93, -Q112_1, -Q91, -Q92, -wtp,-items) %>% 
  dplyr::select(-Q62_1:-Q62_4_)


#trasnformation of the covariates

data_thesis2 <- data_thesis2 %>% 
  mutate(favorite_means_tr_moc = case_when(favorite_means_tr == "My own car" ~ 1,
                                           TRUE ~ 0)) %>% 
  mutate(favorite_means_tr_by = case_when(favorite_means_tr == "Bike" ~ 1,
                                           TRUE ~ 0)) %>%
  mutate(favorite_means_tr_Ot = case_when(favorite_means_tr == "Other" ~ 1,
                                          TRUE ~ 0)) %>%
  mutate(favorite_means_tr_Pt = case_when(favorite_means_tr == "Public transport" ~ 1,
                                          TRUE ~ 0)) %>%
  mutate(favorite_means_tr_rc = case_when(favorite_means_tr == "Rented or leased car" ~ 1,
                                          TRUE ~ 0)) %>%
  mutate(favorite_means_tr_sm = case_when(favorite_means_tr == "Scooter or motorbike" ~ 1,
                                          TRUE ~ 0)) %>% 
  mutate(age_rg_25 = case_when(age_rg == "from 21 years old to 25 years old" ~ 1,
                               TRUE ~ 0)) %>% 
  mutate(age_rg_30 = case_when(age_rg == "from 26 years old to 30 years old" ~ 1,
                               TRUE ~ 0)) %>% 
  mutate(age_rg_35 = case_when(age_rg == "from 31 years old to 35 years old" ~ 1,
                               TRUE ~ 0)) %>%
  mutate(age_rg_40 = case_when(age_rg == "from 36 years old to 40 years old" ~ 1,
                               TRUE ~ 0)) %>%
  mutate(age_rg_45 = case_when(age_rg == "from 41 years old to 45 years old" ~ 1,
                               TRUE ~ 0)) %>% 
  mutate(age_rg_50 = case_when(age_rg == "from 46 years old to 50 years old" ~ 1,
                               TRUE ~ 0))
  
  
  
data_thesis %>% 
  group_by(items_d) %>% 
  summarize( amt = n(), wtp_avg = mean(wtp), sd_wtp = sd(wtp), po_avg = mean(PO), po_sd = sd(PO))


data_thesis %>% 
  group_by(type) %>% 
  summarize( amt = n(), wtp_avg = mean(wtp), sd_wtp = sd(wtp), po_avg = mean(PO), po_sd = sd(PO))



write.csv(data_thesis, "C:/Users/frian/OneDrive/Documentos - copia/tilburg/Lessons/test_1.csv")


sum(is.na(data_thesis2))


process(data = data_thesis2, y = "wtp_", x = "items_d2" , m = "PO", w = "type_2", model = 8, 
         center = 2, moments = 1, modelbt = 1, boot = 1000, seed = 19421)
 
                                                                                      
process(data = data_thesis2, y = "wtp_", x = "items_d2" , m = "PO", w = "type_2", model = 8, 
        cov = c("knowledge_cars", "gender_","favorite_means_tr_by", "favorite_means_tr_Ot",
                "favorite_means_tr_Pt", "favorite_means_tr_rc", "favorite_means_tr_sm", "age_rg_30",
                "age_rg_35", "age_rg_40", "age_rg_45", "age_rg_50"), center = 2, moments = 1, modelbt = 1, boot = 1000, seed = 19421)

process(data = data_thesis2, y = "wtp_", x = "items_d2" , m = "PO", w = "type_2", model = 8, cov = "knowledge_cars",
         center = 2, moments = 1, modelbt = 1, boot = 1000, seed = 19421)



#Individual regressions and t-test
thesis_lm2_0 <- lm(wtp_~items_d2*type_2+gender_+knowledge_cars+favorite_means_tr_by+favorite_means_tr_Ot+favorite_means_tr_Pt+
                     favorite_means_tr_rc+favorite_means_tr_sm+ age_rg_30+age_rg_35+age_rg_40+age_rg_45+age_rg_50 , data_thesis2);
summary(thesis_lm2_0)
Anova(thesis_lm2_0, type=3)

help("Anova")
thesis_lm2_1 <- lm(PO~items_d2*type_2+gender_+knowledge_cars+favorite_means_tr_by+favorite_means_tr_Ot+favorite_means_tr_Pt+
                     favorite_means_tr_rc+favorite_means_tr_sm+ age_rg_30+age_rg_35+age_rg_40+age_rg_45+age_rg_50  , data_thesis2);
summary(thesis_lm2_1)
Anova(thesis_lm2_1, type=3)

#ANOVAs
  #PO
thesis_lm2_2 <- lm(PO~ items_d2, data_thesis2);
#summary(thesis_lm2_2)
Anova(thesis_lm2_2, type=2)

thesis_lm2_3 <- lm(PO~ items_d2*type_2+knowledge_cars, data_thesis2);
#summary(thesis_lm2_3)
Anova(thesis_lm2_3, type=3)

thesis_lm2_6 <- lm(PO~ type_2+knowledge_cars, data_thesis2);
#summary(thesis_lm2_6)
Anova(thesis_lm2_6, type=2)

  #WTP
thesis_lm2_4 <- lm(wtp_~ items_d2+knowledge_cars, data_thesis2);
#summary(thesis_lm2_4)
Anova(thesis_lm2_4, type =2)

thesis_lm2_5 <- lm(wtp_~ items_d2*type_2+knowledge_cars, data_thesis2);
#summary(thesis_lm2_5)
Anova(thesis_lm2_5, type=3)

thesis_lm2_7 <- lm(wtp_~ type_2+knowledge_cars, data_thesis2);
#summary(thesis_lm2_7)
Anova(thesis_lm2_7, type=2)


  #t-test for wtp
t.test(wtp_ ~ items_d2, alternative = "greater", data = data_thesis2, var.equal=TRUE)
t.test(wtp_ ~ items_d2, alternative = "less", data = data_thesis2, var.equal=TRUE)
t.test(wtp_ ~ items_d2, alternative = "two.sided", data = data_thesis2, var.equal=TRUE)

t.test(wtp_ ~ type_2, alternative = "less", data = data_thesis2, var.equal=TRUE)
t.test(wtp_ ~ type_2, alternative = "greater", data = data_thesis2, var.equal=TRUE)
t.test(wtp_ ~ type_2, alternative = "two.sided", data = data_thesis2, var.equal=TRUE)
  #t-tests fo PO

t.test(PO ~ items_d2, alternative = "greater", data = data_thesis2, var.equal=TRUE)
t.test(PO ~ items_d2, alternative = "less", data = data_thesis2, var.equal=TRUE)
t.test(PO ~ items_d2, alternative = "two.sided", data = data_thesis2, var.equal=TRUE)

t.test(PO ~ type_2, alternative = "less", data = data_thesis2, var.equal=TRUE)
t.test(PO ~ type_2, alternative = "greater", data = data_thesis2, var.equal=TRUE)
t.test(PO ~ type_2, alternative = "two.sided", data = data_thesis2, var.equal=TRUE)
help("t.test")


class(data_thesis2$wtp_)
class(data_thesis2$PO)
class(data_thesis2$type_2)
class(data_thesis2$items_d2)

summary(data_thesis2)


data_thesis2 %>% 
  group_by(type) %>% 
  summarize(dw = mean(PO), n = n(), mad = mean(wtp_))

data_thesis2 %>% 
  group_by(items_d) %>% 
  summarize(dw = mean(PO), n = n(), mad = mean(wtp_))

sum(0.4256+0.1827+0.2258+0.1385+0.6637)/5




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


  #Bar visualizations
data_thesis %>% 
  group_by(items_d, type) %>% 
  summarize(wtp_avg = mean(wtp)) %>% 
  ggplot(aes(x = items_d, y = wtp_avg, fill = type))+
  geom_col(position = "dodge")+
  labs(x = "Level of Customization", y = "Willingness to pay", fill = "Type of features")+
  ggtitle("Willingness to pay by level of customization ")+
  theme(panel.background = element_blank())



data_thesis %>% 
  group_by(items_d, type) %>% 
  summarize(po_avg = mean(PO)) %>% 
  ggplot(aes(x = items_d, y = po_avg, fill = type))+
  geom_col(position = "dodge")+
  labs(x = "Level of Customization", y = "Psychological Ownership", fill = "Type of features")+
  ggtitle("Psychological ownership by level of customization ")+
  theme(panel.background = element_blank())


#randomization


evaltot_aov1 <- lm(gender_ ~ items_d2 ,data_thesis2)
#summary(evaltot_aov1)
Anova(evaltot_aov1, type=3)



demographics_thesis <- read.csv("C:/Users/frian/thesis_francisco_riano_ma/demographics_thesis.csv")

demographics_thesis <- demographics_thesis %>% 
  mutate(items_d2 = case_when(items_d == "high" ~ 0,
                              TRUE ~ 1))

evaltot_aov2 <- lm(Age ~ items_d2 ,demographics_thesis)
#summary(evaltot_aov2)
Anova(evaltot_aov2, type=3)


#Cronbach's alpha


data_thesis3 <- data_thesis %>% 
  dplyr::select(Q62_1_,Q62_2_,Q62_3_,Q62_4_)

 cronbach.alpha(data_thesis3)
  
 
 
#assumptions
 
leveneTest(wtp ~ items_d, data_thesis2, center=mean)
leveneTest(PO ~ items_d, data_thesis2, center=mean)
shapiro.test(data_thesis2$wtp)
shapiro.test(data_thesis2$PO)
  

#correlations

data_thesis4 <- data_thesis2 %>% 
  dplyr::select(-items_d,-type,-condition,-favorite_means_tr, -age_rg) %>% 
  dplyr::select(PO:knowledge_cars)


data_thesis4.cor = cor(data_thesis4)
  
data_thesis4.cor = cor(data_thesis4, method = c("spearman"))
library(corrplot)
library(psych)   


cor.test(data_thesis2$wtp, data_thesis2$type_2)
cor.test(data_thesis2$PO, data_thesis2$type_2)
cor.test(data_thesis2$wtp_, data_thesis2$gender_)
cor.test(data_thesis2$PO, data_thesis2$wtp_)
cor.test(data_thesis2$PO, data_thesis2$knowledge_cars)
cor.test(data_thesis2$wtp, data_thesis2$knowledge_cars)
cor.test(data_thesis4$PO, data_thesis4$gender_)
cor.test(data_thesis4$PO, data_thesis4$gender_)

wtp <- (data_thesis2$wtp_)
po <- (data_thesis2$PO)
know_car <- (data_thesis2$knowledge_cars)

plot(log1p(wtp) ~ log1p(po), pch = 19, col = "black")
plot(wtp ~ know_car, pch = 19, col = "black")



  #correlation matrix with level of significance

library(sjPlot)

tab_corr(data_thesis4, p.numeric = T)



#Mediation alternative

library(JSmediation)

moderated_mediation_fit <- 
  mdt_moderated(data = data_thesis2,
                IV   = items_d2,
                DV   = wtp_, 
                M    = PO,
                Mod  = type_2)
moderated_mediation_fit


sum(0.5883, 0.5718, 0.5786, 0.5971, 0.6134 )/5






#individual mediaiton

  #step 1

model_0 <- lm(wtp_ ~ type_2, data_thesis2)
summary(model_0)

  
  #step 2

model_m <- lm(PO ~ type_2, data_thesis2)
summary(model_m)


  #step 3

model_y <- lm(wtp_ ~ type_2 + PO, data_thesis2)
summary(model_y)


library(mediation)
results <- mediate(model_m, model_y, treat='type_2', mediator='PO',
                   boot=TRUE, sims=500)

summary(results)