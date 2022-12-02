library(haven)      # Read and write Stata, SPSS, or SAS data file 
library(lavaan)     # LAtent VAriable ANalysis 
library(semPlot)    # Diagrams of models estimated with lavaan
library(rstudioapi)
library(dplyr)
library(tidyverse)
library(car)
library(ggpubr)
library(mediation)

data_thesis <- read.csv("C:/Users/frian/thesis_francisco_riano_ma/final_data.csv")

head(data_thesis$condition)
head(data_thesis$type)
head(data_thesis$items_d)
dim(data_thesis)



#step1

regression_h1 <- lm(wtp ~ items_d, data_thesis);
summary(regression_h1)

#step2

regression_h2 <- lm(PO ~ items_d, data_thesis);
summary(regression_h2)

#step3

regression_h3 <- lm(wtp ~ items_d + PO, data_thesis);
summary(regression_h3)

pmacroModel(8)

mediation_analysis <- mediate(regression_h1, regression_h3, treat = "condition" , mediator = "PO", boot = TRUE, sims = 500)
summary(mediation_analysis)




process (data = data_thesis, y = "wtp", x = "items_d", m = "PO", w = "type", model = 8, cov = c("Q93", "Q112_1", 
                                                                                                "Q91", "Q92"),
         center = 2, moments = 1, modelbt = 1, boot = 1000, seed = 19421)




