setwd("~/Documents/Uni/Semester 3/Data Literacy/Project")
library(readxl)
library(tidyverse)
library(dplyr)
df <- read_excel("dataset.xls", skip=1)
colnames(df) <- c('Year','Condition','Line','HEA', 'MAT', 'RIP', 'HEI', 'TGW', 'EAR', 'GPE', 'DRY', 'YLD', 'HI')
head(df)


dt_c <- df[df$Condition=="Control",]
dt_t <- df[df$Condition=="Saline",]


library(lavaan)

mod <- 'ST =~ Growth + Yield + HEI + TGW
    Yield =~ EAR + GPE + DRY + YLD
    Growth =~ HEA + MAT
  
'

fit <- sem(mod, dt_c)
summary(fit)


factors_c <- lavPredict(fit,dt_c)
factors_t <- lavPredict(fit,dt_t)

st_score <- factors_t[,1]/factors_c[,1]
h1 <- hist(factors_c)
h2 <- hist(factors_t)

plot(h1,col=rgb(0,0,1,1/10))
plot(h2,col=rgb(1,0,0,1/10), add=T)
