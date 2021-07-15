## Dependant packages -------
require("tidyverse")
require("skimr")
require("likert")
require("corrplot")
##
require(psych)
require(MOTE)
require(lmerTest)
require(lavaan)
require(semTools)
require(broom)
require(tidyLPA)
require(semPlot)

## read the data ------
dat <- readr::read_rds("./data/clean_demo_likert.rds")

## skim
dim(dat)
dat %>% skimr::skim()
str(dat)



#### Section 1: --------
s1 <- dat[, 4:11] 
colnames(s1) <- substr(colnames(s1), 8, 100) ## Remove leading 'source '
## Likert plots
likert_s1 <- s1 %>% as.data.frame() %>% likert()
plot(likert_s1) + ggtitle("Section 1, sourcing papers for Lit Review")

## Correlation plots
lapply(si, as.numeric) %>% as.data.frame() %>% 
  cor(use = 'pairwise.complete.obs', method = 'spearman') %>% 
  corrplot::corrplot(cor1, method = 'number')



#### Section 2: ------
s2 <- dat[, 13:23] 
colnames(s2) <- substr(colnames(s2), 10, 100) ## Remove leading 'citation '
## Likert plots
likert_s2 <- s2 %>% as.data.frame() %>% likert()
plot(likert_s2) + ggtitle("Section 2, imporance for inclusion as citation")

## Correlation plots
lapply(s2, as.numeric) %>% as.data.frame() %>% 
  cor(use = 'pairwise.complete.obs', method = 'spearman') %>% 
  corrplot::corrplot(cor2, method = 'number')



#### Section 3: ------
s2 <- dat[, 24:36] 
colnames(s2) <- substr(colnames(s2), 10, 100) ## Remove leading 'citation '
## Likert plots
likert_s2 <- s2 %>% as.data.frame() %>% likert()
plot(likert_s2) + ggtitle("Section 2, imporance for inclusion as citation")

## Correlation plots
lapply(s2, as.numeric) %>% as.data.frame() %>% 
  cor(use = 'pairwise.complete.obs', method = 'spearman') %>% 
  corrplot::corrplot(cor2, method = 'number')
