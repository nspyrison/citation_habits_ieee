## Dependant packages -------
require("tidyverse")
require("skimr")
require("likert")
require("corrplot")
##
# require(psych)
# require(MOTE)
# require(lmerTest)
# require(lavaan)
# require(semTools)
# require(broom)
# require(tidyLPA)
# require(semPlot)

## read the data ------
dat <- readr::read_rds("./data/clean_survey_pilot.rds")

## skim
dim(dat)
dat %>% skimr::skim()
str(dat)

### Sections partitions of likert items -----
## Section 1, Sourcing papers
s1 <- dat[, 5:12] 
colnames(s1) <- substr(colnames(s1), 8, 100) ## Remove leading 'source '
## Section 2, reading papers in detail
s2 <- dat[, 14:22]
colnames(s2) <- substr(colnames(s2), 6, 100) ## Remove leading 'read '
## Section 3, assessing venue quality
s3 <- dat[, 24:31]
colnames(s3) <- substr(colnames(s3), 7, 100) ## Remove leading 'venue '



### Likert objects -----
## to plot, call plot(likert_obt)
likert_s1 <- s1 %>% as.data.frame() %>% likert()
likert_s2 <- s2 %>% as.data.frame() %>% likert()
likert_s3 <- s3 %>% as.data.frame() %>% likert()

## saving
likert_out1 <- plot(likert_s1) + ggtitle("Section 1, frequency of use when sourcing papers in liturature review")
ggsave("likert_section1.pdf", likert_out1, "pdf", "figures",
       width = 8, height = 3, units = "in")


### Correlation ---

corr_s1 <- lapply(s1, as.numeric) %>% as.data.frame() %>%
  cor(use = 'pairwise.complete.obs', method = 'spearman')
corr_s2 <- lapply(s2, as.numeric) %>% as.data.frame() %>%
  cor(use = 'pairwise.complete.obs', method = 'spearman')
corr_s3 <- lapply(s3, as.numeric) %>% as.data.frame() %>%
  cor(use = 'pairwise.complete.obs', method = 'spearman')

## Saving Correlation plots mock ups
path <- "./figures/"
corr_s1 <- cor(mtcars) ## correlation for pilot is too sparse
name <- "corr_s1.pdf"
pdf(name)
corrplot.mixed(corr_s1, lower = 'number', upper = 'ellipse',
                            order = 'FPC') ## "AOE", "FPC", "hclust"
dev.off()
file.copy(name, to = paste0(path, name), overwrite = TRUE)
file.remove(name)

### Other corrplot options
# corrplot(corr_s1, method = 'number')
# corrplot(corr_s1, method = 'ellipse', order = 'AOE', type = 'upper')
# 
# ## signif, lonni won't be keen 
# corrplot(corr_s1, p.mat = testRes$p, method = 'color', diag = FALSE, type = 'upper',
#          sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9, 
#          insig = 'label_sig', pch.col = 'grey20', order = 'AOE')
# 
# # Visualize confidence interval, hard to read
# corrplot(corr_s1, lowCI = testRes$lowCI, uppCI = testRes$uppCI, order = 'hclust',
#          tl.pos = 'd', rect.col = 'navy', plotC = 'rect', cl.pos = 'n')
