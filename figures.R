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
dat <- readr::read_rds("./data/clean_survey_pilot.rds")

## skim
dim(dat)
dat %>% skimr::skim()
str(dat)

#### partition into sections, create likert & cor matrix -----
## Section 1
s1 <- dat[, 5:12]
colnames(s1) <- substr(colnames(s1), 8, 100) ## Remove leading 'source '
#str(s1)
likert_s1 <- s1 %>% as.data.frame() %>% likert()
corr_s1 <- lapply(s1, as.numeric) %>% as.data.frame() %>%
  cor(use = 'pairwise.complete.obs', method = 'spearman')

## Section 2
s2 <- dat[, 14:24]
colnames(s2) <- substr(colnames(s2), 10, 100) ## Remove leading 'citation '
#str(s2)
likert_s2 <- s2 %>% as.data.frame() %>% likert()
corr_s2 <- lapply(s2, as.numeric) %>% as.data.frame() %>%
  cor(use = 'pairwise.complete.obs', method = 'spearman')

## Section 3
s3 <- dat[, 28:36]
colnames(s3) <- substr(colnames(s3), 7, 100) ## Remove leading 'venue '
#str(s3)
likert_s3 <- s3 %>% as.data.frame() %>% likert()
corr_s3 <- lapply(s3, as.numeric) %>% as.data.frame() %>%
  cor(use = 'pairwise.complete.obs', method = 'spearman')


## Likert plot mock ups -----
likert_out1 <- plot(likert_s1) + ggtitle("Section 1, frequency of use when sourcing papers in liturature review")
ggsave("likert_section1.pdf", likert_out1, "pdf", "figures",
       width = 8, height = 3, units = "in")


## Correlation plots mock ups -----
pal = hcl.colors(10, "OrGrGr", rev = TRUE)
corr_s1 <- cor(mtcars) ## correlation for pilot is too sparse
corr_out1 <- corrplot.mixed(corr_s1, lower = 'number', upper = 'ellipse',
                            order = 'FPC' ## "AOE", "FPC", "hclust"
)
ggsave("corr_section1.pdf", corr_out1, "pdf", "figures",
       width = 3, height = 3, units = "in")

### Other corrplot thoughts & options
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
