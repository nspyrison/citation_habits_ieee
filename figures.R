## Dependant packages -------
require("tidyverse")
require("skimr")
require("likert")
require("corrplot")
require("patchwork")
require("hrbrthemes")
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

## Sections partitions (of likert items) -----
## Section 1, Sourcing papers
s1 <- dat[, 5:12] 
colnames(s1) <- substr(colnames(s1), 8, 100) ## Remove leading 'source '
## Section 2, reading papers in detail
s2 <- dat[, 14:22]
colnames(s2) <- substr(colnames(s2), 6, 100) ## Remove leading 'read '
## Section 3, assessing venue quality
s3 <- dat[, 24:31]
colnames(s3) <- substr(colnames(s3), 7, 100) ## Remove leading 'venue '



## 1) Likert figures -----
## to plot, call plot(likert_obj)

likert_s1 <- s1 %>% as.data.frame() %>% likert()
likert_s2 <- s2 %>% as.data.frame() %>% likert()
likert_s3 <- s3 %>% as.data.frame() %>% likert()

## saving
likert_out1 <- plot(likert_s1) + ggtitle("Section 1, frequency of use when sourcing papers during liturature review")
likert_out2 <- plot(likert_s2) + ggtitle("Section 2, importance for deciding wether or not to read in detail")
likert_out3 <- plot(likert_s3) + ggtitle("Section 3, importance for assessing the quality of a venue")

ggsave("likert_section1.pdf", likert_out1, "pdf", "figures",
       width = 8, height = 3, units = "in")

likert_pw <- likert_out1 / likert_out2 / likert_out3
ggsave("likert_all.pdf", likert_pw, "pdf", "figures",
       width = 8, height = 10, units = "in")


## 2) Correlation figures ------
dat_b <- cbind(s1[, -ncol(s1)], s2[, -ncol(s2)], s3[, -ncol(s3)]) %>% lapply(as.numeric) %>% as_tibble()
corr_b <- dat_b %>% cor(use = 'pairwise.complete.obs', method = 'spearman')

corr_s1 <- s1[, -ncol(s1)] %>% lapply(as.numeric) %>% as.data.frame() %>%
  cor(use = 'pairwise.complete.obs', method = 'spearman')
corr_s2 <- lapply(s2, as.numeric) %>% as.data.frame() %>%
  cor(use = 'pairwise.complete.obs', method = 'spearman')
corr_s3 <- lapply(s3, as.numeric) %>% as.data.frame() %>%
  cor(use = 'pairwise.complete.obs', method = 'spearman')

## Saving Correlation plots
path <- "./figures/"
name <- "corr_section1.pdf"
pdf(name)
corrplot.mixed(corr_s1, lower = 'number', upper = 'ellipse',
                            order = 'FPC') ## "AOE", "FPC", "hclust"
dev.off()
file.copy(name, to = paste0(path, name), overwrite = TRUE)
file.remove(name)

path <- "./figures/"
name <- "corr_section2.pdf"
pdf(name)
corrplot.mixed(corr_s2, lower = 'number', upper = 'ellipse',
               order = 'FPC') ## "AOE", "FPC", "hclust"
dev.off()
file.copy(name, to = paste0(path, name), overwrite = TRUE)
file.remove(name)

path <- "./figures/"
name <- "corr_section3.pdf"
pdf(name)
corrplot.mixed(corr_s3, lower = 'number', upper = 'ellipse',
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

## 3) mixed model -----


## (4) demographic heatmap ------

## Change character to factor, include counts in the levels of sex?
str(clean)
sub <- clean %>% select("timestamp", "position", "years vis experience")
(time_hist <- sub %>% ggplot(aes(timestamp)) + geom_density() +
  theme_ipsum())
(demographics <- ggplot(sub, aes(position, `years vis experience`)) +
    ## categorical violine plots?
    geom_violin(width=2.1, size=0.2) +
    scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) +
    theme_ipsum() +
    theme(legend.position="none") +
    coord_flip() + # This switch X and Y axis and allows to get the horizontal version
    xlab("") +
    ylab("Assigned Probability (%)") +
    ## heatmap with counts.
    # stat_bin2d(aes(fill = after_stat(count))) +
    # geom_text(aes(label = after_stat(count)), stat = "bin2d") +
    # scale_fill_gradient(low = "lightpink", high = "firebrick", na.value = NA) +
    # theme_bw() +
    # theme(axis.text.x = element_text(angle = 90, hjust = 1),
    #       # legend.position = "bottom",
    #       # legend.direction = "horizontal",
    #       legend.margin = margin(0, 0, 0, 0)) +
    ggtitle("Participant demographics"))
if(F)
  ggsave(filename = "./figures/demographic_heatmap.pdf",
         plot = demographics / time_hist, device = "pdf", width = 4, height = 2)


## (5) Factor analysis/PCA -----


### Split data on type of response ------
text_attr <- clean %>% ## Freeform text fields
  dplyr::select("timestamp",
                "source other text",   ## ~col 12
                "read other text",     ## ~col 24
                "venue other text",    ## ~col 36
                "other comments text") ## ~col 37


