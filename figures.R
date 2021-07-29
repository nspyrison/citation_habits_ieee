## Dependant packages -------
require("tidyverse")
require("skimr")
require("likert")
require("corrplot")
require("patchwork")
require("hrbrthemes")


## read the data ------
dat <- readr::read_rds("./data/clean_survey.rds")
dat_longer_grp <- readr::read_rds("./data/clean_dat_longer_grp.rds")

## skim
dim(dat)
dat %>% skimr::skim()
str(dat)

## Sections partitions (of likert items) -----
## Section 1, Sourcing papers
s1 <- dat[, 5:11]
colnames(s1) <- substr(colnames(s1), 8, 100) ## Remove leading 'source '
## Section 2, reading papers in detail
s2 <- dat[, 14:21]
colnames(s2) <- substr(colnames(s2), 6, 100) ## Remove leading 'read '
## Section 3, assessing venue quality
s3 <- dat[, 24:30]
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

likert_pw <- likert_out1 / likert_out2 / likert_out3
ggsave("likert_all.pdf", likert_pw, "pdf", "figures",
       width = 8, height = 10, units = "in")

### Likert violins -----
require("ggpubr")
my_theme <- list(
  theme_bw(),
  scale_color_viridis_d(),
  scale_fill_viridis_d(), 
  geom_hline(yintercept = mean(dat_longer_grp$`quality correlation`),
             alpha = .8, color = "grey20", linetype = 2L),
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.margin = margin(-6L))
)
my_ggpubr_violin <- function(df = dat_longer_grp, x = "position",
                             y = "quality correlation", title = waiver(), subtitle = waiver()){
  ## Find height of global significance test text.
  .x_lvls <- df %>% pull({{x}}) %>% levels()
  .y_range <- diff(range(df[y]))
  .n_lvls <- length(.x_lvls)
  .lab_y <- -2 #.1 * .y_range + max(df[y])
  .lab_x <- .n_lvls + .3
  
  ## reverse level order.
  df$position <- factor(df$position, levels = rev(levels(dat_longer_grp$position)))
  ## Construct CIs
  ### NEEDS TO GO OFF NORMAL TBL, NOT PIVOTed LONGER...
  .col_num_x <- which(colnames(df) == x)
  .col_num_y <- which(colnames(df) == y)
  df_ci <- df[, c(1L, .col_num_x, .col_num_y)] %>% unique.data.frame()
  g_mean <- mean(dat_longer_grp$`quality correlation`) ## global mean
  ci_bounds_df <- data.frame(NULL)
  for(i in 1:.n_lvls){
    .idx <- df_ci[, 2] == .x_lvls[i]
    .vec <- df_ci[.idx, 3] %>% pull()
    .mn <- mean(.vec)
    .sd <- sd(.vec)
    .n <- length(.vec)# n  participants
    .err <- qnorm(0.975) * .sd / sqrt(.n) ## 1 sided tail for alpha = .05
    t_val <- (.mn - g_mean) / .sd / sqrt(.n)
    p_val <- dt(t_val, df = .n -1)
    ci_bounds_df <- rbind(
      ci_bounds_df, c(.x_lvls[i], .n, round(.mn - .err, 2), round(.mn, 2),  round(.mn + .err, 2),
                      round(t_val, 2), round(p_val, 2), paste0(.x_lvls[i], "\n n = ", .n))
    )
  }
  colnames(ci_bounds_df) <-
    c("level", "n observations", "lower bound", "mean", "upper bound",
      "t value", "p value", "lvl_nm")
  #browser()
  # ci_bounds_df$y <- 1:.n_lvls +.3
  ci_bounds_df[, -8]
  
  ## Plot
  ggviolin(df_ci, x = x, y = y, fill = x, alpha = .6,
           add = c("mean", "mean_ci"), ## Black circle, can change size, but not shape or alpha?
           draw_quantiles = c(.25, .5, .75)) +
    stat_compare_means( ## Global test
      label.y = .lab_y, label.x = .lab_x,
      aes(label = paste0("Kruskal gobal rank test, p-value = ", ..p.format..))) + ## custom label
    # geom_segment(aes(x = lower_bound, xend = lower_bound, y = y, yend = y + .3),
    #   data = ci_bounds_df, color = "red") +
    # geom_segment(aes(x = upper_bound, xend = upper_bound, y = y, yend = y + .3),
    #              data = ci_bounds_df, color = "red") +
    coord_flip() + #xlim = c(1, 5), ylim = c(1, 5)
    #xlim(1, 5) + ylim(1, 5) +
    my_theme +
    scale_x_discrete(labels = rev(ci_bounds_df[, "lvl_nm"])) +
    theme(legend.position = "off") +
    ggtitle(title, subtitle) +
    labs(y = "Likert item: subjective correlation of a venue and its papers' quality \n [1 = no corrlation, 5 = Strong positive correlation]")

}
(quality_violins <- my_ggpubr_violin())

ggsave("quality_violins.pdf", quality_violins, "pdf", "figures",
       width = 6, height = 4, units = "in")

my_ggpubr_scatter <- function(df = dat_longer_grp, x = "years vis experience", shape = "position",
                              y = "quality correlation", title = waiver(), subtitle = waiver()){
  ## Plot
  ggscatter(df, x = x, y = y, shape = shape, color = shape,
            add = "reg.line",
            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            cor.coeff.args = list(method = "spearman", label.x = 3, label.sep = "\n")
  ) +
    labs(y = "Likert item: subjective correlation of a venue and its papers' quality \n [1 = no corrlation, 5 = Strong positive correlation]")
}
(quality_scatter <- my_ggpubr_scatter())

# require("patchwork")
# likert_pw_violins <- likert_pw | qual_violins.pos
# ggsave("likert_all_w_violins.pdf", likert_pw_violins, "pdf", "figures",
#        width = 8, height = 10, units = "in")

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
name <- "corr_all.pdf"
pdf(name)
corrplot.mixed(corr_b, lower = 'number', upper = 'ellipse',
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
(time_hist <- sub %>% ggplot(aes(timestamp)) + 
    geom_density() +
    theme_ipsum())
(demographics <- ggplot(sub, aes(position, `years vis experience`)) +
    ## categorical violine plots?
    geom_violin(width=2.1, size=0.2) +
    viridis::scale_fill_viridis(discrete = TRUE) +
    viridis::scale_color_viridis(discrete = TRUE) +
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


