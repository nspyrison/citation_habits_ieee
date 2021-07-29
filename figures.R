## Dependent packages -------
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
colnames(dat)
colnames(dat_longer_grp)

## Sections partitions (of likert items) -----
## Section 1, Sourcing papers
s1 <- dat[, 6:12]
colnames(s1) <- substr(colnames(s1), 8, 100) ## Remove leading 'source '
## Section 2, reading papers in detail
s2 <- dat[, 15:22]
colnames(s2) <- substr(colnames(s2), 6, 100) ## Remove leading 'read '
## Section 3, assessing venue quality
s3 <- dat[, 25:31]
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
  geom_hline(yintercept = c(0, 5),
             alpha = .8, color = "black", linetype = 1L),
  geom_hline(yintercept = mean(dat_longer_grp$`quality correlation`),
             alpha = .8, color = "grey20", linetype = 2L),
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.margin = margin(-6L))
)
lvls_left <- levels(dat_longer_grp$position)[-1:-2]
dat_longer_grp$position <- factor(dat_longer_grp$position, levels = lvls_left)
my_ggpubr_violin <- function(df = dat_longer_grp, x = "position",
                             y = "quality correlation", title = waiver(), subtitle = waiver()){
  ## Find height of global significance test text.
  .x_lvls <- df %>% pull({{x}}) %>% levels()
  .y_range <- diff(range(df[y]))
  .n_lvls <- length(.x_lvls)
  .lab_y <- -2 #.1 * .y_range + max(df[y])
  .lab_x <- .n_lvls + .3
  
  ## Construct CIs
  ### NEEDS TO GO OFF NORMAL TBL, NOT PIVOTed LONGER...
  g_mean <- mean(dat_longer_grp$`quality correlation`) ## global mean
  .col_num_x <- which(colnames(df) == x)
  .col_num_y <- which(colnames(df) == y)
  df_ci <- df[, c(1L, .col_num_x, .col_num_y)] %>% unique.data.frame() %>% as.data.frame()
  ci_bounds_df <- data.frame(NULL)
  for(i in 1:.n_lvls){
    .idx <- df_ci[, 2] == .x_lvls[i]
    .vec <- df_ci[.idx, 3]
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
  ci_bounds_df[, -8]
  
  ## Plot
  ggviolin(df_ci, x = x, y = y, fill = x, alpha = .6,
           add = c("mean", "mean_ci"), ## Black circle, can change size, but not shape or alpha?
           draw_quantiles = c(.25, .5, .75)) +
    stat_compare_means( ## Global test
      label.y = .lab_y, label.x = .lab_x,
      aes(label = paste0("Kruskal gobal rank test, p-value = ", ..p.format..))) + ## custom label
    coord_flip() +
    my_theme +
    scale_x_discrete(labels = ci_bounds_df[, "lvl_nm"]) +
    theme(legend.position = "off") +
    ggtitle(title, subtitle) +
    labs(y = "Likert item: subjective correlation of a venue and its papers' quality \n [1 = no corrlation, 5 = Strong positive correlation]")

}
(quality_violins <- my_ggpubr_violin())

ggsave("quality_violins.pdf", quality_violins, "pdf", "figures",
       width = 6, height = 4, units = "in")



## 2) Correlation figures ------
dat_b <- cbind(s1, s2, s3) %>% lapply(as.integer) %>% as_tibble()
corr_b <- dat_b %>% cor(use = 'pairwise.complete.obs', method = 'spearman')

## Saving Correlation plots
path <- "./figures/"
name <- "corr_all.pdf"
pdf(name)
corrplot.mixed(corr_b, lower = 'number', upper = 'ellipse',
               order = 'FPC') ## "AOE", "FPC", "hclust"
dev.off()
file.copy(name, to = paste0(path, name), overwrite = TRUE)
file.remove(name)

## New 2) mean/sd table -----

dat_longer_wider <- dat_longer %>%
  dplyr::select(likert_item, position_disp, response) %>%
  #unique.data.frame() %>% 
  tidyr::pivot_wider( ## Doesn't like factors
    #cols = `source ACM/IEEE DL`:`venue research scope`, #`venue other rank`,
    names_from = position_disp, values_from = response
  )
str(dat_longer)
str(dat_longer_wider)

## this woud be ~ 1/4 of the table when structured
mn_sd_tbl <- 
  cbind(dat_longer_wider$likert_item,
        lapply(dat_longer_wider$`Graduate Student\n n = 11`, mean)        %>% unlist %>% round(1),
        lapply(dat_longer_wider$`Graduate Student\n n = 11`, sd)          %>% unlist %>% round(1),
        lapply(dat_longer_wider$`Assistant Professor\n n = 7`, mean)      %>% unlist %>% round(1),
        lapply(dat_longer_wider$`Assistant Professor\n n = 7`, sd)        %>% unlist %>% round(1),
        lapply(dat_longer_wider$`Associate Professor\n n = 12`, mean)     %>% unlist %>% round(1),
        lapply(dat_longer_wider$`Associate Professor\n n = 12`, sd)       %>% unlist %>% round(1),
        lapply(dat_longer_wider$`Research/Staff Scientist\n n = 7`, mean) %>% unlist %>% round(1),
        lapply(dat_longer_wider$`Research/Staff Scientist\n n = 7`, sd)   %>% unlist %>% round(1)
  ) %>% as.data.frame()
colnames(mn_sd_tbl) <- c("Grad mean", "Grad sd",
                         "Assistant Prof mean", "Assistant Prof sd",
                         "Associate Prof mean", "Associate Prof sd",
                         "Scientist mean", "Scientist sd")

g_mean <- mean(dat_longer_grp$`quality correlation`) ## global mean
.col_num_x <- which(colnames(df) == x)
.col_num_y <- which(colnames(df) == y)
df_ci <- df[, c(1L, .col_num_x, .col_num_y)] %>% unique.data.frame() %>% as.data.frame()
ci_bounds_df <- data.frame(NULL)
for(i in 1:.n_lvls){
  .idx <- df_ci[, 2] == .x_lvls[i]
  .vec <- df_ci[.idx, 3]
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


## 3) mixed model -----


## (4) demographic heatmap ------

## Change character to factor, include counts in the levels of sex?
str(clean)
sub <- clean[, c("timestamp", "position", "position_disp", "years vis experience")]
# (time_hist <- sub %>% ggplot(aes(timestamp)) + 
#     geom_density() +
#     theme_ipsum())
(demographics <- 
    ggplot(sub, aes(position_disp, `years vis experience`, fill = position_disp),
           alpha = .5) +
    ## categorical violine plots?
    geom_violin(width = 1.3) +
    theme_bw() +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    #theme_ipsum() +
    theme(legend.position="none") +
    coord_flip() + # This switch X and Y axis and allows to get the horizontal version
    ylab("Years experience of data visualization") +
    xlab("Position") +
    ggtitle("Participant demographics"))

ggsave(filename = "./figures/demographics.pdf",
       plot = demographics, device = "pdf", width = 6, height = 4)


## (5) Factor analysis/PCA -----


### Split data on type of response ------
text_attr <- clean %>% ## Freeform text fields
  dplyr::select("timestamp",
                "source other text",   ## ~col 12
                "read other text",     ## ~col 24
                "venue other text",    ## ~col 36
                "other comments text") ## ~col 37


