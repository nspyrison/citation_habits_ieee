## Dependent packages -------
require("tidyverse")
require("skimr")
require("likert")
require("corrplot")
require("patchwork")
require("hrbrthemes")
require("gt")


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

## shorten levels for the plot
.orig_lvls <- s2[,1] %>% pull() %>% levels()
.new_lvls <- c("Not at all impt.", "Slightly impt.", "Moderately impt.", "Very impt.", "Extremely impt.")

.cn <- colnames(s2)
s2 <- lapply(as.data.frame(s2), function(c){
  plyr::mapvalues(c, .orig_lvls, .new_lvls) 
}) %>% as.data.frame()
colnames(s2) <- .cn

.cn <- colnames(s3)
s3 <- lapply(as.data.frame(s3), function(c){
  plyr::mapvalues(c, .orig_lvls, .new_lvls) 
}) %>% as.data.frame()
colnames(s3) <- .cn

## 1) Likert figures -----
## to plot, call plot(likert_obj)
likert_s1 <- s1 %>% as.data.frame() %>% likert()
likert_s2 <- s2 %>% as.data.frame() %>% likert()
likert_s3 <- s3 %>% as.data.frame() %>% likert()

## saving
likert_out1 <- plot(likert_s1) + 
  ggtitle("Likert scale 1, frequency of use when sourcing papers during liturature review") +
  theme(legend.title = element_blank())
likert_out2 <- plot(likert_s2) +
  ggtitle("Likert scale 2, importance for deciding wether or not to read in detail") +
  theme(legend.title = element_blank())
likert_out3 <- plot(likert_s3) +
  ggtitle("Likert scale 3, importance for assessing the quality of a venue") +
  theme(legend.title = element_blank())
likert_pw <- likert_out1 / likert_out2 / likert_out3
ggsave("likert_all.pdf", likert_pw, "pdf", "figures",
       width = 8, height = 10, units = "in")
if(f)
  likert::likert.bar.plot

### Likert violins -----
require("ggpubr")
my_theme <- list(
  theme_bw(),
  scale_color_viridis_d(name = ""),
  scale_fill_viridis_d(name = ""), 
  geom_hline(yintercept = c(0L, 5L),
             alpha = .8, color = "black", linetype = 1L),
  geom_hline(yintercept = mean(dat_longer_grp$`quality correlation`),
             alpha = .8, color = "grey20", linetype = 2L),
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.margin = margin(-6L))
)
my_ggpubr_violin <- function(
  df = dat_longer_grp, x = "position",
  y = "quality correlation", title = waiver(), subtitle = waiver(),
  adjust = 1){
  ## Find height of global significance test text.
  .x_lvls <- df %>% pull({{x}}) %>% levels()
  .y_range <- diff(range(df[y]))
  .n_lvls <- length(.x_lvls)
  .lab_y <- 1.3 #.1 * .y_range + max(df[y])
  .lab_x <- .n_lvls + .3
  
  ## Ensure rows aren't dupicated duplicated counts.
  .col_idx <- c(1,  which(colnames(df) == x), which(colnames(df) == y))
  df_ci <- df[, .col_idx] %>% unique.data.frame() %>% as.data.frame()
  
  ## Plot
  ggviolin(df_ci, x = x, y = y, fill = x, alpha = .6,
           add = c("mean", "mean_ci"), ## Black circle, can change size, but not shape or alpha?
           draw_quantiles = c(.25, .5, .75),
           trim = TRUE, adjust = adjust) +
    stat_compare_means( ## Global test
      label.y = .lab_y, label.x = .lab_x,
      aes(label = paste0("Kruskal gobal rank test, p-value  = ", ..p.format..))) + ## custom label
    coord_flip() +
    my_theme +
    theme(legend.position = "off") +
    ggtitle(title, subtitle) +
    ylim(0, 5) +
    labs(x = "Position",
         y = "Likert item: subjective correlation of a venue and its papers' quality \n [1 = no corrlation, 5 = Strong positive correlation]")

}
(quality_violins <- my_ggpubr_violin(adjust = .1))
ggsave("quality_violins.pdf", quality_violins, "pdf", "figures",
       width = 6, height = 4, units = "in")


## New 2) position response mean/sd table -----
str(dat_longer_grp)
table(dat_longer_grp$position)

## Reverse levels
.lvls <- levels(dat_longer_grp$position)
dat_longer_grp$position <- plyr::mapvalues(dat_longer_grp$position, .lvls, rev(.lvls))
position_table <- dat_longer_grp %>%
  dplyr::select(position, likert_item, response) %>%
  group_by(position, likert_item) %>%
  skim %>%
  yank("numeric") %>%
  dplyr::select(position, likert_item, mean, sd) %>%
  pivot_wider(names_from = position,
              values_from = c(mean, sd))
colnames(position_table)[1] <- "Likert item"

require("gt")
position_gt <- position_table %>% 
  gt() %>%
  tab_header(title = 'Response of Likert item across position',
             subtitle = "Mean (standard deviation)") %>%
  gt::data_color(
    columns = starts_with('mean'),
    colors = scales::col_numeric(
      palette = RColorBrewer::brewer.pal(5, "BrBG"),
      domain = 1:5)
  ) %>%
  fmt_number(columns = 2:9, decimals = 2) %>%
  cols_merge(columns = vars(`mean_Graduate Student\n n = 11`, `sd_Graduate Student\n n = 11`), pattern = '{1} ({2})') %>%
  cols_merge(columns = vars(`mean_Associate Professor\n n = 12`, `sd_Associate Professor\n n = 12`), pattern = '{1} ({2})') %>%
  
  cols_merge(columns = vars(`mean_Assistant Professor\n n = 7`,  `sd_Assistant Professor\n n = 7`), pattern = '{1} ({2})') %>%
  cols_merge(columns = vars(`mean_Research/Staff Scientist\n n = 7`, `sd_Research/Staff Scientist\n n = 7`), pattern = '{1} ({2})') %>%
  tab_spanner("Position", columns = `mean_Graduate Student\n n = 11`:`mean_Research/Staff Scientist\n n = 7`) %>% 
  cols_label(
    `mean_Graduate Student\n n = 11` = "Graduate Student, n = 11",
    `mean_Associate Professor\n n = 12` = "Associate Professor, n = 12",
    `mean_Assistant Professor\n n = 7` = "Assistant Professor, n = 7",
    `mean_Research/Staff Scientist\n n = 7` = "Research/Staff Scientist, n = 7") %>% 
  cols_align(align = 'center', columns = starts_with('mean'))
gtsave(position_gt, filename = './figures/position_gt.png')


## 3) mixed model -----
#### described in text from the model performance and coeffient table of the summary in `survey_analyses.r`

## (4) demographic heatmap ------

## Change character to factor, include counts in the levels of sex?
str(dat)
sub <- dat[, c("timestamp", "position", "years vis experience")]
(demographics <-
    ggplot(sub, aes(position, `years vis experience`, fill = position),
           alpha = .5) +
    geom_violin(width = 1.3) +
    theme_bw() +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    theme(legend.position="none") +
    coord_flip() + # This switch X and Y axis and allows to get the horizontal version
    ylab("Years experience of data visualization") +
    xlab("Position") +
    ggtitle("Participant demographics"))
ggsave(filename = "./figures/demographics.pdf",
       plot = demographics, device = "pdf", width = 6, height = 4)


## (5) Factor analysis/PCA -----


### Split data on type of response ------
text_attr <- dat %>% ## Freeform text fields
  dplyr::select("timestamp",
                "source other text",   ## ~col 12
                "read other text",     ## ~col 24
                "venue other text",    ## ~col 36
                "other comments text") ## ~col 37


