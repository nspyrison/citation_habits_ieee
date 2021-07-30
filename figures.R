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

## shorten levls for the plot
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
  ggtitle("Section 1, frequency of use when sourcing papers during liturature review") +
  theme(legend.title = element_blank()) 
likert_out2 <- plot(likert_s2) +
  ggtitle("Section 2, importance for deciding wether or not to read in detail") +
  theme(legend.title = element_blank()) +
  scale_color_manual(labels = s23_lvls)
likert_out3 <- plot(likert_s3) +
  ggtitle("Section 3, importance for assessing the quality of a venue") +
  theme(legend.title = element_blank()) +
  scale_color_manual(labels = s23_lvls)
likert_pw <- likert_out1 / likert_out2 / likert_out3
ggsave("likert_all.pdf", likert_pw, "pdf", "figures",
       width = 8, height = 10, units = "in")

### Likert violins -----
require("ggpubr")
my_theme <- list(
  theme_bw(),
  scale_color_viridis_d(name=""),
  scale_fill_viridis_d(name=""), 
  geom_hline(yintercept = c(0, 5),
             alpha = .8, color = "black", linetype = 1L),
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



## Credibility of preprints, Fig7 ------
if(F){
  preprintcred_means_by_discipline <- survey_data %>%
    select(-c(consent, HDI_2017)) %>%
    group_by(discipline_collapsed) %>%
    skim %>%
    yank("numeric") %>%
    rename(question = skim_variable) %>%
    select(discipline_collapsed, question, mean, sd) %>%
    filter(grepl('preprint', question)) %>%
    filter(!is.na(mean)) %>%
    pivot_wider(names_from = discipline_collapsed, 
                names_glue = "{discipline_collapsed}_{.value}",
                values_from = c(mean, sd)) %>%
    mutate(var_name = case_when(question == 'preprint_cred1_1' ~ "Author's previous work",
                                question == 'preprint_cred1_2' ~ "Author's institution",
                                question == 'preprint_cred1_3' ~ "Professional identity links",
                                question == 'preprint_cred1_4' ~ "COI disclosures",
                                question == 'preprint_cred1_5' ~ "Author's level of open scholarship",
                                question == 'preprint_cred2_1' ~ "Funders of research",
                                question == 'preprint_cred2_2' ~ "Preprint submitted to a journal",
                                question == 'preprint_cred2_3' ~ "Usage metrics",
                                question == 'preprint_cred2_4' ~ "Citations of preprints",
                                question == 'preprint_cred3_1' ~ "Anonymous comments",
                                question == 'preprint_cred3_2' ~ "Identified comments",
                                question == 'preprint_cred3_3' ~ "Simplified endorsements",
                                question == 'preprint_cred4_1' ~ "Link to study data",
                                question == 'preprint_cred4_2' ~ "Link to study analysis scripts",
                                question == 'preprint_cred4_3' ~ "Link to materials",
                                question == 'preprint_cred4_4' ~ "Link to pre-reg",
                                question == 'preprint_cred5_1' ~ "Info about indep groups accessing linked info",
                                question == 'preprint_cred5_2' ~ "Info about indep group reproductions",
                                question == 'preprint_cred5_3' ~ "Info about indep robustness checks",
                                TRUE ~ 'Courtney missed a variable')) %>%
    select(var_name, starts_with('Psychology'), starts_with('Other_Social'), starts_with('Life'), starts_with('Med'), starts_with('Phy'))
  
  # get df of complete response Ns
  discipline_complete_ns <- survey_data %>%
    filter(!is.na(discipline_collapsed)) %>%
    select(starts_with('preprint_'), discipline_collapsed) %>%
    pivot_longer(-discipline_collapsed, names_to = 'question', values_to = 'response') %>%
    filter(!is.na(response)) %>%
    group_by(discipline_collapsed, question) %>%
    tally()
  
  #building table
  discipline_table <- preprintcred_means_by_discipline  %>% 
    gt() %>%
    tab_header(title = 'Credibility of Preprints by Discipline') %>%
    data_color(
      columns = ends_with('mean'),
      colors = scales::col_numeric(
        palette = paletteer::paletteer_d(
          package = "RColorBrewer",
          palette = "BrBG"
        ),
        domain = c(1, 5))
    ) %>%
    fmt_number(columns = 2:11, decimals = 2) %>%
    cols_merge(columns = vars(Psychology_mean, Psychology_sd), pattern = '{1} ({2})') %>%
    cols_merge(columns = vars(Life_Sciences_mean, Life_Sciences_sd), pattern = '{1} ({2})') %>%
    cols_merge(columns = vars(Other_SocialSciences_mean, Other_SocialSciences_sd), pattern = '{1} ({2})') %>%
    cols_merge(columns = vars(Phys_Math_mean, Phys_Math_sd), pattern = '{1} ({2})') %>%
    cols_merge(columns = vars(Med_Health_mean, Med_Health_sd), pattern = '{1} ({2})') %>%
    tab_spanner(label = 'Psychology', columns = 'Psychology_mean') %>%
    tab_spanner(label = 'Life Sci (Bio)', columns = 'Life_Sciences_mean') %>%
    tab_spanner(label = 'Med & Health Sci', columns = 'Med_Health_mean') %>%
    tab_spanner(label = 'Soc Sci', columns = 'Other_SocialSciences_mean') %>%
    tab_spanner(label = 'Math & Phys Sci', columns = 'Phys_Math_mean') %>%
    cols_align(align = 'center', columns = ends_with('mean')) %>%
    cols_label(var_name = 'Potential Icon',
               Psychology_mean =  paste0('n = ', discipline_complete_ns %>% filter(discipline_collapsed == 'Psychology') %>% summarize(min = min(n)) %>% pull(min),'-', 
                                         discipline_complete_ns %>% filter(discipline_collapsed == 'Psychology') %>% summarize(max = max(n)) %>% pull(max)),
               Life_Sciences_mean =  paste0('n = ', discipline_complete_ns %>% filter(discipline_collapsed == 'Life_Sciences') %>% summarize(min = min(n)) %>% pull(min),'-', 
                                            discipline_complete_ns %>% filter(discipline_collapsed == 'Life_Sciences') %>% summarize(max = max(n)) %>% pull(max)),
               Phys_Math_mean =  paste0('n = ', discipline_complete_ns %>% filter(discipline_collapsed == 'Phys_Math') %>% summarize(min = min(n)) %>% pull(min),'-', 
                                        discipline_complete_ns %>% filter(discipline_collapsed == 'Phys_Math') %>% summarize(max = max(n)) %>% pull(max)),
               Med_Health_mean = paste0('n = ', discipline_complete_ns %>% filter(discipline_collapsed == 'Med_Health') %>% summarize(min = min(n)) %>% pull(min),'-', 
                                        discipline_complete_ns %>% filter(discipline_collapsed == 'Med_Health') %>% summarize(max = max(n)) %>% pull(max)),
               Other_SocialSciences_mean = paste0('n = ', discipline_complete_ns %>% filter(discipline_collapsed == 'Other_SocialSciences') %>% summarize(min = min(n)) %>% pull(min),'-', 
                                                  discipline_complete_ns %>% filter(discipline_collapsed == 'Other_SocialSciences') %>% summarize(max = max(n)) %>% pull(max)))
  
  gtsave(discipline_table, filename = 'Fig7.png')
}

## New 2) mean/sd table -----
str(dat_longer_grp)
table(dat_longer_grp$position_disp)


dat_longer_grp %>%
  dplyr::select(position_disp, likert_item, response) %>%
  group_by(position_disp, likert_item) %>%
  skim %>%
  yank("numeric") %>%
  dplyr::select(position_disp, likert_item, mean, sd) %>%
  pivot_wider(names_from = position_disp, 
              values_from = c(mean, sd))
#   
# dat_longer_wider <- dat_longer %>%
#   dplyr::select(likert_item, position_disp, response) %>%
#   #unique.data.frame() %>% 
#   tidyr::pivot_wider( ## Doesn't like factors
#     #cols = `source ACM/IEEE DL`:`venue research scope`, #`venue other rank`,
#     names_from = position_disp, values_from = response
#   )
# str(dat_longer)
# str(dat_longer_wider)
# 
# ## this woud be ~ 1/4 of the table when structured
# mn_sd_tbl <- 
#   cbind(dat_longer_wider$likert_item,
#         lapply(dat_longer_wider$`Graduate Student\n n = 11`, mean)        %>% unlist %>% round(1),
#         lapply(dat_longer_wider$`Graduate Student\n n = 11`, sd)          %>% unlist %>% round(1),
#         lapply(dat_longer_wider$`Assistant Professor\n n = 7`, mean)      %>% unlist %>% round(1),
#         lapply(dat_longer_wider$`Assistant Professor\n n = 7`, sd)        %>% unlist %>% round(1),
#         lapply(dat_longer_wider$`Associate Professor\n n = 12`, mean)     %>% unlist %>% round(1),
#         lapply(dat_longer_wider$`Associate Professor\n n = 12`, sd)       %>% unlist %>% round(1),
#         lapply(dat_longer_wider$`Research/Staff Scientist\n n = 7`, mean) %>% unlist %>% round(1),
#         lapply(dat_longer_wider$`Research/Staff Scientist\n n = 7`, sd)   %>% unlist %>% round(1)
#   ) %>% as.data.frame()
# colnames(mn_sd_tbl) <- c("Grad mean", "Grad sd",
#                          "Assistant Prof mean", "Assistant Prof sd",
#                          "Associate Prof mean", "Associate Prof sd",
#                          "Scientist mean", "Scientist sd")


## 3) mixed model -----
#### described in text from the model performance and coeffient table of the summary in `survey_analyses.r`

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


