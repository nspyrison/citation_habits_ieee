## required libraries
library(osfr)
library(tidyverse)
library(likert)
library(here)
library(skimr)
library(gt)
library(patchwork)

## reading in data
osf_retrieve_file("https://osf.io/86upq/") %>% 
  osf_download()

services_table <- read_csv(here::here('pp_services_coding.csv'), col_types = cols(.default = col_factor())) %>%
                      mutate(arxiv = fct_expand(arxiv, 'Partly'),
                             ssrn = fct_expand(ssrn, 'Partly'),
                             peerj = fct_expand(peerj, 'Partly'),
                             nber = fct_expand(nber, 'Partly', 'Yes')) %>%
                      mutate_at(vars(arxiv:nber), ~fct_relevel(., c('No', 'Partly', 'Yes')))

all_data <- read_csv(here::here('cleaned_data.csv'), col_types = cols(.default = col_number(),
                                                                         StartDate = col_datetime(format = '%m/%d/%y %H:%M'),
                                                                         EndDate = col_datetime(format = '%m/%d/%y %H:%M'),
                                                                         ResponseId = col_character(),
                                                                         position_7_TEXT = col_character(), 
                                                                         familiar = col_factor(),
                                                                         preprints_submitted = col_factor(),
                                                                         preprints_used = col_factor(),
                                                                         position = col_factor(),
                                                                         acad_career_stage = col_factor(),
                                                                         country = col_factor(),
                                                                         continent = col_factor(),
                                                                         discipline = col_character(),
                                                                         discipline_specific = col_character(),
                                                                         discipline_other = col_character(),
                                                                         bepress_tier1 = col_character(),
                                                                         bepress_tier2 = col_character(),
                                                                         bepress_tier3 = col_character(),
                                                                         discipline_collapsed = col_factor(),
                                                                         how_heard = col_character(),
                                                                         hdi_level = col_factor(),
                                                                         age = col_character())) %>%
                  mutate(acad_career_stage = fct_recode(acad_career_stage, Full_Prof = 'Full Prof', Assoc_Prof = 'Assoc Prof', Assist_Prof = 'Assist Prof', Post_doc = 'Post doc', Grad_Student = 'Grad Student'),
                         acad_career_stage = fct_relevel(acad_career_stage, 'Full_Prof', 'Assoc_Prof', 'Assist_Prof', 'Post_doc', 'Grad_Student'),
                         discipline_collapsed = fct_recode(discipline_collapsed, Other_SocialSciences = 'Other Social Sciences', Life_Sciences = 'Life Sciences (Biology)', Med_Health = 'Medicine and Health Sciences', Phys_Math = 'Physical Sciences and Mathematics'),
                         preprints_used = recode_factor(preprints_used, `Not sure` = NA_character_),
                         preprints_submitted = recode_factor(preprints_submitted, `Not sure` = NA_character_),
                         preprints_used = fct_relevel(preprints_used, 'No', 'Yes, once', 'Yes, a few times', 'Yes, many times', 'Not sure'),
                         preprints_submitted = fct_relevel(preprints_submitted, 'No', 'Yes, once', 'Yes, a few times', 'Yes, many times', 'Not sure'))



all_data <- all_data %>%
  mutate(missing_qs = rowSums(is.na(all_data)))

survey_data <- all_data %>%
  filter(missing_qs < 54)

#### Overall icons importance (Fig 5)####
preprint_cred <- survey_data %>%
  dplyr::select(preprint_cred1_1:preprint_cred5_3)

choices  <- c('Not at all important', 'Slightly important', 'Moderately important', 'Very important', 'Extremely important')

colnames(preprint_cred) <- c(preprint_cred1_1 = "Author's previous work",
                             preprint_cred1_2 = "Author's institutions",
                             preprint_cred1_3 = "Professional Identify links (e.g. ORCID, GoogleScholar)",
                             preprint_cred1_4 = "COI disclosures",
                             preprint_cred1_5 = "Author(s) general levels open scholarship",
                             preprint_cred2_1 = "Funder(s) of the research",
                             preprint_cred2_2 = "Preprint submitted to a journal",
                             preprint_cred2_3 = "Usage metrics about the preprint",
                             preprint_cred2_4 = "Citations of the preprint",
                             preprint_cred3_1 = "Anonymous users comments",
                             preprint_cred3_2 = "Identified user comments",
                             preprint_cred3_3 = "Simplified endorsement by users",
                             preprint_cred4_1 = "Links to any available study data",
                             preprint_cred4_2 = "Links to any available analysis scripts",
                             preprint_cred4_3 = "Links to any available materials",
                             preprint_cred4_4 = "Links to any pre-registrations or pre-analysis plans",
                             preprint_cred5_1 = "Info about whether indep groups could access linked info",
                             preprint_cred5_2 = "Info about indep reproductions",
                             preprint_cred5_3 = "Info about indep robustness checks")

preprint_cred <- preprint_cred %>%
  mutate_all(factor, levels=1:5, labels=str_wrap(choices, 10), ordered=TRUE)

cred_preprints<- expression(atop("When assessing the credibility of a preprint", paste("how important would it be to have each of the following pieces of information?")))
pdf("Fig5.pdf", width=12.5, height=10)
plot(likert(as.data.frame(preprint_cred)), ordered=T, text.size = 4) + 
  ggtitle(cred_preprints)+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.text=element_text(size=12), axis.text = element_text(size = 12))
dev.off()

## outputs for PLOSBio formatting
cred_preprints<- expression(atop("When assessing the credibility of a preprint", paste("how important would it be to have each of the following pieces of information?")))
icon_plot <- plot(likert(as.data.frame(preprint_cred)), ordered=T, text.size = 2.8, wrap = 35) + 
  ggtitle(cred_preprints)+
  theme(plot.title = element_text(hjust = 0.5, size = 10), 
        legend.title = element_blank(), 
        legend.text=element_text(size=8), 
        legend.justification = 'left',
        legend.margin = margin(l = 0, unit = 'pt'),
        axis.text = element_text(size = 8))

ggsave(plot = icon_plot, "Fig5.eps", width=7.5, height=8.75, dpi = 600, units = 'in')

#### table by career stage (Fig8) ####
preprintcred_means_by_position <- survey_data %>%
  select(-c(consent, HDI_2017)) %>%
  group_by(acad_career_stage) %>%
  skim %>%
  yank("numeric") %>%
  rename(question = skim_variable) %>%
  select(acad_career_stage, question, mean, sd) %>%
  filter(grepl('preprint', question)) %>%
  filter(!is.na(mean)) %>%
  pivot_wider(names_from = acad_career_stage, 
              names_glue = "{acad_career_stage}_{.value}",
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
                              TRUE ~ 'Courtney missed a question')) %>%
  select(var_name, starts_with('Grad'), starts_with('Post'), starts_with('Assist'), starts_with('Assoc'), starts_with('Full'))

# get df of complete response Ns
career_stage_complete_ns <- survey_data %>%
  filter(!is.na(acad_career_stage)) %>%
  select(starts_with('preprint_'), acad_career_stage) %>%
  pivot_longer(-acad_career_stage, names_to = 'question', values_to = 'response') %>%
  filter(!is.na(response)) %>%
  group_by(acad_career_stage, question) %>%
  tally()

#building table
career_stage_table <- preprintcred_means_by_position %>% 
  gt() %>%
  tab_header(title = 'Career Stage') %>%
  data_color(
    columns = vars(Grad_Student_mean, Post_doc_mean, Assist_Prof_mean, Assoc_Prof_mean, Full_Prof_mean),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        package = "RColorBrewer",
        palette = "BrBG"
      ),
      domain = c(1, 5))
  ) %>%
  fmt_number(columns = 2:11, decimals = 2) %>%
  cols_merge(columns = vars(Grad_Student_mean, Grad_Student_sd), pattern = '{1} ({2})') %>%
  cols_merge(columns = vars(Post_doc_mean, Post_doc_sd), pattern = '{1} ({2})') %>%
  cols_merge(columns = vars(Assist_Prof_mean, Assist_Prof_sd), pattern = '{1} ({2})') %>%
  cols_merge(columns = vars(Assoc_Prof_mean, Assoc_Prof_sd), pattern = '{1} ({2})') %>%
  cols_merge(columns = vars(Full_Prof_mean, Full_Prof_sd), pattern = '{1} ({2})') %>%
  tab_spanner(label = 'Grad Student', columns = 'Grad_Student_mean') %>%
  tab_spanner(label = 'Post doc', columns = 'Post_doc_mean') %>%
  tab_spanner(label = 'Assist Prof', columns = 'Assist_Prof_mean') %>%
  tab_spanner(label = 'Assoc Prof', columns = 'Assoc_Prof_mean') %>%
  tab_spanner(label = 'Full Prof', columns = 'Full_Prof_mean') %>%
  cols_align(align = 'center', columns = ends_with('mean')) %>%
  cols_label(var_name = 'Potential Icon',
             Grad_Student_mean = paste0('n = ', career_stage_complete_ns %>% filter(acad_career_stage == 'Grad_Student') %>% summarize(min = min(n)) %>% pull(min),'-', 
             career_stage_complete_ns %>% filter(acad_career_stage == 'Grad_Student') %>% summarize(max = max(n)) %>% pull(max)),
             Post_doc_mean = paste0('n = ', career_stage_complete_ns %>% filter(acad_career_stage == 'Post_doc') %>% summarize(min = min(n)) %>% pull(min),'-', 
                                    career_stage_complete_ns %>% filter(acad_career_stage == 'Post_doc') %>% summarize(max = max(n)) %>% pull(max)),
             Assist_Prof_mean = paste0('n = ', career_stage_complete_ns %>% filter(acad_career_stage == 'Assist_Prof') %>% summarize(min = min(n)) %>% pull(min),'-', 
                                        career_stage_complete_ns %>% filter(acad_career_stage == 'Assist_Prof') %>% summarize(max = max(n)) %>% pull(max)),
             Assoc_Prof_mean = paste0('n = ', career_stage_complete_ns %>% filter(acad_career_stage == 'Assoc_Prof') %>% summarize(min = min(n)) %>% pull(min),'-', 
                                      career_stage_complete_ns %>% filter(acad_career_stage == 'Assoc_Prof') %>% summarize(max = max(n)) %>% pull(max)),
             Full_Prof_mean =  paste0('n = ', career_stage_complete_ns %>% filter(acad_career_stage == 'Full_Prof') %>% summarize(min = min(n)) %>% pull(min),'-', 
                                      career_stage_complete_ns %>% filter(acad_career_stage == 'Full_Prof') %>% summarize(max = max(n)) %>% pull(max)))

gtsave(career_stage_table, filename = 'Fig8.png')


#### table by academic discipline (Fig7)####

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


## Overall service credibilitys

service_cred <- survey_data %>%
  dplyr::select(services_cred1_1:service_credible4_5)

choices <- c('Decrease a lot', "Moderately decrease", "Slightly decrease", "Neither decrease nor increase", "Slightly increase", "Moderately increase", "Increase a lot")

colnames(service_cred) <- c(services_cred1_1 = "Moderators that screen for spam and non-scholarly content", 
                            services_cred1_2 = "Scholars in my field are involved in the operation",
                            services_cred1_3 = "Clear policies about misconduct and plagiarism", 
                            services_cred1_4 = "Assesses the reproducibility of reported findings",
                            service_cred2_1 = "Open source (e.g. openly licensed) software", 
                            service_cred2_2 = "Business model is transparent, stable and sustainable", 
                            service_cred2_3 = "Mechanism for long-term preservation of content",
                            service_cred2_4 = "Free for preprint submitters and readers", 
                            service_cred3_1 = "Enables preprints to be submitted to journals directly from the service site", 
                            service_cred3_2 = "Indicates which content published in a peer-reviewed journal",
                            service_cred3_3 = "Allows endorsements of preprints by independent users (i.e. non-authors)", 
                            service_cred3_4 = "Indexed by search and discovery services (e.g., Google Scholar, Crossref)", 
                            service_cred3_5 = "Popular in my discipline", 
                            service_credible4_1 = "Ability to assign a DOI to the preprint", 
                            service_credible4_2 = "Enables anonymous posting of preprints", 
                            service_credible4_3 = "Allows authors to remove their preprints for any reason",
                            service_credible4_4 = "Controls remove/withdrawal of preprints", 
                            service_credible4_5 = "Enables submission of new versions of the preprint")


service_cred <- service_cred %>%
  mutate_all(factor, levels=-3:3, labels=choices, ordered=TRUE)

service_preprints<- expression(atop("To what extent would having each of the following features ", paste("decrease or increase the credibility of a preprint service?")))
pdf("service_cred.pdf", width=12.5, height=10)
plot(likert(as.data.frame(service_cred)), ordered=T, text.size = 4) + ggtitle(service_preprints)+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.text=element_text(size=12), axis.text = element_text(size = 12))
dev.off()


# Alter names of levels and wrap for better graph display
survey_data <- survey_data %>%
                      mutate(discipline_collapsed = fct_recode(discipline_collapsed, 
                                                               Psych = 'Psychology',
                                                               `Soc Sci` = 'Other_SocialSciences', 
                                                               `Life Sci (Bio)` = 'Life_Sciences', 
                                                               `Med & Health` = 'Med_Health', 
                                                               `Math & Phys Sci` = 'Phys_Math',
                                                               Eng = 'Engineering'))

levels(survey_data$discipline_collapsed) <- str_wrap(levels(survey_data$discipline_collapsed),10)

#### use/submissions of preprints by discipline (Fig3) ####

cols <- c('No' = '#D7B463', 'Yes, once' = '#ECD9AE', 'Yes, a few times' = '#ACDBD6', 'Yes, many times' = '#57B5AD')

discipline_used <- survey_data %>%
    filter(!is.na(preprints_used), discipline_collapsed != 'Other', discipline_collapsed != '(Missing)', preprints_used != 'Not sure') %>%  
    mutate(preprints_used = fct_rev(preprints_used)) %>%
    group_by(discipline_collapsed, preprints_used) %>%
    tally() %>%
    mutate(perc = round(100*n/sum(n),2),
           percentage = paste0(perc, '%')) %>%
    ggplot(aes(fill = preprints_used, x = discipline_collapsed, y = perc)) +
    geom_col(stat = 'identity', position = 'fill') +
    geom_text(aes(x = discipline_collapsed ,label = percentage), size = 2.5, position=position_fill(vjust=0.5)) +
    ggtitle(' ') +  
    scale_y_continuous(labels=scales::percent, expand = c(0, 0)) +  
    scale_fill_manual(values = cols) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(legend.text=element_text(size=8), legend.title = element_blank(),
          axis.text = element_text(size = 8), axis.title = element_blank(),
          legend.position = 'bottom',
          plot.margin = margin(t = 15, l = 15, r = 15, b = 15, "pt"), axis.ticks.length.x = unit(5, 'pt'))
    
discipline_submit <- survey_data %>%
  filter(!is.na(preprints_submitted), discipline_collapsed != 'Other', discipline_collapsed != '(Missing)', preprints_submitted != 'Not sure') %>%
  mutate(preprints_submitted = fct_rev(preprints_submitted)) %>%
  group_by(discipline_collapsed, preprints_submitted) %>%
  tally() %>%
  mutate(perc = round(100*n/sum(n),2),
         percentage = paste0(perc, '%')) %>%
  ggplot(aes(fill = preprints_submitted, x = discipline_collapsed, y = perc)) +
  geom_col(stat = 'identity', position = 'fill') +
  geom_text(aes(x = discipline_collapsed ,label = percentage), size = 2.5, position=position_fill(vjust=0.5)) +
  ggtitle(' ') +
  scale_y_continuous(labels=scales::percent, expand = c(0, 0)) +
  scale_fill_manual(values = cols) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.text=element_text(size=8), legend.title = element_blank(),
        axis.text = element_text(size = 8), axis.title = element_blank(),
        legend.position = 'bottom',
        plot.margin = margin(t = 15, l = 15, r = 15, b = 15, "pt"), axis.ticks.length.x = unit(5, 'pt'))


discipline_use <- discipline_used + discipline_submit + plot_layout(guides = 'collect') + 
  plot_annotation(tag_levels = 'A', tag_prefix = 'Fig.3') & 
  theme(legend.position = 'bottom', legend.text=element_text(size=8), plot.tag = element_text(size = 8), axis.text = element_text(size = 8))

ggsave(plot = discipline_use, "Fig3.eps", width = 7.5, height = 5.0, dpi = 600, units = 'in')

#### favor-use by discipline (Figure 1) ####
discipline_favor <- survey_data %>% 
                    mutate(favor_use = as.factor(favor_use),
                           favor_use = fct_recode(favor_use, `Very unfavorable` = '-3', `Somewhat unfavorable` = '-2', `Slightly unfavorable` = '-1', `Neither unfavorable nor favorable` = '0', `Slightly favorable` = '1', `Somewhat favorable` = '2', `Very favorable`= '3')) %>% 
                    select(favor_use, discipline_collapsed, ResponseId) %>% 
                    filter(!is.na(discipline_collapsed) & discipline_collapsed != 'Other' & discipline_collapsed != '(Missing)') %>%
                    pivot_wider(names_from = discipline_collapsed, values_from = favor_use, id_cols = ResponseId) %>%
                    select(-ResponseId)

favor_use_plot <- plot(likert(as.data.frame(discipline_favor)), text.size = 2.8) +
  theme(legend.title = element_blank(), legend.text=element_text(size=8), axis.text = element_text(size = 8))

ggsave(plot = favor_use_plot, "Fig1.eps", width=7.5, height=5.0, dpi = 600, units = 'in')


# Alter names of levels and wrap for better graph display
survey_data <- survey_data %>%
                    mutate(acad_career_stage = fct_recode(acad_career_stage, 
                                                             `Grad Student` = 'Grad_Student',
                                                             `Post doc` = 'Post_doc', 
                                                             `Assist Prof` = 'Assist_Prof', 
                                                             `Assoc Prof` = 'Assoc_Prof', 
                                                             `Full Prof` = 'Full_Prof'))

levels(survey_data$acad_career_stage) <- str_wrap(levels(survey_data$acad_career_stage),6)
  

#### use/submissions of preprints by academic career stage ####
career_used <- survey_data %>%
  filter(!is.na(preprints_used), acad_career_stage != '(Missing)', preprints_used != 'Not sure') %>%
  mutate(preprints_used = fct_rev(preprints_used)) %>%
  group_by(acad_career_stage, preprints_used) %>%
  tally() %>%
  mutate(perc = round(100*n/sum(n),2),
         percentage = paste0(perc, '%')) %>%
  ggplot(aes(fill = preprints_used, x = reorder(acad_career_stage, desc(acad_career_stage)), y = perc)) +
  geom_col(stat = 'identity', position = 'fill') +
  geom_text(aes(x = acad_career_stage ,label = percentage), size = 2.8, position=position_fill(vjust=0.5)) +
  ggtitle(' ') + 
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels=scales::percent, expand = c(0, 0)) +
  scale_fill_manual(values = cols) +
  theme(legend.text=element_text(size=8), legend.title = element_blank(),
        axis.text = element_text(size = 8), axis.title = element_blank(),
        legend.position = 'bottom',
        plot.margin = margin(t = 15, l = 15, r = 15, b = 15, "pt"), axis.ticks.length.x = unit(5, 'pt'))



career_submit <- survey_data %>%
  filter(!is.na(preprints_submitted), acad_career_stage != '(Missing)', preprints_submitted != 'Not sure') %>%
  mutate(preprints_submitted = fct_rev(preprints_submitted)) %>%
  group_by(acad_career_stage, preprints_submitted) %>%
  tally() %>%
  mutate(perc = round(100*n/sum(n),2),
         percentage = paste0(perc, '%')) %>%
  ggplot(aes(fill = preprints_submitted, x = reorder(acad_career_stage, desc(acad_career_stage)), y = perc)) +
  geom_col(stat = 'identity', position = 'fill') +
  geom_text(aes(x = acad_career_stage ,label = percentage), size = 2.8, position=position_fill(vjust=0.5)) +
  ggtitle(' ') +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels=scales::percent, expand = c(0, 0)) +
  scale_fill_manual(values = cols) +
  theme(legend.text=element_text(size=8), legend.title = element_blank(),
        axis.text = element_text(size = 8), axis.title = element_blank(),
        legend.position = 'bottom',
        plot.margin = margin(t = 15, l = 15, r = 15, b = 15, "pt"), axis.ticks.length.x = unit(5, 'pt'))


career_use <- career_used + career_submit + plot_layout(guides = 'collect') + 
  plot_annotation(tag_levels = 'A', tag_prefix = 'Fig.4') & 
  theme(legend.position = 'bottom', plot.tag = element_text(size = 8))

ggsave(plot = career_use, "Fig4.eps", width = 7.5, height = 5.0, dpi = 600, units = 'in')

#### favor-use by career stage (Figure 2)####
career_stage <- survey_data %>% 
  mutate(favor_use = as.factor(favor_use),
         favor_use = fct_recode(favor_use, `Very unfavorable` = '-3', `Somewhat unfavorable` = '-2', `Slightly unfavorable` = '-1', `Neither unfavorable nor favorable` = '0', `Slightly favorable` = '1', `Somewhat favorable` = '2', `Very favorable`= '3')) %>% 
  select(favor_use, acad_career_stage, ResponseId) %>% 
  filter(!is.na(acad_career_stage)) %>%
  pivot_wider(names_from = acad_career_stage, values_from = favor_use, id_cols = ResponseId) %>%
  select(-ResponseId)

favor_use_career_plot <- plot(likert(as.data.frame(career_stage)), text.size = 2.8) +
  theme(legend.title = element_blank(), legend.text=element_text(size=8), axis.text = element_text(size = 8))

ggsave(plot = favor_use_career_plot, "Fig2.eps", width=7.5, height=5.0, dpi = 600, units = 'in')

#### correlation favor-use/use/submissions and credibility questions (Fig6) ####
correlations1 <- survey_data %>%
  select(preprints_used, preprints_submitted, starts_with('preprint_cred')) %>%
  mutate(preprints_used = as.numeric(preprints_used),
         preprints_submitted = as.numeric(preprints_submitted)) %>%
  cor(use = 'pairwise.complete.obs', method = 'spearman')

correlations2 <- survey_data %>%
  select(favor_use, starts_with('preprint_cred')) %>%
  cor(use = 'pairwise.complete.obs')

correlations <- cbind(correlations1[3:21, 1:2], correlations2[2:20, 1])

correlation_table <- as.data.frame(correlations) %>%
  rename(favor_use = V3) %>%
  rownames_to_column('question') %>% 
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
  select(-question) %>%
  gt(rowname_col = 'var_name') %>%
  fmt_number(everything(), decimals = 2) %>%
  data_color(
    columns = vars(favor_use,preprints_used,preprints_submitted),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        package = "RColorBrewer",
        palette = "BrBG"
      ),
      domain = c(-1, 1))
  ) %>%
  cols_label(
    favor_use = 'Favor use',
    preprints_used = 'View/Download Preprints',
    preprints_submitted = 'Submit Preprints'
  ) %>%
  cols_align(align = 'center')

gtsave(correlation_table, filename = 'Fig6.png')


#### table of icons on services ####
services_icon_table <- services_table %>%
  mutate(nber = fct_expand(nber, 'Yes')) %>%
  gt(rowname_col = 'icon') %>%
  cols_align(align = 'center') %>%
  data_color(
    columns = vars(arxiv, ssrn, osf_preprints, chemarxiv, bioarxiv, preprints_org, peerj, nber),
    colors = scales::col_factor(
      palette = c('#D7B463', '#E5E5E5', '#57B5AD'),
      domain = NULL
    )
  ) %>%
  tab_options(data_row.padding = px(10)) %>% 
  cols_label(
    arxiv = 'arXiv',
    ssrn = 'SSRN',
    osf_preprints = 'OSFpreprints',
    chemarxiv = 'ChemRxiv',
    bioarxiv = 'bioRxiv',
    preprints_org = 'Preprints.org',
    peerj = 'PeerJ',
    nber = 'NBER'
  ) %>%
  tab_footnote(
    footnote = "Service require commenters to have a public username, but username doesn't have to be a real name",
    location = cells_body(
      columns = vars(osf_preprints, bioarxiv, preprints_org),
      rows = vars(`Identified comments`, `Anonymous comments`)
    )
  ) %>%
  tab_footnote(
    footnote = "Service has a specific location for data/code information, but does not differentiate between the two",
    location = cells_body(
      columns = vars(bioarxiv),
      rows = vars(`Link to study data`, `Link to study analysis scripts`)
    )
  ) %>%
  tab_footnote(
    footnote = "Service has a general place to link/upload other files, but types of files are not clearly identified",
    location = cells_body(
      columns = vars(osf_preprints, chemarxiv, peerj),
      rows = vars(`Link to study analysis scripts`, `Link to materials`, `Link to pre-reg`)
    )
  ) %>%
  tab_footnote(
    footnote = "Service has a general place to link/upload other files, but types of files are not clearly identified",
    location = cells_body(
      columns = vars(osf_preprints, chemarxiv),
      rows = vars(`Link to study data`)
    )
  ) %>%
  tab_footnote(
    footnote = "Service only shows this information for author who uploaded preprint",
    location = cells_body(
      columns = vars(chemarxiv),
      rows = vars(`Author's institution`, `Professional identity links`)
    )
  ) %>%
  tab_footnote(
    footnote = "Service shows which preprints have been accepted to journals, but not which submitted",
    location = cells_body(
      columns = vars(arxiv, ssrn, osf_preprints, chemarxiv, bioarxiv, preprints_org, peerj),
      rows = vars(`Preprint submitted to a journal`)
    )
  ) %>%
  tab_header(title = 'Cues on Preprint Services')
  
  
gtsave(services_icon_table, filename = 'Fig10.png')

  


