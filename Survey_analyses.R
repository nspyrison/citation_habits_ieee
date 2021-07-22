## Dependant packages -------
require("tidyverse")
require("skimr")
require("likert")
require("corrplot")
require("psych")
## mixed models.
require("lme4") ## Random Effects (RE) model creation
require("lmerTest") ## p val interpretation of lme4 tests
require("merTools")
require("performance") ## tidy model eval

##required libraries
library(osfr)
library(tidyverse)
library(here)
library(psych)
library(MOTE)
library(lmerTest)
library(lavaan)
library(semTools)
library(broom)
library(tidyLPA)
# library(semPlot)

## reading in data

## read the data ------
dat <- readr::read_rds("./data/clean_survey_pilot.rds")

## skim
dim(dat)
dat %>% skimr::skim()
str(dat)

## Sections 1:3, and paper/venue quality correlation.
dat_likert <- dat[, c(5:12, 14:22, 24:31, 33)] 


### OSF analyses section:
# The analysis process will follow a similar method as used in a related article by Soderberg et al. on the credibility of preprint [1] (which we will acknowledge in our manuscript). This will be based on the R scripts found in their OSF repository [2]. 
# Overall our process will be the following:
#   - We will produce Likert plots combined with violin plots containing 95% Confidence Intervals (CIs) of the responses. We will analyse our results with an estimation approach and inference by eye [3,4] 
# - We will look at correlation of responses with experience as a researcher. For that we will first conduct a Spearman ranked correlation of the Likert scales responses in general and across experience levels. Whether we use years of experience in the field or position as a proxy for "experience" will depend on the demographics of our participants. 
# - We will create a mixed model regressing responses to the Likert scales. The main effects will be years of experience, position, and a random effect for participants will be included.  
# 
# [1] https://doi.org/10.1098/rsos.201520
# [2] https://osf.io/j7u6z/
# [3] http://link.springer.com/chapter/10.1007%2F978-3-319-26633-6_13 
# [4] https://arxiv.org/pdf/2002.07671.pdf 

## 1) Likerts produced in `figures.r`
## 2) Spearman correlation produced in `figures.r`,
#### - will want need to come up with some form of crossing seniority/experience.
## 3) Mixed models (lmer), produce below
## (4) demographic heatmap; produce below
## (5) factor analysis/PCA, produce below
str(dat_likert)


## 3) Mixed model tables/coef (lmer) -----
str(dat)
dat_mm <- cbind(participant_rownum = 1:nrow(dat), dat[, c(3:12, 14:22, 24:31, 33)])
dat_mm$nchar_ff <- nchar(dat$`other comments text`)
dat_mm$nchar_ff[is.na(dat_mm$nchar_ff)] <- 0L
dat_longer <- dat_mm %>%
  tidyr::pivot_longer(
    cols = `source ACM/IEEE DL`:`venue other rank`,
    names_to = "likert_item",
    values_to = "response",
    values_drop_na = TRUE
  ) %>% 
  mutate(likert_question = factor(
    case_when(
      substr(likert_item, 1, 6) == "source" ~ "source",
      substr(likert_item, 1, 4) == "read" ~ "read",
      substr(likert_item, 1, 5) == "venue" ~ "venue",
    ), levels = c("source", "read", "venue")
  ))

str(dat_longer)
base <- lmer(response ~ position + `years vis experience` + (1 | participant_rownum))
full <- lmer(response ~ position + `years vis experience` + likert_question + nchar_ff + (1 | participant_rownum))
model_ls <- list(base=base, full=full)
###TODO validate that the error/residuals are fine to support application of bootstraping


#-----------------------------------------------------------------------------
# Plot the binned residuals as recommended by Gelman and Hill (2007)
#-----------------------------------------------------------------------------
require("arm")
par(bg="white", cex=1.2, las=1)
binnedplot(predict(base), resid(base), cex.pts=1, col.int="black")
binnedplot(predict(full), resid(full), cex.pts=1, col.int="black")


### Model performance ------
## lapply over models
performance_ls <- list(); factors_vec <- fixef_vec <- vector();
mute <- lapply(seq_along(model_ls), function(i){
  this_model <- model_ls[[i]]
  performance_ls[[i]] <<- performance::model_performance(this_model)
  factors_vec[i] <<- ncol(attr(terms(this_model), "factors"))
  fixef_vec[i] <<- length(fixef(this_model))
})
.perf_df <- dplyr::bind_rows(performance_ls)
.model_comp_colnms <- c("Fixed effects",
                        "AIC", "BIC", "R2 cond. (on RE)",
                        "R2 marg. (w/o RE)", "RMSE")
model_comp_tbl <- tibble(names(model_ls),
                         round(.perf_df[, 1]),
                         round(.perf_df[, 2]),
                         round(.perf_df[, 3], 3),
                         round(.perf_df[, 4], 3),
                         round(.perf_df[, 6], 3),
)
colnames(model_comp_tbl) <- .model_comp_colnms
model_comp_tbl
knitr::kable(model_comp_tbl)


## R2 calculated using Edwards et al (2008) method
base_anova <- anova(base)
position_anova_output <- anova(position_model)
position_r2 <- (base_anova[1,3] / base_anova[1,4] * base_anova[1,5]) /
  (1 + ((base_anova[1,3]) / base_anova[1,4] * base_anova[1,5]))
question_r2 <- ((base_anova[2,3]) / base_anova[2,4] * base_anova[2,5]) /
  (1 + (base_anova[2,3] / base_anova[2,4] * position_anova_output[2,5]))


### Model coefficients ------
summary(base)$coefficients
summary(full)$coefficients



## (4) demographic heatmap ------


## (5) Factor analysis/PCA ------

str(dat_likert)
(pca_obj <- prcomp(dat_likert))

psych::fa.parallel(dat_likert)

fa3 <- fa(dat_likert, nfactors = 3, rotate = 'oblimin') 
fa3
fa.diagram(fa3)

### Intrinsic dimension estimations with Rdimtools.

#' @example 
#' dat <- as.matrix(tourr::flea[, 1:6])
#' ide_vect(data = dat, inc_slow = FALSE)
#' ide_vect(data = dat, inc_slow = TRUE)
ide_vect <- function(data, inc_slow = FALSE){
  ls_funcs <- list(Rdimtools::est.boxcount, Rdimtools::est.correlation,
                   Rdimtools::est.made, Rdimtools::est.mle2,
                   Rdimtools::est.twonn)
  nms <- c("est.boxcount", "est.correlation", "est.made", "est.mle2", "est.twonn")
  if(inc_slow == TRUE){
    ls_funcs <- c(
      ls_funcs, list(Rdimtools::est.clustering, Rdimtools::est.danco,
                     Rdimtools::est.gdistnn, Rdimtools::est.incisingball,
                     Rdimtools::est.mindkl, Rdimtools::est.Ustat0)
    )
    nms <- c(nms, "est.clustering", "est.danco", "est.gdistnn",
             "est.incisingball", "est.mindkl", "est.Ustat")
  } ## est.incisingball prints histogram...
  ret <- sapply(1:length(ls_funcs), function(i){
    tryCatch(ls_funcs[[i]](data)$estdim,
             error=function(cond){
               message("Error in est.* function:")
               message(cond)
               return(NA)
             })
  })
  names(ret) <- c(nms)
  return(ret)
}




#### PREPRINT CODE BELOW -----
## Will probably have to see their script to replicate it
if(F){ ## NOT RUN ##
  file.edit("./survey_analyses_preprints_script.r")
  
  # correlation favor-use/use/submissions and credibility questions
  correlations1 <- survey_data %>%
    select(preprints_used, preprints_submitted, starts_with('preprint_cred')) %>%
    mutate(preprints_used = as.numeric(preprints_used),
           preprints_submitted = as.numeric(preprints_submitted)) %>%
    cor(use = 'pairwise.complete.obs', method = 'spearman')
  
  correlations2 <- survey_data %>%
    select(favor_use, starts_with('preprint_cred')) %>%
    cor(use = 'pairwise.complete.obs')
  
  correlations <- as.data.frame(cbind(correlations1[3:21, 1:2], correlations2[2:20, 1])) 
  
  # median correlation magnitude
  median(correlations %>%
           pivot_longer(names_to = 'corr_variable', cols = preprints_used:V3) %>%
           select(value) %>%
           mutate(value = abs(value)) %>%
           pull(value))
  
  ### cues by career/disicpline analyses ###
  
  ## by discipline analysis ##
  discipline_q_means <- survey_data %>%
    select(-c(consent, HDI_2017)) %>%
    group_by(discipline_collapsed) %>%
    skim() %>%
    yank('numeric') %>%
    rename(question = skim_variable) %>%
    filter(discipline_collapsed != 'Other' & discipline_collapsed != 'Engineering' & discipline_collapsed != '(Missing)') %>%
    select(discipline_collapsed, question, mean) %>%
    filter(grepl('preprint', question)) %>%
    filter(!is.na(mean)) %>%
    mutate(mean = as.numeric(mean))
  
  # largest diff between discipline wtihin question
  discipline_q_means %>%
    group_by(question) %>%
    summarize(min = min(mean), max = max(mean)) %>%
    mutate(diff = max-min) %>%
    arrange(desc(diff)) %>%
    slice(1L)
  
  # largest diff between question wtihin discipline
  discipline_q_means %>%
    group_by(discipline_collapsed) %>%
    summarize(min = min(mean), max = max(mean)) %>%
    mutate(diff = max-min) %>%
    arrange(desc(diff)) %>%
    slice(1L)
  
  # reformat data for lme models
  credibility_data_long <- survey_data %>%
    dplyr::select(ResponseId, starts_with('preprint_cred'), discipline_collapsed, acad_career_stage) %>%
    drop_na() %>%
    pivot_longer(cols = starts_with('preprint_cred'), names_to = 'question', values_to = 'response') %>%
    mutate(question = as.factor(question))
  
  ## by discipline analysis ##
  model <- lmer(resp ~ yr_exp + position + (1|timestamp))
  model_anova <- anova(discipline_model)
  discipline_model <- lmer(response ~ discipline_collapsed + question + discipline_collapsed:question + (1|ResponseId), credibility_data_long %>% filter(discipline_collapsed != 'Other' & discipline_collapsed != 'Engineering'))
  discipline_anova_output <- anova(discipline_model)
  
  # R2 calculated using Edwards et al (2008) method
  discipline_r2 <- ((discipline_anova_output[1,3])/discipline_anova_output[1,4] * discipline_anova_output[1,5])/(1 + ((discipline_anova_output[1,3])/discipline_anova_output[1,4] * discipline_anova_output[1,5]))
  question_r2 <- ((discipline_anova_output[2,3])/discipline_anova_output[2,4] * discipline_anova_output[2,5])/(1 + ((discipline_anova_output[2,3])/discipline_anova_output[2,4] * discipline_anova_output[2,5]))
  
  
  ## by career_stage ##
  career_q_means <- survey_data %>%
    select(-c(consent, HDI_2017)) %>%
    group_by(acad_career_stage) %>%
    skim() %>%
    yank('numeric') %>%
    rename(question = skim_variable) %>%
    select(acad_career_stage, question, mean) %>%
    filter(grepl('preprint', question)) %>%
    filter(!is.na(mean)) %>%
    mutate(mean = as.numeric(mean))
  
  # largest diff between career stage wtihin question
  career_q_means %>%
    group_by(question) %>%
    summarize(min = min(mean), max = max(mean)) %>%
    mutate(diff = max-min) %>%
    arrange(desc(diff)) %>%
    slice(1L)
  
  # largest diff between question wtihin career stage
  career_q_means %>%
    group_by(acad_career_stage) %>%
    summarize(min = min(mean), max = max(mean)) %>%
    mutate(diff = max-min) %>%
    arrange(desc(diff)) %>%
    slice(1L)
  
  ## by academic position analysis ##
  position_model <- lmer(response ~ acad_career_stage + question + acad_career_stage:question + (1|ResponseId), credibility_data_long)
  position_anova_output <- anova(position_model)
  
  # R2 calculated using Edwards et al (2008) method
  position_r2 <- ((position_anova_output[1,3])/position_anova_output[1,4] * position_anova_output[1,5])/(1 + ((position_anova_output[1,3])/position_anova_output[1,4] * position_anova_output[1,5]))
  question_r2 <- ((position_anova_output[2,3])/position_anova_output[2,4] * position_anova_output[2,5])/(1 + ((position_anova_output[2,3])/position_anova_output[2,4] * position_anova_output[2,5]))
  
  
  
  #### exploratory factor analysis ####
  
  ## MOVED UP ##
  
  ## measurement invariance calculations ##
  base_model <- 'traditional =~ preprint_cred1_1 + preprint_cred1_2 + preprint_cred1_3	
               open_icons =~ preprint_cred4_1 + preprint_cred4_2 + preprint_cred4_3 + preprint_cred4_4	
               verifications =~ preprint_cred5_1 + preprint_cred5_2 + preprint_cred5_3	
               opinions =~ preprint_cred3_1 + preprint_cred3_2 + preprint_cred3_3	
               external_support    =~ preprint_cred1_4 + preprint_cred2_1	
               usage   =~ preprint_cred2_3 + preprint_cred2_4'
  
  # by career stages testing
  position_models <- cfa(model = base_model, data = survey_data, group = 'acad_career_stage')
  summary(position_models, fit.measures = T)
  
  measurementInvariance(model = base_model, data = survey_data, group = 'acad_career_stage')
  
  # by discipline testing
  discipline_models <- cfa(model = base_model, data = survey_data %>% filter(discipline_collapsed != 'Other' & discipline_collapsed != 'Engineering'), group = 'discipline_collapsed')
  summary(discipline_models , fit.measures = T)
  
  measurementInvariance(model = base_model, data = survey_data %>% filter(discipline_collapsed != 'Other' & discipline_collapsed != 'Engineering'), group = 'discipline_collapsed')
  
}