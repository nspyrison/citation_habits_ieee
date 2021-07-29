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


## read the data ------
dat <- readr::read_rds("./data/clean_survey.rds")

## skim
dim(dat)
dat %>% skimr::skim()
colnames(dat)

## Sections 1:3, and paper/venue quality correlation.
#dat_violin <- dat[, c(3:11, 14:21, 24:30, 33)]
dat_likert <- dat[, c(5:12, 14:22, 24:31, 33)]
colnames(dat_likert)

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

## 0) Pivot longer -----
str(dat)
dat_sub <- cbind(participant_rownum = 1:nrow(dat),
                 dat[, c(3:12, 14:22, 24:31, 33)])

## coerce to integer for pivoting
dat_sub <- dat_sub %>% mutate(
  .keep = "none",
  participant_rownum = participant_rownum,
  position = position,
  `years vis experience` = `years vis experience`,
  ## S1
  `source ACM/IEEE DL`       = as.integer(`source ACM/IEEE DL`),
  `source relevant articles` = as.integer(`source relevant articles`),
  `source ResearchGate`      = as.integer(`source ResearchGate`),
  `source PubMed`            = as.integer(`source PubMed`),
  `source peer/mentor`       = as.integer(`source peer/mentor`),
  `source journal/confrence` = as.integer(`source journal/confrence`),
  `source Google Scholar`    = as.integer(`source Google Scholar`),
  #`source other rank`        = as.integer(`source other rank`),
  ## S2
  `read available research materials` = as.integer(`read available research materials`),
  `read author familiarity`           = as.integer(`read author familiarity`),
  `read recency of publication`       = as.integer(`read recency of publication`),
  `read authors institution`          = as.integer(`read authors institution`),
  `read publication venue`            = as.integer(`read publication venue`),
  `read data available`               = as.integer(`read data available`),
  `read pre-registration`             = as.integer(`read pre-registration`),
  `read usage metrics`                = as.integer(`read usage metrics`),
  #`read other rank`                   = as.integer(`read other rank`),
  ## S3
  `venue attendance/citations/downloads`    = as.integer(`venue attendance/citations/downloads`),
  `venue audience scope`                    = as.integer(`venue audience scope`),
  `venue acceptance rate/total submissions` = as.integer(`venue acceptance rate/total submissions`),
  `venue ranking system`                    = as.integer(`venue ranking system`),
  `venue metrics`                           = as.integer(`venue metrics`),
  `venue peer/mentor opinion`               = as.integer(`venue peer/mentor opinion`),
  `venue research scope`                    = as.integer(`venue research scope`),
  #`venue other rank`                        = as.integer(`venue other rank`),
  ##
  `quality correlation` = as.integer(`quality correlation`)
)

str(dat_sub)
dat_longer <- dat_sub %>%
  tidyr::pivot_longer( ## Doesn't like factors
    cols = `source ACM/IEEE DL`:`venue research scope`, #`venue other rank`,
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
table(dat$position)



### Group/drop levels with few obs -----
dat_longer_grp <- dat_longer %>% mutate(
  .keep = "unused",
  position = case_when(
    position == "Graduate Student (Masters, PhD)"    ~ "Graduate Student",
    position == "Post-doctoral Researcher"           ~ "Post-doctoral",
    position == "Assistant Professor, or equivalent" ~ "Assistant Professor",
    position == "Associate Professor, or equivalent" ~ "Associate Professor",
    position == "Research Scientist/Staff Scientist, or equivalent" ~ "Research/Staff Scientist",
    TRUE ~ "REMOVE")
)
## and remove the "REMOVE" rows.
dat_longer_grp <- dat_longer_grp[which(dat_longer_grp$position != "REMOVE"), ]
dat_longer_grp$position <- factor(
  dat_longer_grp$position, 
  levels = c("Graduate Student", "Post-doctoral", "Assistant Professor",
             "Associate Professor", "Research/Staff Scientist"))

## Save dat_longer for figures -----
readr::write_rds(dat_longer_grp, "./data/clean_dat_longer_grp.rds")

## 3) Mixed model tables/coef (lmer) -----
colnames(dat_longer)
base <- lmer(response ~ position + `years vis experience` + (1 | participant_rownum) + (1 | likert_question), dat_longer)
quality.pos <- lmer(`quality correlation` ~ position + `years vis experience` +
                      (1 | participant_rownum) + likert_question, dat_longer)
quality.yrs <- lmer(`years vis experience` ~ position + `years vis experience` +
                      (1 | participant_rownum) + likert_question, dat_longer)

model_ls <- list(base = base)
#,
                 # quality.pos = quality.pos,
                 # quality.yrs = quality.yrs)
### Validate that the error/residuals are fine to support application of bootstraping
# Plot the binned residuals as recommended by Gelman and Hill (2007)
require("arm")
par(bg="white", cex=1.2, las=1)
binnedplot(predict(base), resid(base), cex.pts=1, col.int="black")
binnedplot(predict(quality), resid(quality), cex.pts=1, col.int="black")
## Good, residuals are homoskedastic


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

model_comp_tbl <- tibble(names(model_ls),
                         round(.perf_df[, 1]),
                         round(.perf_df[, 2]),
                         round(.perf_df[, 3], 3),
                         round(.perf_df[, 4], 3),
                         round(.perf_df[, 6], 3),
)
colnames(model_comp_tbl) <- c(
  "Model", "AIC", "BIC", "R2 cond. (on RE)",
  "R2 marg. (w/o RE)", "RMSE")
model_comp_tbl
knitr::kable(model_comp_tbl)


## R2 calculated using Edwards et al (2008) method
base_anova <- anova(base)
#position_anova_output <- anova(position_model)
position_r2 <- (base_anova[1,3] / base_anova[1,4] * base_anova[1,5]) /
  (1 + ((base_anova[1,3]) / base_anova[1,4] * base_anova[1,5]))
question_r2 <- ((base_anova[2,3]) / base_anova[2,4] * base_anova[2,5]) /
  (1 + (base_anova[2,3] / base_anova[2,4] * base_anova[2,5]))


### Model coefficients ------
summary(base)$coefficients


## (4) demographic heatmap ------


## (5) Factor analysis/PCA ------
str(dat_mm)
dat_num <- dat_mm[, c(4:10, 12:19, 21:27, 29)] %>% spinifex::scale_sd()
(pca_obj <- prcomp(dat_num))

psych::fa.parallel(cor(dat_num, method = "spearman")) ## 7 on racked cor
psych::fa.parallel(dat_num) ## 2 on data

fa2 <- fa(dat_num,
          nfactors = 2, rotate = 'oblimin')
fa7 <- fa(cor(dat_num, method = "spearman"),
          nfactors = 7, rotate = 'oblimin')
fa2
fa7
fa.diagram(fa2)
fa.diagram(fa7)

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
                     Rdimtools::est.mindkl, Rdimtools::est.Ustat)
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
est_vec <- ide_vect(dat_num)
summary(est_vec)
#system.time(print(ide_vect(dat_num, inc_slow = TRUE)))



