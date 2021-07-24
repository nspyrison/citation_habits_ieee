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
# library(osfr)
# library(tidyverse)
# library(here)
# library(psych)
# library(MOTE)
# library(lmerTest)
# library(lavaan)
# library(semTools)
# library(broom)
# library(tidyLPA)
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



## 3) Mixed model tables/coef (lmer) -----
str(dat)
dat_mm <- cbind(participant_rownum = 1:nrow(dat), dat[, c(3:12, 14:22, 24:31, 33)])
dat_mm$nchar_ff <- nchar(dat$`other comments text`)
dat_mm$nchar_ff[is.na(dat_mm$nchar_ff)] <- 0L

## integers, for pivoting :(
dat_mm <- dat_mm %>% mutate(
  `source ACM/IEEE DL`       = as.integer(`source ACM/IEEE DL`),
  `source relevant articles` = as.integer(`source relevant articles`),
  `source ResearchGate`      = as.integer(`source ResearchGate`),
  `source PubMed`            = as.integer(`source PubMed`),
  `source peer/mentor`       = as.integer(`source peer/mentor`),
  `source journal/confrence` = as.integer(`source journal/confrence`),
  `source Google Scholar`    = as.integer(`source Google Scholar`),
  `source other rank`        = as.integer(`source other rank`),
  `read available research materials` = as.integer(`read available research materials`),
  `read author familiarity`           = as.integer(`read author familiarity`),
  `read recency of publication`       = as.integer(`read recency of publication`),
  `read authors institution`          = as.integer(`read authors institution`),
  `read publication venue`            = as.integer(`read publication venue`),
  `read data available`               = as.integer(`read data available`),
  `read pre-registration`             = as.integer(`read pre-registration`),
  `read usage metrics`                = as.integer(`read usage metrics`),
  `read other rank`                   = as.integer(`read other rank`),
  `venue attendance/citations/downloads`    = as.integer(`venue attendance/citations/downloads`),
  `venue audience scope`                    = as.integer(`venue audience scope`),
  `venue acceptance rate/total submissions` = as.integer(`venue acceptance rate/total submissions`),
  `venue ranking system`                    = as.integer(`venue ranking system`),
  `venue metrics`                           = as.integer(`venue metrics`),
  `venue peer/mentor opinion`               = as.integer(`venue peer/mentor opinion`),
  `venue research scope`                    = as.integer(`venue research scope`),
  `venue other rank`                        = as.integer(`venue other rank`),
  `correlation of venue & paper quality`    = as.integer(`correlation of venue & paper quality`)
)

str(dat_mm)
dat_longer <- dat_mm %>%
  tidyr::pivot_longer(
    cols = `source ACM/IEEE DL`:`venue other rank`, ## Doesn't want to mix factors.
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
base <- lmer(response ~ position + `years vis experience` + (1 | participant_rownum) + (1 | likert_question), dat_longer)
model_ls <- list(base = base)
### Validate that the error/residuals are fine to support application of bootstraping
# Plot the binned residuals as recommended by Gelman and Hill (2007)
require("arm")
par(bg="white", cex=1.2, las=1)
binnedplot(predict(base), resid(base), cex.pts=1, col.int="black")
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
dat_num <- dat_mm[, c(3:10, 12:19, 21:27, 29)] %>% spinifex::scale_sd()
(pca_obj <- prcomp(dat_num))

psych::fa.parallel(dat_num)

fa3 <- fa(dat_num, nfactors = 3, rotate = 'oblimin') 
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
ide_vect(dat_num)
#system.time(print(ide_vect(dat_num, inc_slow = TRUE)))
