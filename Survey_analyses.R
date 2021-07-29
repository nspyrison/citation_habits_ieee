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
dim(dat)
colnames(dat)
pos_tbl <- table(dat$position)
names_to_keep <- names(pos_tbl)[pos_tbl >= 3]

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
colnames(dat)
dat_sub <- dat[, c(1, 4, 36, 5:12, 16:22, 26:31, 34)]
## coerce to integer for pivoting
dat_sub[, 5:25] <- lapply(dat_sub[, 5:25], as.integer)
## Pivot longer
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
pos_tbl <- table(dat$position)
names_to_keep <- names(pos_tbl)[pos_tbl >= 3]
## remove rows less than this
dat_longer_grp <- dat_longer[which(dat_longer$position %in% names_to_keep), ]
str(dat_longer_grp)

## Save dat_longer for figures -----
readr::write_rds(dat_longer_grp, "./data/clean_dat_longer_grp.rds")

## 3) Mixed model tables/coef (lmer) -----
colnames(dat_longer)
base <- lmer(response ~ position + `years vis experience` + likert_question + (1 | participant_rownum) + (1 | likert_item), dat_longer)
pos.exp <- lmer(response ~ position * `years vis experience` + likert_question + (1 | participant_rownum) + (1 | likert_item), dat_longer)

# quality.pos <- lmer(`quality correlation` ~ position + `years vis experience` +
#                       (1 | participant_rownum) + likert_question, dat_longer)
# quality.yrs <- lmer(`years vis experience` ~ position + `years vis experience` +
#                       (1 | participant_rownum) + likert_question, dat_longer)
model_ls <- list(base = base, pos.exp = pos.exp)
                 # ,quality.pos = quality.pos,
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
summary(pos.exp)$coefficients


## (4) demographic heatmap ------


## (5) Factor analysis/PCA ------
str(dat_sub)
dat_num <- cbind(as.integer(dat_sub$position), dat_sub[, 4:25])# %>% spinifex::scale_01()
(pca_obj <- prcomp(dat_num))

psych::fa.parallel(cor(dat_num, method = "spearman"), n.obs = nrow(dat_num)) ## 8 on ranked cor
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

(est_vec <- ide_vect(dat_num))
ide_vect(cor(dat_num, method = "spearman"))
summary(est_vec)
#system.time(print(ide_vect(dat_num, inc_slow = TRUE)))



