### Package dependencies -----
require("tidyverse")
require("skimr")

### Import data ------
## pilot data -- NOT IEEE
raw <- readr::read_csv("./data/raw_Citation criteria, IEEE VIS_44resp.csv") 
dim(raw)
colnames(raw)

## Initialize factor levels
position_lvls <- c("Graduate Student (Masters, PhD)",
                   "Post-doctoral Researcher",
                   "Assistant Professor, or equivalent",
                   "Associate Professor, or equivalent",
                   "Professor, or equivalent",
                   "Research Scientist/Staff Scientist, or equivalent",
                   "Private Sector/Practitioner, or equivalent",
                   "Other")
position_new_lvls <- c("Graduate Student",
                       "Post-doctoral",
                       "Assistant Professor",
                       "Associate Professor",
                       "Professor",
                       "Research/Staff Scientist",
                       "Private Sector/Practitioner",
                       "Other")
s1_lvls <- c("Never", "Rarely", "Sometimes", "Frequently", "Always")
s23_lvls <- c("Not at all important", "Slightly important", "Moderately important", "Very important", "Extremely important")
corr_lvls <- c("Not correlated", "Weak positive correlation", "Medium positive correlation", "Fairly high positive correlation", "Strong positive correlation")



## Clean data -----
clean <-
  raw %>%
  dplyr::mutate(
    .keep = "none",
    participant_rownum = 1:n(),
    timestamp = lubridate::as_datetime(Timestamp),
    consent = `Consent and understanding` == "I understand that my data will be used and published on a repository, and I certify that I meet the criteria for participation in this survey",
    position = factor(
      `What is your title/position?`,
      levels = position_lvls),
    `years vis experience` = `How many years of experience in visualization research do you have?`,
    ## Section 1, frequency of use sourcing citations:
    `source ACM/IEEE DL`= factor(
      `How often do you use the following sources when finding papers related to your research? [ACM/IEEE DL]`,
      levels = s1_lvls, ordered = TRUE, exclude = NA),
    `source relevant articles` = factor(
      `How often do you use the following sources when finding papers related to your research? [Citations from relevant articles]`,
      levels = s1_lvls, ordered = TRUE, exclude = NA),
    `source ResearchGate` = factor(
      `How often do you use the following sources when finding papers related to your research? [ResearchGate]`,
      levels = s1_lvls, ordered = TRUE, exclude = NA),
    `source PubMed` = factor(
      `How often do you use the following sources when finding papers related to your research? [PubMed]`,
      levels = s1_lvls, ordered = TRUE, exclude = NA),
    `source peer/mentor` = factor(
      `How often do you use the following sources when finding papers related to your research? [Suggestions from peers/mentors]`,
      levels = s1_lvls, ordered = TRUE, exclude = NA),
    `source journal/confrence` = factor(
      `How often do you use the following sources when finding papers related to your research? [Journal issues/conference proceedings]`,
      levels = s1_lvls, ordered = TRUE, exclude = NA),
    `source Google Scholar` = factor(
      `How often do you use the following sources when finding papers related to your research? [Google Scholar]`,
      levels = s1_lvls, ordered = TRUE, exclude = NA),
    `source other rank` = factor(
      `...and how often you use it [Other criteria]`,
      levels = s1_lvls, ordered = TRUE, exclude = NA),
    `source other text` = `If you use any other source, indicate here which one`,
    ## Section 2, importance for reading paper in detail:
    `read available research materials` = factor(
      `How important are the following criteria when deciding whether or not to read a paper in detail? Assume that you have all of this information available to you. [Availability of research materials]`,
      levels = s23_lvls, ordered = TRUE, exclude = NA),
    `read author familiarity` = factor(
      `How important are the following criteria when deciding whether or not to read a paper in detail? Assume that you have all of this information available to you. [Personal familiarity with authors]`,
      levels = s23_lvls, ordered = TRUE, exclude = NA),
    `read recency of publication` = factor(
      `How important are the following criteria when deciding whether or not to read a paper in detail? Assume that you have all of this information available to you. [Recency of the publication]`,
      levels = s23_lvls, ordered = TRUE, exclude = NA),
    `read authors institution` = factor(
      `How important are the following criteria when deciding whether or not to read a paper in detail? Assume that you have all of this information available to you. [Institutional affiliation of the authors]`,
      levels = s23_lvls, ordered = TRUE, exclude = NA),
    `read publication venue` = factor(
      `How important are the following criteria when deciding whether or not to read a paper in detail? Assume that you have all of this information available to you. [Publication venue]`,
      levels = s23_lvls, ordered = TRUE, exclude = NA),
    `read data available` = factor(
      `How important are the following criteria when deciding whether or not to read a paper in detail? Assume that you have all of this information available to you. [Availability of data]`,
      levels = s23_lvls, ordered = TRUE, exclude = NA),
    `read pre-registration` = factor(
      `How important are the following criteria when deciding whether or not to read a paper in detail? Assume that you have all of this information available to you. [Availability of pre-registration/study registration]`,
      levels = s23_lvls, ordered = TRUE, exclude = NA),
    `read usage metrics` = factor(
      `How important are the following criteria when deciding whether or not to read a paper in detail? Assume that you have all of this information available to you. [Usage metrics (downloads, citations, altmetric)]`,
      levels = s23_lvls, ordered = TRUE, exclude = NA),
    `read other rank` = factor(
      `...and how important it is [Other criteria]`,
      levels = s23_lvls, ordered = TRUE, exclude = NA),
    `read other text` = `If you use any other criteria, indicate here which one`,
    ## Section 3, importance for assessing venue quality:
    `venue attendance/citations/downloads` = factor(
      `How important are the following criteria when assessing the quality of a venue? [Conference attendance/citations/downloads]`,
      levels = s23_lvls, ordered = TRUE, exclude = NA),
    `venue audience scope` = factor(
      `How important are the following criteria when assessing the quality of a venue? [Audience scope (regional/national vs international)]`,
      levels = s23_lvls, ordered = TRUE, exclude = NA),
    `venue acceptance rate/total submissions` = factor(
      `How important are the following criteria when assessing the quality of a venue? [Acceptance rate/total submissions]`,
      levels = s23_lvls, ordered = TRUE, exclude = NA),
    `venue ranking system` = factor(
      `How important are the following criteria when assessing the quality of a venue? [Institutional/National ranking system (e.g., CORE)]`,
      levels = s23_lvls, ordered = TRUE, exclude = NA),
    `venue metrics` = factor(
      `How important are the following criteria when assessing the quality of a venue? [Metrics of the venue (e.g., Journal Impact Factor, H5 index, Google Scholar ranking)]`,
      levels = s23_lvls, ordered = TRUE, exclude = NA),
    `venue peer/mentor opinion` = factor(
      `How important are the following criteria when assessing the quality of a venue? [Opinions of peers/mentors]`,
      levels = s23_lvls, ordered = TRUE, exclude = NA),
    `venue research scope` = factor(
      `How important are the following criteria when assessing the quality of a venue? [Research scope of the venue (i.e., focused vs broad)]`,
      levels = s23_lvls, ordered = TRUE, exclude = NA),
    `venue other rank` = factor(
      `...and how important it is [Other criteria]_1`,
      levels = s23_lvls, ordered = TRUE, exclude = NA),
    `venue other text` = `If you use any other criteria, indicate here which one_1`,
    ## correlation of venue & paper correlation 
    `quality correlation` = factor(
      corr_lvls[raw$`How correlated do you think the quality of a venue is with the quality of its research papers?`],
      levels = corr_lvls, ordered = TRUE, exclude = NA),
    ## Other freeform text
    `other comments text` = `Are there any other thoughts or comments you wish to share with us?`
  )
position_cnt <- table(clean$position)
clean <- clean %>% mutate(
  position_disp = case_when(
    position == position_lvls[1] ~ paste0(position_new_lvls[1], "\n n = ", position_cnt[1]),
    position == position_lvls[2] ~ paste0(position_new_lvls[2], "\n n = ", position_cnt[2]),
    position == position_lvls[3] ~ paste0(position_new_lvls[3], "\n n = ", position_cnt[3]),
    position == position_lvls[4] ~ paste0(position_new_lvls[4], "\n n = ", position_cnt[4]),
    position == position_lvls[5] ~ paste0(position_new_lvls[5], "\n n = ", position_cnt[5]),
    position == position_lvls[6] ~ paste0(position_new_lvls[6], "\n n = ", position_cnt[6]),
    position == position_lvls[7] ~ paste0(position_new_lvls[7], "\n n = ", position_cnt[7]),
    position == position_lvls[8] ~ paste0(position_new_lvls[8], "\n n = ", position_cnt[8])
  )
)
## reverse level order
clean$position <- factor(clean$position, levels = rev(levels(clean$position)))
str(clean)

## Write data -----
readr::write_rds(clean, "./data/clean_survey.rds")
