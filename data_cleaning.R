### Package dependencies -----
require("tidyverse")
require("skimr")

### Import data ------
## pilot data -- NOT IEEE
raw <- readr::read_csv("./data/raw_survey_pilot.csv") 

## Initialize factor levels
position_lvls <- c("Graduate Student (Masters, PhD)",
                   "Post-doctoral Researcher",
                   "Assistant Professor, or equivalent",
                   "Associate Professor, or equivalent",
                   "Professor, or equivalent",
                   "Research Scientist/Staff Scientist, or equivalent",
                   "Private Sector/Practitioner, or equivalent",
                   "Other")
s1_lvls <- c("Never", "Rarely", "Sometimes", "Frequently", "Always")
s23_lvls <- c("Not at all important", "Slightly important", "Moderately important", "Very important", "Extremely important")
y1_lvls <- c("No additional scrutiny", "Some additional scrutiny", "Moderate additional scrutiny", "A lot of additional scrutiny", "Highest additional scrutiny")
y2_lvls <- c("Not correlated", "Weak positive correlation", "Medium positive correlation", "Fairly high positive correlation", "Strong positive correlatati")

## Clean data -----
clean <-
  raw %>%
  dplyr::mutate(
    .keep = "none",
    timestamp = lubridate::as_datetime(Timestamp),
    consent = `Consent and understanding` == "I understand that my data will be used and published on a repository, and I certify that I meet the criteria for participation in this survey",
    position = factor(
      `What is your title/position?`, 
      levels = position_lvls),
    `years vis experience` = `How many years of experience in visualization research do you have?`,
    ## Section 1, frequency of use sourcing citations:
    `source ACM/IEEE DL`= factor(
      `How often do you use the following sources when finding papers related to your research? [ACM/IEEE DL]`,
      levels = s1_lvls, ordered = TRUE),
    `source relevant articles` = factor(
      `How often do you use the following sources when finding papers related to your research? [Citations from relevant articles]`,
      levels = s1_lvls, ordered = TRUE),
    `source ResearchGate` = factor(
      `How often do you use the following sources when finding papers related to your research? [ResearchGate]`,
      levels = s1_lvls, ordered = TRUE),
    `source PubMed` = factor(
      `How often do you use the following sources when finding papers related to your research? [PubMed]`,
      levels = s1_lvls, ordered = TRUE),
    `source peer/mentor` = factor(
      `How often do you use the following sources when finding papers related to your research? [Suggestions from peers/mentors]`,
      levels = s1_lvls, ordered = TRUE),
    `source journal/confrence` = factor(
      `How often do you use the following sources when finding papers related to your research? [Journal issues/conference proceedings]`,
      levels = s1_lvls, ordered = TRUE),
    `source Google Scholar` = factor(
      `How often do you use the following sources when finding papers related to your research? [Google Scholar]`,
      levels = s1_lvls, ordered = TRUE),
    `source other rank` = factor(
      s1_lvls[`...and how often you use it`],
      levels = s1_lvls, ordered = TRUE),
    `source other text` = `If you use any other source, indicate here which one`,
    ## Section 2, importance for including as citation:
    `citation authors institution` = factor(
      `How important are the following criteria when deciding to cite a research paper? [Institutional affiliation of the authors]`,
      levels = s23_lvls, ordered = TRUE),
    `citation publication venue` = factor(
      `How important are the following criteria when deciding to cite a research paper? [Publication venue]`,
      levels = s23_lvls, ordered = TRUE),
    `citation data available` = factor(
      `How important are the following criteria when deciding to cite a research paper? [Availability of data]`,
      levels = s23_lvls, ordered = TRUE),
    `citation author's previous work` = factor(
      `How important are the following criteria when deciding to cite a research paper? [Information about the authors' previous work]`,
      levels = s23_lvls, ordered = TRUE),
    `citation work robustness/replicability` = factor(
      `How important are the following criteria when deciding to cite a research paper? [Robustness/replicability of the work]`,
      levels = s23_lvls, ordered = TRUE),
    `citation peer/mentor comments` = factor(
      `How important are the following criteria when deciding to cite a research paper? [Comments from peers/mentors]`,
      levels = s23_lvls, ordered = TRUE),
    `citation pre-registration` = factor(
      `How important are the following criteria when deciding to cite a research paper? [Availability of pre-registration/study registration]`,
      levels = s23_lvls, ordered = TRUE),
    `citation usage metrics` = factor(
      `How important are the following criteria when deciding to cite a research paper? [Usage metrics (downloads, citations, altmetric)]`,
      levels = s23_lvls, ordered = TRUE),
    `citation relevance` = factor(
      `How important are the following criteria when deciding to cite a research paper? [Relevance to your work]`,
      levels = s23_lvls, ordered = TRUE),
    `citaion research materials available` = factor(
      `How important are the following criteria when deciding to cite a research paper? [Availability of research materials]`,
      levels = s23_lvls, ordered = TRUE),
    `citation other rank` = factor(
      s23_lvls[`...and how important it is`],
      levels = s23_lvls, ordered = TRUE),
    `citation other text` = `If you use any other criteria, indicate here which one`,
    ## Y1
    `sub-optimal additional scrutiny` = factor(
      `If a research paper is relevant to your topic, but has been published in what you consider to be a sub-optimal venue, how much extra scrutiny would you give it?`,
      levels = y1_lvls, ordered = TRUE),
    ## Y2
    `venue/paper quality correlation` = factor(
      `How correlated do you think the quality of a venue is with the quality of its papers?`,
      levels = y2_lvls, ordered = TRUE), 
    ## Section 3, importance for assessing venue quality:
    `venue attendance/citations/downloads` = factor(
      `How important are the following criteria when assessing the quality of a venue? [Conference attendance/citations/downloads]`,
      levels = s23_lvls, ordered = TRUE),
    `venue audience scope` = factor(
      `How important are the following criteria when assessing the quality of a venue? [Audience scope (regional/national vs international)]`,
      levels = s23_lvls, ordered = TRUE),
    `venue acceptance rate/total submissions` = factor(
      `How important are the following criteria when assessing the quality of a venue? [Acceptance rate/total submissions]`,
      levels = s23_lvls, ordered = TRUE),
    `venue ranking system` = factor(
      `How important are the following criteria when assessing the quality of a venue? [Institutional/National ranking system (e.g., CORE)]`,
      levels = s23_lvls, ordered = TRUE),
    `venue metrics` = factor(
      `How important are the following criteria when assessing the quality of a venue? [Metrics of the venue (e.g., Journal Impact Factor, H5 index, Google Scholar ranking)]`,
      levels = s23_lvls, ordered = TRUE),
    `venue peer/mentor opinion` = factor(
      `How important are the following criteria when assessing the quality of a venue? [Opinions of peers/mentors]`,
      levels = s23_lvls, ordered = TRUE),
    `venue research scope` = factor(
      `How important are the following criteria when assessing the quality of a venue? [Research scope of the venue (i.e., focused vs broad)]`,
      levels = s23_lvls, ordered = TRUE),
    `venue other rank` = factor(
      s23_lvls[`...and how important it is_1`],
      levels = s23_lvls, ordered = TRUE),
    `venue other text` = `If you use any other criteria, indicate here which one_1`,
    `other comments text` = `Are there any other thoughts or comments you wish to share with us?`
  )

skimr::skim(clean)

### Split data on type of response ------
clean_freeform <- clean %>% ## Freeform text fields
  dplyr::select("timestamp",
                "source other text", ## ~col 12
                "citation other text", ## ~col 24
                "venue other text", ## ~col 36
                "other comments text") ## ~col 37

clean_demo_likert <- clean %>% ## Demographic and 5 pt likert scale data
  dplyr::select(!"consent", ## col 2
                !"source other text", ## ~col 12
                !"citation other text", ## ~col 24
                !"venue other text", ## ~col 36
                !"other comments text") ## ~col 37

## Write data -----
readr::write_csv(clean_freeform,    "./data/clean_freeform.csv")
readr::write_csv(clean_demo_likert, "./data/clean_demo_likert.csv")


