### Package dependancies -----
require(tidyverse)
require(here)
require(osfr)

### Import data ------

## download file locally
if(F) ## Don't run: 
  osf_retrieve_file("https://osf.io/XXXX/") %>% ## TODO fix url, other proj is; q4zf8
  osf_download(path = "./data/")

## read to R
## pilot data -- NOT IEEE
dat <- readr::read_csv("./data/raw_survey_pilot.csv") ## TODO fix url
#dat <- readr::read_csv(here::here('./data/raw_survey_data.csv'))

### Split data on type of response ------
dat_freeform <- dat %>% ## Freeform text fields
  dplyr::select("If you use any other source, indicate here which one", ## col 12
                "If you use any other criteria, indicate here which one", ## col 24
                "If you use any other criteria, indicate here which one_1", ## col 36
                "Are there any other thoughts or comments you wish to share with us?") ## col 37

dat_demo_likert <- dat %>% ## Demographic and 5 pt likert scale data
  dplyr::select(!"Consent and understanding", ## col 2
                !"If you use any other source, indicate here which one", ## col 12
                !"If you use any other criteria, indicate here which one", ## col 24
                !"If you use any other criteria, indicate here which one_1", ## col 36
                !"Are there any other thoughts or comments you wish to share with us?") ## col 37

## we will source this file when creating figures,
#### not heavy enough changes to warrant new writes/read
# ### Write split data to .csv for
# readr::write_csv(dat_freeform, "./data/dat_freeform.csv")
# readr::write_csv(dat_freeform, "./data/dat_freeform.csv")

