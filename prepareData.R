
library(tidyverse)

data_path <- "data/SICSS Projects_December 28, 2021_09.48.csv"

types <- cols(
  StartDate = col_datetime(format = ""),
  EndDate = col_datetime(format = ""),
  Status = col_character(),
  IPAddress = col_character(),
  Progress = col_double(),
  `Duration (in seconds)` = col_double(),
  Finished = col_logical(),
  RecordedDate = col_datetime(format = ""),
  ResponseId = col_character(),
  RecipientLastName = col_character(),
  RecipientFirstName = col_character(),
  RecipientEmail = col_character(),
  ExternalReference = col_character(),
  LocationLatitude = col_double(),
  LocationLongitude = col_double(),
  DistributionChannel = col_character(),
  UserLanguage = col_character(),
  name = col_character(),
  year = col_character(),
  site_2017_1 = col_character(),
  site_2018_1 = col_character(),
  site_2019_1 = col_character(),
  site_2020_1 = col_character(),
  site_2021_1 = col_character(),
  type_1 = col_character(),
  proj_1 = col_character(),
  auth_1 = col_character(),
  publication_1 = col_character(),
  doi_1 = col_character(),
  website_1 = col_character(),
  media_1 = col_character(),
  end_1 = col_character(),
  opend_ended = col_character(),
  type_2 = col_character(),
  proj_2 = col_character(),
  auth_2 = col_character(),
  publication_2 = col_character(),
  doi_2 = col_character(),
  website_2 = col_character(),
  media_2 = col_character(),
  end_2 = col_character(),
  type_3 = col_character(),
  proj_3 = col_character(),
  auth_3 = col_character(),
  publication_3 = col_character(),
  doi_3 = col_character(),
  website_3 = col_character(),
  media_3 = col_character(),
  end_3 = col_character(),
  type_4 = col_character(),
  proj_4 = col_character(),
  auth_4 = col_character(),
  publication_4 = col_character(),
  doi_4 = col_character(),
  website_4 = col_character(),
  media_4 = col_character(),
  end_4 = col_character(),
  type_5 = col_character(),
  proj_5 = col_character(),
  auth_5 = col_character(),
  publication_5 = col_character(),
  doi_5 = col_character(),
  website_5 = col_character(),
  media_5 = col_character()
)

varnames <- colnames(read_csv(data_path, n_max = 1))
df <- read_csv(data_path, skip = 3, col_names = varnames, col_types = types)

# process

df <- df |> 
  # remove tests
  filter(DistributionChannel == "anonymous") |> 
  janitor::clean_names()

cat(colnames(df) |> paste("\n"))
s_remove <- c("recipient_last_name", "recipient_first_name", "recipient_email", "external_reference", "user_language") 

df_clean <- df |> 
  # keep only people who finished
  filter(finished == TRUE) |> 
  rename(open_ended = opend_ended) |> 
  select(-all_of(s_remove)) |> 
  relocate(response_id) |> 
  # clean some NAs
  mutate(across(where(is.character), \(x) ifelse(x == "N/A", NA_character_, as.character(x))))


df_clean <- df_clean |> 
  ## nest multiple years
  mutate(year = str_split(year, ",")) |> 
  ## nest multiple sites
  unite("site", matches("site_\\d{4}_1"), sep = ";", na.rm = TRUE) |>
  mutate(site = str_split(site, ";")) |> 
  ## remove «end»
  select(-matches("end_\\d"))


df_long <- df_clean |> 
  ## pivot all variables with a suffix —i.e., tidyr magick
  pivot_longer(
    cols = matches(".*_\\d$"),
    names_to = c(".value", "proj_id"),
    names_pattern = "(.*)_(\\d+)", 
    values_drop_na = TRUE
  ) 


# s_vars <- c("n", "type", "proj", "auth", "publication", "doi", "website", "media")


# df_clean |> 
#   group_by(response_id) |> 
#   summarise(across(all_of(s_vars), \(x) list(unique(x))))


rm(types, data_path, s_remove, varnames)


#

