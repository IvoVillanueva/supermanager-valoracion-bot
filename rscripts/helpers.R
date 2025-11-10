# Helper functions and data loading for SuperManager analysis
library(tidyverse)
library(janitor)

# Load SuperManager player data 
superManager <- read_csv("https://raw.githubusercontent.com/IvoVillanueva/DATOS-JUGADORES-SUPERMANAGER-2025_26/refs/heads/main/data/supermanager_juagadores_2026.csv",
  show_col_types = FALSE
)

# Load boxscore statistics and merge with SuperManager data
stats_boxscores <- read_csv("https://raw.githubusercontent.com/IvoVillanueva/pbp-acb-2025-26/refs/heads/main/data/boxscores_2025_26.csv",
  show_col_types = FALSE
) %>%
  rename(fullName = license_license_str15) %>%
  left_join(superManager, join_by(fullName),
    relationship = "many-to-many"
  ) %>%
  clean_names()

# Create a short name for each team based on whether they are local or visitor
data_shortName <- stats_boxscores %>%
  mutate(shortName = ifelse(is_local == FALSE,
    visitor_team_team_actual_short_name,
    local_team_team_actual_short_name
  )) %>%
  select(abb, shortName) %>%
  unique()

# Load club logos data
clubs <- read.csv("https://raw.githubusercontent.com/IvoVillanueva/datos_aFAvor_eContra/refs/heads/main/2026/clubs_logosCuadrados.csv") 

