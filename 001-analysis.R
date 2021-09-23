library(tidyverse)
library(here)
library(readxl)

# get list of data file names
data_files <- list.files(here("data/"), full.names = TRUE)

# just one sheet for this one
data_c <- read_excel(data_files[1])

# one sheet with farsan data
data_f <- read_excel(data_files[2])

# multiple sheets for Mirak, combine them
data_m_1A <- read_excel(data_files[3], sheet = "1A")
data_m_1B <- read_excel(data_files[3], sheet = "1B")
data_m_3B <- read_excel(data_files[3], sheet = "3b")
data_m_1 <-  read_excel(data_files[3], sheet = "Sheet1")

data_m <-
bind_rows(data_m_1A,
          data_m_1B,
          data_m_3B,
          data_m_1)

# import data from all excel files
data_files_tbls <- list(data_c,
                        data_f,
                        data_m)

names(data_files_tbls) <-
  str_remove(basename(data_files), ".xlsx")


#-----------------------------------------------------------------
## Typology at each site
typology_at_each_site <-
map(data_files_tbls, ~.x %>%
      select(Typology)) %>%
  bind_rows(.id = "Site") %>%
  mutate(across(everything(), ~str_to_lower(.x))) %>%
  # simplify the typology
  mutate(Typology = case_when(
    str_detect(Typology, "end scraper") ~ "end scraper",
    str_detect(Typology, "convergent") ~ "convergent",
    str_detect(Typology, "point") ~ "point",
    str_detect(Typology, "dejet") ~ "dejet",
    str_detect(Typology, "multifunction") ~ "multifunction",
    str_detect(Typology, "notch") ~ "notch",
    str_detect(Typology, "end") ~ "end scraper",
    str_detect(Typology, "used") ~ "used",
    str_detect(Typology, "scraper on core") ~ "scraper",
    str_detect(Typology, "scraper/ naturally backed") ~ "scraper",
    str_detect(Typology, "retouched") ~ "retouched",
    TRUE ~ Typology
  )) %>%
  mutate(Typology = case_when(
    Typology == "multifunction" ~ NA_character_,
    Typology == "used" ~ NA_character_,
    Typology == "core fragment" ~ NA_character_,
    Typology == "retouched" ~ NA_character_,
    TRUE ~ Typology
  )) %>%
  group_by(Site, Typology) %>%
  tally() %>%
  pivot_wider(names_from = Site,
              values_from = n)

# how many artefacts in each site sample?
artefacts_per_site <-
typology_at_each_site %>%
  pivot_longer(-Typology,
               names_to = "Site",
               values_to = "N") %>%
  group_by(Site) %>%
  tally(N)

# plot
typology_at_each_site %>%
  pivot_longer(-Typology,
               names_to = "Site",
               values_to = "N") %>%
  filter(!is.na(Typology)) %>%
  left_join(artefacts_per_site) %>%
  mutate(Site = paste0(Site, "\n(n = ", n, ")")) %>%
  mutate(Site = str_to_title(Site)) %>%
ggplot() +
  aes(Site,
      N,
      fill = Typology) +
  geom_col(position = "fill") +
  scale_fill_viridis_d() +
  xlab("") +
  ylab("") +
  theme_minimal(base_size = 14) +
  theme(panel.background = element_rect(fill='white',
                                        colour = "white"),
        plot.background = element_rect(fill='white',
                                       colour = "white"))

ggsave(here::here("figures/typology-plot.png"),
       h = 5, w = 7)

#-----------------------------------------------------------------
## Levallois typology
levallois_typology_at_each_site <-
  map(data_files_tbls, ~.x %>%
        select(`Levallois Type`)) %>%
  bind_rows(.id = "Site") %>%
  mutate(across(everything(), ~str_to_lower(.x))) %>%
  # simplify the typology
  mutate(`Levallois Type` = case_when(
    str_detect(`Levallois Type`, "seconfary") ~ "secondary",
    str_detect(`Levallois Type`, "prep") ~ "secondary levallois flake",
    `Levallois Type` == "lateral preparatory levallois flake" ~ "secondary levallois flake",
    `Levallois Type` == "modified secondary levallois flake" ~ "secondary levallois flake",
    `Levallois Type` == "modified secondary flake" ~ "secondary levallois flake",
    `Levallois Type` == "secondary levallois  flake" ~ "secondary levallois flake",
    `Levallois Type` == "levallois end product flake" ~ "levallois flake",
    `Levallois Type` == "levallois fake" ~ "levallois flake",
    TRUE ~ `Levallois Type`
  )) %>%
  mutate(`Levallois Type` = ifelse(`Levallois Type` == "secondary",
                                   "secondary levallois flake",
                                   `Levallois Type`)) %>%
  group_by(Site, `Levallois Type`) %>%
  tally() %>%
  pivot_wider(names_from = Site,
              values_from = n)

# plot
levallois_typology_at_each_site %>%
  pivot_longer(-`Levallois Type`,
               names_to = "Site",
               values_to = "N") %>%
  filter(!is.na(`Levallois Type`)) %>%
  left_join(artefacts_per_site) %>%
  mutate(Site = paste0(Site, "\n(n = ", n, ")")) %>%
  mutate(Site = str_to_title(Site)) %>%
  ggplot() +
  aes(Site,
      N,
      fill = `Levallois Type`) +
  geom_col(position = "fill") +
  scale_fill_viridis_d() +
  xlab("") +
  ylab("") +
  theme_minimal(base_size = 14) +
  theme(panel.background = element_rect(fill='white',
                                        colour = "white"),
        plot.background = element_rect(fill='white',
                                       colour = "white"))

ggsave(here::here("figures/levallois-type-plot.png"),
       h = 5, w = 7)

#-----------------------------------------------------------------
## Levallois method

levallois_method_at_each_site <-
  map(data_files_tbls, ~.x %>%
        select(`Levallois Method`)) %>%
  bind_rows(.id = "Site") %>%
  mutate(across(everything(), ~str_to_lower(.x))) %>%
  # simplify the typology
  mutate(`Levallois Method` = case_when(
    # centripital and classic are the same.
    # Nubian and point methods also can be considered as one category.
    `Levallois Method` == "classic (centripital)" ~ "classic",
    `Levallois Method` == "classic (cebtripetal)" ~ "classic",
    `Levallois Method` == "classical (centripital_" ~ "classic",
    `Levallois Method` == "classic (centripetal)" ~ "classic",
    `Levallois Method` == "centripital" ~ "classic",
    `Levallois Method` == "nubian 1" ~ "point",
    `Levallois Method` == "point method" ~ "point",
    str_detect(`Levallois Method`, "parallel") ~ "parallel",
    TRUE ~ `Levallois Method`
  )) %>%
  group_by(Site, `Levallois Method`) %>%
  tally() %>%
  pivot_wider(names_from = Site,
              values_from = n)

# plot
levallois_method_at_each_site %>%
  pivot_longer(-`Levallois Method`,
               names_to = "Site",
               values_to = "N") %>%
  filter(!is.na(`Levallois Method`)) %>%
  left_join(artefacts_per_site) %>%
  mutate(Site = paste0(Site, "\n(n = ", n, ")")) %>%
  mutate(Site = str_to_title(Site)) %>%
  ggplot() +
  aes(Site,
      N,
      fill = `Levallois Method`) +
  geom_col(position = "fill") +
  scale_fill_viridis_d() +
  xlab("") +
  ylab("") +
  theme_minimal(base_size = 14) +
  theme(panel.background = element_rect(fill='white',
                                        colour = "white"),
        plot.background = element_rect(fill='white',
                                       colour = "white"))

ggsave(here::here("figures/levallois-method-plot.png"),
       h = 5, w = 7)

#-----------------------------------------------------------------
## Butt type

butt_type_at_each_site <-
  map(data_files_tbls, ~.x %>%
        select(Butt)) %>%
  bind_rows(.id = "Site") %>%
  mutate(across(everything(), ~str_to_lower(.x))) %>%
  # simplify the typology
  mutate(Butt = case_when(
    Butt == "chapeau  de gendarme" ~ "chapeau de gendarme",
    Butt == "chapeau de hendarme" ~ "chapeau de gendarme",
    Butt == "linear" ~ "plain",
    Butt == "broken" ~ NA_character_,
    Butt == "removed" ~ NA_character_,
    Butt == "indet" ~ NA_character_,
    TRUE ~ Butt
  )) %>%
  group_by(Site, Butt) %>%
  tally() %>%
  pivot_wider(names_from = Site,
              values_from = n)

# plot
butt_type_at_each_site %>%
  pivot_longer(-Butt,
               names_to = "Site",
               values_to = "N") %>%
  filter(!is.na(Butt)) %>%
  left_join(artefacts_per_site) %>%
  mutate(Site = paste0(Site, "\n(n = ", n, ")")) %>%
  mutate(Site = str_to_title(Site)) %>%
  ggplot() +
  aes(Site,
      N,
      fill = Butt) +
  geom_col(position = "fill") +
  scale_fill_viridis_d() +
  xlab("") +
  theme_minimal(base_size = 14) +
  theme(panel.background = element_rect(fill='white',
                                        colour = "white"),
        plot.background = element_rect(fill='white',
                                       colour = "white"))

ggsave(here::here("figures/butt-type-plot.png"),
       h = 5, w = 7)









