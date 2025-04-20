
#Setup
library(tidyverse)
library(readxl)
library(readr)
library(kableExtra)
library(knitr)

#Read data files
load("vdem.RData")
Corrup <- read_excel("CPI2024-Results-and-trends.xlsx")
InfLab <- read_csv('EMP_NIFL_SEX_INS_RT_A-20250420T1917.csv')

#Filter VDem Data
vdem2 <- vdem %>%
  filter(year == 2024) %>%
  select(country_name, year, v2x_polyarchy, v2x_libdem, v2x_partipdem,
         v2x_egaldem, v2xel_frefair, v2xeg_eqaccess,
       ) %>%
  rename(CountryName = country_name) %>%
  rename(Polyarchy = v2x_polyarchy) %>%
  rename(LibDem = v2x_libdem) %>%
  rename(ParticipDem = v2x_partipdem) %>%
  rename(EgalDem = v2x_egaldem) %>%
  rename(FreeFairElect = v2xel_frefair) %>%
  rename(EqualAccess = v2xeg_eqaccess)%>%
  rename(YearExclInfLab = year)

#Filter Corrup Data
Corrup2 <- Corrup %>%
  select('Corruption Perceptions Index 2024: Global scores', ...4, ...5, 
         ...6, ...13, ...15, ...21) %>%
  rename(CPIScore = ...4) %>%
  rename(CPIScoreRank = ...5) %>%
  rename(CPIScoreStdErr = ...6) %>%
  rename(EconIntelRating = ...13) %>%
  rename(SnPGloInsightsRating = ...15) %>%
  rename(WEFRating = ...21)

#Filter InfLab Data
InfLab2 <- InfLab %>%
  group_by(ref_area.label) %>%
  filter(time == max(time, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(classif1.label == "Institutional sector: Total") %>%
  pivot_wider(names_from = sex.label, values_from = obs_value)%>%
  select(ref_area.label, time, Total, Male, Female) %>%
  rename(InfLabYear = time) %>%
  rename(MaleInfLab = Male) %>%
  rename(FemInfLab = Female) %>%
  rename(TotInfLab = Total)

#Join Datasets
jointdata <- vdem2 %>%
  left_join(Corrup2, by = 
    c("CountryName" = "Corruption Perceptions Index 2024: Global scores"))%>%
  left_join(InfLab2, by = c("CountryName" = "ref_area.label"))%>%
  ##Make all variables on a 0 to 1 scale.
  mutate(across(c(CPIScore, CPIScoreStdErr, 
                  SnPGloInsightsRating, EconIntelRating, WEFRating), 
                ~ as.numeric(.x) / 100))

#Write CSV
write_csv(jointdata, "CorruptionPolitEcon.csv")

