library(tidyverse)
library(arrow)
library(readxl)
library(openxlsx)
library(here)
ici <- here()

emm <- read_parquet("emm.parquet")
ENSJ0 <- read_excel("D:/Document/Cours&TD/QESS 2024-25/MonMaster/Candidatures/MM/MM_in/Candidatures-EHESS PARIS-1900125MCBW2_20250602.xlsx")
EHESSJ0 <- read_excel("D:/Document/Cours&TD/QESS 2024-25/MonMaster/Candidatures/MM/MM_in/Candidatures-EHESS PARIS-1900125M6PVU_20250602.xlsx")

mmJ0 <- read_parquet("emm.parquet") |> 
  select(id, Mail, Civilite, Nom, Prenom, Cursus, Commentaires, fusion, PDI, DOB, Rang) |> 
  left_join(read_excel("D:/Document/Cours&TD/QESS 2024-25/MonMaster/Candidatures/MM/MM_in/Candidatures-EHESS PARIS-1900125MCBW2_20250602.xlsx") |> 
              rename(id = 'Identifiant candidat', R_ENS = Classement, LC_ENS = "Position sur liste d’attente") |> 
              select(id, R_ENS, LC_ENS), by = "id") |> 
  left_join(read_excel("D:/Document/Cours&TD/QESS 2024-25/MonMaster/Candidatures/MM/MM_in/Candidatures-EHESS PARIS-1900125M6PVU_20250602.xlsx") |> 
              rename(id = 'Identifiant candidat', R_EHESS = Classement, LC_EHESS = "Position sur liste d’attente") |> 
              select(id, R_EHESS, LC_EHESS), by = "id") |> 
  filter(!is.na(Rang))

write.xlsx(mmJ0, "D:/Document/Cours&TD/QESS 2024-25/MonMaster/Candidatures/MM/MM_out/syntheseJ0.xlsx")
  
mmJ0 |> 
  filter(is.na(R_ENS)) |> 
  count()

mmJ0 |> 
  filter(is.na(R_EHESS)) |> 
  count()
