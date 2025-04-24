# objectif : lire la gsheet (voire la modifier)
# tuto ici : https://www.digitalocean.com/community/tutorials/google-sheets-in-r
13.71-11.
library(tidyverse)
library(googlesheets4)
library(openxlsx)
library(here)

gs <- read_sheet('https://docs.google.com/spreadsheets/d/1Jrciq7sZvzFKO9qpPaHBaHP85_VR33G1OWo9mCU2zWE/')

gs <- gs |> 
  filter(R1 == "DC") |> 
  select(NumAlpha, Nom, Prenom, R1Note) |> 
  mutate(
    Lu = if_else(is.na(R1Note), "", "OK")
  )
ici <- here()

setwd(here("D:/Document/Cours&TD/QESS 2024-25/MonMaster/Candidatures/MM/"))

listDamien <- read.xlsx("ListeDamien.xlsx") |> 
  left_join(gs |> select(NumAlpha, Lu), by = "NumAlpha")

listDamien <- rbind(
  listDamien |> filter(Lu != "OK"),
  listDamien |> filter(Lu == "OK") |> arrange(NumAlpha)
)

write.xlsx(listDamien, "ListeDamienLu.xlsx")

setwd(ici)

listDamien |> 
  filter(Lu == "OK") |> 
  count()
