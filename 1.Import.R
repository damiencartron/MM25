library(readxl)
library(openxlsx)
library(tidyverse)
library(descr)
library(babynames)

PDI <- read_excel("D:/Document/Cours&TD/QESS 2024-25/MonMaster/Candidatures/MM/MM_in/20250401_11H10_0753742K_EHESS PARIS_329.xlsx", .name_repair = "universal") |> 
  rename(id = "Numéro.de.candidat") |> 
  mutate(PDI = "PDI")

Cand <- read_excel("D:/Document/Cours&TD/QESS 2024-25/MonMaster/Candidatures/MM/MM_in/20250401_08H56_0753742K_EHESS PARIS_330.xlsx", .name_repair = "universal") |>   
  rename(
    Ref = Référence, 
    Statut = Statut.de.la.candidature,
    Statpred = "Statut.précédent.de.la.candidature",
    Statdatechange = "Date.du.dernier.changement.de.statut",
    Nom = Nom.de.naissance, 
    Prenom = Prénom, 
    id = "Numéro.de.candidat", 
    Demissionnaire = Démissionnaire, 
    Mail = Adresse.e.mail,
    Tel1 = "Numéro.de.téléphone.principal" ,
    Tel2 = "Numéro.de.téléphone.secondaire" ,
    Civilite = Civilité,
    INE = "INE.INA.BEA.saisi.par.le.candidat", 
    DOB = "Date.de.naissance" , 
    Nationalite = Nationalité, 
    Refugie = "Statut.de.réfugié", 
    CP = Code.Postal, 
    Ville = "Ville...Commune", 
    Pays = "Pays.de.résidence"  ,
    DateBac = "Année.d.obtention.du.baccalauréat" ,
    TypeBac = "Type.de.baccalauréat.ou.titre.admis.en.équivalence", 
    SerieBac = "Série.du.baccalauréat" ,
    MentionBac = "Mention.obtenue"  ,
    Sportif = "Sportif.haut.niveau" ,
    Artiste = "Artiste.confirmé" , 
    Handicap = "Situation.de.handicap", 
    Actu = "Situation.actuelle", 
    FormationContinue = "Avez.vous.engagé.une.démarche.de.financement.dans.le.cadre.de.la.formation.continue..", 
    LYetude = "Année.universitaire...36",
    LYDiplFR = "Diplôme.français.....37",
    LYNivDipl = "Niveau.du.diplôme...38" , 
    LYFormation = "Type.de.formation.ou.de.diplôme.préparé...39",
    LYAnnCursus = "Année.dans.le.cursus...40", 
    LYSpe = "Mention.ou.spécialité...42"   , 
    LYParcours = "Parcours...43",
    LYMoyS1 = "Moyenne.au.premier.semestre...44",
    LYMoyS2 = "Moyenne.au.second.semestre...45", 
    LYEtb = "Établissement...46"  
    
    ) |> 
  arrange(Nom, Prenom)|> 
  mutate(
    Nom = str_to_upper(Nom), 
    Prenom = str_to_title(Prenom),
    NumAlpha = row_number(),
    Cursus = case_when(
      (str_detect(LYFormation, "CPGE") | str_detect(LYFormation, "Préparation concours")) ~"CPGE",
      str_detect(LYFormation, "CPES") ~"CPES",
      str_detect(LYEtb, regex("normale", ignore_case = TRUE)) ~"ENS", 
      str_detect(LYFormation, "Licence") ~str_glue("L3:{LYSpe}"),
      str_detect(LYFormation, "Maîtrise") ~"Maitrise",
      str_detect(LYFormation, "Magistère") ~"Magistere", 
      str_detect(LYFormation, "Master") ~str_glue("Master:{LYSpe}"),
      str_detect(LYFormation, "Autre diplôme") ~"Autre diplôme"
    ),
    Cursus = if_else(Cursus == "L3:Mathématiques et informatique appliquées aux sciences humaines et sociales", "L3:MIASHS", Cursus), 
    Cursus = if_else(str_detect(Cursus, "MEEF"), "Master:MEEF", Cursus), 
    R1 = if_else(NumAlpha%%2 == 0, "DC","EP"),
    R2 = case_when(
      (NumAlpha - 1) %% 9 == 0 ~"BG", 
      (NumAlpha - 2) %% 9 == 0 ~"AA", 
      (NumAlpha - 3) %% 9 == 0 ~"RD", 
      (NumAlpha - 4) %% 9 == 0 ~"FM", 
      (NumAlpha - 5) %% 9 == 0 ~"JD", 
      (NumAlpha - 6) %% 9 == 0 ~"CB", 
      (NumAlpha - 7) %% 9 == 0 ~"FM", 
      (NumAlpha - 8) %% 9 == 0 ~"JD", 
      (NumAlpha - 9) %% 9 == 0 ~"CB" 
      
    )
  )  |> 
  relocate(NumAlpha, R1, R2) 

Cand <- left_join(Cand, PDI |> select(id, PDI), by = "id")

Cand |> filter(PDI == "PDI") |> count()

names(Cand)

table(Cand$MentionBac)

PrExport <- Cand |> 
  relocate(NumAlpha,Nom, Prenom, Civilite, id, Cursus,R1, R2, PDI, DOB, Nationalite, LYetude, LYDiplFR, LYNivDipl, LYFormation, LYAnnCursus, LYSpe, LYParcours, LYMoyS1, LYMoyS2, LYEtb ,DateBac, TypeBac, SerieBac, MentionBac) 
  

setwd(here())
write.xlsx(PrExport, "Candidatures.xlsx")
write_parquet(PrExport, "Cand_Origine.parquet")

# t <- Cand |> 
#   select(LYFormation, LYSpe, LYEtb, PDI) |> 
#   mutate(
# Cursus = case_when(
#   (str_detect(LYFormation, "CPGE") | str_detect(LYFormation, "Préparation concours")) ~"CPGE",
#   str_detect(LYFormation, "CPES") ~"CPES",
#   str_detect(LYEtb, regex("normale", ignore_case = TRUE)) ~"ENS", 
#   str_detect(LYFormation, "Licence") ~str_glue("L3:{LYSpe}"),
#   str_detect(LYFormation, "Maîtrise") ~"Maitrise",
#   str_detect(LYFormation, "Magistère") ~"Magistere", 
#   str_detect(LYFormation, "Master") ~str_glue("Master:{LYSpe}"),
#   str_detect(LYFormation, "Autre diplôme") ~"Autre diplôme"
#   ),
# Cursus = if_else(Cursus == "L3:Mathématiques et informatique appliquées aux sciences humaines et sociales", "L3:MIASHS", Cursus), 
# Cursus = if_else(str_detect(Cursus, "MEEF"), "Master:MEEF", Cursus)) 
#   

table(Cand$R2)
################################################
str_view(fruit, "berry")

str_view(c("a", "ab", "ae", "bd", "ea", "eab"), "a.")
str_view(fruit, "a...e")
str_detect(c("a", "b", "c"), "[aeiou]")
