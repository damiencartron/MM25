# objectif : lire la gsheet (voire la modifier)
# tuto ici : 
# https://www.digitalocean.com/community/tutorials/google-sheets-in-r13.71-11.
library(tidyverse)
library(googlesheets4)
library(openxlsx)
library(readxl)
library(here)
library(arrow)

# Lecture du Gsheet ------
Eval <- read_sheet('https://docs.google.com/spreadsheets/d/1Jrciq7sZvzFKO9qpPaHBaHP85_VR33G1OWo9mCU2zWE/') |> 
  mutate(
    PDI = as.character(PDI),
    PDI = if_else(PDI == "NULL", NA, PDI)
  )

gs <- Eval |> 
  filter(R1 == "DC") |> 
  select(NumAlpha, Nom, Prenom, R1Note) |> 
  mutate(
    Lu = if_else(is.na(R1Note), "", "OK")
  )
ici <- here()


# Création de la liste des dossiers encore à lire par Damien (suivi de ceux déjà lu par ordre de NumAlpha)
setwd(here("D:/Document/Cours&TD/QESS 2024-25/MonMaster/Candidatures/MM/"))

listDamien <- read.xlsx("ListeDamien.xlsx") |> 
  left_join(gs |> select(NumAlpha, Lu), by = "NumAlpha")

listDamien <- rbind(
  listDamien |> filter(Lu != "OK"),
  listDamien |> filter(Lu == "OK") |> arrange(NumAlpha)
)

# write.xlsx(listDamien, "ListeDamienLu.xlsx")

setwd(ici)

listDamien |> 
  filter(Lu == "OK") |> 
  count()

# qqs stats sur les évaluations 


# Fichier PDI 
CommunPDI <-  Eval |> 
  filter(!is.na(PDI))

Jury_PDI <- read_excel("D:/Document/Cours&TD/QESS 2024-25/MonMaster/Candidatures/MM/Jury PDI 2025.xlsx", 
                            sheet = "PourR")

## reconstitution du fichier PDI ----
DetailNom <- Jury_PDI_2025 <- read_excel("D:/Document/Cours&TD/QESS 2024-25/MonMaster/Candidatures/MM/Jury PDI 2025.xlsx", 
                                         sheet = "listenum") |> 
  rename(NOM = 'Nom de naissance', 
         Firstname = "Prénom", 
         id = "Numéro de candidat") |> 
  filter(id != "CANDJFY7LBI8" & id != "CANDF8SJET61") |> # je gère les homonymie 
  select(NOM,  id)

t <- left_join(Jury_PDI, DetailNom, by = "NOM") |>
  mutate(
    rang = row_number(), 
    Liste = if_else(rang<=24, str_glue("LP{rang}"), str_glue("LC{rang-24}"))
  )

## Fusion avec le fichier QESS 
FinalCommun <-  left_join(CommunPDI |> select(id, Cursus, R1Note, R2Note, Commentaires, NumAlpha),
                          t |> select(!'N°'), by = "id")

setwd(here("D:/Document/Cours&TD/QESS 2024-25/MonMaster/Candidatures/MM/"))
FinalCommun |> 
  select(NumAlpha, Liste) |> 
  arrange(NumAlpha) 

FinalCommun |> count(!is.na(Liste))
# PDI nous en pique possiblement 15 

# liste des étudiants communs classés par rang dans PDI 
pdi <- FinalCommun |>
  filter(!is.na(Liste)) |> 
  arrange(rang)|> 
  write.xlsx("PDIbyDamien.xlsx")


# Tentative de regex sur les notes 
library(babynames) # c'est pour tester des exemples si j'ai bien compris 
e <- Eval

e <- Eval |> 
  mutate(
    fusion = str_glue("{R1Note}/{R2Note}"),
    synthese = case_when(
      #str_detect(fusion, "A\\+\\+.*A\\+\\+.*") ~"A++A++",
      #str_detect(fusion, "A\\+\\+.*A\\+.*|A\\+.*A\\+\\+.*") ~"A++A+",
      str_detect(fusion, "A.*/C.*|C.*/A.*") ~"AC",
      str_detect(fusion, "A\\+.*A\\+.*|A\\+\\+/A|A/A\\+\\+") ~"A+A+",
      #str_detect(fusion, "A\\+/A[^-]|A[^-]/A\\+") ~"A+A",
      str_detect(fusion, "A\\+/A|A/A\\+") ~"A+A",
      str_detect(fusion,"A-/A.*|A.*/A-") ~"A-",
      str_detect(fusion, "A.*A.*") ~"AA", 
      str_detect(fusion, "B\\+.*|.*B\\+") ~"B+",
      str_detect(fusion, "C\\+.*C.*|C.*/C\\+") ~"C+", 
      str_detect(fusion, "C/C") ~"CC"
    ), 
    NoteDC = as.character(NoteDC), 
    NoteDC = if_else(NoteDC == "NULL", NA, NoteDC),
    NoteDC = as.numeric(NoteDC), 
    MentionBac = if_else(MentionBac == "Très bien avec les félicitations du jury", "Bravo !", MentionBac),
    # MentionBac = as_factor(MentionBac),
    MentionBac = fct_relevel(as_factor(MentionBac), "Sans mention", "Assez bien", "Bien", "Très bien", "Bravo !")
  )|> 
    relocate(synthese, fusion, NoteDC, .before = R1Note) |> 
  arrange(synthese)

table(e$synthese)
e <- e |> arrange(desc(NoteDC))


e |> filter(str_detect(R1Note, "A"))

e |> group_by(R1) |> summarize(
  prop_A = mean(str_detect(R1Note, "A")*100),
  prop_B = mean(str_detect(R1Note, "B")*100),
  prop_C = mean(str_detect(R1Note, "C")*100),
  )
# j'ai mis nettemetn plus de A et de C que Etienne

e |> group_by(R1) |> 
  summarize(
    Moy = mean(NoteDC, na.rm= TRUE )
  )

mean(e$NoteDC, na.rm = TRUE)

e |> group_by(R2) |> summarize(
  prop_A = mean(str_detect(R1Note, "A")*100),
  prop_B = mean(str_detect(R1Note, "B")*100),
  prop_C = mean(str_detect(R1Note, "C")*100),
)
# Cécile a mis bcp bcp de A et peu de C  ; Raphael très peu de A mais bcp de B 

# Stats post-commission -----
t <- e |> filter(Rang!="NC") |> 
  group_by(Cursus) |> 
  arrange(Cursus) |> 
  distinct(LYEtb) 

e <-e |> 
  rename(
    LY2MoyS1 = Moyenne.au.premier.semestre...60,
    LY2MoyS2 = Moyenne.au.second.semestre...61
  ) |> 
  mutate(
  Rang = as.character(Rang)) |> 
  # filter(Rang != "NC" & Rang != "NULL" & !is.na(Rang)) |> 
  mutate(
    Rang = as.numeric(Rang),
    LYMoyS1 = if_else(LYMoyS1 == 99, NA, LYMoyS1),
    LYMoyS2 = if_else(LYMoyS2 == 99, NA, LYMoyS2), 
    LY2MoyS1 = if_else(LY2MoyS1 == 99, NA, LY2MoyS1),
    LY2MoyS2 = if_else(LY2MoyS2 == 99, NA, LY2MoyS2), 
    Group = case_when(
      Rang <=20 ~"1-20",
      Rang <=40 ~"21-40",
      Rang <=60 ~"41-60",
      Rang <=80 ~"61-80", 
      Rang <=100 ~"81-90"
    )
    ) |> 
  rowwise() |> 
  mutate(
    LYMoy = mean(c(LYMoyS1, LYMoyS2), na.rm = TRUE), 
    LY2Moy = mean(c(LY2MoyS1, LY2MoyS2), na.rm = TRUE)
  ) |> 
  relocate(LYMoy, LY2Moy,  .before = LYMoyS1) |> 
  arrange(Rang)


# Ecriture du parquet emm (evaluation mon master----
# solution copilot face au problème de passage en parquet 
# Apparemmetn c'est la variable DOB qui met le bazar !
# Supprime la liste en aplatissant
dob_chr <- sapply(e$DOB, function(x) {
  # Si POSIXct ou Date, convertit en chaîne
  if (inherits(x, "POSIXt") || inherits(x, "Date")) {
    format(x, "%Y-%m-%d")
  } else if (is.character(x)) {
    x
  } else {
    NA_character_
  }
})

# Convertit tout en Date (en gérant les deux formats possibles)
e$DOB <- as.Date(dob_chr, format = "%Y-%m-%d")
e$DOB[is.na(e$DOB)] <- as.Date(dob_chr[is.na(e$DOB)], format = "%d/%m/%Y")


# 2ème problème il reste des listes et faut les applatir : 
for(n in names(e)) {
  if (is.list(e[[n]])) {
    e[[n]] <- sapply(e[[n]], function(x) {
      if (is.null(x)) NA else as.character(x)
    })
  }
}

setwd(here())

e <- e |> 
  left_join(read_parquet("Cand_Origine.parquet") |> 
              select(id, Mail, Tel1, Tel2, INE, "INE.maître..INES.", Adresse, "Complément.d.adresse"), 
            by = "id") |> 
  rename(INEINES = "INE.maître..INES.", 
         CptAdr = "Complément.d.adresse") |> 
  relocate(Adresse, CptAdr, .before = CP) |> 
  mutate(`Aménagement.d.études...93` = as.character(unlist(`Aménagement.d.études...93`)), 
         `Parcours...95` = as.character(unlist(`Parcours...95`)))

e <- e |> (\(df) { df[["Aménagement.d.études...93"]] <- as.character(unlist(df[["Aménagement.d.études...93"]])) ; df })()
e <- e |> (\(df) { df[["Parcours...95"]] <- as.character(unlist(df[["Parcours...95"]])) ; df })()


write_parquet(e, "emm.parquet")
### fin de l'écriture du parquet 


f <- e |> 
  filter(Rang != "NC" & Rang != "NULL" & !is.na(Rang)) 

f |> group_by(Cursus) |> 
  mutate(
    MoyN = (mean(LYMoy, na.rm = TRUE)), 
    N = row_number(),
  ) |> 
  arrange(desc(N)) |> 
  distinct(Cursus, .keep_all = TRUE) |> 
  select(Cursus, MoyN, N) |> 
  print(n=100)


# Mentions bac 
freq(e$MentionBac)
freq(f$MentionBac)

t <- f |> filter(!is.na(Rang)) |> 
  group_by(Group) |> 
  mutate(
    # MTBTTB = if_else(MentionBac == "Bravo ! " | MentionBac == "Très bien", 1,0),
    MTBTTB = case_when(
      (MentionBac == "Bravo ! " | MentionBac == "Très bien") ~1, 
      is.na(MentionBac) ~NA, 
      .default = 0 #inatendu le case when et le if else donnent le même résultat
    )) |> 
  summarize(PCTTB = mean(MTBTTB, na.rm= TRUE )*100,
            N = sum(!is.na(MTBTTB)))

## Y a-t-il des Ste Marie dans la salle ? 

e |> filter(LYEtb == "Lycée général et technologique privé Sainte Marie - 92200 - Neuilly-sur-Seine") |> 
  select(LYEtb)
# bon bcp ont mal saisi l'adresse 

e |> str_detect(LYEtb, "*Sainte Marie*")
# ça marche pas et c'est bizarre 

str(e)

# Création du fichier pour mon master ----
Clstt_in <- read_excel("Classement_330_20250513_org.xlsx", 
                                                  col_types = c("text", "text", "text", 
                                                                "text", "text", "text", "text")) |> 
  left_join(e |> select(id, Rang, Commentaires) |> rename('Identifiant candidat' = id), by = "Identifiant candidat") |> 
  # rowwise() |>
  mutate(
    Classement = as.character(Rang),
    Classement = as.numeric(Classement), 
    Classement = as.character(Classement),
    Classement = if_else(is.na(Classement), "NC", Classement)
    ) |> 
  select(!Rang)

write.xlsx(Clstt_in, "Classement_330_20250513.xlsx")
