# Script de création du fichier de suivi de mon master avec une fonction qui crée tous les fichiers de synthèse et les fichiers de refus 
# pour chaque jour depuis le 4 juin 2024 jusqu'à aujourd'hui 
# fonctionne nickel
# À AJOUTER : - un système qui récupère un vecteur pour les dates disponibles (depusi les noms des fichiers excels) pour permettre d'avoir des trous dans les séries 
#             - une fonction qui permette de signaler le nom d'entrée des fichiers pour pouvoir l'adapter facilement à PDI (par exemple) 
#             - une représentation graphique des trajectoire individuelles des étudiants (changement de statut par date) ; j'avais commencé mais je l'ai perdu
#             - un merge avec les fichiers PDI pour voir ce que les bi-demandeurs PDI / QESS ont eu comme trajectoire 
#             - un export du fichier des admis avec les infos utiles du type DOB, mèl, établissement d'inscription, etc. 
# ATTENTION : M6PUV = EHESS // BW = ENS (j'avais inversé dans la première version du script)


library(readxl)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(here)
library(arrow)
MM25 <- here()
MMin <- here("D:/Document/Cours&TD/QESS 2024-25/MonMaster/Candidatures/MM/MM_in")
MMout <- here("D:/Document/Cours&TD/QESS 2024-25/MonMaster/Candidatures/MM/MM_out")
setwd(MM25)
# Import des bases qui ne varient pas ----- 

## Base Evaluation : le fichier avec les détails de l'évaluation des dossiers-----
Eval <- read_parquet("emm.parquet")
# attention, le nom Eval a été utilisé pour le fichier d'origne également, j'ai donc deux fichiers Eval mais qui ne doivent pas être utilisés en même temps 


Eval <- Eval |>  rename(NumOrdre = NumAlpha,  ClassTot = NoteDC, Classement = Rang, NoteEval = fusion, NoteE1 = R1Note, NoteE2 = R2Note, Eval1 = R1, Eval2= R2)
EvalComplete <- Eval 
Eval <- Eval  |> 
  select(NumOrdre, id, Cursus, ClassTot, NoteEval,  Classement, NoteE1, NoteE2, Commentaires, Eval1, Eval2)
  # select(c('NumOrdre', 'id', 'ClassTot', 'NoteEval',  'Classement', 'NoteE1', 'NoteE2', 'Commentaires', 'Eval1' , 'Eval2'))

## Base Candidat : le fichier monmaster avant évaluation -----

setwd(MMin)

# petit bloc pour modifier le nom des fichiers qui a été changé en cours de route !----- 
listdossiers<- list.files(recursive = TRUE, full.names = FALSE) |> 
  as_tibble() |> 
  filter(str_starts(value, "Candidatures_EHESS PARIS-1900125")) |> 
  mutate(
    correctname = str_replace(value, "Candidatures_EHESS PARIS-1900125", "Candidatures-EHESS PARIS-1900125")
  )
file.rename(from = listdossiers$value, to = listdossiers$correctname)

# lecture base candidat ---- 
BaseCandidat <- read_excel("20250401_08H56_0753742K_EHESS PARIS_330_org.xlsx") |> 
                  rename(id = `Numéro de candidat`,
                         Nom = 'Nom de naissance',
                         Prenom = 'Prénom',
                         mail = 'Adresse e-mail',
                         Civilite = 'Civilité',
                         DOB = 'Date de naissance', 
                         MentionBac = 'Mention obtenue') |> 
  mutate(
    Prenom = str_to_title(Prenom, locale = "fr"), 
    Nom = str_to_upper(Nom, locale = "fr"), 
    CandidatP = str_glue("{Nom} {Prenom}")
  ) |> select(id, Nom, Prenom, CandidatP, mail, Civilite, DOB, MentionBac)

setwd(MMin)

# Ma super fonction -----
importdate <- function(date){
  assign(paste0("ENS_",gsub("-", "", date)), value = read_excel(paste0("Candidatures-EHESS PARIS-1900125MCBW2_",gsub("-", "", date),".xlsx")),envir = .GlobalEnv)
  assign(paste0("EHESS_",gsub("-", "", date)), value = read_excel(paste0("Candidatures-EHESS PARIS-1900125M6PVU_",gsub("-", "", date),".xlsx")),envir = .GlobalEnv)
  # assign(paste0("PDI-ENS_",gsub("-", "", date)), value = read_excel(paste0("Candidatures-EHESS PARIS-1900125MM1FR_",gsub("-", "", date),".xlsx")),envir = .GlobalEnv)
  # assign(paste0("PDI-EHESS_",gsub("-", "", date)), value = read_excel(paste0("Candidatures-EHESS PARIS-1900125MHFU8_",gsub("-", "", date),".xlsx")),envir = .GlobalEnv)


  #fichiers du jour 
    assign("ENS", value = read_excel(paste0("Candidatures-EHESS PARIS-1900125MCBW2_",gsub("-", "", date),".xlsx")),envir = .GlobalEnv)
    assign("EHESS", value = read_excel(paste0("Candidatures-EHESS PARIS-1900125M6PVU_",gsub("-", "", date),".xlsx")),envir = .GlobalEnv) # l'autre mode d'assignation global eest <<- 
    assign("PDI_EHESS", value = read_excel(paste0("Candidatures-EHESS PARIS-1900125MHFU8_",gsub("-", "", Sys.Date()),".xlsx")),envir = .GlobalEnv)
    assign("PDI_ENS", value = read_excel(paste0("Candidatures-EHESS PARIS-1900125MM1FR_",gsub("-", "", Sys.Date()),".xlsx")),envir = .GlobalEnv)
    
  
  #supprimer dans EHESS les candidats avec statut== non_classee
    EHESS <- EHESS[EHESS$Statut != "NON_CLASSEE",]
    ENS <- ENS[ENS$Statut != "NON_CLASSEE",]
  
  # renommer Statut en Statut_EHESS et Statut en Statut_ENS, démissionnaire en dem_EHESS et en dem_ENS
  
  EHESS <- EHESS %>% dplyr::rename(Statut_EHESS = Statut, 
                                   dem_EHESS = Démissionnaire, 
                                   R_EHESS = Classement, 
                                   LC_EHESS = `Position sur liste d’attente`, 
                                   id = `Identifiant candidat`) |> 
    select(-Candidat) |> 
    left_join(BaseCandidat |> select(id, CandidatP), by = "id") |> 
    rename(Candidat = CandidatP)
  
  ENS <- ENS %>% dplyr::rename(Statut_ENS = Statut, 
                               dem_ENS = Démissionnaire, 
                               R_ENS = Classement, 
                               LC_ENS = `Position sur liste d’attente`,
                               id = `Identifiant candidat`)|> 
    select(-Candidat) |> 
    left_join(BaseCandidat |> select(id, CandidatP), by = "id") |> 
    rename(Candidat = CandidatP)
  
  PDI_EHESS <- PDI_EHESS  |> 
    rename(Statut_EHESS = Statut, 
            dem_EHESS = Démissionnaire, 
            R_EHESS = Classement, 
            LC_EHESS = `Position sur liste d’attente`, 
            id = `Identifiant candidat`)
  
  PDI_ENS <- PDI_ENS  |> 
    rename(Statut_ENS = Statut, 
            dem_ENS = Démissionnaire, 
            R_ENS = Classement, 
            LC_ENS = `Position sur liste d’attente`,
            id = `Identifiant candidat`)
  
  
  # suppression de colonnes inutiles 
  EHESS <<- EHESS %>% select(-`Motif de non classement / refus`)
  ENS <<- ENS %>% select( -`Motif de non classement / refus`)
  
  
  # merger les deux tables par identifiant candidat
  tot <- merge(ENS,EHESS, by = "id", all = TRUE)
  tot <- merge(tot,Eval, by = "id", all = FALSE)
  tot <- merge(tot,BaseCandidat, by = "id", all = FALSE)
  
  tot$Candidat <- ifelse(is.na(tot$Candidat.x), tot$Candidat.y, tot$Candidat.x)
  
  PDI <<- PDI_EHESS |> 
    select(id, Statut_EHESS, LC_EHESS) |> 
    left_join(PDI_ENS |> select(id, Statut_ENS, LC_ENS), by = "id") |> 
    mutate(
      Resultat_PDI =  case_when(
        Statut_EHESS == "ACCEPTEE_DEFINITIVEMENT"     | Statut_ENS == "ACCEPTEE_DEFINITIVEMENT"      ~"Admis_Def",
        Statut_EHESS == "ACCEPTEE_NON_DEFINITIVEMENT" | Statut_ENS == "ACCEPTEE_NON_DEFINITIVEMENT"  ~"Admis_Prov",
        Statut_EHESS == "EN_ATTENTE_DE_REPONSE"       | Statut_ENS == "EN_ATTENTE_DE_REPONSE"        ~"AttenteRep",
        Statut_EHESS == "LISTE_ATTENTE"               | Statut_ENS == "LISTE_ATTENTE"                ~"LC",
        Statut_EHESS == "REFUSEE"                     | Statut_ENS == "REFUSEE"                      ~"Refusé",
        Statut_EHESS == "NON_CLASSEE"                 | Statut_ENS == "NON_CLASSEE"                  ~"NC",
        .default = "Non Traité"), 
      
      Etb_PDI = case_when(
        Statut_EHESS == "ACCEPTEE_DEFINITIVEMENT" | Statut_EHESS == "ACCEPTEE_NON_DEFINITIVEMENT" ~"EHESS",
        Statut_ENS == "ACCEPTEE_DEFINITIVEMENT" | Statut_ENS == "ACCEPTEE_NON_DEFINITIVEMENT" ~"ENS"), 
    ) |> 
    relocate(Statut_ENS, .after = Statut_EHESS) |> 
    rename(Statut_EHESS_PDI = Statut_EHESS, 
           Statut_ENS_PDI = Statut_ENS, 
           LC_EHESS_PDI = LC_EHESS, 
           LC_ENS_PDI = LC_ENS)
  
  # suppression des colonnes inutiles
  tot <- tot %>% select(-c(Candidat.x, Candidat.y))
  

  ## Détermination de la variable résultat (meilleur résultat entre EHESS et ENS 
  ## version ultra-simplifiée par rapport à l'original !
    tot <- tot |>
      mutate(
        KeENS =  if_else(is.na(Statut_EHESS), 1, 0),
        KeEHESS = if_else(is.na(Statut_ENS), 1, 0),
        NbCandidatures = ifelse(is.na(Statut_EHESS), 0, 1) + ifelse(is.na(Statut_ENS), 0, 1),
        # NbCandidatures = KeENS + KeEHESS
        
        Resultat =  case_when(
            Statut_EHESS == "ACCEPTEE_DEFINITIVEMENT"     | Statut_ENS == "ACCEPTEE_DEFINITIVEMENT"      ~"Admis_Def",
            Statut_EHESS == "ACCEPTEE_NON_DEFINITIVEMENT" | Statut_ENS == "ACCEPTEE_NON_DEFINITIVEMENT"  ~"Admis_Prov",
            Statut_EHESS == "EN_ATTENTE_DE_REPONSE"       | Statut_ENS == "EN_ATTENTE_DE_REPONSE"        ~ "AttenteRep",
            Statut_EHESS == "LISTE_ATTENTE"               | Statut_ENS == "LISTE_ATTENTE"               ~"LC",
            Statut_EHESS == "REFUSEE"                     | Statut_ENS == "REFUSEE"                     ~"Refusé",
            .default = "Non Traité"), 
        
        St_EHESS = case_when(
          Statut_EHESS == "ACCEPTEE_DEFINITIVEMENT"     ~"Admis_Def",
          Statut_EHESS == "ACCEPTEE_NON_DEFINITIVEMENT" ~"Admis_Prov",
          Statut_EHESS == "EN_ATTENTE_DE_REPONSE"       ~ "AttenteRep",
          Statut_EHESS == "LISTE_ATTENTE"               ~"LC",
          Statut_EHESS == "REFUSEE"                     ~"Refusé",
          is.na(Statut_EHESS) ~ NA_character_,
          .default = "Non Traité"), 
        
        St_ENS = case_when(
          Statut_ENS == "ACCEPTEE_DEFINITIVEMENT"      ~"Admis_Def",
          Statut_ENS == "ACCEPTEE_NON_DEFINITIVEMENT"  ~"Admis_Prov",
          Statut_ENS == "EN_ATTENTE_DE_REPONSE"        ~ "AttenteRep",
          Statut_ENS == "LISTE_ATTENTE"                ~"LC",
          Statut_ENS == "REFUSEE"                      ~"Refusé",
          is.na(Statut_ENS) ~ NA_character_,
          .default = "Non Traité"), 
      
            
        Etb = case_when(
              Statut_EHESS == "ACCEPTEE_DEFINITIVEMENT" | Statut_EHESS == "ACCEPTEE_NON_DEFINITIVEMENT" ~"EHESS",
              Statut_ENS == "ACCEPTEE_DEFINITIVEMENT" | Statut_ENS == "ACCEPTEE_NON_DEFINITIVEMENT" ~"ENS"), 
              
        Genre = factor(ifelse(Civilite == "M.", "Homme", "Femme"), levels = c("Homme", "Femme")), 
        RankCl = cut(Classement, breaks = seq(0,90, by = 10), 
                     labels = c("1-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90")), 
        Age = as.period(interval(dmy(DOB), dmy("01/09/2025")), unit = "year")$year, 
        MentionBac = case_when(
          MentionBac == "Très bien avec les félicitations du jury" ~"TTB",
          MentionBac == "Très bien" ~"TB",
          MentionBac == "Bien" ~"Bien",
          MentionBac == "Assez bien" ~"AB",
          MentionBac == "Sans mention" ~"sans"), 
        MentionBac = fct_relevel(as_factor(MentionBac), "sans", "AB", "Bien", "TB", "TTB")
            ) |> 
      select(-c(`Adresse électronique.x`,`Adresse électronique.y`)) |> 
      relocate(id,Genre, Candidat, Etb, Resultat, St_EHESS, St_ENS, Statut_EHESS, Statut_ENS, R_EHESS, R_ENS, LC_EHESS, LC_ENS, MentionBac) |> 
      arrange(Classement)

  # tot$Etb[tot$Statut_EHESS == "ACCEPTEE_DEFINITIVEMENT" | tot$Statut_EHESS == "ACCEPTEE_NON_DEFINITIVEMENT"] <- "EHESS" 
  # tot$Etb[tot$Statut_ENS == "ACCEPTEE_DEFINITIVEMENT" | tot$Statut_ENS == "ACCEPTEE_NON_DEFINITIVEMENT"] <- "ENS"
  
BaseCandidat |> 
  distinct(MentionBac)
  
  #changer l'ordre des colonnes Candidat en premier, id en second 
  # tot <- tot %>% select(id,Candidat, Etb, Resultat, Statut_EHESS, Statut_ENS, R_EHESS, R_ENS, LC_EHESS, LC_ENS, everything())
  
  #trier par NoteEval
  # tot$NoteEval <- as.numeric(tot$NoteEval)
  
  # tot <- tot[order(tot$NoteEval, decreasing = FALSE),] 
  # Synthese_dtejr <<- tot
  assign(paste0("Synthese_",date), value = tot, envir = .GlobalEnv)
  SyntheseDuJour <<- tot # <<- assigne en global 
  
  if(date == gsub("-", "", Sys.Date()-1)){
    assign("SyntheseVeille", value = tot, envir = .GlobalEnv)
  }
  # table des refusés 
  Refus <- tot[tot$Resultat == "Refusé",]
  Refus <<- Refus %>% select(c(Candidat, NoteEval, Commentaires)) 
  write.xlsx(Refus, paste0("Refus_",date,".xlsx"), rowNames = FALSE)
  # exporter tot en excel 
  write.xlsx(tot, paste0("Synthèse_",date,".xlsx"), rowNames = FALSE)
  
  overview <- table(tot$Resultat)
  tot <<- tot
  return(overview)
}

# importdate("20240604") # pour importer de veilles dates 

# importdate(gsub("-", "", Sys.Date()))   # pour importer les données du jour sans avoir à taper la date 

# Routine pour importer les données de tous les jours depuis le 2 juin 2025 jusqu'à aujourd'hui ----
setwd(MMin)

DateDeb <- as.Date("20250602", format = "%Y%m%d")
while(DateDeb <= Sys.Date()){
  print(DateDeb)
  dtejr <- gsub("-","",DateDeb)
  #print(dtejr)
  importdate(dtejr)
  DateDeb <- DateDeb +1
}


# Création du fichier Empilé (PileData) ----- 
df_list <- mget(ls(pattern = "^Synthese_")) # Crée une liste (très particulière car en fait quasiment une compile des fichiers) de tous les fichiers du global environment dont le nom commence par Synthese_

Synthese_20250603 <- Synthese_20250603 |> mutate(R_EHESS = as.character(R_EHESS), R_ENS = as.character(R_ENS))
PileData <- bind_rows(df_list, .id = "source") # le merge qui marche avec une variable  source qui porte le nom du fichier 
PileData$DateFichier <- as.Date(gsub("Synthese_","",PileData$source), format = "%Y%m%d") # je crée ma variable DateFichier qui est bien une date avec un format date
# names(PileData)
PileData <- PileData |> 
  left_join(EvalComplete |> select(id, PDI), by = "id") |> 
  left_join(PDI |> select(id, Resultat_PDI, Etb_PDI, LC_EHESS_PDI, LC_ENS_PDI), by = "id") |> 
    mutate(
    Synt_PDI = if_else(PDI != "NC" & (Resultat_PDI == "Admis_Def" | Resultat_PDI == "Admis_Prov" | Resultat_PDI == "AttenteRep") , Resultat_PDI, str_glue("LCEH_{LC_EHESS_PDI}/LCEN_{LC_ENS_PDI}"))
    # Synt_PDI = if_else(Resultat_PDI %in% c("Admis_Def", "Admis_Prov", "AttenteRep") & Resultat_PDI != "NC" & !is.na(Resultat_PDI), Resultat_PDI, str_glue("LCEH_{LC_EHESS_PDI}/LCEN_{LC_ENS_PDI}"))
  )

# il faudrait créer synthèse PDI dès la fonctoin pour l'avoir dans les fichier synthses
# il faudrait faire un version abbrégée de Statut_EHESS et ENS avec une nouvelle variable pour éviter les pbs de changement de noms 



## Base Candidat pour Envoi à l'ENS IAMaster -----
IAmaster <- SyntheseDuJour |> 
  filter(Etb == "ENS") |> 
  left_join(read_excel("20250401_08H56_0753742K_EHESS PARIS_330_org.xlsx") |> 
              rename(id = "Numéro de candidat", 
                     INE = "INE/INA/BEA saisi par le candidat", 
                     tel = "Numéro de téléphone principal", 
                     tel2 = "Numéro de téléphone secondaire", 
                     Nationalite = "Nationalité") |>
              select(id, INE, tel, tel2, Nationalite),
            by = "id") |> 
  select(id, INE, Civilite, Nom, Prenom, Nationalite, DOB, mail, tel, tel2,  Resultat) |> 
  write.xlsx("IAmaster.xlsx")

#écrire aux étudiants admis prov et def 
# SyntheseDuJour |>
#   filter(Resultat == "Admis_Def" | Resultat == "Admis_Prov") |>
#   select(Candidat, Resultat, Etb, mail) |>
#   arrange(Candidat) |>
#   select(mail)

# écrire aux étudiants sur liste d'attente 
# SyntheseDuJour |>
#   filter(Resultat == "LC" ) |>
#   select(Candidat, Resultat, Classement, mail) |>
#   arrange(Classement) |>
#   select(mail)



