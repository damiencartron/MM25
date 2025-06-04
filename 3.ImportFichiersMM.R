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
  select(NumOrdre, id, ClassTot, NoteEval,  Classement, NoteE1, NoteE2, Commentaires, Eval1, Eval2)
  # select(c('NumOrdre', 'id', 'ClassTot', 'NoteEval',  'Classement', 'NoteE1', 'NoteE2', 'Commentaires', 'Eval1' , 'Eval2'))

## Base Candidat : le fichier monmaster avant évaluation -----

setwd(MMin)
BaseCandidat <- read_excel("20250401_08H56_0753742K_EHESS PARIS_330_org.xlsx") |> 
                  rename(id = `Numéro de candidat`,
                         Nom = 'Nom de naissance',
                         Prenom = 'Prénom',
                         mail = 'Adresse e-mail',
                         Civilite = 'Civilité',
                         DOB = 'Date de naissance', 
                          MentionBac = 'Mention obtenue')

BaseCandidat <- BaseCandidat %>% select(id, Nom, Prenom, mail, Civilite, DOB, MentionBac)

setwd(MMin)

# Ma super fonction -----
importdate <- function(date){
  assign(paste0("ENS_",gsub("-", "", date)), value = read_excel(paste0("Candidatures-EHESS PARIS-1900125MCBW2_",gsub("-", "", date),".xlsx")),envir = .GlobalEnv)
  assign(paste0("EHESS_",gsub("-", "", date)), value = read_excel(paste0("Candidatures-EHESS PARIS-1900125M6PVU_",gsub("-", "", date),".xlsx")),envir = .GlobalEnv)


  #fichiers du jour 
    assign("ENS", value = read_excel(paste0("Candidatures-EHESS PARIS-1900125MCBW2_",gsub("-", "", date),".xlsx")),envir = .GlobalEnv)
    assign("EHESS", value = read_excel(paste0("Candidatures-EHESS PARIS-1900125M6PVU_",gsub("-", "", date),".xlsx")),envir = .GlobalEnv) # l'autre mode d'assignation global eest <<- 

  
  #supprimer dans EHESS les candidats avec statut== non_classee
    EHESS <- EHESS[EHESS$Statut != "NON_CLASSEE",]
    ENS <- ENS[ENS$Statut != "NON_CLASSEE",]
  
  # renommer Statut en Statut_EHESS et Statut en Statut_ENS, démissionnaire en dem_EHESS et en dem_ENS
  
  EHESS <- EHESS %>% dplyr::rename(Statut_EHESS = Statut, 
                                   dem_EHESS = Démissionnaire, 
                                   R_EHESS = Classement, 
                                   LC_EHESS = `Position sur liste d’attente`, 
                                   id = `Identifiant candidat`)
  ENS <- ENS %>% dplyr::rename(Statut_ENS = Statut, 
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
      relocate(id,Genre, Candidat, Etb, Resultat, Statut_EHESS, Statut_ENS, R_EHESS, R_ENS, LC_EHESS, LC_ENS, MentionBac) |> 
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

PileData <- PileData |> 
  left_join(EvalComplete |> select(id, PDI), by = "id")


## Base Candidat pour Envoi à Lionel Arnoux ENS -----
# Arnoux <- read_excel("0753742K_EHESS PARIS1900125MCBW2.xlsx") %>% select(`Numéro de candidat`, `Civilité`, `Nom de naissance`, `Prénom`, `Date de naissance`, `Adresse e-mail`,  `Nationalité`, `INE maître (INES)` )
# Arnoux <- merge(Arnoux, assign("ENSduJour", value = read_excel(paste0("Candidatures-EHESS PARIS-1900125MCBW2-",gsub("-", "", Sys.Date()),".xlsx")),envir = .GlobalEnv),
#                 by.x = "Numéro de candidat", by.y = "Identifiant candidat", all = TRUE) %>%  select(-Candidat)
# 
# Arnoux[dim(Arnoux)[1]+1,1] <- "MasterScDurabilité"
# Arnoux[dim(Arnoux)[1],2] <- "Mme"
# Arnoux[dim(Arnoux)[1],3] <- "CHOISY"
# Arnoux[dim(Arnoux)[1],4] <- "Lisa"
# Arnoux[dim(Arnoux)[1],5] <- "13/05/2000"
# Arnoux[dim(Arnoux)[1],6] <- "lisachoisy@hotmail.fr"
# Arnoux[dim(Arnoux)[1],7] <- "Française"
# Arnoux[dim(Arnoux)[1],8] <- "040011552HF"
# Arnoux[dim(Arnoux)[1],9] <- "ACCEPTEE_DEFINITIVEMENT"
# 
# Arnoux %>% filter(Statut == "ACCEPTEE_DEFINITIVEMENT" | Statut == "ACCEPTEE_NON_DEFINITIVEMENT") %>%  write.xlsx(paste0("Envoi_Lionel-Arnoux_", gsub("-", "", Sys.Date()), ".xlsx"), rowNames = FALSE)

