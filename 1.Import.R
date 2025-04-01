library(readxl)
library(tidyverse)
library(descr)

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
    
    )

names(Cand)

table(Cand$MentionBac)
