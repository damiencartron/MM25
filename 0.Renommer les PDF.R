library(readxl)
library(tidyverse)
# tout est ici : https://thinkr.fr/operations-sur-les-fichiers-et-les-dossiers-sous-r/ 
wdorigin <- getwd()

setwd("C:/Users/dcartron/Downloads/MM25/")

listdossiers<- list.files(recursive = TRUE, full.names = FALSE) |> 
  as_tibble() |> 
  mutate(id = str_sub(value,5,16))


#setwd(wdorigin)

NomFichier <- Cand |> 
  mutate(
    NumAlpha = case_when(
      NumAlpha <10 ~ str_glue("00{NumAlpha}"), 
      NumAlpha < 100 ~str_glue("0{NumAlpha}"), 
      .default =  str_glue("{NumAlpha}")
    ),
  NomFic = str_replace_all(str_glue("{NumAlpha}_{Nom}_{Prenom}_{R1}_{R2}_{id}.pdf"), ' ','--'),
  ) |> 
  select(id,NomFic)


d <- NomFichier |> left_join(listdossiers, by = "id")


file.rename(from = d$value, to = d$NomFic)

# shell("path=%path%;C:\\Program Files\\7-Zip")
shell('"C:\\Program Files\\7-Zip\\7z.exe" a -Tzip _FM.zip *_FM_*.pdf')
shell('"C:\\Program Files\\7-Zip\\7z.exe" a -Tzip _BG.zip *_BG_*.pdf')
shell('"C:\\Program Files\\7-Zip\\7z.exe" a -Tzip _AA.zip *_AA_*.pdf')
shell('"C:\\Program Files\\7-Zip\\7z.exe" a -Tzip _RD.zip *_RD_*.pdf')
shell('"C:\\Program Files\\7-Zip\\7z.exe" a -Tzip _JD.zip *_JD_*.pdf')
shell('"C:\\Program Files\\7-Zip\\7z.exe" a -Tzip _CB.zip *_CB_*.pdf')
shell('"C:\\Program Files\\7-Zip\\7z.exe" a -Tzip _DC.zip *_DC_*.pdf')
shell('"C:\\Program Files\\7-Zip\\7z.exe" a -Tzip _EP.zip *_EP_*.pdf')

table(Cand$R1,Cand$R2)

setwd(wdorigin)

