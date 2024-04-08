library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)

db.cirion <- read_excel('STF/ESTRUCTURADO_STF_CIRION_DICIEMBRE_2023.xlsx',
                        sheet = 'Estructurado'
                        )
db.cnt <- read_excel('STF/ESTRUCTURADO_STF_CNT_DICIEMBRE_2023.xlsx',
                        sheet = 'Estructurado'
)
db.conecel <- read_excel('STF/ESTRUCTURADO_STF_CONECEL_DICIEMBRE_2023.xlsx',
                        sheet = 'Estructurado'
)
db.etapa <- read_excel('STF/ESTRUCTURADO_STF_ETAPA_DICIEMBRE_2023.xlsx',
                        sheet = 'Estructurado'
)
db.linkotel <- read_excel('STF/ESTRUCTURADO_STF_LINKOTEL_DICIEMBRE_2023.xlsx',
                        sheet = 'Estructurado'
)
db.setel <- read_excel('STF/ESTRUCTURADO_STF_SETEL_DICIEMBRE_2023.xlsx',
                        sheet = 'Estructurado'
)

class(db.cirion$POPULAR)
db.cirion[which(is.na(db.cirion[, c(11)])), 11] = 0
db.cirion[which(is.na(db.cirion[, c(12)])), 12] = 0
db.cirion[which(is.na(db.cirion[, c(13)])), 13] = 0
db.cirion <- db.cirion |> mutate(total = POPULAR + RESIDENCIAL + COMERCIAL)
db.cirion <- db.cirion |> group_by(`COD DE PARROQUIA`) |>
  summarise(
    total = sum(total)
  )
