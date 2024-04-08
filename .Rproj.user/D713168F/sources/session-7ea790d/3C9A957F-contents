library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)

source(here::here('process_stf.R'))

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

# Process and group the operators
db.cirion <- process_stf(db.cirion)
db.cnt <- process_stf(db.cnt)
db.conecel <- process_stf(db.conecel)
db.etapa <- process_stf(db.etapa)
db.linkotel <- process_stf(db.linkotel)
db.setel <- process_stf(db.setel)


