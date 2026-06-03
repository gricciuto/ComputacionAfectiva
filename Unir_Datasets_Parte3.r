#Esta parte va a unir los dos datasets
library(tidyverse)
library(readxl)
df1 <- read.csv("personas_dataset.csv")
df2 <- read_excel(
    "Práctico Computacion Afectiva.xlsx",
    sheet = 2
)

resultado <- left_join(
  df1,
  df2,
  by = "identificacion"
)

write.csv(
    resultado,
    "datosMerged.csv",
    row.names = FALSE
)