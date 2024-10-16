# Rotina de atualização de dados de comércio internacional regional mensal

# Libraries
library(tidyverse)
library(curl)
library(rlang)

# estabelecendo o diretório de trabalho (conectado com github)
setwd("C:/Users/malve/Documents/Observatório PUC Camp/Projects/Bancos-de-dados")

# Variáveis de seleção 
ano_final = 2024
ano_inicial = 2013

# EXP : ler versão atual e excluir dados do ano corrente (link "raw")
load(url("https://github.com/Observatorio-Informativos/Bancos-de-dados/blob/main/EXP_MUN_COMPLETA_SP.RData?raw=true"))
exp_up <- exp_up %>%
  mutate(CO_ANO = as.numeric(CO_ANO)) %>%
  filter(CO_ANO < ano_final & CO_ANO > ano_inicial & SG_UF_MUN=="SP")

# EXP : ler dados do MDIC (Conferir link atual)
exp_up <- read.csv2("https://balanca.economia.gov.br/balanca/bd/comexstat-bd/mun/EXP_2024_MUN.csv",
                    stringsAsFactors = F)  %>%
  filter(SG_UF_MUN=="SP") %>%
  bind_rows(exp_up)

# IMP : ler versão atual e excluir dados do ano corrente (link "raw")
load(url("https://github.com/Observatorio-Informativos/Bancos-de-dados/blob/main/IMP_MUN_COMPLETA_SP.RData?raw=true"))
imp_up <- imp_up %>%
  mutate(CO_ANO = as.numeric(CO_ANO)) %>%
  filter(CO_ANO < ano_final & CO_ANO > ano_inicial & SG_UF_MUN=="SP")

# IMP : ler dados do MDIC (Conferir link atual)
imp_up <- read.csv2("https://balanca.economia.gov.br/balanca/bd/comexstat-bd/mun/IMP_2024_MUN.csv",
                    stringsAsFactors = F) %>%
  filter(SG_UF_MUN=="SP") %>%
  bind_rows(imp_up)

# Saving updated file to git directory 
save(exp_up, file="EXP_MUN_COMPLETA_SP.RData")
save(imp_up, file="IMP_MUN_COMPLETA_SP.RData")

library(beepr)
beep(1)
