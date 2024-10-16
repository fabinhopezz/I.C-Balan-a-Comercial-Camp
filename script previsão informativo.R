# Rotina de previsão dos dados de comércio da RMC e da PMC
library(rio)
library(dplyr)

options(timeout=1000)

# o link abaixo faz o download dos dados de comércio internacional do Brasil, 
# agregado ao nível de município, desde o início da série histórica 
exp <- import("https://balanca.economia.gov.br/balanca/bd/comexstat-bd/mun/EXP_COMPLETA_MUN.zip")
imp <- import("https://balanca.economia.gov.br/balanca/bd/comexstat-bd/mun/IMP_COMPLETA_MUN.zip")

exp_sp <- exp %>%  # exportacões
  mutate(VL_FOB=as.numeric(VL_FOB))%>%
  filter(SG_UF_MUN == "SP") %>%  # filtra para o estado de SP
  select(- SG_UF_MUN, - KG_LIQUIDO) %>%
  mutate(VL_EXP = VL_FOB) %>% 
  mutate(SH4 = as.factor(SH4))

imp_sp <- imp %>%  # importacões
  mutate(VL_FOB=as.numeric(VL_FOB))%>%
  filter(SG_UF_MUN == "SP") %>%  # filtra para o estado de SP
  select(- SG_UF_MUN, - KG_LIQUIDO) %>%
  mutate(VL_IMP = VL_FOB) %>% 
  mutate(SH4 = as.factor(SH4))

layout <- exp_sp %>%  # união das exportacões com importacões
  full_join(imp_sp)   

layout_rmc <- layout %>% # filtro para as cidades da RMC 
  mutate(RMC=ifelse(CO_MUN==3401608|
                      CO_MUN==3403802|
                      CO_MUN==3409502|
                      CO_MUN==3412803|
                      CO_MUN==3415152|
                      CO_MUN==3419055|
                      CO_MUN==3419071|
                      CO_MUN==3420509|
                      CO_MUN==3423404|
                      CO_MUN==3424709|
                      CO_MUN==3431803|
                      CO_MUN==3432009|
                      CO_MUN==3433403|
                      CO_MUN==3436505|
                      CO_MUN==3437107|
                      CO_MUN==3445803|
                      CO_MUN==3448005|
                      CO_MUN==3452403|
                      CO_MUN==3456206|
                      CO_MUN==3456701, 1,0))  

layout_rmc <- layout_rmc %>% 
  filter(RMC == 1) %>%
  group_by(CO_ANO, CO_MES) %>%
  summarise_at(c("VL_EXP","VL_IMP"),sum,na.rm=TRUE) %>%
  arrange(CO_ANO, CO_MES) %>% 
  ungroup()

layout_camp <- layout %>% # filtro para a cidade de Campinas 
  mutate(Campinas = ifelse(CO_MUN == 3409502, 1,0))  

layout_camp <- layout_camp %>%
  filter(Campinas == 1) %>%
  group_by(CO_ANO, CO_MES) %>%
  summarise_at(c("VL_EXP","VL_IMP"),sum,na.rm=TRUE) %>%
  arrange(CO_ANO, CO_MES) %>% 
  ungroup()

# substituir o endereco abaixo pela pasta utilizada na sua máquina para o sync com o github 
setwd("C:/Users/malve/Documents/Observatório PUC Camp/Projects/Bancos-de-dados")

write.csv2(layout_rmc, "base_rmc.csv", row.names = FALSE) # base final RMC
write.csv2(layout_camp, "base_camp.csv", row.names = FALSE) # base final Campinas  





# Previsões Holt-Winters RMC

url <- "https://github.com/Observatorio-Informativos/Bancos-de-dados/raw/main/base_rmc.csv"
baseRMC <- read.csv2(url, sep=";")

ts_exp_RMC <- ts(baseRMC$VL_EXP, start = c(1997, 1), frequency = 12)
ts_imp_RMC <- ts(baseRMC$VL_IMP, start = c(1997, 1), frequency = 12)

ts.plot(ts_exp_RMC, 
        main = "Exportações da RMC entre 1997 e ago/2023", 
        xlab = "Meses", 
        ylab = "Valor exportado")

ts.plot(ts_imp_RMC, 
        main = "Importações da RMC entre 1997 e ago/2023", 
        xlab = "Meses", 
        ylab = "Valor importado")  

# Análise da sazonalidade
# a partir destes boxplots é feita a análise da sazonalidade para o mês nos informativos
boxplot(ts_exp_RMC ~ cycle(ts_exp_RMC), ylab = "Valor (USD FOB), em milhões", xlab = "Meses do ano",
        main = "Sazonalidade mensal das exportações da RMC") # exportacões

boxplot(as.numeric(ts_imp_RMC) ~ cycle(ts_imp_RMC), ylab = "Valor (USD FOB), em milhões", xlab = "Meses do ano",
        main = "Sazonalidade mensal das importações da RMC") # importacões




# EXPORTAÇÕES
#Aplicando o método Holt-Winters
ts_exp_RMC.hw <- HoltWinters(ts_exp_RMC, seasonal= "add") #no último exercício, descobrimos que a série é aditiva

plot(ts_exp_RMC.hw) #Gráfico da linha de previsão

#Previsão até dezembro de 2024
# substituir o número abaixo, n.ahead, pelo número de meses restante para a análise
# se os dados mais recentes são de fevereiro entao são 10 meses restantes ao fim de ano
ts_exp_RMC.hw_predict <- predict(ts_exp_RMC.hw, n.ahead= 4)

ts.plot(ts_exp_RMC, ts_exp_RMC.hw_predict, lty= 1:2) #Gráfico da previsão com o realizado 



# IMPORTAÇÕES
#Aplicando o método Holt-Winters
ts_imp_RMC.hw <- HoltWinters(ts_imp_RMC, seasonal= "add") #no último exercício, descobrimos que a série é aditiva

plot(ts_imp_RMC.hw) #Gráfico da linha de previsão

#Previsão até dezembro de 2024
# substituir o número abaixo, n.ahead, pelo número de meses restante para a análise
# se os dados mais recentes são de fevereiro entao são 10 meses restantes ao fim de ano
ts_imp_RMC.hw_predict <- predict(ts_imp_RMC.hw, n.ahead = 4)

ts.plot(ts_imp_RMC, ts_imp_RMC.hw_predict, lty = 1:2) #Gráfico da previsão com o realizado 




# Valores de previsão para exportação
ts_exp_RMC.hw_predict

export_predict <- sum(ts_exp_RMC.hw_predict) # soma dos valores dos meses previstos

#Somando 
export_meses_passados <- baseRMC %>% filter(CO_ANO == 2024) %>% # soma dos meses passados do mesmo ano
  summarise(VL_EXP = sum(VL_EXP)) 
exp2024_RMC <- export_predict + export_meses_passados # soma dos valores passados com previstos
exp2024_RMC


#Valores de previsão para importacao
ts_imp_RMC.hw_predict

import_predict_RMC <- sum(ts_imp_RMC.hw_predict) # soma dos valores dos meses previstos

#Somando 
import_meses_passados <- baseRMC %>% filter(CO_ANO == 2024) %>% # soma dos meses passados do mesmo ano
  summarise(VL_IMP = sum(VL_IMP)) 
imp2024_RMC <- import_predict_RMC + import_meses_passados # soma dos valores passados com previstos
imp2024_RMC

# Saldo Balança Comercial
BC2024_RMC <- exp2024_RMC - imp2024_RMC
BC2024_RMC


# Taxa de variação 2023 para 2024
exp2023_RMC <- baseRMC %>% # soma dos valores do ano anterior (exportacões)
  filter(CO_ANO == 2023) %>% 
  summarise(VL_EXP = sum(VL_EXP))

imp2023_RMC <- baseRMC %>% # soma dos valores do ano anterior (importacões) 
  filter(CO_ANO == 2023) %>% 
  summarise(VL_IMP = sum(VL_IMP))
exp2023_RMC
imp2023_RMC

# Variacão do ano anterior para o atual (meses passados mais previstos)
VarImport_RMC <- (imp2024_RMC - imp2023_RMC)/imp2023_RMC *100 
VarExport_RMC <- (exp2024_RMC - exp2023_RMC)/exp2023_RMC *100

VarImport_RMC
VarExport_RMC





# Previsões Holt-Winters Campinas
url <- "https://github.com/Observatorio-Informativos/Bancos-de-dados/raw/main/base_camp.csv"
baseCAMP <- read.csv2(url, sep=";")

# baseCAMP <- baseCAMP[-326, ]

ts_exp_CAMP <- ts(baseCAMP$VL_EXP, start = c(1997, 1), frequency = 12)
ts_imp_CAMP <- ts(baseCAMP$VL_IMP, start = c(1997, 1), frequency = 12)

ts.plot(ts_exp_CAMP, 
        main = "Exportações de Campinas entre 1997 e ago/2023", 
        xlab = "Meses", 
        ylab = "Valor exportado")

ts.plot(ts_imp_CAMP, 
        main = "Importações de Campinas entre 1997 e ago/2023", 
        xlab = "Meses", 
        ylab = "Valor importado")  

# Análise da sazonalidade
boxplot(ts_exp_CAMP ~ cycle(ts_exp_CAMP), ylab = "Valor (USD FOB), em milhões", xlab = "Meses do ano",
        main = "Sazonalidade mensal das exportações de Campinas")

boxplot(ts_imp_CAMP ~ cycle(ts_imp_CAMP), ylab = "Valor (USD FOB), em milhões", xlab = "Meses do ano",
        main = "Sazonalidade mensal das importações de Campinas")

# EXPORTAÇÕES
#Aplicando o método Holt-Winters
ts_exp_CAMP.hw <- HoltWinters(ts_exp_CAMP, seasonal= "add") #no último exercício, descobrimos que a série é aditiva

# para conferir a previsão do mês atual, com os dados passados
# ts_exportacao.abr <- ts(head(ts_exportacao, n = length(ts_exportacao) - 1), start = c(1997, 1), frequency = 12)
# ts_exportacao.hw <- HoltWinters(ts_exportacao.abr, seasonal= "add") #no último exercício, descobrimos que a série é aditiva

plot(ts_exp_CAMP.hw) #Gráfico da linha de previsão

#Previsão até dezembro de 2023
ts_exp_CAMP.hw_predict <- predict(ts_exp_CAMP.hw, n.ahead = 4)

ts.plot(ts_exp_CAMP, ts_exp_CAMP.hw_predict, lty= 1:2) #Gráfico da previsão com o realizado 



# IMPORTAÇÕES
#Aplicando o método Holt-Winters
ts_imp_CAMP.hw <- HoltWinters(ts_imp_CAMP, seasonal= "add") #no último exercício, descobrimos que a série é aditiva

plot(ts_imp_CAMP.hw) #Gráfico da linha de previsão

#Previsão até dezembro de 2023
ts_imp_CAMP.hw_predict <- predict(ts_imp_CAMP.hw, n.ahead = 4)

ts.plot(ts_imp_CAMP, ts_imp_CAMP.hw_predict, lty = 1:2) #Gráfico da previsão com o realizado 



# Valores de previsão para exportação
ts_exp_CAMP.hw_predict

export_predict_CAMP <- sum(ts_exp_CAMP.hw_predict)

#Somando 
export_meses_passados_CAMP <- baseCAMP %>% filter(CO_ANO == 2024) %>% 
  summarise(VL_EXP = sum(VL_EXP)) 
exp2024_CAMP <- export_predict_CAMP + export_meses_passados_CAMP
exp2024_CAMP


#Valores de previsão para importacao
ts_imp_CAMP.hw_predict

import_predict_CAMP <- sum(ts_imp_CAMP.hw_predict)

#Somando 
import_meses_passados_CAMP <- baseCAMP %>% filter(CO_ANO == 2024) %>% 
  summarise(VL_IMP = sum(VL_IMP)) 
imp2024_CAMP <- import_predict_CAMP + import_meses_passados_CAMP
imp2024_CAMP

# Saldo Balança Comercial
BC2024_CAMP <- exp2024_CAMP - imp2024_CAMP
BC2024_CAMP


# Taxa de variação 2022 para 2023
exp2023_CAMP <- baseCAMP %>% 
  filter(CO_ANO == 2023) %>% 
  summarise(VL_EXP = sum(VL_EXP))

imp2023_CAMP <- baseCAMP %>% 
  filter(CO_ANO == 2023) %>% 
  summarise(VL_IMP = sum(VL_IMP))
exp2023_CAMP
imp2023_CAMP

VarImport_CAMP <- (imp2024_CAMP - imp2023_CAMP)/imp2023_CAMP *100
VarExport_CAMP <- (exp2024_CAMP - exp2023_CAMP)/exp2023_CAMP *100

VarImport_CAMP
VarExport_CAMP

