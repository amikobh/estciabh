#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
# ADOLESCENTE ENCAMINHADOS. Retirados do banco SEM MBA sem adolescentes duplicados
# ordenar nesta ordem para que, quando cortar nome repetidos, preservar data do último ato.
banco_sem_mba <-banco_sem_mba[order(banco_sem_mba$DATA_ATO, decreasing=TRUE),]#ordenar, decrescente, data do ato
banco_sem_mba <-banco_sem_mba[order(banco_sem_mba$NOME2, decreasing=FALSE),]#ordenar, crescente, nome2

#retirar nomes duplicados:snr=sem nome repetido

#df_snr_banco_sem_mba <- banco_sem_mba[!duplicated(data.frame(banco_sem_mba$NOME2, banco_sem_mba$NASCIMENTO)),]
library(dplyr)
df_snr_banco_sem_mba = distinct(banco_sem_mba, NOME2,NASCIMENTO, .keep_all= TRUE)

#banco_para_amostra <-banco[!duplicated(data.frame(banco$NOME2, banco$NASCIMENTO)),]
##write.csv(banco_para_amostra, file ="banco_para_amostra.csv",row.names=TRUE)

total_de_adolescentes_encaminhados <- df_snr_banco_sem_mba
#write.csv(df_snr_banco_sem_mba, file ="df_snr_banco_sem_mba.csv",row.names=TRUE)
#write.xlsx(df_snr_banco_sem_mba, file ="df_snr_banco_sem_mba.xlsx")
#########################################################################################################
#########################################################################################################

# 9 Idade e sexo adolescente atendido (colocar todos acima de 18 nos S/Informação ou #valor!)
df_snr_banco_sem_mba_bkp = df_snr_banco_sem_mba

#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_idade = df_snr_banco_sem_mba %>%
  select(SEXO, IDADE)

table(df_snr_sexo_idade$SEXO)

#########################################################################################################
#########################################################################################################

df_snr_sexo_idade$SEXO <- as.character(df_snr_sexo_idade$SEXO)

df_snr_sexo_idade$SEXO[df_snr_sexo_idade$SEXO == ""]<- "M"
table(df_snr_sexo_idade$SEXO)

df_snr_sexo_idade <- table(df_snr_sexo_idade$IDADE, df_snr_sexo_idade$SEXO, useNA ="always")
#write.csv(df_snr_sexo_idade, file ="df_snr_sexo_idade.csv",row.names=TRUE)
#write.csv(df_snr_sexo_idade, file ="df_snr_sexo_idade.csv",row.names=TRUE)
sum(df_snr_sexo_idade)

df_snr_sexo_idade = data.frame(df_snr_sexo_idade)
#########################################################################################################
#########################################################################################################


df_snr_sexo_idade_bkp = df_snr_sexo_idade


df_snr_sexo_idade

colnames(df_snr_sexo_idade) <- c("IDADE", "SEXO", "QUANTIDADE")

df_snr_sexo_idade

df_snr_sexo_idade$IDADE <- as.character(df_snr_sexo_idade$IDADE)
df_snr_sexo_idade$SEXO <- as.character(df_snr_sexo_idade$SEXO)
sum(df_snr_sexo_idade$QUANTIDADE)


df_snr_sexo_idade = filter(df_snr_sexo_idade, !QUANTIDADE == 0)
df_snr_sexo_idade

#PREENCHER COM NA'S CELULAS VAZIAS
#df_snr_sexo_idade$IDADE[df_snr_sexo_idade$IDADE == "SEM INFORMACAO"]<- "<NA>"
df_snr_sexo_idade$IDADE[which(is.na(df_snr_sexo_idade$IDADE))] <- "S/Informação"
df_snr_sexo_idade


df_snr_sexo_idade$IDADE[df_snr_sexo_idade$IDADE == "S/Informação anos"]<- "S/Informação"

df_snr_sexo_idade <- reshape(data = df_snr_sexo_idade, idvar = "IDADE", timevar = "SEXO", direction = "wide")
df_snr_sexo_idade

colnames(df_snr_sexo_idade) <- c("IDADE", "FEMININO", "MASCULINO")
df_snr_sexo_idade

df_snr_sexo_idade$FEMININO[which(is.na(df_snr_sexo_idade$FEMININO))] <- 0
df_snr_sexo_idade$MASCULINO[which(is.na(df_snr_sexo_idade$MASCULINO))] <- 0

df_snr_sexo_idade





#########################################################################################################
df_snr_sexo_idade2 = df_snr_sexo_idade #salvando para proximo modelo de tabela
#########################################################################################################
df_snr_sexo_idade<- rbind(df_snr_sexo_idade,
                          data.frame(IDADE = "TOTAL",
                                     FEMININO = sum(df_snr_sexo_idade$FEMININO),
                                     MASCULINO = sum(df_snr_sexo_idade$MASCULINO),
                                     stringsAsFactors = FALSE))

df_snr_sexo_idade
#########################################################################################################
#########################################################################################################
#require(ggpubr)
#library(gridExtra)

#df_snr_sexo_idade = ggtexttable(df_snr_sexo_idade, rows = NULL,
 #                               theme = ttheme(
  #                              colnames.style = colnames_style(face = "bold", color = "white", fill = "#bb1e23"),
   #                             tbody.style = tbody_style(color = "black", fill = c("#edece0", "#edece0"))))
#df_snr_sexo_idade
#########################################################################################################
#negrito na linha total
#df_snr_sexo_idade <- table_cell_font(df_snr_sexo_idade, row = 10, column = 1, face = "bold")
#df_snr_sexo_idade <- table_cell_font(df_snr_sexo_idade, row = 10, column = 2, face = "bold")
#df_snr_sexo_idade <- table_cell_font(df_snr_sexo_idade, row = 10, column = 3, face = "bold")
#df_snr_sexo_idade
#########################################################################################################
#salvando tabela
#pdf(file="TABELA_003_df_snr_sexo_idade_geral_alternativa.pdf", width = 3.5, height = 3.2, title = "tabela_df_snr_sexo_idade_geral_alternativa")
##setwd(file.path("~/diretorio_r/estciabh/imagens"))
#svg(filename="TABELA_002_idade_e_sexo.svg", width=5, height=3.5, pointsize=12)
#df_snr_sexo_idade +  labs(title = "TABELA 2: Idade e Sexo, Belo Horizonte, 2021",
 #                         caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD") +
  #theme(plot.title = element_text(hjust = 0.5, vjust = 0, face="bold"),
   #     plot.caption =element_text(hjust = 0.5, vjust = 1)  )

#dev.off()

#########################################################################################################
df_snr_sexo_idade = df_snr_sexo_idade2
#########################################################################################################

df_snr_sexo_idade$FEMININO <- as.numeric(df_snr_sexo_idade$FEMININO)
df_snr_sexo_idade$MASCULINO <- as.numeric(df_snr_sexo_idade$MASCULINO)

#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
df_snr_sexo_idade$F <- round_preserve_sum(prop.table(df_snr_sexo_idade$FEMININO)*100, 2)
df_snr_sexo_idade$M <- round_preserve_sum(prop.table(df_snr_sexo_idade$MASCULINO)*100, 2)
df_snr_sexo_idade
#########################################################################################################
df_snr_sexo_idade <- df_snr_sexo_idade[c("IDADE", "FEMININO", "F", "MASCULINO", "M")]
df_snr_sexo_idade

#########################################################################################################
#########################################################################################################

#script para o bookdown

df_snr_sexo_idade_rmark = df_snr_sexo_idade

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
df_snr_sexo_idade_rmark = df_snr_sexo_idade_rmark %>%
  top_n(3, MASCULINO) %>% arrange(desc(MASCULINO))

#somando
sum(df_snr_sexo_idade_rmark$M)

#para escolher linhas e posicoes
df_snr_sexo_idade_rmark[2,1]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################

df_snr_sexo_idade$IDADE <- paste(df_snr_sexo_idade$IDADE, "anos", sep=" ")

#########################################################################################################
#########################################################################################################

df_snr_sexo_idade<- rbind(df_snr_sexo_idade,
                          data.frame(IDADE = "TOTAL",
                                     FEMININO = sum(df_snr_sexo_idade$FEMININO),
                                     F = sum(df_snr_sexo_idade$F),
                                     MASCULINO = sum(df_snr_sexo_idade$MASCULINO),
                                     M = sum(df_snr_sexo_idade$M),
                                     stringsAsFactors = FALSE))

df_snr_sexo_idade

colnames(df_snr_sexo_idade) <- c("IDADE", "FEM", "%", "MAS", "%")
df_snr_sexo_idade
#########################################################################################################
#require(ggpubr)
#library(gridExtra)
#########################################################################################################
#salvando tabela
#pdf(file="TABELA_003_df_snr_sexo_idade_geral_alternativa2.pdf", width = 3.5, height = 3.2, title = "tabela_df_snr_sexo_idade_geral_alternativa")
#setwd(file.path("~/diretorio_r/estciabh/imagens"))
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
#GRAFICO IDADE/SEXO
#########################################################################################################
#########################################################################################################

library(ggplot2)
library(scales)
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_idade = df_snr_banco_sem_mba %>%
  select(SEXO, IDADE)

table(df_snr_sexo_idade$SEXO)

df_snr_sexo_idade$SEXO <- as.character(df_snr_sexo_idade$SEXO)

df_snr_sexo_idade$SEXO[df_snr_sexo_idade$SEXO == ""]<- "M"
table(df_snr_sexo_idade$SEXO)

df_snr_sexo_idade <- table(df_snr_sexo_idade$IDADE, df_snr_sexo_idade$SEXO, useNA ="always")
##write.csv(df_snr_sexo_idade, file ="df_snr_sexo_idade.csv",row.names=TRUE)
##write.csv(df_snr_sexo_idade, file ="df_snr_sexo_idade.csv",row.names=TRUE)
sum(df_snr_sexo_idade)

df_snr_sexo_idade = data.frame(df_snr_sexo_idade)
#########################################################################################################
#########################################################################################################
#########################################################################################################


df_snr_sexo_idade_bkp = df_snr_sexo_idade


df_snr_sexo_idade

colnames(df_snr_sexo_idade) <- c("IDADE", "SEXO", "QUANTIDADE")

df_snr_sexo_idade

df_snr_sexo_idade$IDADE <- as.character(df_snr_sexo_idade$IDADE)
df_snr_sexo_idade$SEXO <- as.character(df_snr_sexo_idade$SEXO)
sum(df_snr_sexo_idade$QUANTIDADE)


df_snr_sexo_idade = filter(df_snr_sexo_idade, !QUANTIDADE == 0)
df_snr_sexo_idade

#PREENCHER COM NA'S CELULAS VAZIAS
#df_snr_sexo_idade$IDADE[df_snr_sexo_idade$IDADE == "SEM INFORMACAO"]<- "<NA>"
df_snr_sexo_idade$IDADE[which(is.na(df_snr_sexo_idade$IDADE))] <- "s/inf"
df_snr_sexo_idade


df_snr_sexo_idade$IDADE <- paste(df_snr_sexo_idade$IDADE, "anos", sep=" ")
df_snr_sexo_idade$IDADE[df_snr_sexo_idade$IDADE == "s/inf anos"]<- "s/inf"
df_snr_sexo_idade


########################################################################################################
#########################################################################################################
# GRAFICO SEXO PIZZA
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo = df_snr_banco_sem_mba %>%
  select(SEXO)

table(df_snr_sexo$SEXO)

#########################################################################################################
#########################################################################################################

df_snr_sexo$SEXO <- as.character(df_snr_sexo$SEXO)

df_snr_sexo$SEXO[df_snr_sexo$SEXO == ""]<- "M"
table(df_snr_sexo$SEXO)

df_snr_sexo = data.frame(table(df_snr_sexo$SEXO))

colnames(df_snr_sexo) <- c("SEXO", "QUANTIDADE")

sum(df_snr_sexo$QUANTIDADE)

#########################################################################################################
#########################################################################################################


df_snr_sexo$SEXO <- as.character(df_snr_sexo$SEXO)

df_snr_sexo$SEXO[df_snr_sexo$SEXO == "F"]<- "FEMININO"
df_snr_sexo$SEXO[df_snr_sexo$SEXO == "M"]<- "MASCULINO"

df_snr_sexo_original=df_snr_sexo

#########################################################################################################
#funcao para preservar soma de 100 no processamento do round:
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  return(y / up)
}
#########################################################################################################
#usando a funcao criada:
df_snr_sexo

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

df_snr_sexo <- df_snr_sexo %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

df_snr_sexo$PERCENTUAL <- paste(df_snr_sexo$PERCENTUAL, "%", sep=" ")

df_snr_sexo



setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
########################################################################################################
#FIM
##################################################################################################################################################################################################

#########################################################################################################
#########################################################################################################
#grafico linha comparando atendimentos anuais. Obs: acrescentar ano e número de atendimentos(df numero_de_casos_geral
######. Corrigir titulos.

#criando data frame com os dados:
#criando data frame com os dados:
df_atendimento <- data.frame(ANO = c(2015,2016,2017, 2018, 2019, 2020, 2021, 2022, format(Sys.Date()-365*1, "%Y")),
                             ATENDIMENTO = c(8518,8176,8244,7786, 6838, 4672, 3649, 3054, nrow(numero_de_casos_geral)))

df_atendimento1 <- data.frame(ANO = c(2015,2016,2017, 2018, 2019, 2020, 2021, 2022, format(Sys.Date()-365*1, "%Y")),
                              ADOLESCENTES = c(5718, 5456, 4854, 4520, 4022, 2680, 2102, 1848, nrow(total_de_adolescentes_encaminhados)))


df_atendimento = full_join(df_atendimento,df_atendimento1,by="ANO")
df_atendimento

df_atendimento = data.table(df_atendimento)


df_atendimento <- melt(df_atendimento, id = c("ANO"))


colnames(df_atendimento) <- c("ANO", "TIPO", "QUANTIDADE")


df_atendimento$TIPO <- as.character(df_atendimento$TIPO)

df_atendimento$TIPO[df_atendimento$TIPO == "ATENDIMENTO"]<- "TOTAL DE CASOS ATENDIDOS"
df_atendimento$TIPO[df_atendimento$TIPO == "ADOLESCENTES"]<- "ADOLESCENTES ATENDIDOS"

#########################################################################################################


#tabela alternativa
#########################################################################################################
#salvando tabela
#pdf(file="TABELA_001_df_atendimento.pdf", width = 6, height = 3, title = "tabela_df_atendimento_alternativa")
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
