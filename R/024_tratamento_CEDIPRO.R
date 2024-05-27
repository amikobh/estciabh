#########################################################################################################
#########################################################################################################
# 1 CARREGANDO O banco_cedipro PARA TRATAMENTO NO R: observar se variaveis são iguais
#Ao salvar o banco_cedipro como .csv escolher separador ":"

dir.create(file.path("~/diretorio_r/estciabh", "cedipro"))

setwd(file.path("~/diretorio_r/estciabh"))

banco_cedipro <- read.csv("banco_cedipro_atual.csv",header=TRUE, sep="|", dec=".", encoding = "UTF-8", skip = 3) ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.
banco_cedipro_inicial_bkp <- banco_cedipro



##SALVANDO banco_cedipro ORIGINAL
#write.csv(banco_cedipro_inicial_bkp, file ="banco_cedipro_inicial_bkp.csv",row.names=TRUE)
#write.xlsx(banco_cedipro_inicial_bkp, file ="banco_cedipro_inicial_bkp.xlsx")
#  verificando o aluno das variaveis pertencentes ao objeto dados:
#names(banco_cedipro)


banco_cedipro =
  banco_cedipro %>%
  clean_names() # Limpar os alunos das variáveis


#############################################################################################################
#########################################################################################################
# Para converter um dataframe em tibble:

banco_cedipro <- as_tibble(banco_cedipro)

#str(banco_cedipro)
#head(banco_cedipro, n=10)[,77:84]
#########################################################################################################

# 4 TRATAMENTOS INICIAIS:



#preencher celulas sem alunos com a palavra VAZIO

banco_cedipro$tem_cedipro[banco_cedipro$tem_cedipro == ""]<- "VAZIO"

#formatando datas:


banco_cedipro$nascimento = dmy(banco_cedipro$nascimento)
banco_cedipro$data_ato = dmy(banco_cedipro$data_ato)
banco_cedipro$data_audiencia_preliminar = dmy(banco_cedipro$data_audiencia_preliminar)
banco_cedipro$data_nao_comparecimento = dmy(banco_cedipro$data_nao_comparecimento)
banco_cedipro$data_saida = dmy(banco_cedipro$data_saida)
banco_cedipro$data_sentenca = dmy(banco_cedipro$data_sentenca)
banco_cedipro$data_aplicacao = dmy(banco_cedipro$data_aplicacao)

## calcula o intervalo em anos (idade)
banco_cedipro$idade = as.period(interval(banco_cedipro$nascimento, banco_cedipro$data_ato))

# SEPARAR SO O PRIMEIRO ITEM DE "17y 2m 28d 0H 0M 0S" GERADO PELO SCRIPT ANTERIOR.
banco_cedipro$idade = banco_cedipro$idade@year


#excluir celulas com alunos VAZIO

#banco_cedipro <- banco_cedipro[!(banco_cedipro$tem_cedipro == "VAZIO"),]
#banco_cedipro_sem_na_alunos = banco_cedipro
#write.csv(banco_cedipro_sem_na_alunos, file = "banco_cedipro_sem_na_alunos.csv")
#rm(banco_cedipro_sem_na_alunos)
#############################################################################################################
#banco_curso_adolescente_CEDIPRO encaminhado
#########################################################################################################
#

banco_curso_adolescente_CEDIPRO =
  banco_cedipro %>%
  select(134:138, 154) %>%
  filter(!tipo_de_aluno == "VITIMA")

# Para juntando variáveis numa só:

banco_curso_adolescente_CEDIPRO =
banco_curso_adolescente_CEDIPRO %>%
  pivot_longer(cols = starts_with("curso_ce"), values_to = "CURSO_CEDIPRO") %>%
  #select(-name) %>%
  filter(CURSO_CEDIPRO != "" & CURSO_CEDIPRO != "NSA")


#########################################################################################################
# salvando para gráfico
banco_curso_adolescente_CEDIPRO_bkp = banco_curso_adolescente_CEDIPRO

banco_curso_adolescente_CEDIPRO_bkp =
  banco_curso_adolescente_CEDIPRO_bkp %>%
  janitor::tabyl(CURSO_CEDIPRO) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:

colnames(banco_curso_adolescente_CEDIPRO_bkp)[1]<-'banco_curso_adolescente_CEDIPRO_bkp'
colnames(banco_curso_adolescente_CEDIPRO_bkp)[2]<-'QUANTIDADE'
colnames(banco_curso_adolescente_CEDIPRO_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

banco_curso_adolescente_CEDIPRO =
  banco_curso_adolescente_CEDIPRO %>%
  janitor::tabyl(CURSO_CEDIPRO) %>%
  arrange(desc(n)) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:

colnames(banco_curso_adolescente_CEDIPRO)[1]<-'CURSO'
colnames(banco_curso_adolescente_CEDIPRO)[2]<-'QUANTIDADE'
colnames(banco_curso_adolescente_CEDIPRO)[3]<-'PERCENTUAL'

#############################################################################################################
#banco_curso_adolescente_CEDIPRO FIM
#########################################################################################################
#############################################################################################################
#banco_curso_parente_CEDIPRO encaminhado
#########################################################################################################
#

banco_curso_parente_CEDIPRO =
  banco_cedipro %>%
  select(140:145, 147:152, 154) %>%
  filter(!tipo_de_aluno == "VITIMA")

# Para juntando variáveis numa só:

banco_curso_parente_CEDIPRO =
  banco_curso_parente_CEDIPRO %>%
  pivot_longer(cols = starts_with("curso_ce"), values_to = "CURSO_CEDIPRO") %>%
  #select(-name) %>%
  filter(CURSO_CEDIPRO != "" & CURSO_CEDIPRO != "NSA")


#########################################################################################################

#########################################################################################################
# salvando para gráfico
banco_curso_parente_CEDIPRO_bkp = banco_curso_parente_CEDIPRO

banco_curso_parente_CEDIPRO_bkp =
  banco_curso_parente_CEDIPRO_bkp %>%
  janitor::tabyl(CURSO_CEDIPRO) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:

colnames(banco_curso_parente_CEDIPRO_bkp)[1]<-'banco_curso_parente_CEDIPRO_bkp'
colnames(banco_curso_parente_CEDIPRO_bkp)[2]<-'QUANTIDADE'
colnames(banco_curso_parente_CEDIPRO_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
#########################################################################################################

#script para o bookdown

banco_curso_parente_CEDIPRO_bkp_rmark = banco_curso_parente_CEDIPRO_bkp

banco_curso_parente_CEDIPRO_bkp_rmark = banco_curso_parente_CEDIPRO_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
banco_curso_parente_CEDIPRO_bkp_rmark =
  banco_curso_parente_CEDIPRO_bkp_rmark %>% slice(1:4)

library (stringr)

#replace "%" with "" in the percentual column
banco_curso_parente_CEDIPRO_bkp_rmark$PERCENTUAL <- str_replace (banco_curso_parente_CEDIPRO_bkp_rmark$PERCENTUAL, "%", "")
banco_curso_parente_CEDIPRO_bkp_rmark$PERCENTUAL = as.numeric(banco_curso_parente_CEDIPRO_bkp_rmark$PERCENTUAL)

#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

banco_curso_parente_CEDIPRO =
  banco_curso_parente_CEDIPRO %>%
  janitor::tabyl(CURSO_CEDIPRO) %>%
  arrange(CURSO_CEDIPRO) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:

colnames(banco_curso_parente_CEDIPRO)[1]<-'CURSO'
colnames(banco_curso_parente_CEDIPRO)[2]<-'QUANTIDADE'
colnames(banco_curso_parente_CEDIPRO)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#banco_curso_parente_CEDIPRO FIM
#########################################################################################################
#############################################################################################################
#banco_desistencia_CEDIPRO
#############################################################################################################

banco_desistencia_CEDIPRO =

  banco_cedipro %>%
  filter(!tipo_de_aluno == "VITIMA") %>%
  filter(tem_cedipro == "VAZIO")


#########################################################################################################
# banco_desistencia_CEDIPRO FIM
#########################################################################################################
#############################################################################################################
#banco_matriculados_CEDIPRO
#############################################################################################################

banco_matriculados_CEDIPRO =

  banco_cedipro %>%
  filter(!tipo_de_aluno == "VITIMA") %>%
  filter(!tem_cedipro == "VAZIO")


#########################################################################################################
# banco_matriculados_CEDIPRO FIM
#########################################################################################################
#############################################################################################################
#banco_parente_CEDIPRO
#############################################################################################################

banco_parente_CEDIPRO =

  banco_cedipro %>%
  filter(cedipro_parente_01 == "SIM" | cedipro_parente_02 == "SIM")

banco_parente_CEDIPRO =
  banco_parente_CEDIPRO %>%
  pivot_longer(cols = starts_with("cedipro_parente_0"), values_to = "CEDIPRO_PARENTE") %>%
  #select(-name) %>%
  filter(CEDIPRO_PARENTE != "")

#########################################################################################################
# banco_parente_CEDIPRO FIM
#########################################################################################################

#############################################################################################################
#banco_encaminhamento_CEDIPRO
#############################################################################################################
banco_encaminhamento_CEDIPRO = data.table(rbind(cbind("ADOLESCENTES MATRICULADOS", nrow(banco_matriculados_CEDIPRO)),
                                                cbind("PARENTES MATRICULADOS", nrow(banco_parente_CEDIPRO)),
                                                cbind("ADOLESCENTES DESISTENTES", nrow(banco_desistencia_CEDIPRO) )
))

#############################################################################################################

# Adaptando:

colnames(banco_encaminhamento_CEDIPRO)[1]<-'GRUPO'
colnames(banco_encaminhamento_CEDIPRO)[2]<-'QUANTIDADE'


#############################################################################################################
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
banco_encaminhamento_CEDIPRO$QUANTIDADE = as.numeric(banco_encaminhamento_CEDIPRO$QUANTIDADE)

banco_encaminhamento_CEDIPRO$PERCENTUAL <- round_preserve_sum(prop.table(banco_encaminhamento_CEDIPRO$QUANTIDADE)*100, 2)

#############################################################################################################
banco_encaminhamento_CEDIPRO_bkp = banco_encaminhamento_CEDIPRO #para gráfico de pizza
banco_encaminhamento_CEDIPRO_bkp$PERCENTUAL <- paste(banco_encaminhamento_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
#########################################################################################################
banco_encaminhamento_CEDIPRO <- rbind(banco_encaminhamento_CEDIPRO,
                                 data.frame(GRUPO = "TOTAL",
                                            QUANTIDADE = sum(banco_encaminhamento_CEDIPRO$QUANTIDADE),
                                            PERCENTUAL = sum(banco_encaminhamento_CEDIPRO$PERCENTUAL),
                                            stringsAsFactors = FALSE))
#########################################################################################################
#banco_encaminhamento_CEDIPRO FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_matriculados_CEDIPRO
#########################################################################################################
#sexo_idade_banco_matriculados_CEDIPRO
#########################################################################################################
# ADOLESCENTE ENCAMINHADOS. Retirados do banco SEM MBA sem adolescentes duplicados
# ordenar nesta ordem para que, quando cortar nome repetidos, preservar data do último ato.
banco_matriculados_CEDIPRO <-banco_matriculados_CEDIPRO[order(banco_matriculados_CEDIPRO$data_ato, decreasing=TRUE),]#ordenar, decrescente, data do ato
banco_matriculados_CEDIPRO <-banco_matriculados_CEDIPRO[order(banco_matriculados_CEDIPRO$nome, decreasing=FALSE),]#ordenar, crescente, nome2

#retirar nomes duplicados:snr=sem nome repetido

#df_snr_banco_matriculados_CEDIPRO <- banco_matriculados_CEDIPRO[!duplicated(data.frame(banco_matriculados_CEDIPRO$NOME2, banco_matriculados_CEDIPRO$NASCIMENTO)),]
library(dplyr)
df_snr_banco_matriculados_CEDIPRO = distinct(banco_matriculados_CEDIPRO, nome, nascimento, .keep_all= TRUE)

#banco_para_amostra <-banco[!duplicated(data.frame(banco$NOME2, banco$NASCIMENTO)),]
##write.csv(banco_para_amostra, file ="banco_para_amostra.csv",row.names=TRUE)

#write.csv(df_snr_banco_matriculados_CEDIPRO, file ="df_snr_banco_matriculados_CEDIPRO.csv",row.names=TRUE)
#write.xlsx(df_snr_banco_matriculados_CEDIPRO, file ="df_snr_banco_matriculados_CEDIPRO.xlsx")
#########################################################################################################
#########################################################################################################

# 9 Idade e sexo adolescente atendido (colocar todos acima de 18 nos S/Informação ou #valor!)
df_snr_banco_matriculados_CEDIPRO_bkp = df_snr_banco_matriculados_CEDIPRO

#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_idade_banco_matriculados_CEDIPRO = df_snr_banco_matriculados_CEDIPRO %>%
  select(sexo, idade)

table(df_snr_sexo_idade_banco_matriculados_CEDIPRO$sexo)

#########################################################################################################
#########################################################################################################

df_snr_sexo_idade_banco_matriculados_CEDIPRO$sexo <- as.character(df_snr_sexo_idade_banco_matriculados_CEDIPRO$sexo)

df_snr_sexo_idade_banco_matriculados_CEDIPRO$sexo[df_snr_sexo_idade_banco_matriculados_CEDIPRO$sexo == ""]<- "s/inf"
df_snr_sexo_idade_banco_matriculados_CEDIPRO$idade[df_snr_sexo_idade_banco_matriculados_CEDIPRO$idade == ""]<- "s/inf"

table(df_snr_sexo_idade_banco_matriculados_CEDIPRO$sexo)
table(df_snr_sexo_idade_banco_matriculados_CEDIPRO$idade)

df_snr_sexo_idade_banco_matriculados_CEDIPRO <- table(df_snr_sexo_idade_banco_matriculados_CEDIPRO$idade, df_snr_sexo_idade_banco_matriculados_CEDIPRO$sexo, useNA ="always")
#write.csv(df_snr_sexo_idade_banco_matriculados_CEDIPRO, file ="df_snr_sexo_idade_banco_matriculados_CEDIPRO.csv",row.names=TRUE)
#write.csv(df_snr_sexo_idade_banco_matriculados_CEDIPRO, file ="df_snr_sexo_idade_banco_matriculados_CEDIPRO.csv",row.names=TRUE)
sum(df_snr_sexo_idade_banco_matriculados_CEDIPRO)

df_snr_sexo_idade_banco_matriculados_CEDIPRO = data.frame(df_snr_sexo_idade_banco_matriculados_CEDIPRO)
#########################################################################################################
#########################################################################################################


df_snr_sexo_idade_banco_matriculados_CEDIPRO_bkp = df_snr_sexo_idade_banco_matriculados_CEDIPRO


df_snr_sexo_idade_banco_matriculados_CEDIPRO

colnames(df_snr_sexo_idade_banco_matriculados_CEDIPRO) <- c("idade", "sexo", "QUANTIDADE")

df_snr_sexo_idade_banco_matriculados_CEDIPRO

df_snr_sexo_idade_banco_matriculados_CEDIPRO$idade <- as.character(df_snr_sexo_idade_banco_matriculados_CEDIPRO$idade)
df_snr_sexo_idade_banco_matriculados_CEDIPRO$sexo <- as.character(df_snr_sexo_idade_banco_matriculados_CEDIPRO$sexo)
sum(df_snr_sexo_idade_banco_matriculados_CEDIPRO$QUANTIDADE)


df_snr_sexo_idade_banco_matriculados_CEDIPRO = filter(df_snr_sexo_idade_banco_matriculados_CEDIPRO, !QUANTIDADE == 0)
df_snr_sexo_idade_banco_matriculados_CEDIPRO

#PREENCHER COM NA'S CELULAS VAZIAS
#df_snr_sexo_idade_banco_matriculados_CEDIPRO$idade[df_snr_sexo_idade_banco_matriculados_CEDIPRO$idade == "SEM INFORMACAO"]<- "<NA>"
df_snr_sexo_idade_banco_matriculados_CEDIPRO$idade[which(is.na(df_snr_sexo_idade_banco_matriculados_CEDIPRO$idade))] <- "S/Informação"
df_snr_sexo_idade_banco_matriculados_CEDIPRO


df_snr_sexo_idade_banco_matriculados_CEDIPRO$idade[df_snr_sexo_idade_banco_matriculados_CEDIPRO$idade == "S/Informação anos"]<- "S/Informação"

df_snr_sexo_idade_banco_matriculados_CEDIPRO <- reshape(data = df_snr_sexo_idade_banco_matriculados_CEDIPRO, idvar = "idade", timevar = "sexo", direction = "wide")
df_snr_sexo_idade_banco_matriculados_CEDIPRO

#colnames(df_snr_sexo_idade_banco_matriculados_CEDIPRO) <- c("IDADE", "MASCULINO") #sem feminino
colnames(df_snr_sexo_idade_banco_matriculados_CEDIPRO) <- c("IDADE", "FEMININO", "MASCULINO")
df_snr_sexo_idade_banco_matriculados_CEDIPRO

df_snr_sexo_idade_banco_matriculados_CEDIPRO$FEMININO[which(is.na(df_snr_sexo_idade_banco_matriculados_CEDIPRO$FEMININO))] <- 0
df_snr_sexo_idade_banco_matriculados_CEDIPRO$MASCULINO[which(is.na(df_snr_sexo_idade_banco_matriculados_CEDIPRO$MASCULINO))] <- 0


df_snr_sexo_idade_banco_matriculados_CEDIPRO





#########################################################################################################
df_snr_sexo_idade_banco_matriculados_CEDIPRO2 = df_snr_sexo_idade_banco_matriculados_CEDIPRO #salvando para proximo modelo de tabela
#########################################################################################################
df_snr_sexo_idade_banco_matriculados_CEDIPRO<- rbind(df_snr_sexo_idade_banco_matriculados_CEDIPRO,
                                                     data.frame(IDADE = "TOTAL",
                                                                FEMININO = sum(df_snr_sexo_idade_banco_matriculados_CEDIPRO$FEMININO),
                                                                MASCULINO = sum(df_snr_sexo_idade_banco_matriculados_CEDIPRO$MASCULINO),
                                                                stringsAsFactors = FALSE))

df_snr_sexo_idade_banco_matriculados_CEDIPRO
#########################################################################################################
#########################################################################################################
#require(ggpubr)
#library(gridExtra)

#df_snr_sexo_idade_banco_matriculados_CEDIPRO = ggtexttable(df_snr_sexo_idade_banco_matriculados_CEDIPRO, rows = NULL,
#                               theme = ttheme(
#                              colnames.style = colnames_style(face = "bold", color = "white", fill = "#bb1e23"),
#                             tbody.style = tbody_style(color = "black", fill = c("#edece0", "#edece0"))))
#df_snr_sexo_idade_banco_matriculados_CEDIPRO
#########################################################################################################
#negrito na linha total
#df_snr_sexo_idade_banco_matriculados_CEDIPRO <- table_cell_font(df_snr_sexo_idade_banco_matriculados_CEDIPRO, row = 10, column = 1, face = "bold")
#df_snr_sexo_idade_banco_matriculados_CEDIPRO <- table_cell_font(df_snr_sexo_idade_banco_matriculados_CEDIPRO, row = 10, column = 2, face = "bold")
#df_snr_sexo_idade_banco_matriculados_CEDIPRO <- table_cell_font(df_snr_sexo_idade_banco_matriculados_CEDIPRO, row = 10, column = 3, face = "bold")
#df_snr_sexo_idade_banco_matriculados_CEDIPRO
#########################################################################################################
#salvando tabela
#pdf(file="TABELA_003_df_snr_sexo_idade_banco_matriculados_CEDIPRO_geral_alternativa.pdf", width = 3.5, height = 3.2, title = "tabela_df_snr_sexo_idade_banco_matriculados_CEDIPRO_geral_alternativa")
##setwd(file.path("~/diretorio_r/estciabh/imagens"))
#svg(filename="TABELA_002_idade_e_sexo.svg", width=5, height=3.5, pointsize=12)
#df_snr_sexo_idade_banco_matriculados_CEDIPRO +  labs(title = "TABELA 2: Idade e Sexo, Belo Horizonte, 2021",
#                         caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD") +
#theme(plot.title = element_text(hjust = 0.5, vjust = 0, face="bold"),
#     plot.caption =element_text(hjust = 0.5, vjust = 1)  )

#dev.off()

#########################################################################################################
df_snr_sexo_idade_banco_matriculados_CEDIPRO = df_snr_sexo_idade_banco_matriculados_CEDIPRO2
#########################################################################################################

#df_snr_sexo_idade_banco_matriculados_CEDIPRO$FEMININO <- as.numeric(df_snr_sexo_idade_banco_matriculados_CEDIPRO$FEMININO)
df_snr_sexo_idade_banco_matriculados_CEDIPRO$MASCULINO <- as.numeric(df_snr_sexo_idade_banco_matriculados_CEDIPRO$MASCULINO)

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
df_snr_sexo_idade_banco_matriculados_CEDIPRO$F <- round_preserve_sum(prop.table(df_snr_sexo_idade_banco_matriculados_CEDIPRO$FEMININO)*100, 2)
df_snr_sexo_idade_banco_matriculados_CEDIPRO$M <- round_preserve_sum(prop.table(df_snr_sexo_idade_banco_matriculados_CEDIPRO$MASCULINO)*100, 2)


df_snr_sexo_idade_banco_matriculados_CEDIPRO
#########################################################################################################
df_snr_sexo_idade_banco_matriculados_CEDIPRO <- df_snr_sexo_idade_banco_matriculados_CEDIPRO[c("IDADE", "FEMININO", "F", "MASCULINO", "M")]
#df_snr_sexo_idade_banco_matriculados_CEDIPRO <- df_snr_sexo_idade_banco_matriculados_CEDIPRO[c("IDADE", "MASCULINO", "M")] #sem feminino
df_snr_sexo_idade_banco_matriculados_CEDIPRO

#########################################################################################################
#########################################################################################################

#script para o bookdown

df_snr_sexo_idade_banco_matriculados_CEDIPRO_rmark = df_snr_sexo_idade_banco_matriculados_CEDIPRO

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
df_snr_sexo_idade_banco_matriculados_CEDIPRO_rmark = df_snr_sexo_idade_banco_matriculados_CEDIPRO_rmark %>%
  top_n(3, MASCULINO) %>% arrange(desc(MASCULINO))

#somando
sum(df_snr_sexo_idade_banco_matriculados_CEDIPRO_rmark$M)

#para escolher linhas e posicoes
df_snr_sexo_idade_banco_matriculados_CEDIPRO_rmark[2,1]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################

df_snr_sexo_idade_banco_matriculados_CEDIPRO$IDADE <- paste(df_snr_sexo_idade_banco_matriculados_CEDIPRO$IDADE, "anos", sep=" ")

#########################################################################################################
#########################################################################################################

df_snr_sexo_idade_banco_matriculados_CEDIPRO<- rbind(df_snr_sexo_idade_banco_matriculados_CEDIPRO,
                                                     data.frame(IDADE = "TOTAL",
                                                                FEMININO = sum(df_snr_sexo_idade_banco_matriculados_CEDIPRO$FEMININO),
                                                                F = sum(df_snr_sexo_idade_banco_matriculados_CEDIPRO$F),
                                                                MASCULINO = sum(df_snr_sexo_idade_banco_matriculados_CEDIPRO$MASCULINO),
                                                                M = sum(df_snr_sexo_idade_banco_matriculados_CEDIPRO$M),
                                                                stringsAsFactors = FALSE))

df_snr_sexo_idade_banco_matriculados_CEDIPRO

colnames(df_snr_sexo_idade_banco_matriculados_CEDIPRO) <- c("IDADE", "FEM", "%", "MAS", "%")
#colnames(df_snr_sexo_idade_banco_matriculados_CEDIPRO) <- c("IDADE", "MAS", "%")
df_snr_sexo_idade_banco_matriculados_CEDIPRO
#########################################################################################################
#require(ggpubr)
#library(gridExtra)
#########################################################################################################
#salvando tabela
#pdf(file="TABELA_003_df_snr_sexo_idade_banco_matriculados_CEDIPRO_geral_alternativa2.pdf", width = 3.5, height = 3.2, title = "tabela_df_snr_sexo_idade_banco_matriculados_CEDIPRO_geral_alternativa")
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

df_snr_sexo_idade_banco_matriculados_CEDIPRO = df_snr_banco_matriculados_CEDIPRO %>%
  select(sexo, idade)

table(df_snr_sexo_idade_banco_matriculados_CEDIPRO$sexo)

df_snr_sexo_idade_banco_matriculados_CEDIPRO$sexo <- as.character(df_snr_sexo_idade_banco_matriculados_CEDIPRO$sexo)

df_snr_sexo_idade_banco_matriculados_CEDIPRO$sexo[df_snr_sexo_idade_banco_matriculados_CEDIPRO$sexo == ""]<- "M"
table(df_snr_sexo_idade_banco_matriculados_CEDIPRO$sexo)

df_snr_sexo_idade_banco_matriculados_CEDIPRO <- table(df_snr_sexo_idade_banco_matriculados_CEDIPRO$idade, df_snr_sexo_idade_banco_matriculados_CEDIPRO$sexo, useNA ="always")
##write.csv(df_snr_sexo_idade_banco_matriculados_CEDIPRO, file ="df_snr_sexo_idade_banco_matriculados_CEDIPRO.csv",row.names=TRUE)
##write.csv(df_snr_sexo_idade_banco_matriculados_CEDIPRO, file ="df_snr_sexo_idade_banco_matriculados_CEDIPRO.csv",row.names=TRUE)
sum(df_snr_sexo_idade_banco_matriculados_CEDIPRO)

df_snr_sexo_idade_banco_matriculados_CEDIPRO = data.frame(df_snr_sexo_idade_banco_matriculados_CEDIPRO)
#########################################################################################################
#########################################################################################################
#########################################################################################################


df_snr_sexo_idade_banco_matriculados_CEDIPRO_bkp = df_snr_sexo_idade_banco_matriculados_CEDIPRO


df_snr_sexo_idade_banco_matriculados_CEDIPRO

colnames(df_snr_sexo_idade_banco_matriculados_CEDIPRO) <- c("idade", "sexo", "QUANTIDADE")

df_snr_sexo_idade_banco_matriculados_CEDIPRO

df_snr_sexo_idade_banco_matriculados_CEDIPRO$idade <- as.character(df_snr_sexo_idade_banco_matriculados_CEDIPRO$idade)
df_snr_sexo_idade_banco_matriculados_CEDIPRO$sexo <- as.character(df_snr_sexo_idade_banco_matriculados_CEDIPRO$sexo)
sum(df_snr_sexo_idade_banco_matriculados_CEDIPRO$QUANTIDADE)


df_snr_sexo_idade_banco_matriculados_CEDIPRO = filter(df_snr_sexo_idade_banco_matriculados_CEDIPRO, !QUANTIDADE == 0)
df_snr_sexo_idade_banco_matriculados_CEDIPRO

#PREENCHER COM NA'S CELULAS VAZIAS
#df_snr_sexo_idade_banco_matriculados_CEDIPRO$idade[df_snr_sexo_idade_banco_matriculados_CEDIPRO$idade == "SEM INFORMACAO"]<- "<NA>"
df_snr_sexo_idade_banco_matriculados_CEDIPRO$idade[which(is.na(df_snr_sexo_idade_banco_matriculados_CEDIPRO$idade))] <- "s/inf"
df_snr_sexo_idade_banco_matriculados_CEDIPRO


df_snr_sexo_idade_banco_matriculados_CEDIPRO$idade <- paste(df_snr_sexo_idade_banco_matriculados_CEDIPRO$idade, "anos", sep=" ")
df_snr_sexo_idade_banco_matriculados_CEDIPRO$idade[df_snr_sexo_idade_banco_matriculados_CEDIPRO$idade == "s/inf anos"]<- "s/inf"
df_snr_sexo_idade_banco_matriculados_CEDIPRO

########################################################################################################
#########################################################################################################
# GRAFICO SEXO PIZZA
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_pizza_banco_matriculados_CEDIPRO = df_snr_banco_matriculados_CEDIPRO %>%
  select(sexo)

table(df_snr_sexo_pizza_banco_matriculados_CEDIPRO$sexo)

#########################################################################################################
#########################################################################################################

df_snr_sexo_pizza_banco_matriculados_CEDIPRO$sexo <- as.character(df_snr_sexo_pizza_banco_matriculados_CEDIPRO$sexo)

df_snr_sexo_pizza_banco_matriculados_CEDIPRO$sexo[df_snr_sexo_pizza_banco_matriculados_CEDIPRO$sexo == ""]<- "M"
table(df_snr_sexo_pizza_banco_matriculados_CEDIPRO$sexo)

df_snr_sexo_pizza_banco_matriculados_CEDIPRO = data.frame(table(df_snr_sexo_pizza_banco_matriculados_CEDIPRO$sexo))

colnames(df_snr_sexo_pizza_banco_matriculados_CEDIPRO) <- c("sexo", "QUANTIDADE")

sum(df_snr_sexo_pizza_banco_matriculados_CEDIPRO$QUANTIDADE)

#########################################################################################################
#########################################################################################################


df_snr_sexo_pizza_banco_matriculados_CEDIPRO$sexo <- as.character(df_snr_sexo_pizza_banco_matriculados_CEDIPRO$sexo)

df_snr_sexo_pizza_banco_matriculados_CEDIPRO$sexo[df_snr_sexo_pizza_banco_matriculados_CEDIPRO$sexo == "F"]<- "FEMININO"
df_snr_sexo_pizza_banco_matriculados_CEDIPRO$sexo[df_snr_sexo_pizza_banco_matriculados_CEDIPRO$sexo == "M"]<- "MASCULINO"

df_snr_sexo_pizza_banco_matriculados_CEDIPRO_original=df_snr_sexo_pizza_banco_matriculados_CEDIPRO

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
df_snr_sexo_pizza_banco_matriculados_CEDIPRO

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

df_snr_sexo_pizza_banco_matriculados_CEDIPRO <- df_snr_sexo_pizza_banco_matriculados_CEDIPRO %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

df_snr_sexo_pizza_banco_matriculados_CEDIPRO$PERCENTUAL <- paste(df_snr_sexo_pizza_banco_matriculados_CEDIPRO$PERCENTUAL, "%", sep=" ")

df_snr_sexo_pizza_banco_matriculados_CEDIPRO



setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
########################################################################################################
##sexo_idade_banco_matriculados_CEDIPRO FIM
####################################################################################################################




#############################################################################################################
#ESCOLARIDADE_banco_matriculados_CEDIPRO
#########################################################################################################

ESCOLARIDADE_banco_matriculados_CEDIPRO =
  banco_matriculados_CEDIPRO |>
  select(escolaridade_termo)

#adaptando para o restante dos scripts
colnames(ESCOLARIDADE_banco_matriculados_CEDIPRO)[1]<-'ESCOLARIDADE_banco_matriculados_CEDIPRO'

ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO = ajustar_nomes(ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO)
#########################################################################################################
#AJUSTA OS FORA DE PADRÃO AQUI:
ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO == "EJA"]<- "EJAENSFUND" #FIZ OPÇÃO PELO FUND
ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO == ""]<- "SEMINFORMACAO" #FIZ OPÇÃO PELO FUND
ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO == "NSA"]<- "NAORESPONDEU" #FIZ OPÇÃO PELO FUND
#ORDENANDO

ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO == "1ªSERIE-ENSFUND"]<- "A1ªSERIE-ENSFUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO == "2ªSERIE-ENSFUND"]<- "B2ªSERIE-ENSFUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO == "3ªSERIE-ENSFUND"]<- "C3ªSERIE-ENSFUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO == "4ªSERIE-ENSFUND"]<- "D4ªSERIE-ENSFUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO == "5ªSERIE-ENSFUND"]<- "E5ªSERIE-ENSFUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO == "6ªSERIE-ENSFUND"]<- "F6ªSERIE-ENSFUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO == "7ªSERIE-ENSFUND"]<- "G7ªSERIE-ENSFUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO == "8ªSERIE-ENSFUND"]<- "H8ªSERIE-ENSFUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO == "9ªSERIE-ENSFUND"]<- "I9ªSERIE-ENSFUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO == "1ºANO-ENSMEDIO"]<- "J1ºANO-ENSMEDIO"
ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO == "2ºANO-ENSMEDIO"]<- "K2ºANO-ENSMEDIO"
ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO == "3ºANO-ENSMEDIO"]<- "L3ºANO-ENSMEDIO"
ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO == "FACULDADE1ºPERIODO"]<- "LAFACULDADE1ºPERIODO"
ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO == "EJAENSFUND"]<- "MEJAENSFUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO == "EJAENSMEDIO"]<- "NEJAENSMEDIO"
ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO == "NAOSABE"]<- "ONAOSABE"
ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO == "NAORESPONDEU"]<- "PNAORESPONDEU"
ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO == "SEMINFORMACAO"]<- "QSEMINFORMACAO"

#########################################################################################################
# salvando para gráfico
ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp = ESCOLARIDADE_banco_matriculados_CEDIPRO

ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp =
  ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp %>%
  janitor::tabyl(ESCOLARIDADE_banco_matriculados_CEDIPRO) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO == "A1ªSERIE-ENSFUND"]<- "1º ANO - ENS FUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO == "B2ªSERIE-ENSFUND"]<- "2º ANO - ENS FUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO == "C3ªSERIE-ENSFUND"]<- "3º ANO - ENS FUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO == "D4ªSERIE-ENSFUND"]<- "4º ANO - ENS FUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO == "E5ªSERIE-ENSFUND"]<- "5º ANO - ENS FUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO == "F6ªSERIE-ENSFUND"]<- "6º ANO - ENS FUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO == "G7ªSERIE-ENSFUND"]<- "7º ANO - ENS FUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO == "H8ªSERIE-ENSFUND"]<- "8º ANO - ENS FUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO == "I9ªSERIE-ENSFUND"]<- "9º ANO - ENS FUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO == "J1ºANO-ENSMEDIO"]<- "1º ANO - ENS MÉDIO"
ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO == "K2ºANO-ENSMEDIO"]<- "2º ANO - ENS MÉDIO"
ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO == "L3ºANO-ENSMEDIO"]<- "3º ANO - ENS MÉDIO"
ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO == "LAFACULDADE1ºPERIODO"]<- "FACULDADE - 1º PERÍODO"
ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO == "MEJAENSFUND"]<- "EJA - ENS FUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO == "NEJAENSMEDIO"]<- "EJA - ENS MÉDIO"
ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO == "ONAOSABE"]<- "NÃO SABE"
ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO == "PNAORESPONDEU"]<- "NÃO RESPONDEU"
ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$ESCOLARIDADE_banco_matriculados_CEDIPRO == "QSEMINFORMACAO"]<- "SEM INFORMAÇÃO"


#########################################################################################################
#replace "%" with "" in the percentual column
ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$PERCENTUAL2 <- str_replace (ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$percent, "%", "")
ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$PERCENTUAL2 = as.numeric(ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp)[1]<-'ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp'
colnames(ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp)[2]<-'QUANTIDADE'
colnames(ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#script para o bookdown

ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp_rmark = ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp

ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp_rmark = ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp_rmark =
  ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp_rmark %>% slice(1:4)

library (stringr)


#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO == "VNÃO SABE"]<- "UNÃO SABE"
#ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO$ESCOLARIDADE_banco_matriculados_CEDIPRO == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA =
  ESCOLARIDADE_banco_matriculados_CEDIPRO %>%
  janitor::tabyl(ESCOLARIDADE_banco_matriculados_CEDIPRO) %>%
  arrange(ESCOLARIDADE_banco_matriculados_CEDIPRO) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
#ordenando:

ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO == "A1ªSERIE-ENSFUND"]<- "1º ANO - ENS FUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO == "B2ªSERIE-ENSFUND"]<- "2º ANO - ENS FUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO == "C3ªSERIE-ENSFUND"]<- "3º ANO - ENS FUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO == "D4ªSERIE-ENSFUND"]<- "4º ANO - ENS FUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO == "E5ªSERIE-ENSFUND"]<- "5º ANO - ENS FUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO == "F6ªSERIE-ENSFUND"]<- "6º ANO - ENS FUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO == "G7ªSERIE-ENSFUND"]<- "7º ANO - ENS FUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO == "H8ªSERIE-ENSFUND"]<- "8º ANO - ENS FUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO == "I9ªSERIE-ENSFUND"]<- "9º ANO - ENS FUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO == "J1ºANO-ENSMEDIO"]<- "1º ANO - ENS MÉDIO"
ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO == "K2ºANO-ENSMEDIO"]<- "2º ANO - ENS MÉDIO"
ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO == "L3ºANO-ENSMEDIO"]<- "3º ANO - ENS MÉDIO"
ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO == "LAFACULDADE1ºPERIODO"]<- "FACULDADE - 1º PERÍODO"
ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO == "MEJAENSFUND"]<- "EJA - ENS FUND"
ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO == "NEJAENSMEDIO"]<- "EJA - ENS MÉDIO"
ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO == "ONAOSABE"]<- "NÃO SABE"
ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO == "PNAORESPONDEU"]<- "NÃO RESPONDEU"
ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO[ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA$ESCOLARIDADE_banco_matriculados_CEDIPRO == "QSEMINFORMACAO"]<- "SEM INFORMAÇÃO"

#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA)[1]<-'ESCOLARIDADE'
colnames(ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA)[2]<-'QUANTIDADE'
colnames(ESCOLARIDADE_banco_matriculados_CEDIPRO_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#ESCOLARIDADE_banco_matriculados_CEDIPRO FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO INCIDENCIA_banco_matriculados_CEDIPRO:
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
INCIDENCIA_banco_matriculados_CEDIPRO = banco_matriculados_CEDIPRO
#########################################################################################################

#preenchendo vazios

INCIDENCIA_banco_matriculados_CEDIPRO$ato_infracional_ata_01 = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ato_infracional_ata_01 == "",
                                                                      INCIDENCIA_banco_matriculados_CEDIPRO$ato_infracional_termo_01, INCIDENCIA_banco_matriculados_CEDIPRO$ato_infracional_ata_01)


INCIDENCIA_banco_matriculados_CEDIPRO$ato_infracional_ata_02 = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ato_infracional_ata_02 == "",
                                                                      INCIDENCIA_banco_matriculados_CEDIPRO$ato_infracional_termo_02, INCIDENCIA_banco_matriculados_CEDIPRO$ato_infracional_ata_02)


INCIDENCIA_banco_matriculados_CEDIPRO$ato_infracional_ata_01 = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ato_infracional_ata_01 == "",
                                                                      "SEM INFORMAÇÃO", INCIDENCIA_banco_matriculados_CEDIPRO$ato_infracional_ata_01)

INCIDENCIA_banco_matriculados_CEDIPRO$ato_infracional_ata_02 = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ato_infracional_ata_02 == "",
                                                                      "NSA", INCIDENCIA_banco_matriculados_CEDIPRO$ato_infracional_ata_02)


INCIDENCIA_banco_matriculados_CEDIPRO$ato_infracional_ata_03 = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ato_infracional_ata_03 == "",
                                                                      "NSA", INCIDENCIA_banco_matriculados_CEDIPRO$ato_infracional_ata_03)


#########################################################################################################
#########################################################################################################


#DESMEMBRANDO PARA QUE NÃO FIQUE MAIS DE UM ATO NA MESMA LINHA. TODOS INDO PARA NOVA COLUNA ATO_INFRACIONAL.
INCIDENCIA_banco_matriculados_CEDIPRO =

  INCIDENCIA_banco_matriculados_CEDIPRO %>%
  pivot_longer(cols = starts_with("ato_infracional_ata_0"), values_to = "ATO_INFRACIONAL") %>%
  #select(-name) %>%
  filter(ATO_INFRACIONAL != "NSA" & ATO_INFRACIONAL != "MBA")

#########################################################################################################
INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL <- gsub(" ","", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)
#########################################################################################################
#############################################################################################################
#########################################################################################################
#AMEAÇA

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "147.ARTCPB",
                                                               "AMEAÇA", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


########################################################################################################
#########################################################################################################
#CRIME DE TRÂNSITO

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "309.ARTCTB",
                                                               "CRIME DE TRÂNSITO (DIRIGIR SEM PERMISSÃO/HABILITAÇÃO)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "310.ARTCTB",
                                                               "CRIME DE TRÂNSITO (ENTREGAR DIREÇÃO A NÃO HABILITADO)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "311.ARTCTB",
                                                               "CRIME DE TRÂNSITO (VELOCIDADE INCOMPATÍVEL)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)





#para discrinar: é so anular com #

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "CRIME DE TRÂNSITO (DIRIGIR SEM PERMISSÃO/HABILITAÇÃO)",
                                                               "CRIME DE TRÂNSITO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "CRIME DE TRÂNSITO (ENTREGAR DIREÇÃO A NÃO HABILITADO)",
                                                               "CRIME DE TRÂNSITO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "CRIME DE TRÂNSITO (VELOCIDADE INCOMPATÍVEL)",
                                                               "CRIME DE TRÂNSITO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)
#########################################################################################################
#########################################################################################################
#DANO

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "163.ARTCPB",
                                                               "DANO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#ESTUPRO

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "213.ARTCPB",
                                                               "ESTUPRO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#ESTUPRO

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "215.ARTCPB",
                                                               "VIOLAÇÃO SEXUAL MEDIANTE FRAUDE", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#ESTUPRO

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "215-A.ARTCPB",
                                                               "IMPORTUNAÇÃO SEXUAL", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#ESTUPRO DE VULNERÁVEL

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "217-A.ARTCPB",
                                                               "ESTUPRO DE VULNERÁVEL", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#FURTO

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "155.ARTCPB",
                                                               "FURTO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#FURTO (TENTATIVA)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "155C/C14.ARTCPB",
                                                               "FURTO (TENTATIVA)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#HOMICÍDIO

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "121.ARTCPB",
                                                               "HOMICÍDIO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#HOMICÍDIO (TENTATIVA)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "121C/C14,II.ARTCPB",
                                                               "HOMICÍDIO (TENTATIVA)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#LESÃO CORPORAL

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "129.ARTCPB",
                                                               "LESÃO CORPORAL", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "129§3º.ARTCPB",
                                                               "LESÃO CORPORAL", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "129§9º.ARTCPB",
                                                               "LESÃO CORPORAL", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#LESÃO CORPORAL (TENTATIVA). Ordem para trocar LESÃO CORPORAL (TENTATIVA) por VIAS DE FATO

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "129C/C14,II.ARTCPB",
                                                               "VIAS DE FATO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#PICHAÇÃO

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "65.ART9.605",
                                                               "PICHAÇÃO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#PORTE/POSSE DE ARMA

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "12.ART10.826",
                                                               "ARMA DE FOGO - POSSE IRREGULAR (USO PERMITIDO)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "14.ART10.826",
                                                               "ARMA DE FOGO - PORTE ILEGAL (USO PERMITIDO)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "16.ART10.826",
                                                               "ARMA DE FOGO - POSSE/PORTE ILEGAL (USO RESTRITO)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "15.ART10.826",
                                                               "ARMA DE FOGO - PORTE ILEGAL (DISPARO)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "17.ART10.826",
                                                               "ARMA DE FOGO - POSSE/PORTE ILEGAL (COMÉRCIO ILEGAL)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)



INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "ARMA DE FOGO - POSSE IRREGULAR (USO PERMITIDO)",
                                                               "PORTE/POSSE DE ARMA", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "ARMA DE FOGO - PORTE ILEGAL (USO PERMITIDO)",
                                                               "PORTE/POSSE DE ARMA", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "ARMA DE FOGO - POSSE/PORTE ILEGAL (USO RESTRITO)",
                                                               "PORTE/POSSE DE ARMA", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "ARMA DE FOGO - PORTE ILEGAL (DISPARO)",
                                                               "PORTE/POSSE DE ARMA", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "ARMA DE FOGO - POSSE/PORTE ILEGAL (COMÉRCIO ILEGAL)",
                                                               "PORTE/POSSE DE ARMA", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#RECEPTAÇÃO

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "180.ARTCPB",
                                                               "RECEPTAÇÃO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)
#########################################################################################################
#########################################################################################################
#ROUBO

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "157.ARTCPB",
                                                               "ROUBO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "157§2ºAICPB",
                                                               "ROUBO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "157§2ºAIIARTCPB",
                                                               "ROUBO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "157§2º,I,IIeVARTCPB",
                                                               "ROUBO (EM CONCURSO DE PESSOAS)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "157§2º,I.ARTCPB",
                                                               "ROUBO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "157§2º,IeII.ARTCPB",
                                                               "ROUBO (EM CONCURSO DE PESSOAS)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "157§2ºA,IARTCPB",
                                                               "ROUBO (EMPREGO DE ARMA)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "157§2ºIIARTCPB",
                                                               "ROUBO (EM CONCURSO DE PESSOAS)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "157§2ºIIeVARTCPB",
                                                               "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "157§2ºA,I",
                                                               "ROUBO (EMPREGO DE ARMA)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "157§2ºAIARTCPB",
                                                               "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "157§2ºAIeIIARTCPB",
                                                               "ROUBO (EMPREGO DE ARMA)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "157§2ºII,VeVIIARTCPB",
                                                               "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "157§2ºIIeVIARTCPB",
                                                               "ROUBO (EMPREGO DE ARMA)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)



INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "157§2ºIIeVIIARTCPB",
                                                               "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "157§2ºIIEVIIARTCPB",
                                                               "ROUBO (EMPREGO DE ARMA)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "157§2ºVARTCPB",
                                                               "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "157§2ºVIIARTCPB",
                                                               "ROUBO (EMPREGO DE ARMA)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "157§2ºAIICPB",
                                                               "ROUBO (EMPREGO DE ARMA)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


#para discrinar: é so anular com #

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "ROUBO",
                                                               "ROUBO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "ROUBO (EM CONCURSO DE PESSOAS)",
                                                               "ROUBO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "ROUBO",
                                                               "ROUBO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "ROUBO (EM CONCURSO DE PESSOAS)",
                                                               "ROUBO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "ROUBO (EMPREGO DE ARMA)",
                                                               "ROUBO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "ROUBO (EM CONCURSO DE PESSOAS)",
                                                               "ROUBO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)",
                                                               "ROUBO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "ROUBO (EMPREGO DE ARMA)",
                                                               "ROUBO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)



#########################################################################################################
#ROUBO (TENTATIVA)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "157§2º,IeIIC/C14,II.ARTCPB",
                                                               "ROUBO (TENTATIVA)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "157C/C14,II.ARTCPB",
                                                               "ROUBO (TENTATIVA)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#ROUBO (§3º)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "157§3ºARTCPB",
                                                               "ROUBO (§3º)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "ROUBO (§3º)",
                                                               "LATROCÍNIO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)
#########################################################################################################
#########################################################################################################
#ROUBO (§3º) (TENTATIVA)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "157§3ºARTCPBC/C14,II,CPB",
                                                               "ROUBO (§3º) (TENTATIVA)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#SEQUESTRO

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "148.ARTCPB",
                                                               "SEQUESTRO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#TRÁFICO DE DROGAS


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "33.ART11.343",
                                                               "TRÁFICO DE DROGAS", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


#INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "35.ART11.343",
# "TRÁFICO DE DROGAS", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "37.ART11.343",
                                                               "TRÁFICO DE DROGAS (INFORMANTE)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)



#para discrinar: é so anular com #

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "TRÁFICO DE DROGAS",
                                                               "TRÁFICO DE DROGAS", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


#INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "35.ART11.343",
# "TRÁFICO DE DROGAS", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "TRÁFICO DE DROGAS (INFORMANTE)",
                                                               "TRÁFICO DE DROGAS", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#########################################################################################################
#ASSOCIAÇÃO TRÁFICO DE DROGAS


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "35.ART11.343",
                                                               "TRÁFICO DE DROGAS (ASSOCIAÇÃO)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "34.ART11.343",
                                                               "TRÁFICO DE DROGAS (ASSOCIAÇÃO)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

#para discrinar: é so anular com #

#INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "TRÁFICO DE DROGAS (ASSOCIAÇÃO)",
#                                           "TRÁFICO DE DROGAS", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#USO DE DROGAS

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "28.ART11.343",
                                                               "POSSE DE DROGAS PARA USO PESSOAL", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)



#########################################################################################################
#########################################################################################################
#VIAS DE FATO

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "21.ARTLCP",
                                                               "VIAS DE FATO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


#########################################################################################################
#OUTROS

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "140.ARTCPB",
                                                               "INJÚRIA", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "140§3º.ARTCPB",
                                                               "INJÚRIA", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)



INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "330.ARTCPB",
                                                               "DESOBEDIÊNCIA", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "331.ARTCPB",
                                                               "DESACATO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "139.ARTCPB",
                                                               "DESOBEDIÊNCIA", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "2.ART13.185",
                                                               "INTIMIDAÇÃO SISTEMÁTICA (BULLYING)", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "329.ARTCPB",
                                                               "RESISTÊNCIA", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "311.ARTCPB",
                                                               "ADULTERAÇÃO DE SINAL IDENTIFICADOR DE VEÍCULO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#ajustando termos
#########################################################################################################
INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "SEMINFORMAÇÃO",
                                                               "SEM INFORMAÇÃO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "TERMOSEMINF.",
                                                               "SEM INFORMAÇÃO", INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#SUBSTITUIR
INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL[INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "SEM INFORMAÇÃO"]<- "ZSEM INFORMAÇÃO"
#########################################################################################################
#########################################################################################################
#########################################################################################################
# salvando para gráfico
INCIDENCIA_banco_matriculados_CEDIPRO_bkp = INCIDENCIA_banco_matriculados_CEDIPRO

INCIDENCIA_banco_matriculados_CEDIPRO_bkp =
  INCIDENCIA_banco_matriculados_CEDIPRO_bkp %>%
  janitor::tabyl(ATO_INFRACIONAL) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR
INCIDENCIA_banco_matriculados_CEDIPRO_bkp$ATO_INFRACIONAL[INCIDENCIA_banco_matriculados_CEDIPRO_bkp$ATO_INFRACIONAL == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"


colnames(INCIDENCIA_banco_matriculados_CEDIPRO_bkp)[1]<-'INCIDENCIA_banco_matriculados_CEDIPRO_bkp'
colnames(INCIDENCIA_banco_matriculados_CEDIPRO_bkp)[2]<-'QUANTIDADE'
colnames(INCIDENCIA_banco_matriculados_CEDIPRO_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

INCIDENCIA_banco_matriculados_CEDIPRO =
  INCIDENCIA_banco_matriculados_CEDIPRO %>%
  janitor::tabyl(ATO_INFRACIONAL) %>%
  arrange(ATO_INFRACIONAL) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL[INCIDENCIA_banco_matriculados_CEDIPRO$ATO_INFRACIONAL == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"


colnames(INCIDENCIA_banco_matriculados_CEDIPRO)[1]<-'ATO INFRACIONAL'
colnames(INCIDENCIA_banco_matriculados_CEDIPRO)[2]<-'QUANTIDADE'
colnames(INCIDENCIA_banco_matriculados_CEDIPRO)[3]<-'PERCENTUAL'

#############################################################################################################
# INCIDENCIA_banco_matriculados_CEDIPRO =
#   INCIDENCIA_banco_matriculados_CEDIPRO %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# INCIDENCIA_banco_matriculados_CEDIPRO FIM
#########################################################################################################
#############################################################################################################
#########################################################################################################
###DECISAO_banco_matriculados_CEDIPRO
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/escola"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

DECISAO_banco_matriculados_CEDIPRO = banco_matriculados_CEDIPRO
#decisao_geral_bkp = decisao_geral
#decisao02 = banco_sem_mba %>%
# select(SEXO, DATA_ATO, DATA_AUDIENCIA_PRELIMINAR, SENTENCA, DATA_SENTENCA, DECISAO, DECISAO_PROTETIVA, QUAL_DECISAO_PROTETIVA_01,
#       QUAL_DECISAO_PROTETIVA_02, QUAL_DECISAO_PROTETIVA_03, COMPARECIMENTO_AUD_PRELIMINAR)
#########################################################################################################
#########################################################################################################
#########################################################################################################
DECISAO_banco_matriculados_CEDIPRO$decisao <- gsub(" ","", DECISAO_banco_matriculados_CEDIPRO$decisao)
#########################################################################################################

#preenchimento de celulas:
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "ABSOLVICAO"]<-	"ABSOLVIÇÃO"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "ADVERTENCIA"]<-	"ADVERTÊNCIA"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "ARQUIVAMENTO"]<-	"ARQUIVAMENTO"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "DECISAOINCONCLUSIVA"]<-	"VOUTROS"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "EXTINCAODOPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "EXTINCAOPORMORTE"]<-	"EXTINÇÃO POR MORTE"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "EXTINCAOPORPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "INSTRUCAODOFEITO"]<-	"INSTRUÇÃO DO FEITO"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "INTERNACAO"]<-	"INTERNAÇÃO"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "INTERNACAOPROVISORIA"]<-	"INTERNAÇÃO PROVISÓRIA"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "INTERNACAOPROVISORIA/REGIMEDOMICILIAR"]<-	"INTERNAÇÃO PROVISÓRIA (REGIME DOMICILIAR)"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "JUSTICARESTAURATIVA"]<-	"JUSTIÇA RESTAURATIVA"
#DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "LA"]<-	"REMISSAO c/c LA"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "OUTRAS(OS)"]<-	"VOUTROS"
#DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "PSC"]<-	"REMISSAO c/c PSC"
#DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "PSC/LA"]<-	"REMISSAO c/c LA/PSC"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "REMESSAAOJUIZOCOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "REMESSAAOJUIZOCOMPETENTE-MAIORIDADE"]<-	"REMESSA AO JUÍZO COMPETENTE"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "REMESSACOMARCACOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "REMETIDOSAUTOSJ.COMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "REMISSAOEXTINTIVA"]<-	"REMISSÃO EXTINTIVA"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "REMISSAOEXTINTIVA/ADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "REMISSAOEXTINTIVAc/cADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "REMISSAOSUSPENSIVA-L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "REMISSAOSUSPENSIVA–L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "REMISSAOSUSPENSIVA-LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "REMISSAOSUSPENSIVA-PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "REMISSAOSUSPENSIVA-PSC/REPARACAODEDANO"]<-	"REMISSÃO SUSPENSIVA c/c PSC/REPARAÇÃO DE DANO"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "REMISSAOSUSPENSIVA-REPARACAODEDANO"]<- "REMISSÃO SUSPENSIVA c/c REPARAÇÃO DE DANO"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "REMISSAOSUSPENSIVA/LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "REMISSAOSUSPENSIVA/LA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c LA/PSC"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "REMISSAOSUSPENSIVA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "RESPONDERPROCESSOEMLIBERDADE"]<-	"RESPONDER EM LIBERDADE"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "ENTREGUEAOSRESPONSAVEIS"]<-	"RESPONDER EM LIBERDADE"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "RETORNOAINTERNACAO"]<-	"RETORNO A INTERNAÇÃO"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "RETORNOAOCEIP"]<-	"RETORNO AO CEIP"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "RETORNOASEMILIBERDADE"]<-	"RETORNO A SEMILIBERDADE"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "RETORNODOSAUTOSADELEGACIA"]<-	"RETORNO DOS AUTOS A DELEGACIA"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "SEMILIBERDADE"]<-	"SEMILIBERDADE"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "SEMINFORMACAO"]<-	"VAZIO"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "REMISSAO/ADVERTENCIA/REPARACAODEDANO"]<-	"REMISSÃO/ADVERTÊNCIA/REPARAÇÃO DE DANO"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "RETORNOAOCUMPRIMENTODEPSC"]<-	"RETORNO A PSC"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "RETORNOAPSC"]<-	"RETORNO A PSC"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "RETORNOALA"]<-	"RETORNO A LA"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "RETORNOALA/PSC"]<-	"RETORNO A LA/PSC"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "RETORNOAOCUMPRIMENTODELA/PSC"]<-	"RETORNO A LA/PSC"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "RETORNOAOCUMPRIMENTODEL.A"]<-	"RETORNO A LA"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "EXTINCAOPORPRESCRICAO"]<-	"EXTINÇÃO POR PRESCRIÇÃO"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "REMESSAAOJUIZOCOMPETENTE-MAIORIDADECONSTATADA"]<-	"REMESSA AO JUÍZO COMPETENTE"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "REMISSAO SUSPENSIVA c/c LA"]<-	"REMISSÃO c/c LA"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "REMISSAO SUSPENSIVA c/c PSC"]<-	"REMISSÃO c/c PSC"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "REMISSAOSUSPENSIVA/PSC/LA"]<-	"REMISSÃO c/c LA"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == ""]<-	"VAZIO"

#########################################################################################################
#########################################################################################################
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "VAZIO"]<-	"SEM INFORMAÇÃO"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "VOUTROS"]<-	"OUTROS"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
#########################################################################################################
#########################################################################################################
#########################################################################################################
# salvando para gráfico
DECISAO_banco_matriculados_CEDIPRO_bkp = DECISAO_banco_matriculados_CEDIPRO

DECISAO_banco_matriculados_CEDIPRO_bkp =
  DECISAO_banco_matriculados_CEDIPRO_bkp %>%
  janitor::tabyl(decisao) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"

colnames(DECISAO_banco_matriculados_CEDIPRO_bkp)[1]<-'DECISAO_banco_matriculados_CEDIPRO_bkp'
colnames(DECISAO_banco_matriculados_CEDIPRO_bkp)[2]<-'QUANTIDADE'
colnames(DECISAO_banco_matriculados_CEDIPRO_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#ordenando outros para final da tabela
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "OUTROS"]<-	"ROUTROS"
#########################################################################################################
DECISAO_banco_matriculados_CEDIPRO =
  DECISAO_banco_matriculados_CEDIPRO %>%
  janitor::tabyl(decisao) %>%
  arrange(decisao) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
DECISAO_banco_matriculados_CEDIPRO$decisao[DECISAO_banco_matriculados_CEDIPRO$decisao == "ROUTROS"]<- "OUTROS"

colnames(DECISAO_banco_matriculados_CEDIPRO)[1]<-'DECISÃO'
colnames(DECISAO_banco_matriculados_CEDIPRO)[2]<-'QUANTIDADE'
colnames(DECISAO_banco_matriculados_CEDIPRO)[3]<-'PERCENTUAL'

#############################################################################################################
# DECISAO_banco_matriculados_CEDIPRO =
#   DECISAO_banco_matriculados_CEDIPRO %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# DECISAO_banco_matriculados_CEDIPRO FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO PROTETIVAS_banco_matriculados_CEDIPRO
#########################################################################################################

PROTETIVAS_banco_matriculados_CEDIPRO =

  banco_matriculados_CEDIPRO %>%
  filter(medida_protetiva == "SIM")

PROTETIVAS_banco_matriculados_CEDIPRO =
  PROTETIVAS_banco_matriculados_CEDIPRO %>%
  pivot_longer(cols = starts_with("qual_medida_protetiva_0"), values_to = "MEDIDA_PROTETIVA") %>%
  #select(-name) %>%
  filter(MEDIDA_PROTETIVA != "" & MEDIDA_PROTETIVA != "NSA")

#########################################################################################################
PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA = ifelse(PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA == "1",
                                                                "ART. 101, I", PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA)

table(PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA)
PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA = ifelse(PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA == "2",
                                                                "ART. 101, II", PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA)

table(PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA)
PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA = ifelse(PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA == "3",
                                                                "ART. 101, III", PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA)

table(PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA)
PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA = ifelse(PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA == "4",
                                                                "ART. 101, IV", PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA)

table(PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA)
PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA = ifelse(PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA == "5",
                                                                "ART. 101, V", PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA)

table(PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA)
PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA = ifelse(PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA == "6",
                                                                "ART. 101, VI", PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA)

table(PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA)
PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA = ifelse(PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA == "7",
                                                                "ART. 101, VII", PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA)
#########################################################################################################
#########################################################################################################
#########################################################################################################
#SUBSTITUIR
#PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA[PROTETIVAS_banco_matriculados_CEDIPRO$MEDIDA_PROTETIVA == "SEM INFORMAÇÃO"]<- "ZSEM INFORMAÇÃO"
#########################################################################################################
#########################################################################################################
# salvando para gráfico
PROTETIVAS_banco_matriculados_CEDIPRO_bkp = PROTETIVAS_banco_matriculados_CEDIPRO

PROTETIVAS_banco_matriculados_CEDIPRO_bkp =
  PROTETIVAS_banco_matriculados_CEDIPRO_bkp %>%
  janitor::tabyl(MEDIDA_PROTETIVA) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:

colnames(PROTETIVAS_banco_matriculados_CEDIPRO_bkp)[1]<-'PROTETIVAS_banco_matriculados_CEDIPRO_bkp'
colnames(PROTETIVAS_banco_matriculados_CEDIPRO_bkp)[2]<-'QUANTIDADE'
colnames(PROTETIVAS_banco_matriculados_CEDIPRO_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

PROTETIVAS_banco_matriculados_CEDIPRO =
  PROTETIVAS_banco_matriculados_CEDIPRO %>%
  janitor::tabyl(MEDIDA_PROTETIVA) %>%
  arrange(desc(n)) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:

colnames(PROTETIVAS_banco_matriculados_CEDIPRO)[1]<-'MEDIDA PROTETIVA'
colnames(PROTETIVAS_banco_matriculados_CEDIPRO)[2]<-'QUANTIDADE'
colnames(PROTETIVAS_banco_matriculados_CEDIPRO)[3]<-'PERCENTUAL'

#############################################################################################################
# PROTETIVAS_banco_matriculados_CEDIPRO =
#   PROTETIVAS_banco_matriculados_CEDIPRO %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
#########################################################################################################
#TRATAMENTO PROTETIVAS_banco_matriculados_CEDIPRO FIM
#########################################################################################################
#########################################################################################################
###REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/escola"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO = banco_matriculados_CEDIPRO
#regional_ato_geral_bkp = regional_ato_geral
#regional_ato02 = banco_sem_mba %>%
# select(SEXO, DATA_ATO, DATA_AUDIENCIA_PRELIMINAR, SENTENCA, DATA_SENTENCA, regional_ato, regional_ato_PROTETIVA, QUAL_regional_ato_PROTETIVA_01,
#       QUAL_regional_ato_PROTETIVA_02, QUAL_regional_ato_PROTETIVA_03, COMPARECIMENTO_AUD_PRELIMINAR)
#########################################################################################################
#########################################################################################################
#AGREGAR RMBH
REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO$regional_residencial <- str_replace(REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO$regional_residencial, "RMBH.*", "ZREGIÃO METROPOLITANA")

#SUBSTITUIR
REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO$regional_residencial[REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO$regional_residencial == ""]<- "ZSEM INFORMAÇÃO"


#########################################################################################################
#########################################################################################################
# salvando para gráfico
REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp = REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO

REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp =
  REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp %>%
  janitor::tabyl(regional_residencial) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR
REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp$regional_residencial[REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp$regional_residencial == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp$regional_residencial[REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp$regional_residencial == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"

colnames(REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp)[1]<-'REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp'
colnames(REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp)[2]<-'QUANTIDADE'
colnames(REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO =
  REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO %>%
  janitor::tabyl(regional_residencial) %>%
  arrange(regional_residencial) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO$regional_residencial[REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO$regional_residencial == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO$regional_residencial[REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO$regional_residencial == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"



colnames(REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO)[1]<-'REGIONAL'
colnames(REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO)[2]<-'QUANTIDADE'
colnames(REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO)[3]<-'PERCENTUAL'

#############################################################################################################

#REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO =
#  REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO FIM
#########################################################################################################
#########################################################################################################
###REGIONAL_ATO_banco_matriculados_CEDIPRO
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/escola"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

REGIONAL_ATO_banco_matriculados_CEDIPRO = banco_matriculados_CEDIPRO
#regional_ato_geral_bkp = regional_ato_geral
#regional_ato02 = banco_sem_mba %>%
# select(SEXO, DATA_ATO, DATA_AUDIENCIA_PRELIMINAR, SENTENCA, DATA_SENTENCA, regional_ato, regional_ato_PROTETIVA, QUAL_regional_ato_PROTETIVA_01,
#       QUAL_regional_ato_PROTETIVA_02, QUAL_regional_ato_PROTETIVA_03, COMPARECIMENTO_AUD_PRELIMINAR)
#########################################################################################################
#########################################################################################################
#AGREGAR RMBH
REGIONAL_ATO_banco_matriculados_CEDIPRO$regional_ato <- str_replace(REGIONAL_ATO_banco_matriculados_CEDIPRO$regional_ato, "RMBH.*", "ZREGIÃO METROPOLITANA")

#SUBSTITUIR
REGIONAL_ATO_banco_matriculados_CEDIPRO$regional_ato[REGIONAL_ATO_banco_matriculados_CEDIPRO$regional_ato == ""]<- "ZSEM INFORMAÇÃO"

#########################################################################################################
#########################################################################################################
# salvando para gráfico
REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp = REGIONAL_ATO_banco_matriculados_CEDIPRO

REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp =
  REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp %>%
  janitor::tabyl(regional_ato) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:

#SUBSTITUIR
REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp$regional_ato[REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp$regional_ato == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp$regional_ato[REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp$regional_ato == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"


colnames(REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp)[1]<-'REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp'
colnames(REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp)[2]<-'QUANTIDADE'
colnames(REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

REGIONAL_ATO_banco_matriculados_CEDIPRO =
  REGIONAL_ATO_banco_matriculados_CEDIPRO %>%
  janitor::tabyl(regional_ato) %>%
  arrange(regional_ato) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
REGIONAL_ATO_banco_matriculados_CEDIPRO$regional_ato[REGIONAL_ATO_banco_matriculados_CEDIPRO$regional_ato == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
REGIONAL_ATO_banco_matriculados_CEDIPRO$regional_ato[REGIONAL_ATO_banco_matriculados_CEDIPRO$regional_ato == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"



colnames(REGIONAL_ATO_banco_matriculados_CEDIPRO)[1]<-'REGIONAL'
colnames(REGIONAL_ATO_banco_matriculados_CEDIPRO)[2]<-'QUANTIDADE'
colnames(REGIONAL_ATO_banco_matriculados_CEDIPRO)[3]<-'PERCENTUAL'

#############################################################################################################

#REGIONAL_ATO_banco_matriculados_CEDIPRO =
#  REGIONAL_ATO_banco_matriculados_CEDIPRO %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# REGIONAL_ATO_banco_matriculados_CEDIPRO FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_matriculados_CEDIPRO FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_desistencia_CEDIPRO
#########################################################################################################
#sexo_idade_banco_desistencia_CEDIPRO
#########################################################################################################
# ADOLESCENTE ENCAMINHADOS. Retirados do banco SEM MBA sem adolescentes duplicados
# ordenar nesta ordem para que, quando cortar nome repetidos, preservar data do último ato.
banco_desistencia_CEDIPRO <-banco_desistencia_CEDIPRO[order(banco_desistencia_CEDIPRO$data_ato, decreasing=TRUE),]#ordenar, decrescente, data do ato
banco_desistencia_CEDIPRO <-banco_desistencia_CEDIPRO[order(banco_desistencia_CEDIPRO$nome, decreasing=FALSE),]#ordenar, crescente, nome2

#retirar nomes duplicados:snr=sem nome repetido

#df_snr_banco_desistencia_CEDIPRO <- banco_desistencia_CEDIPRO[!duplicated(data.frame(banco_desistencia_CEDIPRO$NOME2, banco_desistencia_CEDIPRO$NASCIMENTO)),]
library(dplyr)
df_snr_banco_desistencia_CEDIPRO = distinct(banco_desistencia_CEDIPRO, nome, nascimento, .keep_all= TRUE)

#banco_para_amostra <-banco[!duplicated(data.frame(banco$NOME2, banco$NASCIMENTO)),]
##write.csv(banco_para_amostra, file ="banco_para_amostra.csv",row.names=TRUE)

#write.csv(df_snr_banco_desistencia_CEDIPRO, file ="df_snr_banco_desistencia_CEDIPRO.csv",row.names=TRUE)
#write.xlsx(df_snr_banco_desistencia_CEDIPRO, file ="df_snr_banco_desistencia_CEDIPRO.xlsx")
#########################################################################################################
#########################################################################################################

# 9 Idade e sexo adolescente atendido (colocar todos acima de 18 nos S/Informação ou #valor!)
df_snr_banco_desistencia_CEDIPRO_bkp = df_snr_banco_desistencia_CEDIPRO

#########################################################################################################
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_idade_banco_desistencia_CEDIPRO = df_snr_banco_desistencia_CEDIPRO %>%
  select(sexo, idade)

table(df_snr_sexo_idade_banco_desistencia_CEDIPRO$sexo)

#########################################################################################################
#########################################################################################################

df_snr_sexo_idade_banco_desistencia_CEDIPRO$sexo <- as.character(df_snr_sexo_idade_banco_desistencia_CEDIPRO$sexo)

df_snr_sexo_idade_banco_desistencia_CEDIPRO$sexo[df_snr_sexo_idade_banco_desistencia_CEDIPRO$sexo == ""]<- "s/inf"
df_snr_sexo_idade_banco_desistencia_CEDIPRO$idade[df_snr_sexo_idade_banco_desistencia_CEDIPRO$idade == ""]<- "s/inf"

table(df_snr_sexo_idade_banco_desistencia_CEDIPRO$sexo)
table(df_snr_sexo_idade_banco_desistencia_CEDIPRO$idade)

df_snr_sexo_idade_banco_desistencia_CEDIPRO <- table(df_snr_sexo_idade_banco_desistencia_CEDIPRO$idade, df_snr_sexo_idade_banco_desistencia_CEDIPRO$sexo, useNA ="always")
#write.csv(df_snr_sexo_idade_banco_desistencia_CEDIPRO, file ="df_snr_sexo_idade_banco_desistencia_CEDIPRO.csv",row.names=TRUE)
#write.csv(df_snr_sexo_idade_banco_desistencia_CEDIPRO, file ="df_snr_sexo_idade_banco_desistencia_CEDIPRO.csv",row.names=TRUE)
sum(df_snr_sexo_idade_banco_desistencia_CEDIPRO)

df_snr_sexo_idade_banco_desistencia_CEDIPRO = data.frame(df_snr_sexo_idade_banco_desistencia_CEDIPRO)
#########################################################################################################
#########################################################################################################


df_snr_sexo_idade_banco_desistencia_CEDIPRO_bkp = df_snr_sexo_idade_banco_desistencia_CEDIPRO


df_snr_sexo_idade_banco_desistencia_CEDIPRO

colnames(df_snr_sexo_idade_banco_desistencia_CEDIPRO) <- c("idade", "sexo", "QUANTIDADE")

df_snr_sexo_idade_banco_desistencia_CEDIPRO

df_snr_sexo_idade_banco_desistencia_CEDIPRO$idade <- as.character(df_snr_sexo_idade_banco_desistencia_CEDIPRO$idade)
df_snr_sexo_idade_banco_desistencia_CEDIPRO$sexo <- as.character(df_snr_sexo_idade_banco_desistencia_CEDIPRO$sexo)
sum(df_snr_sexo_idade_banco_desistencia_CEDIPRO$QUANTIDADE)


df_snr_sexo_idade_banco_desistencia_CEDIPRO = filter(df_snr_sexo_idade_banco_desistencia_CEDIPRO, !QUANTIDADE == 0)
df_snr_sexo_idade_banco_desistencia_CEDIPRO

#PREENCHER COM NA'S CELULAS VAZIAS
#df_snr_sexo_idade_banco_desistencia_CEDIPRO$idade[df_snr_sexo_idade_banco_desistencia_CEDIPRO$idade == "SEM INFORMACAO"]<- "<NA>"
df_snr_sexo_idade_banco_desistencia_CEDIPRO$idade[which(is.na(df_snr_sexo_idade_banco_desistencia_CEDIPRO$idade))] <- "S/Informação"
df_snr_sexo_idade_banco_desistencia_CEDIPRO


df_snr_sexo_idade_banco_desistencia_CEDIPRO$idade[df_snr_sexo_idade_banco_desistencia_CEDIPRO$idade == "S/Informação anos"]<- "S/Informação"

df_snr_sexo_idade_banco_desistencia_CEDIPRO <- reshape(data = df_snr_sexo_idade_banco_desistencia_CEDIPRO, idvar = "idade", timevar = "sexo", direction = "wide")
df_snr_sexo_idade_banco_desistencia_CEDIPRO

colnames(df_snr_sexo_idade_banco_desistencia_CEDIPRO) <- c("IDADE", "MASCULINO") #sem feminino
#colnames(df_snr_sexo_idade_banco_desistencia_CEDIPRO) <- c("IDADE", "FEMININO", "MASCULINO")
df_snr_sexo_idade_banco_desistencia_CEDIPRO

#df_snr_sexo_idade_banco_desistencia_CEDIPRO$FEMININO[which(is.na(df_snr_sexo_idade_banco_desistencia_CEDIPRO$FEMININO))] <- 0
df_snr_sexo_idade_banco_desistencia_CEDIPRO$MASCULINO[which(is.na(df_snr_sexo_idade_banco_desistencia_CEDIPRO$MASCULINO))] <- 0


df_snr_sexo_idade_banco_desistencia_CEDIPRO





#########################################################################################################
df_snr_sexo_idade_banco_desistencia_CEDIPRO2 = df_snr_sexo_idade_banco_desistencia_CEDIPRO #salvando para proximo modelo de tabela
#########################################################################################################
df_snr_sexo_idade_banco_desistencia_CEDIPRO<- rbind(df_snr_sexo_idade_banco_desistencia_CEDIPRO,
                                                    data.frame(IDADE = "TOTAL",
                                                               #FEMININO = sum(df_snr_sexo_idade_banco_desistencia_CEDIPRO$FEMININO),
                                                               MASCULINO = sum(df_snr_sexo_idade_banco_desistencia_CEDIPRO$MASCULINO),
                                                               stringsAsFactors = FALSE))

df_snr_sexo_idade_banco_desistencia_CEDIPRO
#########################################################################################################
#########################################################################################################
#require(ggpubr)
#library(gridExtra)

#df_snr_sexo_idade_banco_desistencia_CEDIPRO = ggtexttable(df_snr_sexo_idade_banco_desistencia_CEDIPRO, rows = NULL,
#                               theme = ttheme(
#                              colnames.style = colnames_style(face = "bold", color = "white", fill = "#bb1e23"),
#                             tbody.style = tbody_style(color = "black", fill = c("#edece0", "#edece0"))))
#df_snr_sexo_idade_banco_desistencia_CEDIPRO
#########################################################################################################
#negrito na linha total
#df_snr_sexo_idade_banco_desistencia_CEDIPRO <- table_cell_font(df_snr_sexo_idade_banco_desistencia_CEDIPRO, row = 10, column = 1, face = "bold")
#df_snr_sexo_idade_banco_desistencia_CEDIPRO <- table_cell_font(df_snr_sexo_idade_banco_desistencia_CEDIPRO, row = 10, column = 2, face = "bold")
#df_snr_sexo_idade_banco_desistencia_CEDIPRO <- table_cell_font(df_snr_sexo_idade_banco_desistencia_CEDIPRO, row = 10, column = 3, face = "bold")
#df_snr_sexo_idade_banco_desistencia_CEDIPRO
#########################################################################################################
#salvando tabela
#pdf(file="TABELA_003_df_snr_sexo_idade_banco_desistencia_CEDIPRO_geral_alternativa.pdf", width = 3.5, height = 3.2, title = "tabela_df_snr_sexo_idade_banco_desistencia_CEDIPRO_geral_alternativa")
##setwd(file.path("~/diretorio_r/estciabh/imagens"))
#svg(filename="TABELA_002_idade_e_sexo.svg", width=5, height=3.5, pointsize=12)
#df_snr_sexo_idade_banco_desistencia_CEDIPRO +  labs(title = "TABELA 2: Idade e Sexo, Belo Horizonte, 2021",
#                         caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD") +
#theme(plot.title = element_text(hjust = 0.5, vjust = 0, face="bold"),
#     plot.caption =element_text(hjust = 0.5, vjust = 1)  )

#dev.off()

#########################################################################################################
df_snr_sexo_idade_banco_desistencia_CEDIPRO = df_snr_sexo_idade_banco_desistencia_CEDIPRO2
#########################################################################################################

#df_snr_sexo_idade_banco_desistencia_CEDIPRO$FEMININO <- as.numeric(df_snr_sexo_idade_banco_desistencia_CEDIPRO$FEMININO)
df_snr_sexo_idade_banco_desistencia_CEDIPRO$MASCULINO <- as.numeric(df_snr_sexo_idade_banco_desistencia_CEDIPRO$MASCULINO)

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
#df_snr_sexo_idade_banco_desistencia_CEDIPRO$F <- round_preserve_sum(prop.table(df_snr_sexo_idade_banco_desistencia_CEDIPRO$FEMININO)*100, 2)
df_snr_sexo_idade_banco_desistencia_CEDIPRO$M <- round_preserve_sum(prop.table(df_snr_sexo_idade_banco_desistencia_CEDIPRO$MASCULINO)*100, 2)


df_snr_sexo_idade_banco_desistencia_CEDIPRO
#########################################################################################################
#df_snr_sexo_idade_banco_desistencia_CEDIPRO <- df_snr_sexo_idade_banco_desistencia_CEDIPRO[c("IDADE", "FEMININO", "F", "MASCULINO", "M")]
df_snr_sexo_idade_banco_desistencia_CEDIPRO <- df_snr_sexo_idade_banco_desistencia_CEDIPRO[c("IDADE", "MASCULINO", "M")] #sem feminino
df_snr_sexo_idade_banco_desistencia_CEDIPRO

#########################################################################################################
#########################################################################################################

#script para o bookdown

df_snr_sexo_idade_banco_desistencia_CEDIPRO_rmark = df_snr_sexo_idade_banco_desistencia_CEDIPRO

#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))

#banco_incidencia_rmark %>% slice(1:3)

#selecionando os 3 principais e ordenando descrescente por quantidade
df_snr_sexo_idade_banco_desistencia_CEDIPRO_rmark = df_snr_sexo_idade_banco_desistencia_CEDIPRO_rmark %>%
  top_n(3, MASCULINO) %>% arrange(desc(MASCULINO))

#somando
sum(df_snr_sexo_idade_banco_desistencia_CEDIPRO_rmark$M)

#para escolher linhas e posicoes
df_snr_sexo_idade_banco_desistencia_CEDIPRO_rmark[2,1]
#outra forma de calcular percentual
#banco_incidencia = mutate(banco_incidencia,
#                          PERCENTUAL = (QUANTIDADE / sum(QUANTIDADE))*100)

#########################################################################################################
#########################################################################################################

df_snr_sexo_idade_banco_desistencia_CEDIPRO$IDADE <- paste(df_snr_sexo_idade_banco_desistencia_CEDIPRO$IDADE, "anos", sep=" ")

#########################################################################################################
#########################################################################################################

df_snr_sexo_idade_banco_desistencia_CEDIPRO<- rbind(df_snr_sexo_idade_banco_desistencia_CEDIPRO,
                                                    data.frame(IDADE = "TOTAL",
                                                               #FEMININO = sum(df_snr_sexo_idade_banco_desistencia_CEDIPRO$FEMININO),
                                                               #F = sum(df_snr_sexo_idade_banco_desistencia_CEDIPRO$F),
                                                               MASCULINO = sum(df_snr_sexo_idade_banco_desistencia_CEDIPRO$MASCULINO),
                                                               M = sum(df_snr_sexo_idade_banco_desistencia_CEDIPRO$M),
                                                               stringsAsFactors = FALSE))

df_snr_sexo_idade_banco_desistencia_CEDIPRO

#colnames(df_snr_sexo_idade_banco_desistencia_CEDIPRO) <- c("IDADE", "FEM", "%", "MAS", "%")
colnames(df_snr_sexo_idade_banco_desistencia_CEDIPRO) <- c("IDADE", "MAS", "%")
df_snr_sexo_idade_banco_desistencia_CEDIPRO
#########################################################################################################
#require(ggpubr)
#library(gridExtra)
#########################################################################################################
#salvando tabela
#pdf(file="TABELA_003_df_snr_sexo_idade_banco_desistencia_CEDIPRO_geral_alternativa2.pdf", width = 3.5, height = 3.2, title = "tabela_df_snr_sexo_idade_banco_desistencia_CEDIPRO_geral_alternativa")
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

df_snr_sexo_idade_banco_desistencia_CEDIPRO = df_snr_banco_desistencia_CEDIPRO %>%
  select(sexo, idade)

table(df_snr_sexo_idade_banco_desistencia_CEDIPRO$sexo)

df_snr_sexo_idade_banco_desistencia_CEDIPRO$sexo <- as.character(df_snr_sexo_idade_banco_desistencia_CEDIPRO$sexo)

df_snr_sexo_idade_banco_desistencia_CEDIPRO$sexo[df_snr_sexo_idade_banco_desistencia_CEDIPRO$sexo == ""]<- "M"
table(df_snr_sexo_idade_banco_desistencia_CEDIPRO$sexo)

df_snr_sexo_idade_banco_desistencia_CEDIPRO <- table(df_snr_sexo_idade_banco_desistencia_CEDIPRO$idade, df_snr_sexo_idade_banco_desistencia_CEDIPRO$sexo, useNA ="always")
##write.csv(df_snr_sexo_idade_banco_desistencia_CEDIPRO, file ="df_snr_sexo_idade_banco_desistencia_CEDIPRO.csv",row.names=TRUE)
##write.csv(df_snr_sexo_idade_banco_desistencia_CEDIPRO, file ="df_snr_sexo_idade_banco_desistencia_CEDIPRO.csv",row.names=TRUE)
sum(df_snr_sexo_idade_banco_desistencia_CEDIPRO)

df_snr_sexo_idade_banco_desistencia_CEDIPRO = data.frame(df_snr_sexo_idade_banco_desistencia_CEDIPRO)
#########################################################################################################
#########################################################################################################
#########################################################################################################


df_snr_sexo_idade_banco_desistencia_CEDIPRO_bkp = df_snr_sexo_idade_banco_desistencia_CEDIPRO


df_snr_sexo_idade_banco_desistencia_CEDIPRO

colnames(df_snr_sexo_idade_banco_desistencia_CEDIPRO) <- c("idade", "sexo", "QUANTIDADE")

df_snr_sexo_idade_banco_desistencia_CEDIPRO

df_snr_sexo_idade_banco_desistencia_CEDIPRO$idade <- as.character(df_snr_sexo_idade_banco_desistencia_CEDIPRO$idade)
df_snr_sexo_idade_banco_desistencia_CEDIPRO$sexo <- as.character(df_snr_sexo_idade_banco_desistencia_CEDIPRO$sexo)
sum(df_snr_sexo_idade_banco_desistencia_CEDIPRO$QUANTIDADE)


df_snr_sexo_idade_banco_desistencia_CEDIPRO = filter(df_snr_sexo_idade_banco_desistencia_CEDIPRO, !QUANTIDADE == 0)
df_snr_sexo_idade_banco_desistencia_CEDIPRO

#PREENCHER COM NA'S CELULAS VAZIAS
#df_snr_sexo_idade_banco_desistencia_CEDIPRO$idade[df_snr_sexo_idade_banco_desistencia_CEDIPRO$idade == "SEM INFORMACAO"]<- "<NA>"
df_snr_sexo_idade_banco_desistencia_CEDIPRO$idade[which(is.na(df_snr_sexo_idade_banco_desistencia_CEDIPRO$idade))] <- "s/inf"
df_snr_sexo_idade_banco_desistencia_CEDIPRO


df_snr_sexo_idade_banco_desistencia_CEDIPRO$idade <- paste(df_snr_sexo_idade_banco_desistencia_CEDIPRO$idade, "anos", sep=" ")
df_snr_sexo_idade_banco_desistencia_CEDIPRO$idade[df_snr_sexo_idade_banco_desistencia_CEDIPRO$idade == "s/inf anos"]<- "s/inf"
df_snr_sexo_idade_banco_desistencia_CEDIPRO

########################################################################################################
#########################################################################################################
# GRAFICO SEXO PIZZA
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

df_snr_sexo_pizza_banco_desistencia_CEDIPRO = df_snr_banco_desistencia_CEDIPRO %>%
  select(sexo)

table(df_snr_sexo_pizza_banco_desistencia_CEDIPRO$sexo)

#########################################################################################################
#########################################################################################################

df_snr_sexo_pizza_banco_desistencia_CEDIPRO$sexo <- as.character(df_snr_sexo_pizza_banco_desistencia_CEDIPRO$sexo)

df_snr_sexo_pizza_banco_desistencia_CEDIPRO$sexo[df_snr_sexo_pizza_banco_desistencia_CEDIPRO$sexo == ""]<- "M"
table(df_snr_sexo_pizza_banco_desistencia_CEDIPRO$sexo)

df_snr_sexo_pizza_banco_desistencia_CEDIPRO = data.frame(table(df_snr_sexo_pizza_banco_desistencia_CEDIPRO$sexo))

colnames(df_snr_sexo_pizza_banco_desistencia_CEDIPRO) <- c("sexo", "QUANTIDADE")

sum(df_snr_sexo_pizza_banco_desistencia_CEDIPRO$QUANTIDADE)

#########################################################################################################
#########################################################################################################


df_snr_sexo_pizza_banco_desistencia_CEDIPRO$sexo <- as.character(df_snr_sexo_pizza_banco_desistencia_CEDIPRO$sexo)

df_snr_sexo_pizza_banco_desistencia_CEDIPRO$sexo[df_snr_sexo_pizza_banco_desistencia_CEDIPRO$sexo == "F"]<- "FEMININO"
df_snr_sexo_pizza_banco_desistencia_CEDIPRO$sexo[df_snr_sexo_pizza_banco_desistencia_CEDIPRO$sexo == "M"]<- "MASCULINO"

df_snr_sexo_pizza_banco_desistencia_CEDIPRO_original=df_snr_sexo_pizza_banco_desistencia_CEDIPRO

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
df_snr_sexo_pizza_banco_desistencia_CEDIPRO

library("ggplot2")  # Data visualization
library("dplyr")    # Data manipulation

df_snr_sexo_pizza_banco_desistencia_CEDIPRO <- df_snr_sexo_pizza_banco_desistencia_CEDIPRO %>%
  arrange(desc(QUANTIDADE)) %>%
  mutate(PERCENTUAL = round_preserve_sum(prop.table(QUANTIDADE)*100, 2))

df_snr_sexo_pizza_banco_desistencia_CEDIPRO$PERCENTUAL <- paste(df_snr_sexo_pizza_banco_desistencia_CEDIPRO$PERCENTUAL, "%", sep=" ")

df_snr_sexo_pizza_banco_desistencia_CEDIPRO



setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
########################################################################################################
##sexo_idade_banco_desistencia_CEDIPRO FIM
####################################################################################################################




#############################################################################################################
#ESCOLARIDADE_banco_desistencia_CEDIPRO
#########################################################################################################

ESCOLARIDADE_banco_desistencia_CEDIPRO =
  banco_desistencia_CEDIPRO |>
  select(escolaridade_termo)

#adaptando para o restante dos scripts
colnames(ESCOLARIDADE_banco_desistencia_CEDIPRO)[1]<-'ESCOLARIDADE_banco_desistencia_CEDIPRO'

ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO = ajustar_nomes(ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO)
#########################################################################################################
#AJUSTA OS FORA DE PADRÃO AQUI:
ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO == "EJA"]<- "EJAENSFUND" #FIZ OPÇÃO PELO FUND
ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO == ""]<- "SEMINFORMACAO" #FIZ OPÇÃO PELO FUND
ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO == "NSA"]<- "NAORESPONDEU" #FIZ OPÇÃO PELO FUND
#ORDENANDO

ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO == "1ªSERIE-ENSFUND"]<- "A1ªSERIE-ENSFUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO == "2ªSERIE-ENSFUND"]<- "B2ªSERIE-ENSFUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO == "3ªSERIE-ENSFUND"]<- "C3ªSERIE-ENSFUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO == "4ªSERIE-ENSFUND"]<- "D4ªSERIE-ENSFUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO == "5ªSERIE-ENSFUND"]<- "E5ªSERIE-ENSFUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO == "6ªSERIE-ENSFUND"]<- "F6ªSERIE-ENSFUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO == "7ªSERIE-ENSFUND"]<- "G7ªSERIE-ENSFUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO == "8ªSERIE-ENSFUND"]<- "H8ªSERIE-ENSFUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO == "9ªSERIE-ENSFUND"]<- "I9ªSERIE-ENSFUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO == "1ºANO-ENSMEDIO"]<- "J1ºANO-ENSMEDIO"
ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO == "2ºANO-ENSMEDIO"]<- "K2ºANO-ENSMEDIO"
ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO == "3ºANO-ENSMEDIO"]<- "L3ºANO-ENSMEDIO"
ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO == "FACULDADE1ºPERIODO"]<- "LAFACULDADE1ºPERIODO"
ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO == "EJAENSFUND"]<- "MEJAENSFUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO == "EJAENSMEDIO"]<- "NEJAENSMEDIO"
ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO == "NAOSABE"]<- "ONAOSABE"
ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO == "NAORESPONDEU"]<- "PNAORESPONDEU"
ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO == "SEMINFORMACAO"]<- "QSEMINFORMACAO"

#########################################################################################################
# salvando para gráfico
ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp = ESCOLARIDADE_banco_desistencia_CEDIPRO

ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp =
  ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp %>%
  janitor::tabyl(ESCOLARIDADE_banco_desistencia_CEDIPRO) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

#########################################################################################################
ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO == "A1ªSERIE-ENSFUND"]<- "1º ANO - ENS FUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO == "B2ªSERIE-ENSFUND"]<- "2º ANO - ENS FUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO == "C3ªSERIE-ENSFUND"]<- "3º ANO - ENS FUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO == "D4ªSERIE-ENSFUND"]<- "4º ANO - ENS FUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO == "E5ªSERIE-ENSFUND"]<- "5º ANO - ENS FUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO == "F6ªSERIE-ENSFUND"]<- "6º ANO - ENS FUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO == "G7ªSERIE-ENSFUND"]<- "7º ANO - ENS FUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO == "H8ªSERIE-ENSFUND"]<- "8º ANO - ENS FUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO == "I9ªSERIE-ENSFUND"]<- "9º ANO - ENS FUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO == "J1ºANO-ENSMEDIO"]<- "1º ANO - ENS MÉDIO"
ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO == "K2ºANO-ENSMEDIO"]<- "2º ANO - ENS MÉDIO"
ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO == "L3ºANO-ENSMEDIO"]<- "3º ANO - ENS MÉDIO"
ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO == "LAFACULDADE1ºPERIODO"]<- "FACULDADE - 1º PERÍODO"
ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO == "MEJAENSFUND"]<- "EJA - ENS FUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO == "NEJAENSMEDIO"]<- "EJA - ENS MÉDIO"
ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO == "ONAOSABE"]<- "NÃO SABE"
ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO == "PNAORESPONDEU"]<- "NÃO RESPONDEU"
ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$ESCOLARIDADE_banco_desistencia_CEDIPRO == "QSEMINFORMACAO"]<- "SEM INFORMAÇÃO"


#########################################################################################################
#replace "%" with "" in the percentual column
ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$PERCENTUAL2 <- str_replace (ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$percent, "%", "")
ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$PERCENTUAL2 = as.numeric(ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$PERCENTUAL2)
#########################################################################################################

# Adaptando para scrip grafico:

colnames(ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp)[1]<-'ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp'
colnames(ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp)[2]<-'QUANTIDADE'
colnames(ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp)[3]<-'PERCENTUAL'

#########################################################################################################
#########################################################################################################

#script para o bookdown

ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp_rmark = ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp

ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp_rmark = ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp_rmark %>%
  top_n(4, QUANTIDADE) %>% arrange(desc(QUANTIDADE))


#banco_incidencia_rmark <- banco_incidencia_rmark %>%
# arrange(desc(PERCENTUAL))
ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp_rmark =
  ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp_rmark %>% slice(1:4)

library (stringr)


#########################################################################################################
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#########################################################################################################
ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO == "VNÃO SABE"]<- "UNÃO SABE"
#ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO$ESCOLARIDADE_banco_desistencia_CEDIPRO == "VNÃO RESPONDEU"]<- "NÃO RESPONDEU"
#########################################################################################################
ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA =
  ESCOLARIDADE_banco_desistencia_CEDIPRO %>%
  janitor::tabyl(ESCOLARIDADE_banco_desistencia_CEDIPRO) %>%
  arrange(ESCOLARIDADE_banco_desistencia_CEDIPRO) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################
#ordenando:

ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO == "A1ªSERIE-ENSFUND"]<- "1º ANO - ENS FUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO == "B2ªSERIE-ENSFUND"]<- "2º ANO - ENS FUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO == "C3ªSERIE-ENSFUND"]<- "3º ANO - ENS FUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO == "D4ªSERIE-ENSFUND"]<- "4º ANO - ENS FUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO == "E5ªSERIE-ENSFUND"]<- "5º ANO - ENS FUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO == "F6ªSERIE-ENSFUND"]<- "6º ANO - ENS FUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO == "G7ªSERIE-ENSFUND"]<- "7º ANO - ENS FUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO == "H8ªSERIE-ENSFUND"]<- "8º ANO - ENS FUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO == "I9ªSERIE-ENSFUND"]<- "9º ANO - ENS FUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO == "J1ºANO-ENSMEDIO"]<- "1º ANO - ENS MÉDIO"
ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO == "K2ºANO-ENSMEDIO"]<- "2º ANO - ENS MÉDIO"
ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO == "L3ºANO-ENSMEDIO"]<- "3º ANO - ENS MÉDIO"
ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO == "LAFACULDADE1ºPERIODO"]<- "FACULDADE - 1º PERÍODO"
ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO == "MEJAENSFUND"]<- "EJA - ENS FUND"
ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO == "NEJAENSMEDIO"]<- "EJA - ENS MÉDIO"
ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO == "ONAOSABE"]<- "NÃO SABE"
ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO == "PNAORESPONDEU"]<- "NÃO RESPONDEU"
ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO[ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA$ESCOLARIDADE_banco_desistencia_CEDIPRO == "QSEMINFORMACAO"]<- "SEM INFORMAÇÃO"

#########################################################################################################
#########################################################################################################
# Adaptando:

colnames(ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA)[1]<-'ESCOLARIDADE'
colnames(ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA)[2]<-'QUANTIDADE'
colnames(ESCOLARIDADE_banco_desistencia_CEDIPRO_TABELA)[3]<-'PERCENTUAL'

#############################################################################################################
#############################################################################################################
#ESCOLARIDADE_banco_desistencia_CEDIPRO FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO INCIDENCIA_banco_desistencia_CEDIPRO:
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/planilhas"))
#########################################################################################################
INCIDENCIA_banco_desistencia_CEDIPRO = banco_desistencia_CEDIPRO
#########################################################################################################

#preenchendo vazios

INCIDENCIA_banco_desistencia_CEDIPRO$ato_infracional_ata_01 = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ato_infracional_ata_01 == "",
                                                                     INCIDENCIA_banco_desistencia_CEDIPRO$ato_infracional_termo_01, INCIDENCIA_banco_desistencia_CEDIPRO$ato_infracional_ata_01)


INCIDENCIA_banco_desistencia_CEDIPRO$ato_infracional_ata_02 = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ato_infracional_ata_02 == "",
                                                                     INCIDENCIA_banco_desistencia_CEDIPRO$ato_infracional_termo_02, INCIDENCIA_banco_desistencia_CEDIPRO$ato_infracional_ata_02)


INCIDENCIA_banco_desistencia_CEDIPRO$ato_infracional_ata_01 = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ato_infracional_ata_01 == "",
                                                                     "SEM INFORMAÇÃO", INCIDENCIA_banco_desistencia_CEDIPRO$ato_infracional_ata_01)

INCIDENCIA_banco_desistencia_CEDIPRO$ato_infracional_ata_02 = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ato_infracional_ata_02 == "",
                                                                     "NSA", INCIDENCIA_banco_desistencia_CEDIPRO$ato_infracional_ata_02)


INCIDENCIA_banco_desistencia_CEDIPRO$ato_infracional_ata_03 = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ato_infracional_ata_03 == "",
                                                                     "NSA", INCIDENCIA_banco_desistencia_CEDIPRO$ato_infracional_ata_03)


#########################################################################################################
#########################################################################################################


#DESMEMBRANDO PARA QUE NÃO FIQUE MAIS DE UM ATO NA MESMA LINHA. TODOS INDO PARA NOVA COLUNA ATO_INFRACIONAL.
INCIDENCIA_banco_desistencia_CEDIPRO =

  INCIDENCIA_banco_desistencia_CEDIPRO %>%
  pivot_longer(cols = starts_with("ato_infracional_ata_0"), values_to = "ATO_INFRACIONAL") %>%
  #select(-name) %>%
  filter(ATO_INFRACIONAL != "NSA" & ATO_INFRACIONAL != "MBA")

#########################################################################################################
INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL <- gsub(" ","", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)
#########################################################################################################
#############################################################################################################
#########################################################################################################
#AMEAÇA

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "147.ARTCPB",
                                                              "AMEAÇA", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


########################################################################################################
#########################################################################################################
#CRIME DE TRÂNSITO

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "309.ARTCTB",
                                                              "CRIME DE TRÂNSITO (DIRIGIR SEM PERMISSÃO/HABILITAÇÃO)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "310.ARTCTB",
                                                              "CRIME DE TRÂNSITO (ENTREGAR DIREÇÃO A NÃO HABILITADO)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "311.ARTCTB",
                                                              "CRIME DE TRÂNSITO (VELOCIDADE INCOMPATÍVEL)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)





#para discrinar: é so anular com #

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "CRIME DE TRÂNSITO (DIRIGIR SEM PERMISSÃO/HABILITAÇÃO)",
                                                              "CRIME DE TRÂNSITO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "CRIME DE TRÂNSITO (ENTREGAR DIREÇÃO A NÃO HABILITADO)",
                                                              "CRIME DE TRÂNSITO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "CRIME DE TRÂNSITO (VELOCIDADE INCOMPATÍVEL)",
                                                              "CRIME DE TRÂNSITO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)
#########################################################################################################
#########################################################################################################
#DANO

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "163.ARTCPB",
                                                              "DANO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#ESTUPRO

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "213.ARTCPB",
                                                              "ESTUPRO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#ESTUPRO

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "215.ARTCPB",
                                                              "VIOLAÇÃO SEXUAL MEDIANTE FRAUDE", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#ESTUPRO

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "215-A.ARTCPB",
                                                              "IMPORTUNAÇÃO SEXUAL", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#ESTUPRO DE VULNERÁVEL

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "217-A.ARTCPB",
                                                              "ESTUPRO DE VULNERÁVEL", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#FURTO

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "155.ARTCPB",
                                                              "FURTO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#FURTO (TENTATIVA)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "155C/C14.ARTCPB",
                                                              "FURTO (TENTATIVA)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#HOMICÍDIO

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "121.ARTCPB",
                                                              "HOMICÍDIO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#HOMICÍDIO (TENTATIVA)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "121C/C14,II.ARTCPB",
                                                              "HOMICÍDIO (TENTATIVA)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#LESÃO CORPORAL

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "129.ARTCPB",
                                                              "LESÃO CORPORAL", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "129§3º.ARTCPB",
                                                              "LESÃO CORPORAL", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "129§9º.ARTCPB",
                                                              "LESÃO CORPORAL", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#LESÃO CORPORAL (TENTATIVA). Ordem para trocar LESÃO CORPORAL (TENTATIVA) por VIAS DE FATO

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "129C/C14,II.ARTCPB",
                                                              "VIAS DE FATO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#PICHAÇÃO

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "65.ART9.605",
                                                              "PICHAÇÃO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#PORTE/POSSE DE ARMA

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "12.ART10.826",
                                                              "ARMA DE FOGO - POSSE IRREGULAR (USO PERMITIDO)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "14.ART10.826",
                                                              "ARMA DE FOGO - PORTE ILEGAL (USO PERMITIDO)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "16.ART10.826",
                                                              "ARMA DE FOGO - POSSE/PORTE ILEGAL (USO RESTRITO)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "15.ART10.826",
                                                              "ARMA DE FOGO - PORTE ILEGAL (DISPARO)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "17.ART10.826",
                                                              "ARMA DE FOGO - POSSE/PORTE ILEGAL (COMÉRCIO ILEGAL)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)



INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "ARMA DE FOGO - POSSE IRREGULAR (USO PERMITIDO)",
                                                              "PORTE/POSSE DE ARMA", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "ARMA DE FOGO - PORTE ILEGAL (USO PERMITIDO)",
                                                              "PORTE/POSSE DE ARMA", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "ARMA DE FOGO - POSSE/PORTE ILEGAL (USO RESTRITO)",
                                                              "PORTE/POSSE DE ARMA", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "ARMA DE FOGO - PORTE ILEGAL (DISPARO)",
                                                              "PORTE/POSSE DE ARMA", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "ARMA DE FOGO - POSSE/PORTE ILEGAL (COMÉRCIO ILEGAL)",
                                                              "PORTE/POSSE DE ARMA", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#RECEPTAÇÃO

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "180.ARTCPB",
                                                              "RECEPTAÇÃO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)
#########################################################################################################
#########################################################################################################
#ROUBO

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "157.ARTCPB",
                                                              "ROUBO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "157§2ºAICPB",
                                                              "ROUBO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "157§2ºAIIARTCPB",
                                                              "ROUBO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "157§2º,I,IIeVARTCPB",
                                                              "ROUBO (EM CONCURSO DE PESSOAS)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "157§2º,I.ARTCPB",
                                                              "ROUBO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "157§2º,IeII.ARTCPB",
                                                              "ROUBO (EM CONCURSO DE PESSOAS)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "157§2ºA,IARTCPB",
                                                              "ROUBO (EMPREGO DE ARMA)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "157§2ºIIARTCPB",
                                                              "ROUBO (EM CONCURSO DE PESSOAS)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "157§2ºIIeVARTCPB",
                                                              "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "157§2ºA,I",
                                                              "ROUBO (EMPREGO DE ARMA)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "157§2ºAIARTCPB",
                                                              "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "157§2ºAIeIIARTCPB",
                                                              "ROUBO (EMPREGO DE ARMA)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "157§2ºII,VeVIIARTCPB",
                                                              "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "157§2ºIIeVIARTCPB",
                                                              "ROUBO (EMPREGO DE ARMA)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)



INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "157§2ºIIeVIIARTCPB",
                                                              "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "157§2ºIIEVIIARTCPB",
                                                              "ROUBO (EMPREGO DE ARMA)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "157§2ºVARTCPB",
                                                              "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "157§2ºVIIARTCPB",
                                                              "ROUBO (EMPREGO DE ARMA)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "157§2ºAIICPB",
                                                              "ROUBO (EMPREGO DE ARMA)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


#para discrinar: é so anular com #

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "ROUBO",
                                                              "ROUBO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "ROUBO (EM CONCURSO DE PESSOAS)",
                                                              "ROUBO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "ROUBO",
                                                              "ROUBO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "ROUBO (EM CONCURSO DE PESSOAS)",
                                                              "ROUBO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "ROUBO (EMPREGO DE ARMA)",
                                                              "ROUBO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "ROUBO (EM CONCURSO DE PESSOAS)",
                                                              "ROUBO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "ROUBO (EM CONCURSO DE PESSOAS/RESTRICAO LIBERDADE)",
                                                              "ROUBO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "ROUBO (EMPREGO DE ARMA)",
                                                              "ROUBO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)



#########################################################################################################
#ROUBO (TENTATIVA)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "157§2º,IeIIC/C14,II.ARTCPB",
                                                              "ROUBO (TENTATIVA)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "157C/C14,II.ARTCPB",
                                                              "ROUBO (TENTATIVA)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#ROUBO (§3º)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "157§3ºARTCPB",
                                                              "ROUBO (§3º)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "ROUBO (§3º)",
                                                              "LATROCÍNIO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)
#########################################################################################################
#########################################################################################################
#ROUBO (§3º) (TENTATIVA)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "157§3ºARTCPBC/C14,II,CPB",
                                                              "ROUBO (§3º) (TENTATIVA)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#SEQUESTRO

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "148.ARTCPB",
                                                              "SEQUESTRO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#TRÁFICO DE DROGAS


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "33.ART11.343",
                                                              "TRÁFICO DE DROGAS", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


#INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "35.ART11.343",
# "TRÁFICO DE DROGAS", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "37.ART11.343",
                                                              "TRÁFICO DE DROGAS (INFORMANTE)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)



#para discrinar: é so anular com #

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "TRÁFICO DE DROGAS",
                                                              "TRÁFICO DE DROGAS", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


#INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "35.ART11.343",
# "TRÁFICO DE DROGAS", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "TRÁFICO DE DROGAS (INFORMANTE)",
                                                              "TRÁFICO DE DROGAS", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


#########################################################################################################
#########################################################################################################
#########################################################################################################
#ASSOCIAÇÃO TRÁFICO DE DROGAS


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "35.ART11.343",
                                                              "TRÁFICO DE DROGAS (ASSOCIAÇÃO)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "34.ART11.343",
                                                              "TRÁFICO DE DROGAS (ASSOCIAÇÃO)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

#para discrinar: é so anular com #

#INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "TRÁFICO DE DROGAS (ASSOCIAÇÃO)",
#                                           "TRÁFICO DE DROGAS", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#USO DE DROGAS

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "28.ART11.343",
                                                              "POSSE DE DROGAS PARA USO PESSOAL", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)



#########################################################################################################
#########################################################################################################
#VIAS DE FATO

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "21.ARTLCP",
                                                              "VIAS DE FATO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


#########################################################################################################
#OUTROS

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "140.ARTCPB",
                                                              "INJÚRIA", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "140§3º.ARTCPB",
                                                              "INJÚRIA", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)



INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "330.ARTCPB",
                                                              "DESOBEDIÊNCIA", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "331.ARTCPB",
                                                              "DESACATO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)


INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "139.ARTCPB",
                                                              "DESOBEDIÊNCIA", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "2.ART13.185",
                                                              "INTIMIDAÇÃO SISTEMÁTICA (BULLYING)", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "329.ARTCPB",
                                                              "RESISTÊNCIA", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "311.ARTCPB",
                                                              "ADULTERAÇÃO DE SINAL IDENTIFICADOR DE VEÍCULO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#ajustando termos
#########################################################################################################
INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "SEMINFORMAÇÃO",
                                                              "SEM INFORMAÇÃO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL = ifelse(INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "TERMOSEMINF.",
                                                              "SEM INFORMAÇÃO", INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#SUBSTITUIR
INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL[INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "SEM INFORMAÇÃO"]<- "ZSEM INFORMAÇÃO"
#########################################################################################################
#########################################################################################################
#########################################################################################################
# salvando para gráfico
INCIDENCIA_banco_desistencia_CEDIPRO_bkp = INCIDENCIA_banco_desistencia_CEDIPRO

INCIDENCIA_banco_desistencia_CEDIPRO_bkp =
  INCIDENCIA_banco_desistencia_CEDIPRO_bkp %>%
  janitor::tabyl(ATO_INFRACIONAL) %>%
  arrange(n) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR
INCIDENCIA_banco_desistencia_CEDIPRO_bkp$ATO_INFRACIONAL[INCIDENCIA_banco_desistencia_CEDIPRO_bkp$ATO_INFRACIONAL == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"


colnames(INCIDENCIA_banco_desistencia_CEDIPRO_bkp)[1]<-'INCIDENCIA_banco_desistencia_CEDIPRO_bkp'
colnames(INCIDENCIA_banco_desistencia_CEDIPRO_bkp)[2]<-'QUANTIDADE'
colnames(INCIDENCIA_banco_desistencia_CEDIPRO_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

INCIDENCIA_banco_desistencia_CEDIPRO =
  INCIDENCIA_banco_desistencia_CEDIPRO %>%
  janitor::tabyl(ATO_INFRACIONAL) %>%
  arrange(ATO_INFRACIONAL) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL[INCIDENCIA_banco_desistencia_CEDIPRO$ATO_INFRACIONAL == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"


colnames(INCIDENCIA_banco_desistencia_CEDIPRO)[1]<-'ATO INFRACIONAL'
colnames(INCIDENCIA_banco_desistencia_CEDIPRO)[2]<-'QUANTIDADE'
colnames(INCIDENCIA_banco_desistencia_CEDIPRO)[3]<-'PERCENTUAL'

#############################################################################################################
# INCIDENCIA_banco_desistencia_CEDIPRO =
#   INCIDENCIA_banco_desistencia_CEDIPRO %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# INCIDENCIA_banco_desistencia_CEDIPRO FIM
#########################################################################################################
#############################################################################################################
#########################################################################################################
###DECISAO_banco_desistencia_CEDIPRO
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/escola"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

DECISAO_banco_desistencia_CEDIPRO = banco_desistencia_CEDIPRO
#decisao_geral_bkp = decisao_geral
#decisao02 = banco_sem_mba %>%
# select(SEXO, DATA_ATO, DATA_AUDIENCIA_PRELIMINAR, SENTENCA, DATA_SENTENCA, DECISAO, DECISAO_PROTETIVA, QUAL_DECISAO_PROTETIVA_01,
#       QUAL_DECISAO_PROTETIVA_02, QUAL_DECISAO_PROTETIVA_03, COMPARECIMENTO_AUD_PRELIMINAR)
#########################################################################################################
#########################################################################################################
#########################################################################################################
DECISAO_banco_desistencia_CEDIPRO$decisao <- gsub(" ","", DECISAO_banco_desistencia_CEDIPRO$decisao)
#########################################################################################################

#preenchimento de celulas:
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "ABSOLVICAO"]<-	"ABSOLVIÇÃO"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "ADVERTENCIA"]<-	"ADVERTÊNCIA"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "ARQUIVAMENTO"]<-	"ARQUIVAMENTO"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "DECISAOINCONCLUSIVA"]<-	"VOUTROS"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "EXTINCAODOPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "EXTINCAOPORMORTE"]<-	"EXTINÇÃO POR MORTE"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "EXTINCAOPORPROCESSO"]<-	"EXTINÇÃO DO PROCESSO"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "INSTRUCAODOFEITO"]<-	"INSTRUÇÃO DO FEITO"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "INTERNACAO"]<-	"INTERNAÇÃO"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "INTERNACAOPROVISORIA"]<-	"INTERNAÇÃO PROVISÓRIA"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "INTERNACAOPROVISORIA/REGIMEDOMICILIAR"]<-	"INTERNAÇÃO PROVISÓRIA (REGIME DOMICILIAR)"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "JUSTICARESTAURATIVA"]<-	"JUSTIÇA RESTAURATIVA"
#DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "LA"]<-	"REMISSAO c/c LA"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "OUTRAS(OS)"]<-	"VOUTROS"
#DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "PSC"]<-	"REMISSAO c/c PSC"
#DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "PSC/LA"]<-	"REMISSAO c/c LA/PSC"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "REMESSAAOJUIZOCOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "REMESSAAOJUIZOCOMPETENTE-MAIORIDADE"]<-	"REMESSA AO JUÍZO COMPETENTE"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "REMESSACOMARCACOMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "REMETIDOSAUTOSJ.COMPETENTE"]<-	"REMESSA AO JUÍZO COMPETENTE"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "REMISSAOEXTINTIVA"]<-	"REMISSÃO EXTINTIVA"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "REMISSAOEXTINTIVA/ADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "REMISSAOEXTINTIVAc/cADVERTENCIA"]<-	"REMISSÃO EXTINTIVA c/c ADVERTÊNCIA"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "REMISSAOSUSPENSIVA-L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "REMISSAOSUSPENSIVA–L.A"]<-	"REMISSÃO SUSPENSIVA c/c LA"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "REMISSAOSUSPENSIVA-LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "REMISSAOSUSPENSIVA-PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "REMISSAOSUSPENSIVA-PSC/REPARACAODEDANO"]<-	"REMISSÃO SUSPENSIVA c/c PSC/REPARAÇÃO DE DANO"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "REMISSAOSUSPENSIVA-REPARACAODEDANO"]<- "REMISSÃO SUSPENSIVA c/c REPARAÇÃO DE DANO"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "REMISSAOSUSPENSIVA/LA"]<-	"REMISSÃO SUSPENSIVA c/c LA"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "REMISSAOSUSPENSIVA/LA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c LA/PSC"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "REMISSAOSUSPENSIVA/PSC"]<-	"REMISSÃO SUSPENSIVA c/c PSC"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "RESPONDERPROCESSOEMLIBERDADE"]<-	"RESPONDER EM LIBERDADE"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "ENTREGUEAOSRESPONSAVEIS"]<-	"RESPONDER EM LIBERDADE"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "RETORNOAINTERNACAO"]<-	"RETORNO A INTERNAÇÃO"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "RETORNOAOCEIP"]<-	"RETORNO AO CEIP"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "RETORNOASEMILIBERDADE"]<-	"RETORNO A SEMILIBERDADE"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "RETORNODOSAUTOSADELEGACIA"]<-	"RETORNO DOS AUTOS A DELEGACIA"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "SEMILIBERDADE"]<-	"SEMILIBERDADE"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "SEMINFORMACAO"]<-	"VAZIO"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "REMISSAO/ADVERTENCIA/REPARACAODEDANO"]<-	"REMISSÃO/ADVERTÊNCIA/REPARAÇÃO DE DANO"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "RETORNOAOCUMPRIMENTODEPSC"]<-	"RETORNO A PSC"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "RETORNOAPSC"]<-	"RETORNO A PSC"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "RETORNOALA"]<-	"RETORNO A LA"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "RETORNOALA/PSC"]<-	"RETORNO A LA/PSC"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "RETORNOAOCUMPRIMENTODELA/PSC"]<-	"RETORNO A LA/PSC"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "RETORNOAOCUMPRIMENTODEL.A"]<-	"RETORNO A LA"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "EXTINCAOPORPRESCRICAO"]<-	"EXTINÇÃO POR PRESCRIÇÃO"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "REMESSAAOJUIZOCOMPETENTE-MAIORIDADECONSTATADA"]<-	"REMESSA AO JUÍZO COMPETENTE"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "REMISSAO SUSPENSIVA c/c LA"]<-	"REMISSÃO c/c LA"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "REMISSAO SUSPENSIVA c/c PSC"]<-	"REMISSÃO c/c PSC"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "REMISSAOSUSPENSIVA/PSC/LA"]<-	"REMISSÃO c/c LA"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == ""]<-	"VAZIO"

#########################################################################################################
#########################################################################################################
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "VAZIO"]<-	"SEM INFORMAÇÃO"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "VOUTROS"]<-	"OUTROS"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
#########################################################################################################
#########################################################################################################
#########################################################################################################
# salvando para gráfico
DECISAO_banco_desistencia_CEDIPRO_bkp = DECISAO_banco_desistencia_CEDIPRO

DECISAO_banco_desistencia_CEDIPRO_bkp =
  DECISAO_banco_desistencia_CEDIPRO_bkp %>%
  janitor::tabyl(decisao) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"

colnames(DECISAO_banco_desistencia_CEDIPRO_bkp)[1]<-'DECISAO_banco_desistencia_CEDIPRO_bkp'
colnames(DECISAO_banco_desistencia_CEDIPRO_bkp)[2]<-'QUANTIDADE'
colnames(DECISAO_banco_desistencia_CEDIPRO_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem
#ordenando outros para final da tabela
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "OUTROS"]<-	"ROUTROS"
#########################################################################################################
DECISAO_banco_desistencia_CEDIPRO =
  DECISAO_banco_desistencia_CEDIPRO %>%
  janitor::tabyl(decisao) %>%
  arrange(decisao) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
DECISAO_banco_desistencia_CEDIPRO$decisao[DECISAO_banco_desistencia_CEDIPRO$decisao == "ROUTROS"]<- "OUTROS"

colnames(DECISAO_banco_desistencia_CEDIPRO)[1]<-'DECISÃO'
colnames(DECISAO_banco_desistencia_CEDIPRO)[2]<-'QUANTIDADE'
colnames(DECISAO_banco_desistencia_CEDIPRO)[3]<-'PERCENTUAL'

#############################################################################################################
# DECISAO_banco_desistencia_CEDIPRO =
#   DECISAO_banco_desistencia_CEDIPRO %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# DECISAO_banco_desistencia_CEDIPRO FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO PROTETIVAS_banco_desistencia_CEDIPRO
#########################################################################################################

PROTETIVAS_banco_desistencia_CEDIPRO =

  banco_desistencia_CEDIPRO %>%
  filter(medida_protetiva == "SIM")

PROTETIVAS_banco_desistencia_CEDIPRO =
  PROTETIVAS_banco_desistencia_CEDIPRO %>%
  pivot_longer(cols = starts_with("qual_medida_protetiva_0"), values_to = "MEDIDA_PROTETIVA") %>%
  #select(-name) %>%
  filter(MEDIDA_PROTETIVA != "" & MEDIDA_PROTETIVA != "NSA")

#########################################################################################################
PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA = ifelse(PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA == "1",
                                                               "ART. 101, I", PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA)

table(PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA)
PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA = ifelse(PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA == "2",
                                                               "ART. 101, II", PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA)

table(PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA)
PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA = ifelse(PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA == "3",
                                                               "ART. 101, III", PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA)

table(PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA)
PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA = ifelse(PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA == "4",
                                                               "ART. 101, IV", PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA)

table(PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA)
PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA = ifelse(PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA == "5",
                                                               "ART. 101, V", PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA)

table(PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA)
PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA = ifelse(PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA == "6",
                                                               "ART. 101, VI", PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA)

table(PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA)
PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA = ifelse(PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA == "7",
                                                               "ART. 101, VII", PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA)
#########################################################################################################
#########################################################################################################
#########################################################################################################
#SUBSTITUIR
#PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA[PROTETIVAS_banco_desistencia_CEDIPRO$MEDIDA_PROTETIVA == "SEM INFORMAÇÃO"]<- "ZSEM INFORMAÇÃO"
#########################################################################################################
#########################################################################################################
# salvando para gráfico
PROTETIVAS_banco_desistencia_CEDIPRO_bkp = PROTETIVAS_banco_desistencia_CEDIPRO

PROTETIVAS_banco_desistencia_CEDIPRO_bkp =
  PROTETIVAS_banco_desistencia_CEDIPRO_bkp %>%
  janitor::tabyl(MEDIDA_PROTETIVA) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:

colnames(PROTETIVAS_banco_desistencia_CEDIPRO_bkp)[1]<-'PROTETIVAS_banco_desistencia_CEDIPRO_bkp'
colnames(PROTETIVAS_banco_desistencia_CEDIPRO_bkp)[2]<-'QUANTIDADE'
colnames(PROTETIVAS_banco_desistencia_CEDIPRO_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

PROTETIVAS_banco_desistencia_CEDIPRO =
  PROTETIVAS_banco_desistencia_CEDIPRO %>%
  janitor::tabyl(MEDIDA_PROTETIVA) %>%
  arrange(desc(n)) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:

colnames(PROTETIVAS_banco_desistencia_CEDIPRO)[1]<-'MEDIDA PROTETIVA'
colnames(PROTETIVAS_banco_desistencia_CEDIPRO)[2]<-'QUANTIDADE'
colnames(PROTETIVAS_banco_desistencia_CEDIPRO)[3]<-'PERCENTUAL'

#############################################################################################################
# PROTETIVAS_banco_desistencia_CEDIPRO =
#   PROTETIVAS_banco_desistencia_CEDIPRO %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
#########################################################################################################
#TRATAMENTO PROTETIVAS_banco_desistencia_CEDIPRO FIM
#########################################################################################################
#########################################################################################################
###REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/escola"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO = banco_desistencia_CEDIPRO
#regional_ato_geral_bkp = regional_ato_geral
#regional_ato02 = banco_sem_mba %>%
# select(SEXO, DATA_ATO, DATA_AUDIENCIA_PRELIMINAR, SENTENCA, DATA_SENTENCA, regional_ato, regional_ato_PROTETIVA, QUAL_regional_ato_PROTETIVA_01,
#       QUAL_regional_ato_PROTETIVA_02, QUAL_regional_ato_PROTETIVA_03, COMPARECIMENTO_AUD_PRELIMINAR)
#########################################################################################################
#########################################################################################################
#AGREGAR RMBH
REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO$regional_residencial <- str_replace(REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO$regional_residencial, "RMBH.*", "ZREGIÃO METROPOLITANA")

#SUBSTITUIR
REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO$regional_residencial[REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO$regional_residencial == ""]<- "ZSEM INFORMAÇÃO"


#########################################################################################################
#########################################################################################################
# salvando para gráfico
REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp = REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO

REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp =
  REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp %>%
  janitor::tabyl(regional_residencial) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:
#SUBSTITUIR
REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp$regional_residencial[REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp$regional_residencial == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp$regional_residencial[REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp$regional_residencial == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"

colnames(REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp)[1]<-'REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp'
colnames(REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp)[2]<-'QUANTIDADE'
colnames(REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO =
  REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO %>%
  janitor::tabyl(regional_residencial) %>%
  arrange(regional_residencial) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
#SUBSTITUIR
REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO$regional_residencial[REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO$regional_residencial == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO$regional_residencial[REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO$regional_residencial == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"



colnames(REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO)[1]<-'REGIONAL'
colnames(REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO)[2]<-'QUANTIDADE'
colnames(REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO)[3]<-'PERCENTUAL'

#############################################################################################################

#REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO =
#  REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO FIM
#########################################################################################################
#########################################################################################################
###REGIONAL_ATO_banco_desistencia_CEDIPRO
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/escola"))
#########################################################################################################
#SEPARANDO SOMENTE VARIAVEIS NECESSARIAS PARA AGILIZAR TRATAMENTO:
library(dplyr)

REGIONAL_ATO_banco_desistencia_CEDIPRO = banco_desistencia_CEDIPRO
#regional_ato_geral_bkp = regional_ato_geral
#regional_ato02 = banco_sem_mba %>%
# select(SEXO, DATA_ATO, DATA_AUDIENCIA_PRELIMINAR, SENTENCA, DATA_SENTENCA, regional_ato, regional_ato_PROTETIVA, QUAL_regional_ato_PROTETIVA_01,
#       QUAL_regional_ato_PROTETIVA_02, QUAL_regional_ato_PROTETIVA_03, COMPARECIMENTO_AUD_PRELIMINAR)
#########################################################################################################
#########################################################################################################
#AGREGAR RMBH
REGIONAL_ATO_banco_desistencia_CEDIPRO$regional_ato <- str_replace(REGIONAL_ATO_banco_desistencia_CEDIPRO$regional_ato, "RMBH.*", "ZREGIÃO METROPOLITANA")

#SUBSTITUIR
REGIONAL_ATO_banco_desistencia_CEDIPRO$regional_ato[REGIONAL_ATO_banco_desistencia_CEDIPRO$regional_ato == ""]<- "ZSEM INFORMAÇÃO"

#########################################################################################################
#########################################################################################################
# salvando para gráfico
REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp = REGIONAL_ATO_banco_desistencia_CEDIPRO

REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp =
  REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp %>%
  janitor::tabyl(regional_ato) %>%
  arrange(n) %>%
  #arrange(desc(n)) %>%
  #janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

# Adaptando para scrip grafico:

#SUBSTITUIR
REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp$regional_ato[REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp$regional_ato == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp$regional_ato[REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp$regional_ato == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"


colnames(REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp)[1]<-'REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp'
colnames(REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp)[2]<-'QUANTIDADE'
colnames(REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp)[3]<-'PERCENTUAL'
#########################################################################################################
# Fazer uma tabela de frequência com valores totais,
# e porcentagem

REGIONAL_ATO_banco_desistencia_CEDIPRO =
  REGIONAL_ATO_banco_desistencia_CEDIPRO %>%
  janitor::tabyl(regional_ato) %>%
  arrange(regional_ato) %>%
  janitor::adorn_totals() %>%
  adorn_pct_formatting(digits = 2)
#########################################################################################################

# Adaptando:
REGIONAL_ATO_banco_desistencia_CEDIPRO$regional_ato[REGIONAL_ATO_banco_desistencia_CEDIPRO$regional_ato == "ZSEM INFORMAÇÃO"]<- "SEM INFORMAÇÃO"
REGIONAL_ATO_banco_desistencia_CEDIPRO$regional_ato[REGIONAL_ATO_banco_desistencia_CEDIPRO$regional_ato == "ZREGIÃO METROPOLITANA"]<- "REGIÃO METROPOLITANA"



colnames(REGIONAL_ATO_banco_desistencia_CEDIPRO)[1]<-'REGIONAL'
colnames(REGIONAL_ATO_banco_desistencia_CEDIPRO)[2]<-'QUANTIDADE'
colnames(REGIONAL_ATO_banco_desistencia_CEDIPRO)[3]<-'PERCENTUAL'

#############################################################################################################

#REGIONAL_ATO_banco_desistencia_CEDIPRO =
#  REGIONAL_ATO_banco_desistencia_CEDIPRO %>%
#  mutate(PERCENTUAL = PERCENTUAL*100)%>%
#  mutate(PERCENTUAL = sprintf("%.2f", PERCENTUAL))


#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
# REGIONAL_ATO_banco_desistencia_CEDIPRO FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_desistencia_CEDIPRO FIM
#########################################################################################################
