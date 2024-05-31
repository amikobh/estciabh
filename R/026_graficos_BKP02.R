#########################################################################################################
#pdf(file="TABELA_002_incidencia_comparada_alternativa.pdf", width = 5, height = 7, title = "INCIDENCIA COMPARADA")
setwd(file.path("~/diretorio_r/estciabh/img_nao_apagar/"))

GRAFICO <- read.csv("grafico.csv",header=TRUE, sep="|", dec=".", encoding = "UTF-8", skip = 0) ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.

#TABELA GT
#TABELA GT

#########################################################################################################
dir.create(file.path("~/diretorio_r/estciabh", "imagens"))
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#########################################################################################################
#########################################################################################################
#Atendimento ao adolescente
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))

legend_title <- ""
ggplot(data=df_atendimento, aes(x=ANO, y=QUANTIDADE, group=TIPO)) +
  geom_line(aes(color=TIPO) ,  linewidth = 1.2)+
  geom_point(aes(color=TIPO), size=1.5)+
  #scale_color_brewer(palette="Dark2")
  scale_color_brewer(legend_title, palette="Set1")+
  geom_label_repel(aes(label = QUANTIDADE),box.padding = unit(0.5, "lines"), max.overlaps = Inf, label.size = 0.1, size = 3 )+
  labs(title = (str_c(GRAFICO[1,],": Atendimento de adolescentes, Belo Horizonte, 2015 a ", format(Sys.Date()-365*1, "%Y"))),
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "QUANTIDADE") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  theme(legend.position = "bottom")
ggsave("GRAFICO[1,].png", width=10, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
SINAL <- paste(banco_incidencia_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_banco_incidencia_bkp_alternativo.pdf", title = "grafico_banco_incidencia_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

banco_incidencia_bkp =
  banco_incidencia_bkp |>
  mutate(banco_incidencia_bkp = fct_reorder(banco_incidencia_bkp, QUANTIDADE))

ggplot(banco_incidencia_bkp, aes(x = banco_incidencia_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[2,],": Incidência Atos Infracionais, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       #subtitle = "Adolescentes por Atos Infracionais",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 1000))
ggsave("GRAFICO[2,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#Perfil dos adolescentes atendidos
#########################################################################################################
#GRAFICO IDADE/SEXO

#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))

ggplot(banco_GERAL_snr_SEXO_IDADE_pizza, aes(fill=SEXO, y=QUANTIDADE, x=IDADE)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = (str_c(GRAFICO[3,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       #subtitle = "Letalidade",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "", fill = "SEXO") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="plain", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  theme(legend.position = "right") +
  geom_text(aes(label=QUANTIDADE), vjust=0, color="red", fontface = "plain",
            position = position_dodge(0.9), size=3.5)

ggsave("GRAFICO[3,].png", width=13, height=5, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))
mycols <- c("#c0504d", "#4f81bd")

#salvar png
ggpie(banco_GERAL_snr_SEXO_IDADE_graf_pizza,
      x= "QUANTIDADE", label = "PERCENTUAL2",
      lab.pos = "in", lab.font = list(color = "white", face = "bold"),
      lab.adjust = 0,
      fill = "SEXO", color = "white", face="bold",
      palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  labs(caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD") +
  ggtitle((str_c(GRAFICO[4,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))))
ggsave("GRAFICO[4,].png", width=6, height=5, pointsize=12, dpi = 512)
#dev.off()

setwd(file.path("~/diretorio_r/estciabh/imagens"))
#########################################################################################################
#########################################################################################################
SINAL <- paste(df_snr_regional_residencia_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_df_snr_regional_residencia_bkp_alternativo.pdf", title = "grafico_df_snr_regional_residencia_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

df_snr_regional_residencia_bkp =
  df_snr_regional_residencia_bkp |>
  mutate(df_snr_regional_residencia_bkp = fct_reorder(df_snr_regional_residencia_bkp, QUANTIDADE))

ggplot(df_snr_regional_residencia_bkp, aes(x = df_snr_regional_residencia_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[5,],": Regional de Residência, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       #subtitle = "Adolescentes por REGIONAL",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "REGIONAL", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 320))
ggsave("GRAFICO[5,].png", width=10, height=4, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#Total de mandados de busca e apreensão
#########################################################################################################
#GRAFICO IDADE/SEXO
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))

ggplot(df_snr_sexo_MBA_idade_MBA, aes(fill=SEXO, y=QUANTIDADE, x=IDADE)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = (str_c(GRAFICO[6,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "MBAs cumpridos",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       x = "", y = "", fill = "SEXO") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="plain", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  theme(legend.position = "right") +
  geom_text(aes(label=QUANTIDADE), vjust=0, color="red", fontface = "plain",
            position = position_dodge(0.9), size=3.5)

ggsave("GRAFICO[6,].png", width=13, height=5, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))

#salvar png
ggpie(df_snr_sexo_MBA,
      x= "QUANTIDADE", label = "PERCENTUAL",
      lab.pos = "out", lab.font = list(color = "black", face = "plain"),
      lab.adjust = 0,
      fill = "SEXO", color = "white", face="bold",
      palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  labs(caption = "FONTE: VARA INFRACIONAL/COMISSARIADO", subtitle = "MBAs cumpridos") +
  ggtitle((str_c(GRAFICO[7,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))))
ggsave("GRAFICO[7,].png", width=6.5, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
SINAL <- paste(df_snr_regional_residencia_MBA_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_df_snr_regional_residencia_MBA_bkp_alternativo.pdf", title = "grafico_df_snr_regional_residencia_MBA_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

df_snr_regional_residencia_MBA_bkp =
  df_snr_regional_residencia_MBA_bkp |>
  mutate(df_snr_regional_residencia_MBA_bkp = fct_reorder(df_snr_regional_residencia_MBA_bkp, QUANTIDADE))

ggplot(df_snr_regional_residencia_MBA_bkp, aes(x = df_snr_regional_residencia_MBA_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[8,],": Regional de Residência, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "MBAs cumpridos",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "REGIONAL", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 32))
ggsave("GRAFICO[8,].png", width=10, height=4, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
SINAL <- paste(MOTIVO_MBA_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_MOTIVO_MBA_bkp_alternativo.pdf", title = "grafico_MOTIVO_MBA_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

MOTIVO_MBA_bkp =
  MOTIVO_MBA_bkp |>
  mutate(MOTIVO_MBA_bkp = fct_reorder(MOTIVO_MBA_bkp, QUANTIDADE))

ggplot(MOTIVO_MBA_bkp, aes(x = MOTIVO_MBA_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[9,],": Motivo do MBA, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "MBAs cumpridos",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       x = "MOTIVO MBA", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 95))
ggsave("GRAFICO[9,].png", width=10, height=6, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
SINAL <- paste(banco_ato_MBA_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_banco_ato_MBA_bkp_alternativo.pdf", title = "grafico_banco_ato_MBA_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

banco_ato_MBA_bkp =
  banco_ato_MBA_bkp |>
  mutate(banco_ato_MBA_bkp = fct_reorder(banco_ato_MBA_bkp, QUANTIDADE))

ggplot(banco_ato_MBA_bkp, aes(x = banco_ato_MBA_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[10,],": Atos infracionais atribuídos aos adolescentes encaminhados por MBA, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "MBAs cumpridos",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 86))
ggsave("GRAFICO[10,].png", width=12, height=6, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#ATOS EM FOCO: HOMICÍDIO
#########################################################################################################


#########################################################################################################
#########################################################################################################
#########################################################################################################
#GRAFICO IDADE/SEXO
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))

ggplot(df_snr_sexo_idade_HOMICIDIO, aes(fill=SEXO, y=QUANTIDADE, x=IDADE)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = (str_c(GRAFICO[11,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "HOMICÍDIO",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "", fill = "SEXO") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="plain", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  theme(legend.position = "right") +
  geom_text(aes(label=QUANTIDADE), vjust=0, color="red", fontface = "plain",
            position = position_dodge(0.9), size=3.5)

ggsave("GRAFICO[11,].png", width=13, height=5, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))

#salvar png
ggpie(df_sexo_HOMICIDIO,
      x= "QUANTIDADE", label = "PERCENTUAL",
      lab.pos = "in", lab.font = list(color = "white", face = "bold"),
      lab.adjust = 0,
      fill = "SEXO", color = "white", face="bold",
      palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  labs(subtitle = "HOMICÍDIO",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD") +
  ggtitle((str_c(GRAFICO[12,],": Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))))

ggsave("GRAFICO[12,].png", width=6, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#ATOS EM FOCO: ROUBO
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#GRAFICO IDADE/SEXO
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))

ggplot(df_snr_sexo_idade_ROUBO, aes(fill=SEXO, y=QUANTIDADE, x=IDADE)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = (str_c(GRAFICO[13,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "ROUBO",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "", fill = "SEXO") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="plain", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  theme(legend.position = "right") +
  geom_text(aes(label=QUANTIDADE), vjust=0, color="red", fontface = "plain",
            position = position_dodge(0.9), size=3.5)

ggsave("GRAFICO[13,].png", width=13, height=5, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
ggpie(df_sexo_ROUBO,
      x= "QUANTIDADE", label = "PERCENTUAL",
      lab.pos = "in", lab.font = list(color = "white", face = "bold"),
      lab.adjust = 0,
      fill = "SEXO", color = "white", face="bold",
      palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  labs(subtitle = "ROUBO",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD") +
  ggtitle((str_c(GRAFICO[14,],": Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))))

ggsave("GRAFICO[14,].png", width=6, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#ATOS EM FOCO: FURTO
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#GRAFICO IDADE/SEXO
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))

ggplot(df_snr_sexo_idade_FURTO, aes(fill=SEXO, y=QUANTIDADE, x=IDADE)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = (str_c(GRAFICO[15,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "FURTO",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "", fill = "SEXO") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="plain", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  theme(legend.position = "right") +
  geom_text(aes(label=QUANTIDADE), vjust=0, color="red", fontface = "plain",
            position = position_dodge(0.9), size=3.5)

ggsave("GRAFICO[15,].png", width=13, height=5, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
ggpie(df_sexo_FURTO,
      x= "QUANTIDADE", label = "PERCENTUAL",
      lab.pos = "in", lab.font = list(color = "white", face = "bold"),
      lab.adjust = 0,
      fill = "SEXO", color = "white", face="bold",
      palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  labs(subtitle = "FURTO",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD") +
  ggtitle((str_c(GRAFICO[16,],": Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))))

ggsave("GRAFICO[16,].png", width=6, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#ATOS EM FOCO: USO_DE_DROGAS
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#GRAFICO IDADE/SEXO
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))

ggplot(df_snr_sexo_idade_USO_DE_DROGAS, aes(fill=SEXO, y=QUANTIDADE, x=IDADE)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = (str_c(GRAFICO[17,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "POSSE DE DROGAS PARA USO PESSOAL",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "", fill = "SEXO") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="plain", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  theme(legend.position = "right") +
  geom_text(aes(label=QUANTIDADE), vjust=0, color="red", fontface = "plain",
            position = position_dodge(0.9), size=3.5)

ggsave("GRAFICO[17,].png", width=13, height=5, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
ggpie(df_sexo_USO_DE_DROGAS,
      x= "QUANTIDADE", label = "PERCENTUAL",
      lab.pos = "in", lab.font = list(color = "white", face = "bold"),
      lab.adjust = 0,
      fill = "SEXO", color = "white", face="bold",
      palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  labs(subtitle = "POSSE DE DROGAS PARA USO PESSOAL",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD") +
  ggtitle((str_c(GRAFICO[18,],": Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))))

ggsave("GRAFICO[18,].png", width=6, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#ATOS EM FOCO: TRAFICO_DE_DROGAS
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#GRAFICO IDADE/SEXO
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))

ggplot(df_snr_sexo_idade_TRAFICO_DE_DROGAS, aes(fill=SEXO, y=QUANTIDADE, x=IDADE)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = (str_c(GRAFICO[19,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "TRAFICO DE DROGAS",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "", fill = "SEXO") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="plain", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  theme(legend.position = "right") +
  geom_text(aes(label=QUANTIDADE), vjust=0, color="red", fontface = "plain",
            position = position_dodge(0.9), size=3.5)

ggsave("GRAFICO[19,].png", width=13, height=5, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################

setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
ggpie(df_sexo_TRAFICO_DE_DROGAS,
      x= "QUANTIDADE", label = "PERCENTUAL",
      lab.pos = "out", lab.font = list(color = "black", face = "bold"),
      lab.adjust = 0,
      fill = "SEXO", color = "white", face="bold",
      palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  labs(subtitle = "TRÁFICO DE DROGAS",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD") +
  ggtitle((str_c(GRAFICO[20,],": Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))))

ggsave("GRAFICO[20,].png", width=6, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#Dados socioeconômicos
#########################################################################################################
#Raça/Cor
#########################################################################################################
#########################################################################################################
#########################################################################################################
#SINAL <- paste(RACA_COR_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(RACA_COR_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/diretorio_r/estciabh/imagens"))
ggplot(data=RACA_COR_bkp,
       aes(x = QUANTIDADE,
           y = reorder(RACA_COR_bkp, PERCENTUAL2))) +
  geom_bar(stat="identity", fill="#bb1e23")+
  labs(title = (str_c(GRAFICO[21,],": Raça/Cor, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Dados socioeconômicos",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "RAÇA/COR") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        plot.caption =element_text(hjust = 0.5)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  scale_x_continuous(n.breaks=10)
  #scale_x_continuous(limits=c(0, 90))
ggsave("GRAFICO[21,].png", width=14, height=4, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#SINAL <- paste(ESTADO_CIVIL_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(ESTADO_CIVIL_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/diretorio_r/estciabh/imagens"))
ggplot(data=ESTADO_CIVIL_bkp,
       aes(x = QUANTIDADE,
           y = reorder(ESTADO_CIVIL_bkp, PERCENTUAL2))) +
  geom_bar(stat="identity", fill="#bb1e23")+
  labs(title = (str_c(GRAFICO[22,],": Estado Civil, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Dados socioeconômicos",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "ESTADO CIVIL") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        plot.caption =element_text(hjust = 0.5)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  scale_x_continuous(n.breaks=10)
#scale_x_continuous(limits=c(0, 90))
ggsave("GRAFICO[22,].png", width=15, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#SINAL <- paste(POSSUI_FILHOS_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(POSSUI_FILHOS_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/diretorio_r/estciabh/imagens"))
ggplot(data=POSSUI_FILHOS_bkp,
       aes(x = QUANTIDADE,
           y = reorder(POSSUI_FILHOS_bkp, PERCENTUAL2))) +
  geom_bar(stat="identity", fill="#bb1e23")+
  labs(title = (str_c(GRAFICO[23,],": Adolescente Pai ou Mãe, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Dados socioeconômicos",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "POSSUI FILHOS?") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        plot.caption =element_text(hjust = 0.5)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  scale_x_continuous(n.breaks=5)
#scale_x_continuous(limits=c(0, 90))
ggsave("GRAFICO[23,].png", width=15, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#SINAL <- paste(ESTA_GRAVIDA_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(ESTA_GRAVIDA_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/diretorio_r/estciabh/imagens"))
ggplot(data=ESTA_GRAVIDA_bkp,
       aes(x = QUANTIDADE,
           y = reorder(ESTA_GRAVIDA_bkp, PERCENTUAL2))) +
  geom_bar(stat="identity", fill="#bb1e23")+
  labs(title = (str_c(GRAFICO[24,],": Adolescente Grávida, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Dados socioeconômicos",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "ESTÁ GRÁVIDA?") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        plot.caption =element_text(hjust = 0.5)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  scale_x_continuous(n.breaks=5)
#scale_x_continuous(limits=c(0, 90))
ggsave("GRAFICO[24,].png", width=15, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################

#########################################################################################################
#SINAL <- paste(POSSUI_DOC_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(POSSUI_DOC_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/diretorio_r/estciabh/imagens"))
ggplot(data=POSSUI_DOC_bkp,
       aes(x = QUANTIDADE,
           y = reorder(POSSUI_DOC_bkp, PERCENTUAL2))) +
  geom_bar(stat="identity", fill="#bb1e23")+
  labs(title = (str_c(GRAFICO[25,],": Documentação, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Dados socioeconômicos",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "DOCUMENTOS") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        plot.caption =element_text(hjust = 0.5)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  scale_x_continuous(n.breaks=5)
#scale_x_continuous(limits=c(0, 90))
ggsave("GRAFICO[25,].png", width=15, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
SINAL <- paste(SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp_alternativo.pdf", title = "grafico_SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp =
  SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp |>
  mutate(SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp = fct_reorder(SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp, QUANTIDADE))

ggplot(SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp, aes(x = SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[26,],": Escolaridade, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Dados socioeconômicos",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 165))
ggsave("GRAFICO[26,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#SINAL <- paste(NATUREZA_ESCOLA_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(NATUREZA_ESCOLA_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/diretorio_r/estciabh/imagens"))
ggplot(data=NATUREZA_ESCOLA_bkp,
       aes(x = QUANTIDADE,
           y = reorder(NATUREZA_ESCOLA_bkp, PERCENTUAL2))) +
  geom_bar(stat="identity", fill="#bb1e23")+
  labs(title = (str_c(GRAFICO[27,],": Natureza da Escola, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Dados socioeconômicos",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "NATUREZA DA ESCOLA") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        plot.caption =element_text(hjust = 0.5)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  scale_x_continuous(n.breaks=5)
#scale_x_continuous(limits=c(0, 90))
ggsave("GRAFICO[27,].png", width=15, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#SINAL <- paste(TRABALHA_ATUALMENTE_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(TRABALHA_ATUALMENTE_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/diretorio_r/estciabh/imagens"))
ggplot(data=TRABALHA_ATUALMENTE_bkp,
       aes(x = QUANTIDADE,
           y = reorder(TRABALHA_ATUALMENTE_bkp, PERCENTUAL2))) +
  geom_bar(stat="identity", fill="#bb1e23")+
  labs(title = (str_c(GRAFICO[28,],": Trabalho atual, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Dados socioeconômicos",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "TRABALHO ATUAL") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        plot.caption =element_text(hjust = 0.5)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  scale_x_continuous(n.breaks=5)
#scale_x_continuous(limits=c(0, 90))
ggsave("GRAFICO[28,].png", width=15, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
SINAL <- paste(RENDA_MENSAL_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_RENDA_MENSAL_bkp_alternativo.pdf", title = "grafico_RENDA_MENSAL_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

RENDA_MENSAL_bkp =
  RENDA_MENSAL_bkp |>
  mutate(RENDA_MENSAL_bkp = fct_reorder(RENDA_MENSAL_bkp, QUANTIDADE))

ggplot(RENDA_MENSAL_bkp, aes(x = RENDA_MENSAL_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[29,],": Renda Mensal, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Dados socioeconômicos",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 140))
ggsave("GRAFICO[29,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
SINAL <- paste(RENDA_FAMILIAR_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_RENDA_FAMILIAR_bkp_alternativo.pdf", title = "grafico_RENDA_FAMILIAR_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

RENDA_FAMILIAR_bkp =
  RENDA_FAMILIAR_bkp |>
  mutate(RENDA_FAMILIAR_bkp = fct_reorder(RENDA_FAMILIAR_bkp, QUANTIDADE))

ggplot(RENDA_FAMILIAR_bkp, aes(x = RENDA_FAMILIAR_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[30,],": Renda Familiar, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Dados socioeconômicos",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 185))
ggsave("GRAFICO[30,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
SINAL <- paste(TIPO_MORADIA_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_TIPO_MORADIA_bkp_alternativo.pdf", title = "grafico_TIPO_MORADIA_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

TIPO_MORADIA_bkp =
  TIPO_MORADIA_bkp |>
  mutate(TIPO_MORADIA_bkp = fct_reorder(TIPO_MORADIA_bkp, QUANTIDADE))

ggplot(TIPO_MORADIA_bkp, aes(x = TIPO_MORADIA_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[31,],": Tipo de moradia, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Dados socioeconômicos",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 470))
ggsave("GRAFICO[31,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
SINAL <- paste(NATUREZA_MORADIA_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_NATUREZA_MORADIA_bkp_alternativo.pdf", title = "grafico_NATUREZA_MORADIA_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

NATUREZA_MORADIA_bkp =
  NATUREZA_MORADIA_bkp |>
  mutate(NATUREZA_MORADIA_bkp = fct_reorder(NATUREZA_MORADIA_bkp, QUANTIDADE))

ggplot(NATUREZA_MORADIA_bkp, aes(x = NATUREZA_MORADIA_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[32,],": Natureza da Propriedade, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Dados socioeconômicos",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 356))
ggsave("GRAFICO[32,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
SINAL <- paste(DROGAS_USO_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_DROGAS_USO_bkp_alternativo.pdf", title = "grafico_DROGAS_USO_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

DROGAS_USO_bkp =
  DROGAS_USO_bkp |>
  mutate(DROGAS_USO_bkp = fct_reorder(DROGAS_USO_bkp, QUANTIDADE))

ggplot(DROGAS_USO_bkp, aes(x = DROGAS_USO_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[33,],": Uso de drogas, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Dados socioeconômicos",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 318))
ggsave("GRAFICO[33,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

#########################################################################################################
#Medidas protetivas
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
SINAL <- paste(BANCO_MEDIDAS_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_BANCO_MEDIDAS_bkp_alternativo.pdf", title = "grafico_BANCO_MEDIDAS_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

BANCO_MEDIDAS_bkp =
  BANCO_MEDIDAS_bkp |>
  mutate(BANCO_MEDIDAS_bkp = fct_reorder(BANCO_MEDIDAS_bkp, QUANTIDADE))

ggplot(BANCO_MEDIDAS_bkp, aes(x = BANCO_MEDIDAS_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[34,],": Medidas Protetivas, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       #subtitle = "Adolescentes por MEDIDAs Infracionais",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 620))
ggsave("GRAFICO[34,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
SINAL <- paste(so_decisao_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_so_decisao_bkp_alternativo.pdf", title = "grafico_so_decisao_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

so_decisao_bkp =
  so_decisao_bkp |>
  mutate(so_decisao_bkp = fct_reorder(so_decisao_bkp, QUANTIDADE))

ggplot(so_decisao_bkp, aes(x = so_decisao_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[35,],": Decisão em Audiência Preliminar, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       #subtitle = "Adolescentes por MEDIDAs Infracionais",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 620))
ggsave("GRAFICO[35,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
SINAL <- paste(intervalo_decisao_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_intervalo_decisao_bkp_alternativo.pdf", title = "grafico_intervalo_decisao_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

intervalo_decisao_bkp =
  intervalo_decisao_bkp |>
  mutate(intervalo_decisao_bkp = fct_reorder(intervalo_decisao_bkp, QUANTIDADE))

ggplot(intervalo_decisao_bkp, aes(x = intervalo_decisao_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[36,],": Tempo das decisões em audiências preliminares, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       #subtitle = (str_c("Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 1490))
ggsave("GRAFICO[36,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
SINAL <- paste(so_sentenca_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_intervalo_sentenca_bkp_alternativo.pdf", title = "grafico_intervalo_sentenca_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

so_sentenca_bkp =
  so_sentenca_bkp |>
  mutate(so_sentenca_bkp = fct_reorder(so_sentenca_bkp, QUANTIDADE))

ggplot(so_sentenca_bkp, aes(x = so_sentenca_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[37,],": Sentenças, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       #subtitle = "Adolescentes por MEDIDAs Infracionais",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 245))
ggsave("GRAFICO[37,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
SINAL <- paste(intervalo_sentenca_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_intervalo_sentenca_bkp_alternativo.pdf", title = "grafico_intervalo_sentenca_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

intervalo_sentenca_bkp =
  intervalo_sentenca_bkp |>
  mutate(intervalo_sentenca_bkp = fct_reorder(intervalo_sentenca_bkp, QUANTIDADE))

ggplot(intervalo_sentenca_bkp, aes(x = intervalo_sentenca_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[38,],": Tempo das decisões após audiências preliminares, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       #subtitle = (str_c("Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 500))
ggsave("GRAFICO[38,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#Atos infracionais nas escolas de Belo Horizonte
#########################################################################################################
#########################################################################################################
#GRAFICO IDADE/SEXO
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))

ggplot(banco_ESCOLA_snr_SEXO_IDADE_pizza, aes(fill=SEXO, y=QUANTIDADE, x=IDADE)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = (str_c(GRAFICO[39,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Ato infracional nas escolas",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "", fill = "SEXO") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="plain", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  theme(legend.position = "right") +
  geom_text(aes(label=QUANTIDADE), vjust=0, color="red", fontface = "plain",
            position = position_dodge(0.9), size=3.5)

ggsave("GRAFICO[39,].png", width=13, height=5, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))
mycols <- c("#c0504d", "#4f81bd")

#salvar png
ggpie(banco_ESCOLA_snr_SEXO_IDADE_graf_pizza,
      x= "QUANTIDADE", label = "PERCENTUAL2",
      lab.pos = "in", lab.font = list(color = "white", face = "bold"),
      lab.adjust = 0,
      fill = "SEXO", color = "white", face="bold",
      palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "plain"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  labs(caption = "FONTE: VARA INFRACIONAL/COMISSARIADO", subtitle = "Ato infracional nas escolas") +
  ggtitle((str_c(GRAFICO[40,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))))
ggsave("GRAFICO[40,].png", width=6.5, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
#SINAL <- paste(ESCOLARIDADE_banco_escola_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(ESCOLARIDADE_banco_escola_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/diretorio_r/estciabh/imagens"))
ggplot(data=ESCOLARIDADE_banco_escola_bkp,
       aes(x = QUANTIDADE,
           y = reorder(ESCOLARIDADE_banco_escola_bkp, PERCENTUAL2))) +
  geom_bar(stat="identity", fill="#bb1e23")+
  labs(title = (str_c(GRAFICO[41,],": Escolaridade, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Ato infracional nas escolas",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       x = "", y = "ESCOLARIDADE") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "plain"),
        plot.caption =element_text(hjust = 0.5)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  scale_x_continuous(n.breaks=5)
#scale_x_continuous(limits=c(0, 90))
ggsave("GRAFICO[41,].png", width=15, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
SINAL <- paste(banco_ESCOLA_incidencia_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_intervalo_sentenca_bkp_alternativo.pdf", title = "grafico_intervalo_sentenca_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

banco_ESCOLA_incidencia_bkp =
  banco_ESCOLA_incidencia_bkp |>
  mutate(banco_ESCOLA_incidencia_bkp = fct_reorder(banco_ESCOLA_incidencia_bkp, QUANTIDADE))

ggplot(banco_ESCOLA_incidencia_bkp, aes(x = banco_ESCOLA_incidencia_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[42,],": Incidência atos infracionais, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Ato infracional nas escolas",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 45))
ggsave("GRAFICO[42,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################

setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
ggpie(banco_ESCOLA_primariedade_bkp,
      x= "QUANTIDADE", label = "PERCENTUAL",
      lab.pos = "out", lab.font = list(color = "black", face = "plain"),
      lab.adjust = 5,
      fill = "banco_ESCOLA_primariedade_bkp", color = "white", face="bold",
      palette = "Set1") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  labs(caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       subtitle = "Ato infracional nas escolas",
       fill= "PRIMÁRIO") +
  ggtitle((str_c(GRAFICO[43,],": Primariedade, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))))
ggsave("GRAFICO[43,].png", width=6.5, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
SINAL <- paste(banco_ESCOLA_decisao_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_intervalo_sentenca_bkp_alternativo.pdf", title = "grafico_intervalo_sentenca_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

banco_ESCOLA_decisao_bkp =
  banco_ESCOLA_decisao_bkp |>
  mutate(banco_ESCOLA_decisao_bkp = fct_reorder(banco_ESCOLA_decisao_bkp, QUANTIDADE))

ggplot(banco_ESCOLA_decisao_bkp, aes(x = banco_ESCOLA_decisao_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[44,],": Decisão, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Ato infracional nas escolas",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 90))
ggsave("GRAFICO[44,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################

setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
ggpie(banco_ESCOLA_vitima_bkp,
      x= "QUANTIDADE", label = "PERCENTUAL",
      lab.pos = "out", lab.font = list(color = "black", face = "plain"),
      lab.adjust = 5,
      fill = "banco_ESCOLA_vitima_bkp", color = "white", face="plain",
      palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "plain"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  labs(caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       subtitle = "Ato infracional nas escolas",
       fill= "VÍTIMA") +
  ggtitle((str_c(GRAFICO[45,],": Primariedade, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))))
ggsave("GRAFICO[45,].png", width=6.5, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
SINAL <- paste(banco_ESCOLA_regional_residencia_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_intervalo_sentenca_bkp_alternativo.pdf", title = "grafico_intervalo_sentenca_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

banco_ESCOLA_regional_residencia_bkp =
  banco_ESCOLA_regional_residencia_bkp |>
  mutate(banco_ESCOLA_regional_residencia_bkp = fct_reorder(banco_ESCOLA_regional_residencia_bkp, QUANTIDADE))

ggplot(banco_ESCOLA_regional_residencia_bkp, aes(x = banco_ESCOLA_regional_residencia_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[46,],": Regional de Residência, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Ato infracional nas escolas",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 30))
ggsave("GRAFICO[46,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################

setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
ggpie(banco_ESCOLA_tipo_escola_bkp,
      x= "QUANTIDADE", label = "PERCENTUAL",
      lab.pos = "out", lab.font = list(color = "black", face = "plain"),
      lab.adjust = 5,
      fill = "banco_ESCOLA_tipo_escola_bkp", color = "white", face="plain",
      palette = "Set1") +
  theme(legend.position = "top",
        legend.text = element_text(size = 8, face = "plain"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  labs(caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       subtitle = "Ato infracional nas escolas",
       fill= "ESCOLA") +
  ggtitle((str_c(GRAFICO[47,],": Primariedade, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))))
ggsave("GRAFICO[47,].png", width=6.5, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
SINAL <- paste(banco_ESCOLA_regional_ato_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_intervalo_sentenca_bkp_alternativo.pdf", title = "grafico_intervalo_sentenca_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

banco_ESCOLA_regional_ato_bkp =
  banco_ESCOLA_regional_ato_bkp |>
  mutate(banco_ESCOLA_regional_ato_bkp = fct_reorder(banco_ESCOLA_regional_ato_bkp, QUANTIDADE))

ggplot(banco_ESCOLA_regional_ato_bkp, aes(x = banco_ESCOLA_regional_ato_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[48,],": Regional da escola, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Ato infracional nas escolas",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 31))
ggsave("GRAFICO[48,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#Justiça restaurativa
#########################################################################################################
#########################################################################################################
#GRAFICO IDADE/SEXO
#########################################################################################################
#########################################################################################################
#########################################################################################################
#GRAFICO IDADE/SEXO
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))

ggplot(banco_JR_snr_SEXO_IDADE_pizza, aes(fill=SEXO, y=QUANTIDADE, x=IDADE)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = (str_c(GRAFICO[49,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "", fill = "SEXO") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="plain", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  theme(legend.position = "right") +
  geom_text(aes(label=QUANTIDADE), vjust=0, color="red", fontface = "plain",
            position = position_dodge(0.9), size=3.5)

ggsave("GRAFICO[49,].png", width=13, height=5, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))
mycols <- c("#c0504d", "#4f81bd")

#salvar png
ggpie(banco_JR_snr_SEXO_IDADE_graf_pizza,
      x= "QUANTIDADE", label = "PERCENTUAL2",
      lab.pos = "in", lab.font = list(color = "white", face = "bold"),
      lab.adjust = 0,
      fill = "SEXO", color = "white", face="bold",
      palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "plain"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  labs(caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       subtitle = "Justiça Restaurativa") +
  ggtitle((str_c(GRAFICO[50,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))))
ggsave("GRAFICO[50,].png", width=6.5, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
SINAL <- paste(banco_JR_raca_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_intervalo_sentenca_bkp_alternativo.pdf", title = "grafico_intervalo_sentenca_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

banco_JR_raca_bkp =
  banco_JR_raca_bkp |>
  mutate(banco_JR_raca_bkp = fct_reorder(banco_JR_raca_bkp, QUANTIDADE))

ggplot(banco_JR_raca_bkp, aes(x = banco_JR_raca_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[51,],": Raça/Cor, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 26))
ggsave("GRAFICO[51,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
SINAL <- paste(banco_JR_escolaridade_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_intervalo_sentenca_bkp_alternativo.pdf", title = "grafico_intervalo_sentenca_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

banco_JR_escolaridade_bkp =
  banco_JR_escolaridade_bkp |>
  mutate(banco_JR_escolaridade_bkp = fct_reorder(banco_JR_escolaridade_bkp, QUANTIDADE))

ggplot(banco_JR_escolaridade_bkp, aes(x = banco_JR_escolaridade_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[52,],": Escolaridade, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 16))
ggsave("GRAFICO[52,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
SINAL <- paste(banco_JR_natureza_escola_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_intervalo_sentenca_bkp_alternativo.pdf", title = "grafico_intervalo_sentenca_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

banco_JR_natureza_escola_bkp =
  banco_JR_natureza_escola_bkp |>
  mutate(banco_JR_natureza_escola_bkp = fct_reorder(banco_JR_natureza_escola_bkp, QUANTIDADE))

ggplot(banco_JR_natureza_escola_bkp, aes(x = banco_JR_natureza_escola_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[53,],": Natureza da Escola, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 47))
ggsave("GRAFICO[53,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
SINAL <- paste(banco_JR_trabalho_atual_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_intervalo_sentenca_bkp_alternativo.pdf", title = "grafico_intervalo_sentenca_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

banco_JR_trabalho_atual_bkp =
  banco_JR_trabalho_atual_bkp |>
  mutate(banco_JR_trabalho_atual_bkp = fct_reorder(banco_JR_trabalho_atual_bkp, QUANTIDADE))

ggplot(banco_JR_trabalho_atual_bkp, aes(x = banco_JR_trabalho_atual_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[54,],": Trabalho atual, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 37))
ggsave("GRAFICO[54,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
SINAL <- paste(banco_JR_natureza_trabalho_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_intervalo_sentenca_bkp_alternativo.pdf", title = "grafico_intervalo_sentenca_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

banco_JR_natureza_trabalho_bkp =
  banco_JR_natureza_trabalho_bkp |>
  mutate(banco_JR_natureza_trabalho_bkp = fct_reorder(banco_JR_natureza_trabalho_bkp, QUANTIDADE))

ggplot(banco_JR_natureza_trabalho_bkp, aes(x = banco_JR_natureza_trabalho_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[55,],": Natureza do Trabalho, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 7))
ggsave("GRAFICO[55,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
SINAL <- paste(banco_JR_renda_mensal_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_intervalo_sentenca_bkp_alternativo.pdf", title = "grafico_intervalo_sentenca_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

banco_JR_renda_mensal_bkp =
  banco_JR_renda_mensal_bkp |>
  mutate(banco_JR_renda_mensal_bkp = fct_reorder(banco_JR_renda_mensal_bkp, QUANTIDADE))

ggplot(banco_JR_renda_mensal_bkp, aes(x = banco_JR_renda_mensal_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[56,],": Renda Mensal, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 11))
ggsave("GRAFICO[56,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
SINAL <- paste(banco_JR_estado_civil_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_intervalo_sentenca_bkp_alternativo.pdf", title = "grafico_intervalo_sentenca_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

banco_JR_estado_civil_bkp =
  banco_JR_estado_civil_bkp |>
  mutate(banco_JR_estado_civil_bkp = fct_reorder(banco_JR_estado_civil_bkp, QUANTIDADE))

ggplot(banco_JR_estado_civil_bkp, aes(x = banco_JR_estado_civil_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[57,],": Estado Civil, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 49))
ggsave("GRAFICO[57,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
SINAL <- paste(banco_JR_estado_civil_pais_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_intervalo_sentenca_bkp_alternativo.pdf", title = "grafico_intervalo_sentenca_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

banco_JR_estado_civil_pais_bkp =
  banco_JR_estado_civil_pais_bkp |>
  mutate(banco_JR_estado_civil_pais_bkp = fct_reorder(banco_JR_estado_civil_pais_bkp, QUANTIDADE))

ggplot(banco_JR_estado_civil_pais_bkp, aes(x = banco_JR_estado_civil_pais_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[58,],": Estado civil dos pais, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 27))
ggsave("GRAFICO[58,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
SINAL <- paste(banco_JR_uso_drogas_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_intervalo_sentenca_bkp_alternativo.pdf", title = "grafico_intervalo_sentenca_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

banco_JR_uso_drogas_bkp =
  banco_JR_uso_drogas_bkp |>
  mutate(banco_JR_uso_drogas_bkp = fct_reorder(banco_JR_uso_drogas_bkp, QUANTIDADE))

ggplot(banco_JR_uso_drogas_bkp, aes(x = banco_JR_uso_drogas_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[59,],": Uso de drogas, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 10))
ggsave("GRAFICO[59,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
SINAL <- paste(banco_JR_medidaspro_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_intervalo_sentenca_bkp_alternativo.pdf", title = "grafico_intervalo_sentenca_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

banco_JR_medidaspro_bkp =
  banco_JR_medidaspro_bkp |>
  mutate(banco_JR_medidaspro_bkp = fct_reorder(banco_JR_medidaspro_bkp, QUANTIDADE))

ggplot(banco_JR_medidaspro_bkp, aes(x = banco_JR_medidaspro_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[60,],": Medidas Protetivas, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 17))
ggsave("GRAFICO[60,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
SINAL <- paste(banco_JR_decisao_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_intervalo_sentenca_bkp_alternativo.pdf", title = "grafico_intervalo_sentenca_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

banco_JR_decisao_bkp =
  banco_JR_decisao_bkp |>
  mutate(banco_JR_decisao_bkp = fct_reorder(banco_JR_decisao_bkp, QUANTIDADE))

ggplot(banco_JR_decisao_bkp, aes(x = banco_JR_decisao_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[61,],": Medidas Socioeducativas, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 9))
ggsave("GRAFICO[61,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
SINAL <- paste(banco_JR_incidencia_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_intervalo_sentenca_bkp_alternativo.pdf", title = "grafico_intervalo_sentenca_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

banco_JR_incidencia_bkp =
  banco_JR_incidencia_bkp |>
  mutate(banco_JR_incidencia_bkp = fct_reorder(banco_JR_incidencia_bkp, QUANTIDADE))

ggplot(banco_JR_incidencia_bkp, aes(x = banco_JR_incidencia_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[62,],": Incidência atos infracionais, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       x = "", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23", size = 3) +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 17.5))
ggsave("GRAFICO[62,].png", width=10, height=8, pointsize=12, dpi = 512)

#dev.off()
#dev.off()
#########################################################################################################
#Projetos socioeducativos

#########################################################################################################
#graficos banco_matriculados_CEDIPRO
#########################################################################################################
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))
mycols <- c("#c0504d", "#4f81bd")

#salvar png
ggpie(banco_encaminhamento_CEDIPRO_bkp,
      x= "QUANTIDADE", label = "PERCENTUAL",
      lab.pos = "out", lab.font = list(color = "black", face = "bold"),
      lab.adjust = 0,
      fill = "GRUPO", color = "white", face="bold",
      palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  labs(caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO", subtitle = "Projeto CEDIPRO") +
  ggtitle((str_c(GRAFICO[63,],": Casos encaminhados, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))))
ggsave("GRAFICO[63,].png", width=7, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
banco_curso_adolescente_CEDIPRO_bkp$banco_curso_adolescente_CEDIPRO_bkp = factor(banco_curso_adolescente_CEDIPRO_bkp$banco_curso_adolescente_CEDIPRO_bkp)
#SINAL <- paste(banco_curso_adolescente_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- banco_curso_adolescente_CEDIPRO_bkp$PERCENTUAL#para plotar o sinal de porcentagem
#pdf(file="grafico_banco_curso_adolescente_CEDIPRO_bkp_alternativo.pdf", title = "grafico_banco_curso_adolescente_CEDIPRO_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
ggplot(data=banco_curso_adolescente_CEDIPRO_bkp, aes(x=banco_curso_adolescente_CEDIPRO_bkp, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(banco_curso_adolescente_CEDIPRO_bkp[nrow(banco_curso_adolescente_CEDIPRO_bkp),2]+7)) +
  scale_x_discrete(limits = banco_curso_adolescente_CEDIPRO_bkp$banco_curso_adolescente_CEDIPRO_bkp)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[64,],": Cursos, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Projeto CEDIPRO",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO",
       x = "CURSOS", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption =element_text(hjust = 0.5)  )
ggsave("GRAFICO[64,].png", width=10, height=6, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################

#########################################################################################################

#########################################################################################################
#GRAFICO IDADE/SEXO
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))

ggplot(df_snr_sexo_idade_banco_matriculados_CEDIPRO, aes(fill=sexo, y=QUANTIDADE, x=idade)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = (str_c(GRAFICO[65,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Projeto CEDIPRO: alunos matriculados",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO",
       x = "", y = "", fill = "SEXO") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="plain", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  theme(legend.position = "right") +
  geom_text(aes(label=QUANTIDADE), vjust=0, color="red", fontface = "plain",
            position = position_dodge(0.9), size=3.5)

ggsave("GRAFICO[65,].png", width=13, height=5, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))
mycols <- c("#c0504d", "#4f81bd")

#salvar png
ggpie(df_snr_sexo_pizza_banco_matriculados_CEDIPRO,
      x= "QUANTIDADE", label = "PERCENTUAL",
      lab.pos = "in", lab.font = list(color = "white", face = "bold"),
      lab.adjust = 0,
      fill = "sexo", color = "white", face="bold",
      palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  labs(caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO", subtitle = "Projeto CEDIPRO: alunos matriculados") +
  ggtitle((str_c(GRAFICO[66,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))))
ggsave("GRAFICO[66,].png", width=6.5, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#SINAL <- paste(ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/diretorio_r/estciabh/imagens"))
ggplot(data=ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp,
       aes(x = QUANTIDADE,
           y = reorder(ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp, PERCENTUAL2))) +
  geom_bar(stat="identity", fill="#bb1e23")+
  labs(title = (str_c(GRAFICO[67,],": Escolaridade, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Projeto CEDIPRO: alunos matriculados",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO",
       x = "", y = "ESCOLARIDADE") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        plot.caption =element_text(hjust = 0.5)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  scale_x_continuous(n.breaks=5)
#scale_x_continuous(limits=c(0, 90))
ggsave("GRAFICO[67,].png", width=15, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp$REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp = factor(REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp$REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp)
#SINAL <- paste(REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp$PERCENTUAL#para plotar o sinal de porcentagem
#pdf(file="grafico_REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp_alternativo.pdf", title = "grafico_REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
ggplot(data=REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp, aes(x=REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp[nrow(REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp),2]+2)) +
  scale_x_discrete(limits = REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp$REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[68,],": Regional Residencial, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Projeto CEDIPRO: alunos matriculados",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO",
       x = "REGIONAL", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption =element_text(hjust = 0.5)  )
ggsave("GRAFICO[68,].png", width=7, height=4, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
INCIDENCIA_banco_matriculados_CEDIPRO_bkp$INCIDENCIA_banco_matriculados_CEDIPRO_bkp = factor(INCIDENCIA_banco_matriculados_CEDIPRO_bkp$INCIDENCIA_banco_matriculados_CEDIPRO_bkp)
#SINAL <- paste(INCIDENCIA_banco_matriculados_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- INCIDENCIA_banco_matriculados_CEDIPRO_bkp$PERCENTUAL#para plotar o sinal de porcentagem
#pdf(file="grafico_INCIDENCIA_banco_matriculados_CEDIPRO_bkp_alternativo.pdf", title = "grafico_INCIDENCIA_banco_matriculados_CEDIPRO_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
ggplot(data=INCIDENCIA_banco_matriculados_CEDIPRO_bkp, aes(x=INCIDENCIA_banco_matriculados_CEDIPRO_bkp, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(INCIDENCIA_banco_matriculados_CEDIPRO_bkp[nrow(INCIDENCIA_banco_matriculados_CEDIPRO_bkp),2]+10)) +
  scale_x_discrete(limits = INCIDENCIA_banco_matriculados_CEDIPRO_bkp$INCIDENCIA_banco_matriculados_CEDIPRO_bkp)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[69,],": Incidência, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Projeto CEDIPRO: alunos matriculados",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO",
       x = "ATO INFRACIONAL", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption =element_text(hjust = 0.5)  )
ggsave("GRAFICO[69,].png", width=8, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp$REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp = factor(REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp$REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp)
#SINAL <- paste(REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp$PERCENTUAL#para plotar o sinal de porcentagem
#pdf(file="grafico_REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp_alternativo.pdf", title = "grafico_REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
ggplot(data=REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp, aes(x=REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp[nrow(REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp),2]+2)) +
  scale_x_discrete(limits = REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp$REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[70,],": Regional Ato, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Projeto CEDIPRO: alunos matriculados",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO",
       x = "REGIONAL", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption =element_text(hjust = 0.5)  )
ggsave("GRAFICO[70,].png", width=7, height=4, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################

#########################################################################################################
#########################################################################################################
PROTETIVAS_banco_matriculados_CEDIPRO_bkp$PROTETIVAS_banco_matriculados_CEDIPRO_bkp = factor(PROTETIVAS_banco_matriculados_CEDIPRO_bkp$PROTETIVAS_banco_matriculados_CEDIPRO_bkp)
#SINAL <- paste(PROTETIVAS_banco_matriculados_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- PROTETIVAS_banco_matriculados_CEDIPRO_bkp$PERCENTUAL#para plotar o sinal de porcentagem
#pdf(file="grafico_PROTETIVAS_banco_matriculados_CEDIPRO_bkp_alternativo.pdf", title = "grafico_PROTETIVAS_banco_matriculados_CEDIPRO_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
ggplot(data=PROTETIVAS_banco_matriculados_CEDIPRO_bkp, aes(x=PROTETIVAS_banco_matriculados_CEDIPRO_bkp, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(PROTETIVAS_banco_matriculados_CEDIPRO_bkp[nrow(PROTETIVAS_banco_matriculados_CEDIPRO_bkp),2]+5)) +
  scale_x_discrete(limits = PROTETIVAS_banco_matriculados_CEDIPRO_bkp$PROTETIVAS_banco_matriculados_CEDIPRO_bkp)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[71,],": Medidas Protetivas, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Projeto CEDIPRO: alunos matriculados",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO",
       x = "REGIONAL", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption =element_text(hjust = 0.5)  )
ggsave("GRAFICO[71,].png", width=7, height=4, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################

#########################################################################################################
#########################################################################################################
DECISAO_banco_matriculados_CEDIPRO_bkp$DECISAO_banco_matriculados_CEDIPRO_bkp = factor(DECISAO_banco_matriculados_CEDIPRO_bkp$DECISAO_banco_matriculados_CEDIPRO_bkp)
#SINAL <- paste(DECISAO_banco_matriculados_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- DECISAO_banco_matriculados_CEDIPRO_bkp$PERCENTUAL#para plotar o sinal de porcentagem
#pdf(file="grafico_DECISAO_banco_matriculados_CEDIPRO_bkp_alternativo.pdf", title = "grafico_DECISAO_banco_matriculados_CEDIPRO_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
ggplot(data=DECISAO_banco_matriculados_CEDIPRO_bkp, aes(x=DECISAO_banco_matriculados_CEDIPRO_bkp, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(DECISAO_banco_matriculados_CEDIPRO_bkp[nrow(DECISAO_banco_matriculados_CEDIPRO_bkp),2]+5)) +
  scale_x_discrete(limits = DECISAO_banco_matriculados_CEDIPRO_bkp$DECISAO_banco_matriculados_CEDIPRO_bkp)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[72,],": Decisão, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Projeto CEDIPRO: alunos matriculados",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO",
       x = "DECISÃO", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption =element_text(hjust = 0.5)  )
ggsave("GRAFICO[72,].png", width=7, height=4, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################

#########################################################################################################
#########################################################################################################

#########################################################################################################

#########################################################################################################
#TRATAMENTO banco_matriculados_CEDIPRO FIM
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_desistencia_CEDIPRO
#########################################################################################################

#########################################################################################################
#GRAFICO IDADE/SEXO
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))

ggplot(df_snr_sexo_idade_banco_desistencia_CEDIPRO, aes(fill=sexo, y=QUANTIDADE, x=idade)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = (str_c(GRAFICO[73,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Projeto CEDIPRO: alunos desistentes",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO",
       x = "", y = "", fill = "SEXO") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="plain", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  theme(legend.position = "right") +
  geom_text(aes(label=QUANTIDADE), vjust=0, color="red", fontface = "plain",
            position = position_dodge(0.9), size=3.5)

ggsave("GRAFICO[73,].png", width=13, height=5, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))
mycols <- c("#c0504d", "#4f81bd")

#salvar png
ggpie(df_snr_sexo_pizza_banco_desistencia_CEDIPRO,
      x= "QUANTIDADE", label = "PERCENTUAL",
      lab.pos = "in", lab.font = list(color = "white", face = "bold"),
      lab.adjust = 0,
      fill = "sexo", color = "white", face="bold",
      palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  labs(caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO", subtitle = "Projeto CEDIPRO: alunos desistentes") +
  ggtitle((str_c(GRAFICO[74,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))))
ggsave("GRAFICO[74,].png", width=6.5, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#SINAL <- paste(ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/diretorio_r/estciabh/imagens"))
ggplot(data=ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp,
       aes(x = QUANTIDADE,
           y = reorder(ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp, PERCENTUAL2))) +
  geom_bar(stat="identity", fill="#bb1e23")+
  labs(title = (str_c(GRAFICO[75,],": Escolaridade, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Projeto CEDIPRO: alunos desistentes",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO",
       x = "", y = "ESCOLARIDADE") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        plot.caption =element_text(hjust = 0.5)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  scale_x_continuous(n.breaks=5)
#scale_x_continuous(limits=c(0, 90))
ggsave("GRAFICO[75,].png", width=15, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp$REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp = factor(REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp$REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp)
#SINAL <- paste(REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp$PERCENTUAL#para plotar o sinal de porcentagem
#pdf(file="grafico_REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp_alternativo.pdf", title = "grafico_REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
ggplot(data=REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp, aes(x=REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp[nrow(REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp),2]+0.2)) +
  scale_x_discrete(limits = REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp$REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[76,],": Regional Residencial, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Projeto CEDIPRO: alunos desistentes",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO",
       x = "REGIONAL", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption =element_text(hjust = 0.5)  )
ggsave("GRAFICO[76,].png", width=7, height=4, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
INCIDENCIA_banco_desistencia_CEDIPRO_bkp$INCIDENCIA_banco_desistencia_CEDIPRO_bkp = factor(INCIDENCIA_banco_desistencia_CEDIPRO_bkp$INCIDENCIA_banco_desistencia_CEDIPRO_bkp)
#SINAL <- paste(INCIDENCIA_banco_desistencia_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- INCIDENCIA_banco_desistencia_CEDIPRO_bkp$PERCENTUAL#para plotar o sinal de porcentagem
#pdf(file="grafico_INCIDENCIA_banco_desistencia_CEDIPRO_bkp_alternativo.pdf", title = "grafico_INCIDENCIA_banco_desistencia_CEDIPRO_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
ggplot(data=INCIDENCIA_banco_desistencia_CEDIPRO_bkp, aes(x=INCIDENCIA_banco_desistencia_CEDIPRO_bkp, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(INCIDENCIA_banco_desistencia_CEDIPRO_bkp[nrow(INCIDENCIA_banco_desistencia_CEDIPRO_bkp),2]+0.2)) +
  scale_x_discrete(limits = INCIDENCIA_banco_desistencia_CEDIPRO_bkp$INCIDENCIA_banco_desistencia_CEDIPRO_bkp)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[77,],": Incidência, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Projeto CEDIPRO: alunos desistentes",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO",
       x = "DECISÃO", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption =element_text(hjust = 0.5)  )
ggsave("GRAFICO[77,].png", width=7, height=4, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp$REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp = factor(REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp$REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp)
#SINAL <- paste(REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp$PERCENTUAL#para plotar o sinal de porcentagem
#pdf(file="grafico_REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp_alternativo.pdf", title = "grafico_REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
ggplot(data=REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp, aes(x=REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp[nrow(REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp),2]+0.2)) +
  scale_x_discrete(limits = REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp$REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[78,],": Regional Ato, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Projeto CEDIPRO: alunos desistentes",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO",
       x = "REGIONAL", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption =element_text(hjust = 0.5)  )
ggsave("GRAFICO[78,].png", width=7, height=4, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################

#########################################################################################################
#########################################################################################################
PROTETIVAS_banco_desistencia_CEDIPRO_bkp$PROTETIVAS_banco_desistencia_CEDIPRO_bkp = factor(PROTETIVAS_banco_desistencia_CEDIPRO_bkp$PROTETIVAS_banco_desistencia_CEDIPRO_bkp)
#SINAL <- paste(PROTETIVAS_banco_desistencia_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- PROTETIVAS_banco_desistencia_CEDIPRO_bkp$PERCENTUAL#para plotar o sinal de porcentagem
#pdf(file="grafico_PROTETIVAS_banco_desistencia_CEDIPRO_bkp_alternativo.pdf", title = "grafico_PROTETIVAS_banco_desistencia_CEDIPRO_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
ggplot(data=PROTETIVAS_banco_desistencia_CEDIPRO_bkp, aes(x=PROTETIVAS_banco_desistencia_CEDIPRO_bkp, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(PROTETIVAS_banco_desistencia_CEDIPRO_bkp[nrow(PROTETIVAS_banco_desistencia_CEDIPRO_bkp),2]+0.2)) +
  scale_x_discrete(limits = PROTETIVAS_banco_desistencia_CEDIPRO_bkp$PROTETIVAS_banco_desistencia_CEDIPRO_bkp)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[79,],": Medidas Protetivas, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Projeto CEDIPRO: alunos desistentes",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO",
       x = "REGIONAL", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption =element_text(hjust = 0.5)  )
ggsave("GRAFICO[79,].png", width=7, height=4, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################

#########################################################################################################
#########################################################################################################
DECISAO_banco_desistencia_CEDIPRO_bkp$DECISAO_banco_desistencia_CEDIPRO_bkp = factor(DECISAO_banco_desistencia_CEDIPRO_bkp$DECISAO_banco_desistencia_CEDIPRO_bkp)
#SINAL <- paste(DECISAO_banco_desistencia_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- DECISAO_banco_desistencia_CEDIPRO_bkp$PERCENTUAL#para plotar o sinal de porcentagem
#pdf(file="grafico_DECISAO_banco_desistencia_CEDIPRO_bkp_alternativo.pdf", title = "grafico_DECISAO_banco_desistencia_CEDIPRO_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
ggplot(data=DECISAO_banco_desistencia_CEDIPRO_bkp, aes(x=DECISAO_banco_desistencia_CEDIPRO_bkp, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(DECISAO_banco_desistencia_CEDIPRO_bkp[nrow(DECISAO_banco_desistencia_CEDIPRO_bkp),2]+0.2)) +
  scale_x_discrete(limits = DECISAO_banco_desistencia_CEDIPRO_bkp$DECISAO_banco_desistencia_CEDIPRO_bkp)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[80,],": Decisão, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Projeto CEDIPRO: alunos desistentes",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO",
       x = "DECISÃO", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption =element_text(hjust = 0.5)  )
ggsave("GRAFICO[80,].png", width=7, height=4, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################

#########################################################################################################
#########################################################################################################

#########################################################################################################

#########################################################################################################
#TRATAMENTO banco_desistencia_CEDIPRO FIM
#########################################################################################################
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_curso_parente_CEDIPRO
#########################################################################################################
#########################################################################################################
banco_curso_parente_CEDIPRO_bkp$banco_curso_parente_CEDIPRO_bkp = factor(banco_curso_parente_CEDIPRO_bkp$banco_curso_parente_CEDIPRO_bkp)
#SINAL <- paste(banco_curso_parente_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- banco_curso_parente_CEDIPRO_bkp$PERCENTUAL#para plotar o sinal de porcentagem
#pdf(file="grafico_banco_curso_parente_CEDIPRO_bkp_alternativo.pdf", title = "grafico_banco_curso_parente_CEDIPRO_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
ggplot(data=banco_curso_parente_CEDIPRO_bkp, aes(x=banco_curso_parente_CEDIPRO_bkp, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(banco_curso_parente_CEDIPRO_bkp[nrow(banco_curso_parente_CEDIPRO_bkp),2]+0.5)) +
  scale_x_discrete(limits = banco_curso_parente_CEDIPRO_bkp$banco_curso_parente_CEDIPRO_bkp)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[81,],": Incidência, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Projeto CEDIPRO: alunos desistentes",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO",
       x = "DECISÃO", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption =element_text(hjust = 0.5)  )
ggsave("GRAFICO[81,].png", width=8, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_curso_parente_CEDIPRO FIM
#########################################################################################################
#A letalidade de adolescentes e jovens e o sistema socioeducativo
#########################################################################################################
SINAL <- paste(CAUSA_JURIDICA_let_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_CAUSA_JURIDICA_let_bkp_alternativo.pdf", title = "grafico_CAUSA_JURIDICA_let_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

CAUSA_JURIDICA_let_bkp =
  CAUSA_JURIDICA_let_bkp |>
  mutate(CAUSA_JURIDICA_let_bkp = fct_reorder(CAUSA_JURIDICA_let_bkp, QUANTIDADE))

ggplot(CAUSA_JURIDICA_let_bkp, aes(x = CAUSA_JURIDICA_let_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[82,],": Letalidade por causas externas, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: INSTITUTO MÉDICO LEGAL",
       x = "CAUSA JURÍDICA", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  #scale_y_continuous(n.breaks=10)
  scale_y_continuous(limits=c(0, 85))
ggsave("GRAFICO[82,].png", width=9.5, height=4, pointsize=12, dpi = 512)


#dev.off()
#########################################################################################################
#########################################################################################################
SINAL <- paste(soma_intervalo_idade_HOMICIDIO_let_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_soma_intervalo_idade_HOMICIDIO_let_bkp_alternativo.pdf", title = "grafico_soma_intervalo_idade_HOMICIDIO_let_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

soma_intervalo_idade_HOMICIDIO_let_bkp =
  soma_intervalo_idade_HOMICIDIO_let_bkp |>
  mutate(soma_intervalo_idade_HOMICIDIO_let_bkp = fct_reorder(soma_intervalo_idade_HOMICIDIO_let_bkp, QUANTIDADE))

ggplot(soma_intervalo_idade_HOMICIDIO_let_bkp, aes(x = soma_intervalo_idade_HOMICIDIO_let_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[83,],": Incidência de óbitos por homicídio, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: INSTITUTO MÉDICO LEGAL",
       x = "FAIXA ETÁRIA", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 300))
ggsave("GRAFICO[83,].png", width=9.5, height=4, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
SINAL <- paste(soma_intervalo_idade_IGNORADA_let_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_soma_intervalo_idade_IGNORADA_let_bkp_alternativo.pdf", title = "grafico_soma_intervalo_idade_IGNORADA_let_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

soma_intervalo_idade_IGNORADA_let_bkp =
  soma_intervalo_idade_IGNORADA_let_bkp |>
  mutate(soma_intervalo_idade_IGNORADA_let_bkp = fct_reorder(soma_intervalo_idade_IGNORADA_let_bkp, QUANTIDADE))

ggplot(soma_intervalo_idade_IGNORADA_let_bkp, aes(x = soma_intervalo_idade_IGNORADA_let_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[84,],": Incidência de óbitos por causa ignorada, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: INSTITUTO MÉDICO LEGAL",
       x = "FAIXA ETÁRIA", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 1410))
ggsave("GRAFICO[84,].png", width=10, height=4, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
ggplot(tab_letalidade_geral_12_20_HOMICIDIO_bkp, aes(fill=ANO, y=QUANTIDADE, x=IDADE)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = (str_c(GRAFICO[85,],": Incidência de óbitos por homicídio, Belo Horizonte e municípios limítrofes, 2018 a ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: INSTITUTO MÉDICO LEGAL",
       x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="plain", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  theme(legend.position = "bottom") +
  geom_text(aes(label=QUANTIDADE), vjust=0, color="red", fontface = "plain",
            position = position_dodge(0.9), size=2.5)

ggsave("GRAFICO[85,].png", width=10, height=4, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
ggplot(tab_letalidade_geral_12_20_IGNORADA_bkp, aes(fill=ANO, y=QUANTIDADE, x=IDADE)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = (str_c(GRAFICO[86,],": Incidência de óbitos por causa ignorada, Belo Horizonte e municípios limítrofes, 2018 a ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: INSTITUTO MÉDICO LEGAL",
       x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="plain", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  theme(legend.position = "bottom") +
  geom_text(aes(label=QUANTIDADE), vjust=0, color="red", fontface = "plain",
            position = position_dodge(0.9), size=2.5)

ggsave("GRAFICO[86,].png", width=10, height=4, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))

#########################################################################################################
#montando o próximo gráfico:

p1_banco_SEXO_HOMICIDIO_LETALIDADE_pizza =
  ggpie(banco_SEXO_HOMICIDIO_LETALIDADE_pizza,
        x= "QUANTIDADE", label = "QUANTIDADE",
        lab.pos = "in", lab.font = list(color = "white", face = "bold"),
        lab.adjust = 0,
        fill = "SEXO", color = "white", face="bold",
        palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  #plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  # labs(caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL", subtitle = "Letalidade") +
  ggtitle("HOMICÍDIO")
#ggsave("GRAFICO_068_sexo_idade_LET_pizza.png", width=9, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
p2_banco_SEXO_IGNORADA_LETALIDADE_pizza =
  ggpie(banco_SEXO_IGNORADA_LETALIDADE_pizza,
        x= "QUANTIDADE", label = "QUANTIDADE",
        lab.pos = "in", lab.font = list(color = "white", face = "bold"),
        lab.adjust = 0,
        fill = "SEXO", color = "white", face="bold",
        palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  #plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  # labs(caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL", subtitle = "Letalidade") +
  ggtitle("CAUSA IGNORADA")
#ggsave("GRAFICO_068_sexo_idade_LET_pizza.png", width=9, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#OBS: juntado os gráficos de pizza acima:
setwd(file.path("~/diretorio_r/estciabh/imagens"))

p_SEXO_HOM_IGN_let <- plot_grid(p1_banco_SEXO_HOMICIDIO_LETALIDADE_pizza, p2_banco_SEXO_IGNORADA_LETALIDADE_pizza)

title <- ggdraw() + draw_label(str_c(GRAFICO[87,],": Sexo dos adolescente e jovens vitimados por homicídio e causa ignorada, Sexo, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y")), fontface='bold')
subtitle <- ggdraw() + draw_label("Letalidade", size = 12)
#caption <- ggdraw() + draw_label("FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL", fontface=NULL)

p3_SEXO_HOM_IGN_let = plot_grid(title, subtitle, p_SEXO_HOM_IGN_let, ncol=1, rel_heights=c(0.1, 0.1, 1)) # rel_heights values control title margins
ggdraw(add_sub(p3_SEXO_HOM_IGN_let, "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL", size = 12))

ggsave("GRAFICO[87,].pdf", width=16, height=6.9, pointsize=12, dpi = 512)
ggsave("GRAFICO[87,].png", width=16, height=6.9, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))

ggplot(banco_COR_hom_ign_LETALIDADE_pizza_bkp, aes(fill=CAUSA_JURIDICA, y=QUANTIDADE, x=COR)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = (str_c(GRAFICO[88,],": Raça/cor dos adolescentes e jovens vitimados por homicídio e causa ignorada, Belo Horizonte e municípios limítrofes, 2018 a ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: INSTITUTO MÉDICO LEGAL",
       x = "", y = "",
       fill = "CAUSA JURÍDICA") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="plain", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  theme(legend.position = "right") +
  geom_text(aes(label=QUANTIDADE), vjust=0, color="red", fontface = "plain",
            position = position_dodge(0.9), size=2.5)

ggsave("GRAFICO[88,].png", width=13, height=5, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))

ggplot(banco_HOM_IGN_GERAL, aes(fill=CAUSA_JURIDICA, y=QUANTIDADE, x=ANO)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = (str_c(GRAFICO[89,],": Óbitos por homicídio e causa ignorada, Belo Horizonte e municípios limítrofes, 2018 a ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: INSTITUTO MÉDICO LEGAL",
       x = "", y = "",
       fill = "CAUSA JURÍDICA") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="plain", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  theme(legend.position = "right") +
  geom_text(aes(label=QUANTIDADE), vjust=0, color="red", fontface = "plain",
            position = position_dodge(0.9), size=3)

ggsave("GRAFICO[89,].png", width=13, height=5, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################

#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))
ggplot(data=passagem_cia_GERAL_HOM_IGN_LET, aes(x=CAUSA_JURIDICA, y=QUANTIDADE, fill=PASSAGEM_CIABH)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_color_brewer(palette="Set1")+
  theme_minimal()+
  geom_text(aes(label=QUANTIDADE), vjust=1.6, color="white", fontface = "bold",
            position = position_dodge(0.9), size=3.5)+
  labs(title = (str_c(GRAFICO[90,],": Óbitos por causas violentas e Passagem CIABH, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "LETALIDADE",
       caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
       x = "", y = "", fill = "PASSAGEM CIABH") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="plain", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )

ggsave("GRAFICO[90,].png", width=11.5, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))

ggplot(passagem_cia_HOMICIDIO_LET, aes(fill=PASSAGEM_CIABH, y=QUANTIDADE, x=IDADE)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = (str_c(GRAFICO[91,],": Óbitos por homicídio e passagem pelo CIABH, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
       x = "", y = "", fill = "PASSAGEM CIABH") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="plain", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  theme(legend.position = "right") +
  geom_text(aes(label=QUANTIDADE), vjust=0, color="red", fontface = "plain",
            position = position_dodge(0.9), size=3)

ggsave("GRAFICO[91,].png", width=13, height=5, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################

########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))

ggplot(passagem_cia_IGNORADA_LET, aes(fill=PASSAGEM_CIABH, y=QUANTIDADE, x=IDADE)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = (str_c(GRAFICO[92,],":  Óbitos por causa ignorada e passagem pelo CIABH, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
       x = "", y = "", fill = "PASSAGEM CIABH") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="plain", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  theme(legend.position = "right") +
  geom_text(aes(label=QUANTIDADE), vjust=0, color="red", fontface = "plain",
            position = position_dodge(0.9), size=3)

ggsave("GRAFICO[92,].png", width=13, height=5, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#OBS: GRÁFICOS p1 e p2 no script 022_letalidade
setwd(file.path("~/diretorio_r/estciabh/imagens"))

p <- plot_grid(p1, p2)

title <- ggdraw() + draw_label(str_c(GRAFICO[93,],": Óbitos de adolescente e jovens (12 a 20 anos) por homicídio e causa ignorada, Sexo, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y")), fontface='bold')
subtitle <- ggdraw() + draw_label("Letalidade", size = 12)
#caption <- ggdraw() + draw_label("FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL", fontface=NULL)

p3 = plot_grid(title, subtitle, p, ncol=1, rel_heights=c(0.1, 0.1, 1)) # rel_heights values control title margins
ggdraw(add_sub(p3, "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL", size = 12))

ggsave("GRAFICO[93,].pdf", width=16, height=6.9, pointsize=12, dpi = 512)
ggsave("GRAFICO[93,].png", width=16, height=6.9, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
SINAL <- paste(banco_RACA_COR_HOMICIDIO_let_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_banco_RACA_COR_HOMICIDIO_let_bkp_alternativo.pdf", title = "grafico_banco_RACA_COR_HOMICIDIO_let_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

banco_RACA_COR_HOMICIDIO_let_bkp =
  banco_RACA_COR_HOMICIDIO_let_bkp |>
  mutate(banco_RACA_COR_HOMICIDIO_let_bkp = fct_reorder(banco_RACA_COR_HOMICIDIO_let_bkp, QUANTIDADE))

ggplot(banco_RACA_COR_HOMICIDIO_let_bkp, aes(x = banco_RACA_COR_HOMICIDIO_let_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[94,],": Raça/cor, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: INSTITUTO MÉDICO LEGAL",
       x = "RAÇA/COR", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 22))
ggsave("GRAFICO[94,].png", width=10, height=4, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
SINAL <- paste(banco_PROCEDENCIA_LET_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_banco_PROCEDENCIA_LET_bkp_alternativo.pdf", title = "grafico_banco_PROCEDENCIA_LET_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

banco_PROCEDENCIA_LET_bkp =
  banco_PROCEDENCIA_LET_bkp |>
  mutate(banco_PROCEDENCIA_LET_bkp = fct_reorder(banco_PROCEDENCIA_LET_bkp, QUANTIDADE))

ggplot(banco_PROCEDENCIA_LET_bkp, aes(x = banco_PROCEDENCIA_LET_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[95,],": Localidade do óbito, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: INSTITUTO MÉDICO LEGAL",
       x = "LOCAL DA MORTE", y = "")  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 18))
ggsave("GRAFICO[95,].png", width=10, height=4, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
SINAL <- paste(tempo_medio_geral_OBITO_TAB_02_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_tempo_medio_geral_OBITO_TAB_02_bkp_alternativo.pdf", title = "grafico_tempo_medio_geral_OBITO_TAB_02_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

tempo_medio_geral_OBITO_TAB_02_bkp =
  tempo_medio_geral_OBITO_TAB_02_bkp |>
  mutate(tempo_medio_geral_OBITO_TAB_02_bkp = fct_reorder(tempo_medio_geral_OBITO_TAB_02_bkp, QUANTIDADE))

ggplot(tempo_medio_geral_OBITO_TAB_02_bkp, aes(x = tempo_medio_geral_OBITO_TAB_02_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[96,],": Tempo entre última entrada CIABH e óbito")),
       subtitle = (str_c("Belo Horizonte, ", format(Sys.Date()-365*2, "%Y"))),
       caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
       x = "", y = "TEMPO") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 15))
ggsave("GRAFICO[96,].png", width=10, height=4, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
SINAL <- paste(tempo_medio_geral_CIABH_TAB_02_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_tempo_medio_geral_CIABH_TAB_02_bkp_alternativo.pdf", title = "grafico_tempo_medio_geral_CIABH_TAB_02_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

tempo_medio_geral_CIABH_TAB_02_bkp =
  tempo_medio_geral_CIABH_TAB_02_bkp |>
  mutate(tempo_medio_geral_CIABH_TAB_02_bkp = fct_reorder(tempo_medio_geral_CIABH_TAB_02_bkp, QUANTIDADE))

ggplot(tempo_medio_geral_CIABH_TAB_02_bkp, aes(x = tempo_medio_geral_CIABH_TAB_02_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[97,],": Tempo entre a primeira e a última entrada")),
       subtitle = (str_c("Belo Horizonte, ", format(Sys.Date()-365*2, "%Y"))),
       caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
       x = "", y = "TEMPO") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 15))
ggsave("GRAFICO[97,].png", width=10, height=4, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
SINAL <- paste(banco_NUMERO_ENTRADAS_let_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_banco_NUMERO_ENTRADAS_let_bkp_alternativo.pdf", title = "grafico_banco_NUMERO_ENTRADAS_let_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

banco_NUMERO_ENTRADAS_let_bkp =
  banco_NUMERO_ENTRADAS_let_bkp |>
  mutate(banco_NUMERO_ENTRADAS_let_bkp = fct_reorder(banco_NUMERO_ENTRADAS_let_bkp, QUANTIDADE))

ggplot(banco_NUMERO_ENTRADAS_let_bkp, aes(x = banco_NUMERO_ENTRADAS_let_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[98,],": Entradas")),
       subtitle = (str_c("Belo Horizonte, ", format(Sys.Date()-365*2, "%Y"))),
       caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
       x = "ENTRADAS", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 25))
ggsave("GRAFICO[98,].png", width=10, height=4, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/imagens"))

#salvar png
ggpie(graf_pizza_MEDIDAS_LET,
      x= "QUANTIDADE", label = "PERCENTUAL",
      lab.pos = "in", lab.font = list(color = "white", face = "bold"),
      lab.adjust = 0,
      fill = "POSSUI_MEDIDA_PROTETIVA", color = "white", face="bold",
      palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  labs(caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
       subtitle = "Letalidade", fill = "MEDIDA PROTETIVA") +
  ggtitle((str_c(GRAFICO[99,],": Aplicação de medida protetiva, Belo Horizonte, ", format(Sys.Date()-365*2, "%Y"))))
ggsave("GRAFICO[99,].png", width=8.5, height=5, pointsize=12, dpi = 512)
#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
SINAL <- paste(BANCO_MEDIDAS_LET_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_BANCO_MEDIDAS_LET_bkp_alternativo.pdf", title = "grafico_BANCO_MEDIDAS_LET_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

BANCO_MEDIDAS_LET_bkp =
  BANCO_MEDIDAS_LET_bkp |>
  mutate(BANCO_MEDIDAS_LET_bkp = fct_reorder(BANCO_MEDIDAS_LET_bkp, QUANTIDADE))

ggplot(BANCO_MEDIDAS_LET_bkp, aes(x = BANCO_MEDIDAS_LET_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[100,],": Medidas Protetivas, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
       x = "", y = "MEDIDAS") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 19))
ggsave("GRAFICO[100,].png", width=10, height=4, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
SINAL <- paste(BANCO_DECISAO_LET_bkp$PERCENTUAL)#para plotar o sinal de porcentagem
#pdf(file="grafico_BANCO_DECISAO_LET_bkp_alternativo.pdf", title = "grafico_BANCO_DECISAO_LET_bkp", width = 10, height = 8)
setwd(file.path("~/diretorio_r/estciabh/imagens"))
#salvar png
library(forcats)

BANCO_DECISAO_LET_bkp =
  BANCO_DECISAO_LET_bkp |>
  mutate(BANCO_DECISAO_LET_bkp = fct_reorder(BANCO_DECISAO_LET_bkp, QUANTIDADE))

ggplot(BANCO_DECISAO_LET_bkp, aes(x = BANCO_DECISAO_LET_bkp, y = QUANTIDADE)) +
  geom_bar(stat = "identity", fill="#bb1e23") +
  coord_flip() +
  labs(title = (str_c(GRAFICO[101,],": Decisões aplicadas, Belo Horizonte, ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
       x = "", y = "DECISÃO") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  #scale_y_continuous(n.breaks=5)
  scale_y_continuous(limits=c(0, 48))
ggsave("GRAFICO[101,].png", width=10, height=4, pointsize=12, dpi = 512)

#dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
setwd(file.path("~/diretorio_r/estciabh/R/"))#configurar diretorio
#########################################################################################################
