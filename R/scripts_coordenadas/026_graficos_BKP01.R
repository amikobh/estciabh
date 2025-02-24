#########################################################################################################
setwd(file.path("~/tester/imagens"))
#########################################################################################################
#########################################################################################################
#Atendimento ao adolescente
#########################################################################################################
setwd(file.path("~/tester/imagens"))

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
dev.off()
#########################################################################################################
#########################################################################################################
#GRAFICO banco_incidencia ALTERNATIVO
#banco_incidencia_original=banco_incidencia #salvando atos atendimento original
banco_incidencia=banco_incidencia_bkp

banco_incidencia<-banco_incidencia%>%
  arrange(QUANTIDADE)
#salvar png
ggplot(data=banco_incidencia, aes(x=ATO, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, (sum(banco_incidencia_rmark[1,2])+10)) +
  scale_x_discrete(limits = banco_incidencia$ATO)+
  geom_text(aes(label = QUANTIDADE), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[2,],": Incidência Atos Infracionais, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       #subtitle = "Adolescentes por Atos Infracionais",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD", 
       x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        #plot.subtitle = element_text(hjust = 0.5, size = 14),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) 
ggsave("GRAFICO[2,].png", width=15, height=8, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#Perfil dos adolescentes atendidos
#########################################################################################################
#GRAFICO IDADE/SEXO

library(ggplot2)
library(scales)


#salvar png
ggplot(data = df_snr_sexo_idade, aes(x=IDADE, y=QUANTIDADE, fill= SEXO)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_color_brewer(palette="Set1")+
  theme_minimal()+
  geom_text(aes(label=QUANTIDADE), vjust=0, color="black", fontface = "plain",
            position = position_dodge(0.9), size=3.5)+
  labs(title = (str_c(GRAFICO[3,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       #subtitle = "Letalidade",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "IDADE", y = "QUANTIDADE", fill = "SEXO") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        #plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
ggsave("GRAFICO[3,].png", width=8, height=6, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/tester/imagens"))
mycols <- c("#c0504d", "#4f81bd")

#salvar png
ggpie(df_snr_sexo, 
      x= "QUANTIDADE", label = "PERCENTUAL",
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
dev.off()

setwd(file.path("~/tester/imagens"))

#salvar png
ggplot(data=df_snr_regional_residencia, aes(x=REGIONAL, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, (sum(df_snr_regional_residencia_rmark[1,2]+10))) +
  scale_x_discrete(limits = df_snr_regional_residencia$REGIONAL)+
  geom_text(aes(label = QUANTIDADE), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[5,],": Regional de Residência, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       #subtitle = "Adolescentes por REGIONAL",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD", 
       x = "REGIONAL", y = "Nº DE OCORRÊNCIAS") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
#scale_fill_brewer(direction = 1)
#scale_fill_manual(values=c("#c0504d","#4f81bd"))
ggsave("GRAFICO[5,].png", width=10, height=6.5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#Total de mandados de busca e apreensão
#########################################################################################################
#GRAFICO IDADE/SEXO

library(ggplot2)
library(scales)

setwd(file.path("~/tester/imagens"))

#salvar png
ggplot(data = df_snr_sexo_MBA_idade_MBA, aes(x=IDADE, y=QUANTIDADE, fill= SEXO)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_color_brewer(palette="Set1")+
  theme_minimal()+
  geom_text(aes(label=QUANTIDADE), vjust=0, color="black", fontface = "plain",
            position = position_dodge(0.9), size=3.5)+
  labs(title = (str_c(GRAFICO[6,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "MBAs cumpridos",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       x = "IDADE", y = "QUANTIDADE", fill = "SEXO") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
ggsave("GRAFICO[6,].png", width=8, height=6, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/tester/imagens"))

#salvar png
ggpie(df_snr_sexo_MBA, 
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
  labs(caption = "FONTE: VARA INFRACIONAL/COMISSARIADO", subtitle = "MBAs cumpridos") +
  ggtitle((str_c(GRAFICO[7,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))))
ggsave("GRAFICO[7,].png", width=6.5, height=5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
df_snr_regional_residencia_MBA$REGIONAL = factor(df_snr_regional_residencia_MBA$REGIONAL)

#pdf(file="GRAFICO_006_df_snr_regional_residencia_MBA_alternativo.pdf", title = "grafico_df_snr_regional_residencia_MBA", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))

#salvar png
ggplot(data=df_snr_regional_residencia_MBA, aes(x=REGIONAL, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, (sum(df_snr_regional_residencia_MBA_rmark[1,2]+10))) +
  scale_x_discrete(limits = df_snr_regional_residencia_MBA$REGIONAL)+
  geom_text(aes(label = QUANTIDADE), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[8,],": Regional de Residência, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "MBAs cumpridos",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD", 
       x = "REGIONAL", y = "Nº DE OCORRÊNCIAS") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
#scale_fill_brewer(direction = 1)
#scale_fill_manual(values=c("#c0504d","#4f81bd"))
ggsave("GRAFICO[8,].png", width=10, height=6.5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
#MOTIVO_MBA


MOTIVO_MBA$MOTIVO_MBA = factor(MOTIVO_MBA$MOTIVO_MBA)
SINAL <- paste(MOTIVO_MBA$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
#pdf(file="grafico_MOTIVO_MBA_alternativo.pdf", title = "grafico_MOTIVO_MBA", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=MOTIVO_MBA, aes(x=MOTIVO_MBA, y=PERCENTUAL, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, (MOTIVO_MBA_rmark[1,3]+10)) +
  scale_x_discrete(limits = MOTIVO_MBA$MOTIVO_MBA)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[9,],": Motivo do MBA, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "MBAs cumpridos",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO", 
       x = "MOTIVO MBA", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
ggsave("GRAFICO[9,].png", width=8.5, height=4, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
#GRAFICO banco_ato_MBA ALTERNATIVO
#banco_ato_MBA_original=banco_ato_MBA #salvando ATO_INFRACIONAL_MBAs atendimento original
banco_ato_MBA=banco_ato_MBA_bkp

banco_ato_MBA<-banco_ato_MBA%>%
  arrange(QUANTIDADE)
#salvar png
ggplot(data=banco_ato_MBA, aes(x=ATO_INFRACIONAL_MBA, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, (sum(banco_ato_MBA_rmark[1,2])+1)) +
  scale_x_discrete(limits = banco_ato_MBA$ATO_INFRACIONAL_MBA)+
  geom_text(aes(label = QUANTIDADE), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[10,],": Atos infracionais atribuídos aos adolescentes encaminhados por MBA, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "MBAs cumpridos",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO", 
       x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) 
ggsave("GRAFICO[10,].png", width=12.6, height=3.5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
#ATOS EM FOCO: HOMICÍDIO
#########################################################################################################

#GRAFICO IDADE/SEXO

library(ggplot2)
library(scales)

setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data = df_snr_sexo_idade_HOMICIDIO, aes(x=IDADE, y=QUANTIDADE, fill= SEXO)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_color_brewer(palette="Set1")+
  theme_minimal()+
  geom_text(aes(label=QUANTIDADE), vjust=0, color="black", fontface = "plain",
            position = position_dodge(0.9), size=3.5)+
  labs(title = (str_c(GRAFICO[11,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "HOMICÍDIO",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "IDADE", y = "QUANTIDADE", fill = "SEXO") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
ggsave("GRAFICO[11,].png", width=8, height=6, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/tester/imagens"))

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
dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#ATOS EM FOCO: ROUBO
#########################################################################################################
#GRAFICO IDADE/SEXO

library(ggplot2)
library(scales)

setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data = df_snr_sexo_idade_ROUBO, aes(x=IDADE, y=QUANTIDADE, fill= SEXO)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_color_brewer(palette="Set1")+
  theme_minimal()+
  geom_text(aes(label=QUANTIDADE), vjust=0, color="black", fontface = "plain",
            position = position_dodge(0.9), size=3.5)+
  labs(title = (str_c(GRAFICO[13,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "ROUBO",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "IDADE", y = "QUANTIDADE", fill = "SEXO") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
ggsave("GRAFICO[13,].png", width=8, height=6, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#ATOS EM FOCO: FURTO
#########################################################################################################
#GRAFICO IDADE/SEXO

library(ggplot2)
library(scales)

setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data = df_snr_sexo_idade_FURTO, aes(x=IDADE, y=QUANTIDADE, fill= SEXO)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_color_brewer(palette="Set1")+
  theme_minimal()+
  geom_text(aes(label=QUANTIDADE), vjust=0, color="black", fontface = "plain",
            position = position_dodge(0.9), size=3.5)+
  labs(title = (str_c(GRAFICO[15,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "FURTO",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "IDADE", y = "QUANTIDADE", fill = "SEXO") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
ggsave("GRAFICO[15,].png", width=8, height=6, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#ATOS EM FOCO: USO_DE_DROGAS
#########################################################################################################
#GRAFICO IDADE/SEXO

library(ggplot2)
library(scales)

setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data = df_snr_sexo_idade_USO_DE_DROGAS, aes(x=IDADE, y=QUANTIDADE, fill= SEXO)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_color_brewer(palette="Set1")+
  theme_minimal()+
  geom_text(aes(label=QUANTIDADE), vjust=0, color="black", fontface = "plain",
            position = position_dodge(0.9), size=3.5)+
  labs(title = (str_c(GRAFICO[17,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "POSSE DE DROGAS PARA USO PESSOAL",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "IDADE", y = "QUANTIDADE", fill = "SEXO") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
ggsave("GRAFICO[17,].png", width=8, height=6, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#ATOS EM FOCO: TRAFICO_DE_DROGAS
#########################################################################################################
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data = df_snr_sexo_idade_TRAFICO_DE_DROGAS, aes(x=IDADE, y=QUANTIDADE, fill= SEXO)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_color_brewer(palette="Set1")+
  theme_minimal()+
  geom_text(aes(label=QUANTIDADE), vjust=0, color="black", fontface = "plain",
            position = position_dodge(0.9), size=3.5)+
  labs(title = (str_c(GRAFICO[19,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "TRAFICO DE DROGAS",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "IDADE", y = "QUANTIDADE", fill = "SEXO") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
ggsave("GRAFICO[19,].png", width=8, height=6, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################

setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################
#Dados socioeconômicos
#########################################################################################################
#Raça/Cor
#########################################################################################################
#########################################################################################################
#########################################################################################################
#SINAL <- paste(RACA_COR_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(RACA_COR_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#SINAL <- paste(ESTADO_CIVIL_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(ESTADO_CIVIL_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#SINAL <- paste(POSSUI_FILHOS_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(POSSUI_FILHOS_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#SINAL <- paste(ESTA_GRAVIDA_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(ESTA_GRAVIDA_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################
#########################################################################################################

#########################################################################################################
#SINAL <- paste(POSSUI_DOC_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(POSSUI_DOC_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#SINAL <- paste(SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/tester/imagens"))
ggplot(data=SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp, 
       aes(x = QUANTIDADE,
           y = reorder(SERIE_ATUAL_OU_ULTIMA_CURSADA_bkp, PERCENTUAL2))) +
  geom_bar(stat="identity", fill="#bb1e23")+
  labs(title = (str_c(GRAFICO[26,],": Escolaridade, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Dados socioeconômicos",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD", 
       x = "", y = "ESCOLARIDADE") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        plot.caption =element_text(hjust = 0.5)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") + 
  scale_x_continuous(n.breaks=5)
#scale_x_continuous(limits=c(0, 90))
ggsave("GRAFICO[26,].png", width=15, height=5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#SINAL <- paste(NATUREZA_ESCOLA_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(NATUREZA_ESCOLA_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#SINAL <- paste(TRABALHA_ATUALMENTE_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(TRABALHA_ATUALMENTE_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#SINAL <- paste(RENDA_MENSAL_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(RENDA_MENSAL_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/tester/imagens"))
ggplot(data=RENDA_MENSAL_bkp, 
       aes(x = QUANTIDADE,
           y = reorder(RENDA_MENSAL_bkp, PERCENTUAL2))) +
  geom_bar(stat="identity", fill="#bb1e23")+
  labs(title = (str_c(GRAFICO[29,],": Renda Mensal, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Dados socioeconômicos",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD", 
       x = "", y = "RENDA MENSAL") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        plot.caption =element_text(hjust = 0.5)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") + 
  scale_x_continuous(n.breaks=5)
#scale_x_continuous(limits=c(0, 90))
ggsave("GRAFICO[29,].png", width=15, height=5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#SINAL <- paste(RENDA_FAMILIAR_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(RENDA_FAMILIAR_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/tester/imagens"))
ggplot(data=RENDA_FAMILIAR_bkp, 
       aes(x = QUANTIDADE,
           y = reorder(RENDA_FAMILIAR_bkp, PERCENTUAL2))) +
  geom_bar(stat="identity", fill="#bb1e23")+
  labs(title = (str_c(GRAFICO[30,],": Renda Familiar, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Dados socioeconômicos",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD", 
       x = "", y = "RENDA FAMILIAR") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        plot.caption =element_text(hjust = 0.5)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") + 
  scale_x_continuous(n.breaks=5)
#scale_x_continuous(limits=c(0, 90))
ggsave("GRAFICO[30,].png", width=15, height=5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#SINAL <- paste(TIPO_MORADIA_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(TIPO_MORADIA_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/tester/imagens"))
ggplot(data=TIPO_MORADIA_bkp, 
       aes(x = QUANTIDADE,
           y = reorder(TIPO_MORADIA_bkp, PERCENTUAL2))) +
  geom_bar(stat="identity", fill="#bb1e23")+
  labs(title = (str_c(GRAFICO[31,],": Tipo de moradia, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Dados socioeconômicos",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD", 
       x = "", y = "TIPO DE MORADIA") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        plot.caption =element_text(hjust = 0.5)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") + 
  scale_x_continuous(n.breaks=5)
#scale_x_continuous(limits=c(0, 90))
ggsave("GRAFICO[31,].png", width=15, height=5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#SINAL <- paste(NATUREZA_MORADIA_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(NATUREZA_MORADIA_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/tester/imagens"))
ggplot(data=NATUREZA_MORADIA_bkp, 
       aes(x = QUANTIDADE,
           y = reorder(NATUREZA_MORADIA_bkp, PERCENTUAL2))) +
  geom_bar(stat="identity", fill="#bb1e23")+
  labs(title = (str_c(GRAFICO[32,],": Natureza da Propriedade, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Dados socioeconômicos",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD", 
       x = "", y = "NATUREZA DA MORADIA") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        plot.caption =element_text(hjust = 0.5)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") + 
  scale_x_continuous(n.breaks=5)
#scale_x_continuous(limits=c(0, 90))
ggsave("GRAFICO[32,].png", width=15, height=5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
#########################################################################################################
#SINAL <- paste(DROGAS_USO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(DROGAS_USO_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/tester/imagens"))
ggplot(data=DROGAS_USO_bkp, 
       aes(x = QUANTIDADE,
           y = reorder(DROGAS_USO_bkp, PERCENTUAL2))) +
  geom_bar(stat="identity", fill="#bb1e23")+
  labs(title = (str_c(GRAFICO[33,],": Uso de drogas, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Dados socioeconômicos",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD", 
       x = "", y = "DROGA") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        plot.caption =element_text(hjust = 0.5)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") + 
  scale_x_continuous(n.breaks=5)
#scale_x_continuous(limits=c(0, 90))
ggsave("GRAFICO[33,].png", width=15, height=5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################

#########################################################################################################
#Medidas protetivas
#########################################################################################################
#########################################################################################################
#########################################################################################################
#SINAL <- paste(BANCO_MEDIDAS_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(BANCO_MEDIDAS_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/tester/imagens"))
ggplot(data=BANCO_MEDIDAS_bkp, 
       aes(x = QUANTIDADE,
           y = reorder(BANCO_MEDIDAS_bkp, PERCENTUAL2))) +
  geom_bar(stat="identity", fill="#bb1e23")+
  labs(title = (str_c(GRAFICO[34,],": Medidas Protetivas, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       #subtitle = "Adolescentes por MEDIDAs Infracionais",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "", y = "MEDIDAS") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        plot.caption =element_text(hjust = 0.5)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") + 
  scale_x_continuous(n.breaks=5)
#scale_x_continuous(limits=c(0, 90))
ggsave("GRAFICO[34,].png", width=15, height=5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
#GRAFICO so_decisao ALTERNATIVO
#so_decisao_original=so_decisao #salvando MEDIDAs atendimento original
so_decisao=so_decisao_bkp

so_decisao<-so_decisao%>%
  arrange(QUANTIDADE)
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=so_decisao, aes(x=DECISAO, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(so_decisao_rmark1[1,2]+10)) +
  scale_x_discrete(limits = so_decisao$DECISAO)+
  geom_text(aes(label = QUANTIDADE), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[35,],": Decisão em Audiência Preliminar, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       #subtitle = "Adolescentes por MEDIDAs Infracionais",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD", 
       x = "DECISAO", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        #plot.subtitle = element_text(hjust = 0.5, size = 14),
        plot.caption =element_text(hjust = 0.5)  ) 
ggsave("GRAFICO[35,].png", width=10.5, height=5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
#GRAFICO intervalo_decisao ALTERNATIVO
#intervalo_decisao_original=intervalo_decisao #salvando TEMPOs atendimento original
intervalo_decisao=intervalo_decisao_bkp

intervalo_decisao<-intervalo_decisao%>%
  arrange(PERCENTUAL)
SINAL <- paste(intervalo_decisao$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=intervalo_decisao, aes(x=TEMPO, y=PERCENTUAL, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(intervalo_decisao_rmark[1,3]+5)) +
  scale_x_discrete(limits = intervalo_decisao$TEMPO)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[36,],": Tempo das decisões em audiências preliminares")),
       subtitle = (str_c("Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD", 
       x = "TEMPO", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        plot.caption =element_text(hjust = 0.5)  ) 

ggsave("GRAFICO[36,].png", width=8.5, height=3, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
#GRAFICO so_sentenca ALTERNATIVO
#so_sentenca_original=so_sentenca #salvando MEDIDAs atendimento original
so_sentenca=so_sentenca_bkp

so_sentenca<-so_sentenca%>%
  arrange(QUANTIDADE)
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=so_sentenca, aes(x=SENTENCA, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(so_sentenca_rmark[1,2]+10)) +
  scale_x_discrete(limits = so_sentenca$SENTENCA)+
  geom_text(aes(label = QUANTIDADE), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[37,],": Sentenças, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       #subtitle = "Adolescentes por MEDIDAs Infracionais",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD", 
       x = "SENTENÇAS", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        #plot.subtitle = element_text(hjust = 0.5, size = 14),
        plot.caption =element_text(hjust = 0.5)  ) 

ggsave("GRAFICO[37,].png", width=8, height=4, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
#GRAFICO intervalo_sentenca ALTERNATIVO
#intervalo_sentenca_original=intervalo_sentenca #salvando TEMPOs atendimento original
intervalo_sentenca=intervalo_sentenca_bkp

intervalo_sentenca<-intervalo_sentenca%>%
  arrange(PERCENTUAL)
SINAL <- paste(intervalo_sentenca$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=intervalo_sentenca, aes(x=TEMPO, y=PERCENTUAL, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(intervalo_sentenca_rmark[1,3]+10)) +
  scale_x_discrete(limits = intervalo_sentenca$TEMPO)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = str_c(GRAFICO[38,],": Tempo das decisões após audiências preliminares"),
       subtitle = (str_c("Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD", 
       x = "TEMPO", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        plot.caption =element_text(hjust = 0.5)  ) 

ggsave("GRAFICO[38,].png", width=8.7, height=3.5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#Atos infracionais nas escolas de Belo Horizonte
#########################################################################################################

#GRAFICO IDADE/SEXO

library(ggplot2)
library(scales)

setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data = df_snr_sexo_idade_escola, aes(x=IDADE, y=QUANTIDADE, fill= SEXO)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_color_brewer(palette="Set1")+
  theme_minimal()+
  geom_text(aes(label=QUANTIDADE), vjust=0, color="black", fontface = "plain",
            position = position_dodge(0.9), size=3.5)+
  labs(title = (str_c(GRAFICO[39,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Ato infracional nas escolas",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD",
       x = "IDADE", y = "QUANTIDADE", fill = "SEXO") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
ggsave("GRAFICO[39,].png", width=8, height=6, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/tester/imagens"))
mycols <- c("#c0504d", "#4f81bd")

#salvar png
ggpie(df_snr_sexo_escola, 
      x= "QUANTIDADE", label = "PERCENTUAL",
      lab.pos = "in", lab.font = list(color = "white", face = "bold"),
      lab.adjust = 0,
      fill = "SEXO", color = "white", face="bold",
      palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  labs(caption = "FONTE: VARA INFRACIONAL/COMISSARIADO", subtitle = "Ato infracional nas escolas") +
  ggtitle((str_c(GRAFICO[40,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))))
ggsave("GRAFICO[40,].png", width=6.5, height=5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
#SINAL <- paste(ESCOLARIDADE_banco_escola_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(ESCOLARIDADE_banco_escola_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/tester/imagens"))
ggplot(data=ESCOLARIDADE_banco_escola_bkp, 
       aes(x = QUANTIDADE,
           y = reorder(ESCOLARIDADE_banco_escola_bkp, PERCENTUAL2))) +
  geom_bar(stat="identity", fill="#bb1e23")+
  labs(title = (str_c(GRAFICO[41,],": Escolaridade, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       x = "", y = "ESCOLARIDADE") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        plot.caption =element_text(hjust = 0.5)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") + 
  scale_x_continuous(n.breaks=5)
#scale_x_continuous(limits=c(0, 90))
ggsave("GRAFICO[41,].png", width=15, height=5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
#GRAFICO banco_atos_em_foco_escola ALTERNATIVO
#banco_atos_em_foco_escola_original=banco_atos_em_foco_escola #salvando atos atendimento original
banco_atos_em_foco_escola=banco_atos_em_foco_escola_bkp

banco_atos_em_foco_escola<-banco_atos_em_foco_escola%>%
  arrange(QUANTIDADE)
#salvar png
ggplot(data=banco_atos_em_foco_escola, aes(x=ATO, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, (sum(banco_atos_em_foco_escola_rmark[1,2])+0)) +
  scale_x_discrete(limits = banco_atos_em_foco_escola$ATO)+
  geom_text(aes(label = QUANTIDADE), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[42,],": Incidência atos infracionais, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Ato infracional nas escolas",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO", 
       x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) 
ggsave("GRAFICO[42,].png", width=8.7, height=3.5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################

setwd(file.path("~/tester/imagens"))
#salvar png
ggpie(primariedade_ESCOLA, 
      x= "QUANTIDADE", label = "PERCENTUAL",
      lab.pos = "out", lab.font = list(color = "black", face = "bold"),
      lab.adjust = 5,
      fill = "PRIMARIO", color = "white", face="bold",
      palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  labs(caption = "FONTE: VARA INFRACIONAL/COMISSARIADO", subtitle = "Ato infracional nas escolas") +
  ggtitle((str_c(GRAFICO[43,],": Primariedade, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))))
ggsave("GRAFICO[43,].png", width=6.5, height=5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
#GRAFICO decisao_ESCOLA ALTERNATIVO
#decisao_ESCOLA_original=decisao_ESCOLA #salvando MEDIDAs atendimento original
decisao_ESCOLA=decisao_ESCOLA_bkp

decisao_ESCOLA<-decisao_ESCOLA%>%
  arrange(QUANTIDADE)
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=decisao_ESCOLA, aes(x=DECISAO, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(decisao_ESCOLA_rmark1[1,2]+1)) +
  scale_x_discrete(limits = decisao_ESCOLA$DECISAO)+
  geom_text(aes(label = QUANTIDADE), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[44,],": Decisão, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Ato infracional nas escolas",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD", 
       x = "DECISAO", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5)  ) 
ggsave("GRAFICO[44,].png", width=10, height=5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/tester/imagens"))
mycols <- c("#c0504d", "#4f81bd")

#salvar png
ggpie(vitima_ESCOLA_bkp, 
      x= "QUANTIDADE", label = "PERCENTUAL",
      lab.pos = "out", lab.font = list(color = "black", face = "bold"),
      lab.adjust = 0,
      fill = "VITIMA", color = "white", face="bold",
      palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  labs(caption = "FONTE: VARA INFRACIONAL/COMISSARIADO", subtitle = "Ato infracional nas escolas") +
  ggtitle((str_c(GRAFICO[45,],": Vítima, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))))
ggsave("GRAFICO[45,].png", width=6.5, height=5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/tester/imagens"))

#salvar png
ggplot(data=regional_residencia_ESCOLA, aes(x=REGIONAL, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, (sum(regional_residencia_ESCOLA_rmark[1,2]+1))) +
  scale_x_discrete(limits = regional_residencia_ESCOLA$REGIONAL)+
  geom_text(aes(label = QUANTIDADE), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[46,],": Regional de Residência, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Ato infracional nas escolas",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD", 
       x = "REGIONAL", y = "Nº DE OCORRÊNCIAS") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
#scale_fill_brewer(direction = 1)
#scale_fill_manual(values=c("#c0504d","#4f81bd"))
ggsave("GRAFICO[46,].png", width=10, height=6.5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/tester/imagens"))

mycols <- c("#c0504d", "#4f81bd")

#salvar png
ggpie(escola_tipo_ESCOLA, 
      x= "QUANTIDADE", label = "PERCENTUAL",
      lab.pos = "in", lab.font = list(color = "white", face = "bold"),
      lab.adjust = 0,
      fill = "ESCOLA", color = "white", face="bold",
      palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  labs(caption = "FONTE: VARA INFRACIONAL/COMISSARIADO", subtitle = "Ato infracional nas escolas") +
  ggtitle((str_c(GRAFICO[47,],": Tipo de escola, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))))
ggsave("GRAFICO[47,].png", width=6.5, height=5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/tester/imagens"))
regional_ato_ESCOLA$REGIONAL = factor(regional_ato_ESCOLA$REGIONAL)
#salvar png
ggplot(data=regional_ato_ESCOLA, aes(x=REGIONAL, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, (sum(regional_ato_ESCOLA_rmark[1,2]+0))) +
  scale_x_discrete(limits = regional_ato_ESCOLA$REGIONAL)+
  geom_text(aes(label = QUANTIDADE), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[48,],": Regional da escola, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Ato infracional nas escolas",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD", 
       x = "REGIONAL", y = "Nº DE OCORRÊNCIAS") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
#scale_fill_brewer(direction = 1)
#scale_fill_manual(values=c("#c0504d","#4f81bd"))
ggsave("GRAFICO[48,].png", width=10, height=6.5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#Justiça restaurativa
#########################################################################################################
#GRAFICO IDADE/SEXO

library(ggplot2)
library(scales)

setwd(file.path("~/tester/imagens"))

#salvar png
ggplot(data = df_snr_sexo_idade_JR, aes(x=IDADE, y=QUANTIDADE, fill= SEXO)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_color_brewer(palette="Set1")+
  theme_minimal()+
  geom_text(aes(label=QUANTIDADE), vjust=0, color="black", fontface = "plain",
            position = position_dodge(0.9), size=3.5)+
  labs(title = (str_c(GRAFICO[49,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       x = "IDADE", y = "QUANTIDADE", fill = "SEXO") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
ggsave("GRAFICO[49,].png", width=8, height=6, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/tester/imagens"))

#salvar png
ggpie(df_snr_sexo_JR_PIZZA, 
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
  labs(caption = "FONTE: VARA INFRACIONAL/COMISSARIADO", subtitle = "Justiça Restaurativa") +
  ggtitle((str_c(GRAFICO[50,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))))
ggsave("GRAFICO[50,].png", width=6.5, height=5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
RACA_COR_jr$RACA_COR_jr = factor(RACA_COR_jr$RACA_COR_jr)
SINAL <- paste(RACA_COR_jr$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
#pdf(file="grafico_RACA_COR_jr_alternativo.pdf", title = "grafico_RACA_COR_jr", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=RACA_COR_jr, aes(x=RACA_COR_jr, y=PERCENTUAL, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, (RACA_COR_jr_rmark[1,3]+10)) +
  scale_x_discrete(limits = RACA_COR_jr$RACA_COR_jr)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[51,],": Raça/Cor, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO", 
       x = "RAÇA/COR", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
ggsave("GRAFICO[51,].png", width=6.5, height=4, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
#SINAL <- paste(ESCOLARIDADE_jr_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(ESCOLARIDADE_jr_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/tester/imagens"))
ggplot(data=ESCOLARIDADE_jr_bkp, 
       aes(x = QUANTIDADE,
           y = reorder(ESCOLARIDADE_jr_bkp, PERCENTUAL2))) +
  geom_bar(stat="identity", fill="#bb1e23")+
  labs(title = (str_c(GRAFICO[52,],": Escolaridade, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO",
       x = "", y = "ESCOLARIDADE") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        plot.caption =element_text(hjust = 0.5)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") + 
  scale_x_continuous(n.breaks=5)
#scale_x_continuous(limits=c(0, 90))
ggsave("GRAFICO[52,].png", width=15, height=5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr = factor(NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr)
SINAL <- paste(NATUREZA_ESCOLA_jr$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
#pdf(file="grafico_NATUREZA_ESCOLA_jr_alternativo.pdf", title = "grafico_NATUREZA_ESCOLA_jr", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=NATUREZA_ESCOLA_jr, aes(x=NATUREZA_ESCOLA_jr, y=PERCENTUAL, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, (NATUREZA_ESCOLA_jr_rmark[1,3]+10)) +
  scale_x_discrete(limits = NATUREZA_ESCOLA_jr$NATUREZA_ESCOLA_jr)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[53,],": Natureza da Escola, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO", 
       x = "RENDA", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
ggsave("GRAFICO[53,].png", width=6.5, height=3.5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr = factor(TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr)
SINAL <- paste(TRABALHA_ATUALMENTE_jr$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
#pdf(file="grafico_TRABALHA_ATUALMENTE_jr_alternativo.pdf", title = "grafico_TRABALHA_ATUALMENTE_jr", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=TRABALHA_ATUALMENTE_jr, aes(x=TRABALHA_ATUALMENTE_jr, y=PERCENTUAL, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, (TRABALHA_ATUALMENTE_jr_rmark[1,3]+10)) +
  scale_x_discrete(limits = TRABALHA_ATUALMENTE_jr$TRABALHA_ATUALMENTE_jr)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[54,],": Trabalho, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO", 
       x = "TRABALHO", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
ggsave("GRAFICO[54,].png", width=6.5, height=4, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
NATUREZA_DO_TRABALHO_jr$NATUREZA_DO_TRABALHO_jr = factor(NATUREZA_DO_TRABALHO_jr$NATUREZA_DO_TRABALHO_jr)
SINAL <- paste(NATUREZA_DO_TRABALHO_jr$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
#pdf(file="grafico_NATUREZA_DO_TRABALHO_jr_alternativo.pdf", title = "grafico_NATUREZA_DO_TRABALHO_jr", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=NATUREZA_DO_TRABALHO_jr, aes(x=NATUREZA_DO_TRABALHO_jr, y=PERCENTUAL, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, (NATUREZA_DO_TRABALHO_jr_rmark[1,3]+10)) +
  scale_x_discrete(limits = NATUREZA_DO_TRABALHO_jr$NATUREZA_DO_TRABALHO_jr)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[55,],": Natureza do Trabalho, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO", 
       x = "NATUREZA", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
ggsave("GRAFICO[55,].png", width=7, height=4, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
RENDA_MENSAL_jr$RENDA_MENSAL_jr = factor(RENDA_MENSAL_jr$RENDA_MENSAL_jr)
SINAL <- paste(RENDA_MENSAL_jr$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
#pdf(file="grafico_RENDA_MENSAL_jr_alternativo.pdf", title = "grafico_RENDA_MENSAL_jr", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=RENDA_MENSAL_jr, aes(x=RENDA_MENSAL_jr, y=PERCENTUAL, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, (RENDA_MENSAL_jr_rmark[1,3]+10)) +
  scale_x_discrete(limits = RENDA_MENSAL_jr$RENDA_MENSAL_jr)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[56,],": Renda Mensal, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO", 
       x = "RENDA", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
ggsave("GRAFICO[56,].png", width=6.5, height=3.5, pointsize=12, dpi = 512)
dev.off()

#########################################################################################################
#########################################################################################################
ESTADO_CIVIL_jr$ESTADO_CIVIL_jr = factor(ESTADO_CIVIL_jr$ESTADO_CIVIL_jr)
SINAL <- paste(ESTADO_CIVIL_jr$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
#pdf(file="grafico_ESTADO_CIVIL_jr_alternativo.pdf", title = "grafico_ESTADO_CIVIL_jr", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=ESTADO_CIVIL_jr, aes(x=ESTADO_CIVIL_jr, y=PERCENTUAL, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, (ESTADO_CIVIL_jr_rmark[1,3]+10)) +
  scale_x_discrete(limits = ESTADO_CIVIL_jr$ESTADO_CIVIL_jr)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[57,],": Estado Civil, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO", 
       x = "ESTADO CIVIL", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
ggsave("GRAFICO[57,].png", width=7, height=3.5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS = factor(ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS)
SINAL <- paste(ESTADO_CIVIL_PAIS$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
#pdf(file="grafico_ESTADO_CIVIL_PAIS_alternativo.pdf", title = "grafico_ESTADO_CIVIL_PAIS", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=ESTADO_CIVIL_PAIS, aes(x=ESTADO_CIVIL_PAIS, y=PERCENTUAL, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, (ESTADO_CIVIL_PAIS_rmark[1,3]+10)) +
  scale_x_discrete(limits = ESTADO_CIVIL_PAIS$ESTADO_CIVIL_PAIS)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[58,],": Estado civil dos pais, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO", 
       x = "ESTADO CIVIL", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
ggsave("GRAFICO[58,].png", width=7, height=3.5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
DROGAS_USO_jr$DROGAS_USO_jr = factor(DROGAS_USO_jr$DROGAS_USO_jr)
SINAL <- paste(DROGAS_USO_jr$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
#pdf(file="grafico_DROGAS_USO_jr_alternativo.pdf", title = "grafico_DROGAS_USO_jr", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=DROGAS_USO_jr, aes(x=DROGAS_USO_jr, y=PERCENTUAL, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, (DROGAS_USO_jr_rmark[1,3]+10)) +
  scale_x_discrete(limits = DROGAS_USO_jr$DROGAS_USO_jr)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[59,],": Uso de drogas, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO", 
       x = "DROGA", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
ggsave("GRAFICO[59,].png", width=7, height=3.5, pointsize=12, dpi = 512)
dev.off()

#########################################################################################################
#########################################################################################################
#GRAFICO BANCO_MEDIDAS_jr ALTERNATIVO
#BANCO_MEDIDAS_jr_original=BANCO_MEDIDAS_jr #salvando MEDIDAs atendimento original
BANCO_MEDIDAS_jr=BANCO_MEDIDAS_jr_bkp

BANCO_MEDIDAS_jr<-BANCO_MEDIDAS_jr%>%
  arrange(QUANTIDADE)
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=BANCO_MEDIDAS_jr, aes(x=MEDIDA, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(BANCO_MEDIDAS_jr_rmark[1,2]+0.5)) +
  scale_x_discrete(limits = BANCO_MEDIDAS_jr$MEDIDA)+
  geom_text(aes(label = QUANTIDADE), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[60,],": Medidas Protetivas, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO", 
       x = "MEDIDAS", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) 
ggsave("GRAFICO[60,].png", width=7.2, height=4, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
#GRAFICO BANCO_MEDIDAS_SOCIOEDU_jr ALTERNATIVO
#BANCO_MEDIDAS_SOCIOEDU_jr_original=BANCO_MEDIDAS_SOCIOEDU_jr #salvando MEDIDAs atendimento original
BANCO_MEDIDAS_SOCIOEDU_jr=BANCO_MEDIDAS_SOCIOEDU_jr_bkp

BANCO_MEDIDAS_SOCIOEDU_jr<-BANCO_MEDIDAS_SOCIOEDU_jr%>%
  arrange(QUANTIDADE)
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=BANCO_MEDIDAS_SOCIOEDU_jr, aes(x=MEDIDA, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(BANCO_MEDIDAS_SOCIOEDU_jr_rmark[1,2]+0.5)) +
  scale_x_discrete(limits = BANCO_MEDIDAS_SOCIOEDU_jr$MEDIDA)+
  geom_text(aes(label = QUANTIDADE), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[61,],": Medidas Socioeducativas, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO", 
       x = "MEDIDAS", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) 
ggsave("GRAFICO[61,].png", width=8.5, height=4, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
#GRAFICO banco_incidencia_jr ALTERNATIVO
#banco_incidencia_jr_original=banco_incidencia_jr #salvando atos atendimento original
banco_incidencia_jr=banco_incidencia_jr_bkp

banco_incidencia_jr<-banco_incidencia_jr%>%
  arrange(QUANTIDADE)
#salvar png
ggplot(data=banco_incidencia_jr, aes(x=ATO, y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, (sum(banco_incidencia_jr_rmark[1,2])+1)) +
  scale_x_discrete(limits = banco_incidencia_jr$ATO)+
  geom_text(aes(label = QUANTIDADE), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[62,],": Incidência atos infracionais, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Justiça Restaurativa",
       caption = "FONTE: VARA INFRACIONAL/COMISSARIADO", 
       x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) 
ggsave("GRAFICO[62,].png", width=8.5, height=3.5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#Projetos socioeducativos

#########################################################################################################
#graficos banco_matriculados_CEDIPRO
#########################################################################################################
#########################################################################################################
#########################################################################################################
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################
#########################################################################################################
banco_curso_adolescente_CEDIPRO_bkp$banco_curso_adolescente_CEDIPRO_bkp = factor(banco_curso_adolescente_CEDIPRO_bkp$banco_curso_adolescente_CEDIPRO_bkp)
#SINAL <- paste(banco_curso_adolescente_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- banco_curso_adolescente_CEDIPRO_bkp$PERCENTUAL#para plotar o sinal de porcentagem
#pdf(file="grafico_banco_curso_adolescente_CEDIPRO_bkp_alternativo.pdf", title = "grafico_banco_curso_adolescente_CEDIPRO_bkp", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################

#########################################################################################################

ggplot(data = df_snr_sexo_idade_banco_matriculados_CEDIPRO, aes(x=idade, y=QUANTIDADE, fill= sexo)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_color_brewer(palette="Set1")+
  theme_minimal()+
  geom_text(aes(label=QUANTIDADE), vjust=0, color="black", fontface = "plain",
            position = position_dodge(0.9), size=3.5)+
  labs(title = (str_c(GRAFICO[65,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Projeto CEDIPRO: alunos matriculados",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO",
       x = "IDADE", y = "QUANTIDADE", fill = "SEXO") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
ggsave("GRAFICO[65,].png", width=8, height=6, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#SINAL <- paste(ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(ESCOLARIDADE_banco_matriculados_CEDIPRO_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp$REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp = factor(REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp$REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp)
#SINAL <- paste(REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp$PERCENTUAL#para plotar o sinal de porcentagem
#pdf(file="grafico_REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp_alternativo.pdf", title = "grafico_REGIONAL_RESIDENCIAL_banco_matriculados_CEDIPRO_bkp", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################
INCIDENCIA_banco_matriculados_CEDIPRO_bkp$INCIDENCIA_banco_matriculados_CEDIPRO_bkp = factor(INCIDENCIA_banco_matriculados_CEDIPRO_bkp$INCIDENCIA_banco_matriculados_CEDIPRO_bkp)
#SINAL <- paste(INCIDENCIA_banco_matriculados_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- INCIDENCIA_banco_matriculados_CEDIPRO_bkp$PERCENTUAL#para plotar o sinal de porcentagem
#pdf(file="grafico_INCIDENCIA_banco_matriculados_CEDIPRO_bkp_alternativo.pdf", title = "grafico_INCIDENCIA_banco_matriculados_CEDIPRO_bkp", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################
#########################################################################################################
REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp$REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp = factor(REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp$REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp)
#SINAL <- paste(REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp$PERCENTUAL#para plotar o sinal de porcentagem
#pdf(file="grafico_REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp_alternativo.pdf", title = "grafico_REGIONAL_ATO_banco_matriculados_CEDIPRO_bkp", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################

#########################################################################################################
#########################################################################################################
PROTETIVAS_banco_matriculados_CEDIPRO_bkp$PROTETIVAS_banco_matriculados_CEDIPRO_bkp = factor(PROTETIVAS_banco_matriculados_CEDIPRO_bkp$PROTETIVAS_banco_matriculados_CEDIPRO_bkp)
#SINAL <- paste(PROTETIVAS_banco_matriculados_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- PROTETIVAS_banco_matriculados_CEDIPRO_bkp$PERCENTUAL#para plotar o sinal de porcentagem
#pdf(file="grafico_PROTETIVAS_banco_matriculados_CEDIPRO_bkp_alternativo.pdf", title = "grafico_PROTETIVAS_banco_matriculados_CEDIPRO_bkp", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################

#########################################################################################################
#########################################################################################################
DECISAO_banco_matriculados_CEDIPRO_bkp$DECISAO_banco_matriculados_CEDIPRO_bkp = factor(DECISAO_banco_matriculados_CEDIPRO_bkp$DECISAO_banco_matriculados_CEDIPRO_bkp)
#SINAL <- paste(DECISAO_banco_matriculados_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- DECISAO_banco_matriculados_CEDIPRO_bkp$PERCENTUAL#para plotar o sinal de porcentagem
#pdf(file="grafico_DECISAO_banco_matriculados_CEDIPRO_bkp_alternativo.pdf", title = "grafico_DECISAO_banco_matriculados_CEDIPRO_bkp", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))
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
dev.off()
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

ggplot(data = df_snr_sexo_idade_banco_desistencia_CEDIPRO, aes(x=idade, y=QUANTIDADE, fill= sexo)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_color_brewer(palette="Set1")+
  theme_minimal()+
  geom_text(aes(label=QUANTIDADE), vjust=0, color="black", fontface = "plain",
            position = position_dodge(0.9), size=3.5)+
  labs(title = (str_c(GRAFICO[73,],": Idade e Sexo, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Projeto CEDIPRO: alunos desistentes",
       caption = "FONTE: VARA INFRACIONAL/SUASE/DOPCAD/CEDIPRO",
       x = "IDADE", y = "QUANTIDADE", fill = "SEXO") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
ggsave("GRAFICO[73,].png", width=8, height=6, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#SINAL <- paste(ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(ESCOLARIDADE_banco_desistencia_CEDIPRO_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################
#########################################################################################################

#########################################################################################################
#########################################################################################################
REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp$REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp = factor(REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp$REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp)
#SINAL <- paste(REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp$PERCENTUAL#para plotar o sinal de porcentagem
#pdf(file="grafico_REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp_alternativo.pdf", title = "grafico_REGIONAL_RESIDENCIAL_banco_desistencia_CEDIPRO_bkp", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################
INCIDENCIA_banco_desistencia_CEDIPRO_bkp$INCIDENCIA_banco_desistencia_CEDIPRO_bkp = factor(INCIDENCIA_banco_desistencia_CEDIPRO_bkp$INCIDENCIA_banco_desistencia_CEDIPRO_bkp)
#SINAL <- paste(INCIDENCIA_banco_desistencia_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- INCIDENCIA_banco_desistencia_CEDIPRO_bkp$PERCENTUAL#para plotar o sinal de porcentagem
#pdf(file="grafico_INCIDENCIA_banco_desistencia_CEDIPRO_bkp_alternativo.pdf", title = "grafico_INCIDENCIA_banco_desistencia_CEDIPRO_bkp", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################
#########################################################################################################
REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp$REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp = factor(REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp$REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp)
#SINAL <- paste(REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp$PERCENTUAL#para plotar o sinal de porcentagem
#pdf(file="grafico_REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp_alternativo.pdf", title = "grafico_REGIONAL_ATO_banco_desistencia_CEDIPRO_bkp", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################

#########################################################################################################
#########################################################################################################
PROTETIVAS_banco_desistencia_CEDIPRO_bkp$PROTETIVAS_banco_desistencia_CEDIPRO_bkp = factor(PROTETIVAS_banco_desistencia_CEDIPRO_bkp$PROTETIVAS_banco_desistencia_CEDIPRO_bkp)
#SINAL <- paste(PROTETIVAS_banco_desistencia_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- PROTETIVAS_banco_desistencia_CEDIPRO_bkp$PERCENTUAL#para plotar o sinal de porcentagem
#pdf(file="grafico_PROTETIVAS_banco_desistencia_CEDIPRO_bkp_alternativo.pdf", title = "grafico_PROTETIVAS_banco_desistencia_CEDIPRO_bkp", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################

#########################################################################################################
#########################################################################################################
DECISAO_banco_desistencia_CEDIPRO_bkp$DECISAO_banco_desistencia_CEDIPRO_bkp = factor(DECISAO_banco_desistencia_CEDIPRO_bkp$DECISAO_banco_desistencia_CEDIPRO_bkp)
#SINAL <- paste(DECISAO_banco_desistencia_CEDIPRO_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- DECISAO_banco_desistencia_CEDIPRO_bkp$PERCENTUAL#para plotar o sinal de porcentagem
#pdf(file="grafico_DECISAO_banco_desistencia_CEDIPRO_bkp_alternativo.pdf", title = "grafico_DECISAO_banco_desistencia_CEDIPRO_bkp", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))
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
dev.off()
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
setwd(file.path("~/tester/imagens"))
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
dev.off()
#########################################################################################################
#########################################################################################################
#TRATAMENTO banco_curso_parente_CEDIPRO FIM
#########################################################################################################
#A letalidade de adolescentes e jovens e o sistema socioeducativo
#########################################################################################################
CAUSA_JURIDICA_let$CAUSA_JURIDICA_let = factor(CAUSA_JURIDICA_let$CAUSA_JURIDICA_let)
SINAL <- paste(CAUSA_JURIDICA_let$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
#pdf(file="grafico_CAUSA_JURIDICA_let_alternativo.pdf", title = "grafico_CAUSA_JURIDICA_let", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=CAUSA_JURIDICA_let, aes(x=CAUSA_JURIDICA_let, y=PERCENTUAL, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, (CAUSA_JURIDICA_let_rmark[1,3]+5)) +
  scale_x_discrete(limits = CAUSA_JURIDICA_let$CAUSA_JURIDICA_let)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[82,],": Letalidade por causas externas, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: INSTITUTO MÉDICO LEGAL", 
       x = "CAUSA JURÍDICA", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
ggsave("GRAFICO[82,].png", width=9.5, height=4, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
soma_intervalo_idade_HOMICIDIO$FAIXA_ETARIA = factor(soma_intervalo_idade_HOMICIDIO$FAIXA_ETARIA)
SINAL <- paste(soma_intervalo_idade_HOMICIDIO$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
#pdf(file="grafico_soma_intervalo_idade_HOMICIDIO_alternativo.pdf", title = "grafico_soma_intervalo_idade_HOMICIDIO", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=soma_intervalo_idade_HOMICIDIO, aes(x=FAIXA_ETARIA, y=PERCENTUAL, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(soma_intervalo_idade_HOMICIDIO_rmark[1,3]+5)) +
  scale_x_discrete(limits = soma_intervalo_idade_HOMICIDIO$FAIXA_ETARIA)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[83,],": Incidência de óbitos por homicídio, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: INSTITUTO MÉDICO LEGAL", 
       x = "FAIXA ETÁRIA", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
ggsave("GRAFICO[83,].png", width=10.5, height=4, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
#GRAFICO soma_intervalo_idade_IGNORADA
#soma_intervalo_idade_IGNORADA_original=soma_intervalo_idade_IGNORADA #salvando soma_intervalo_idade_IGNORADAs atendimento original

soma_intervalo_idade_IGNORADA =
  soma_intervalo_idade_IGNORADA %>%
  arrange(PERCENTUAL)

soma_intervalo_idade_IGNORADA$FAIXA_ETARIA = factor(soma_intervalo_idade_IGNORADA$FAIXA_ETARIA)
SINAL <- paste(soma_intervalo_idade_IGNORADA$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
#pdf(file="grafico_soma_intervalo_idade_IGNORADA_alternativo.pdf", title = "grafico_soma_intervalo_idade_IGNORADA", width = 10, height = 8)
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=soma_intervalo_idade_IGNORADA, aes(x=FAIXA_ETARIA, y=PERCENTUAL, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(soma_intervalo_idade_IGNORADA_rmark[1,3]+5)) +
  scale_x_discrete(limits = soma_intervalo_idade_IGNORADA$FAIXA_ETARIA)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[84,],": Incidência de óbitos por causa ignorada, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: INSTITUTO MÉDICO LEGAL", 
       x = "FAIXA ETÁRIA", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  )
ggsave("GRAFICO[84,].png", width=10.9, height=4, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
ggplot(data=tab_letalidade_geral_12_20_HOMICIDIO_bkp, aes(x=IDADE, y=QUANTIDADE, group=ANO)) +
  geom_line(aes(color=ANO) , linewidth = 1.2)+
  geom_point(aes(color=ANO), size=1.5)+
  #scale_color_brewer(palette="Dark2")
  scale_color_brewer(palette="Set1")+
  geom_label_repel(aes(label = QUANTIDADE),box.padding = unit(0.5, "lines"), max.overlaps = Inf, label.size = 0.1, size = 3 )+
  labs(title = (str_c(GRAFICO[85,],": Incidência de óbitos por homicídio, Belo Horizonte e municípios limítrofes, 2018 a ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: INSTITUTO MÉDICO LEGAL",
       x = "", y = "QUANTIDADE") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  theme(legend.position = "bottom")

ggsave("GRAFICO[85,].png", width=10, height=4, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
ggplot(data=tab_letalidade_geral_12_20_IGNORADA_bkp, aes(x=IDADE, y=QUANTIDADE, group=ANO)) +
  geom_line(aes(color=ANO) , linewidth = 1.2)+
  geom_point(aes(color=ANO), size=1.5)+
  #scale_color_brewer(palette="Dark2")
  scale_color_brewer(palette="Set1")+
  geom_label_repel(aes(label = QUANTIDADE),box.padding = unit(0.5, "lines"), max.overlaps = Inf, label.size = 0.1, size = 3 )+
  labs(title = (str_c(GRAFICO[86,],": Incidência de óbitos por causa ignorada, Belo Horizonte e municípios limítrofes, 2018 a ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: INSTITUTO MÉDICO LEGAL",
       x = "", y = "QUANTIDADE") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) +
  theme(legend.position = "bottom")

ggsave("GRAFICO[86,].png", width=10.5, height=4, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/tester/imagens"))

ggplot(data=banco_ATUAL_IG_HOM, aes(x=ANO, y=QUANTIDADE, fill=CAUSA_JURIDICA)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_color_brewer(palette="Set1")+
  theme_minimal()+
  geom_text(aes(label=QUANTIDADE), vjust=1.6, color="white", fontface = "bold",
            position = position_dodge(0.9), size=3.5)+
  labs(title = (str_c(GRAFICO[87,],": Óbitos por homicídio e causa ignorada, Belo Horizonte e municípios limítrofes, 2018 a ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: INSTITUTO MÉDICO LEGAL",
       x = "ANO", y = "QUANTIDADE", fill = "CAUSA JURÍDICA") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) 

ggsave("GRAFICO[87,].png", width=11, height=4, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/tester/imagens"))
ggplot(data=passagem_cia_GERAL_HOM_IGN_LET, aes(x=CAUSA_JURIDICA, y=QUANTIDADE, fill=PASSAGEM_CIABH)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_color_brewer(palette="Set1")+
  theme_minimal()+
  geom_text(aes(label=QUANTIDADE), vjust=1.6, color="white", fontface = "bold",
            position = position_dodge(0.9), size=3.5)+
  labs(title = (str_c(GRAFICO[88,],": Óbitos por causas violentas e Passagem CIABH, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "LETALIDADE",
       caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
       x = "", y = "QUANTIDADE", fill = "PASSAGEM CIABH") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) 

ggsave("GRAFICO[88,].png", width=11.5, height=5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/tester/imagens"))
ggplot(data=passagem_cia_HOMICIDIO_LET, aes(x=IDADE, y=QUANTIDADE, fill=PASSAGEM_CIABH)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_color_brewer(palette="Set1")+
  theme_minimal()+
  geom_text(aes(label=QUANTIDADE), vjust=1.6, color="white", fontface = "bold",
            position = position_dodge(0.9), size=3.5)+
  labs(title = (str_c(GRAFICO[89,],": Óbitos por homicídio e passagem pelo CIABH, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
       x = "", y = "QUANTIDADE", fill = "PASSAGEM CIABH") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) 

ggsave("GRAFICO[89,].png", width=11.2, height=5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/tester/imagens"))
ggplot(data=passagem_cia_IGNORADA_LET, aes(x=IDADE, y=QUANTIDADE, fill=PASSAGEM_CIABH)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_color_brewer(palette="Set1")+
  theme_minimal()+
  geom_text(aes(label=QUANTIDADE), vjust=1.6, color="white", fontface = "bold",
            position = position_dodge(0.9), size=3.5)+
  labs(title = (str_c(GRAFICO[90,],":  Óbitos por causa ignorada e passagem pelo CIABH, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL",
       x = "", y = "QUANTIDADE", fill = "PASSAGEM CIABH") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        plot.caption =element_text(hjust = 0.5, size = 12)  ) 

ggsave("GRAFICO[90,].png", width=11.8, height=5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
#OBS: GRÁFICOS p1 e p2 no script 022_letalidade
setwd(file.path("~/tester/imagens"))

p <- plot_grid(p1, p2)

title <- ggdraw() + draw_label(str_c(GRAFICO[91,],": Óbitos de adolescente e jovens (12 a 20 anos) por homicídio e causa ignorada, Sexo, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y")), fontface='bold')
subtitle <- ggdraw() + draw_label("Letalidade", size = 12)
#caption <- ggdraw() + draw_label("FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL", fontface=NULL)

p3 = plot_grid(title, subtitle, p, ncol=1, rel_heights=c(0.1, 0,1, 0,1)) # rel_heights values control title margins
ggdraw(add_sub(p3, "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL", size = 12))
ggsave("GRAFICO[91,].png", width=16, height=6.9, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
#OBS: GRÁFICOS p1_PASSAGEM_CIA e p2_PASSAGEM_CIA no script 022_letalidade
setwd(file.path("~/tester/imagens"))

p_PASSAGEM_CIA <- plot_grid(p1_PASSAGEM_CIA, p2_PASSAGEM_CIA)

title <- ggdraw() + draw_label(str_c(GRAFICO[92,],": Óbitos por causas violentas de adolescentes e jovens (12 a 20 anos) com passagem pelo CIABH, Sexo, ", format(Sys.Date()-365*2, "%Y")), fontface='bold')
subtitle <- ggdraw() + draw_label("Letalidade", size = 12)
#caption <- ggdraw() + draw_label("FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL", fontface=NULL)

p3_PASSAGEM_CIA = plot_grid(title, subtitle, p_PASSAGEM_CIA, ncol=1, rel_heights=c(0.1, 0,1, 0,1)) # rel_heights values control title margins
ggdraw(add_sub(p3_PASSAGEM_CIA, "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL", size = 12))
ggsave("GRAFICO[92,].png", width=13.5, height=6.1, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
#GRAFICO banco_COR_LET ALTERNATIVO
#banco_COR_LET_original=banco_COR_LET #salvando RACA_CORs atendimento original
banco_COR_LET=banco_COR_LET_bkp

banco_COR_LET<-banco_COR_LET%>%
  arrange((QUANTIDADE))
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=banco_COR_LET, aes(x= reorder(RACA_COR, -QUANTIDADE), y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(banco_COR_LET_rmark[1,2]+5)) +
  geom_text(aes(label = QUANTIDADE), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[93,],": Raça/Cor, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: INSTITUTO MÉDICO LEGAL", 
       x = "RAÇA/COR", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        plot.caption =element_text(hjust = 0.5)  ) +
  scale_x_discrete(limits = banco_COR_LET$RACA_COR)

ggsave("GRAFICO[93,].png", width=9, height=3.5, pointsize=12, dpi = 512)
dev.off()

#########################################################################################################
#########################################################################################################
#GRAFICO banco_PROCEDENCIA_LET ALTERNATIVO
#banco_PROCEDENCIA_LET_original=banco_PROCEDENCIA_LET #salvando PROCEDENCIAs atendimento original
banco_PROCEDENCIA_LET=banco_PROCEDENCIA_LET_bkp

banco_PROCEDENCIA_LET<-banco_PROCEDENCIA_LET%>%
  arrange((QUANTIDADE))
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=banco_PROCEDENCIA_LET, aes(x= reorder(PROCEDENCIA, -QUANTIDADE), y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(banco_PROCEDENCIA_LET_rmark[1,2]+5)) +
  geom_text(aes(label = QUANTIDADE), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[94,],": Localidade do óbito, Belo Horizonte e municípios limítrofes, ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: INSTITUTO MÉDICO LEGAL", 
       x = "LOCAL DA MORTE", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        plot.caption =element_text(hjust = 0.5)  ) +
  scale_x_discrete(limits = banco_PROCEDENCIA_LET$PROCEDENCIA)

ggsave("GRAFICO[94,].png", width=10.5, height=3.5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################

#replace "%" with "" in the percentual column
tempo_medio_geral_OBITO_TAB_02_bkp$PERCENTUAL2 <- str_replace (tempo_medio_geral_OBITO_TAB_02_bkp$PERCENTUAL, "%", "")
tempo_medio_geral_OBITO_TAB_02_bkp$PERCENTUAL2 = as.numeric(tempo_medio_geral_OBITO_TAB_02_bkp$PERCENTUAL2)

#SINAL <- paste(tempo_medio_geral_OBITO_TAB_02_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(tempo_medio_geral_OBITO_TAB_02_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/tester/imagens"))
ggplot(data=tempo_medio_geral_OBITO_TAB_02_bkp, 
       aes(x = QUANTIDADE,
           y = reorder(tempo_medio_geral_OBITO_TAB_02_bkp, PERCENTUAL2))) +
  geom_bar(stat="identity", fill="#bb1e23")+
  labs(title = (str_c(GRAFICO[95,],": Tempo entre última entrada CIABH e óbito")),
       subtitle = (str_c("Belo Horizonte, ", format(Sys.Date()-365*2, "%Y"))),
       caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL", 
       x = "", y = "TEMPO") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        plot.caption =element_text(hjust = 0.5)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") + 
  #scale_x_continuous(n.breaks=12)
  scale_x_continuous(limits=c(0, 15))
ggsave("GRAFICO[95,].png", width=8, height=3, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################

#replace "%" with "" in the percentual column
tempo_medio_geral_CIABH_TAB_02_bkp$PERCENTUAL2 <- str_replace (tempo_medio_geral_CIABH_TAB_02_bkp$PERCENTUAL, "%", "")
tempo_medio_geral_CIABH_TAB_02_bkp$PERCENTUAL2 = as.numeric(tempo_medio_geral_CIABH_TAB_02_bkp$PERCENTUAL2)

#SINAL <- paste(tempo_medio_geral_CIABH_TAB_02_bkp$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
SINAL <- paste(tempo_medio_geral_CIABH_TAB_02_bkp$PERCENTUAL2, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/tester/imagens"))
ggplot(data=tempo_medio_geral_CIABH_TAB_02_bkp, 
       aes(x = QUANTIDADE,
           y = reorder(tempo_medio_geral_CIABH_TAB_02_bkp, PERCENTUAL2))) +
  geom_bar(stat="identity", fill="#bb1e23")+
  labs(title = (str_c(GRAFICO[96,],": Tempo entre a primeira e a última entrada")),
       subtitle = (str_c("Belo Horizonte, ", format(Sys.Date()-365*2, "%Y"))),
       caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL", 
       x = "", y = "TEMPO") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        plot.caption =element_text(hjust = 0.5)  ) +
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") + 
  #scale_x_continuous(n.breaks=12)
  scale_x_continuous(limits=c(0, 14))
ggsave("GRAFICO[96,].png", width=8, height=3, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#GRAFICO numero_ENTRADAS ALTERNATIVO
#numero_ENTRADAS_original=numero_ENTRADAS #salvando TEMPOs atendimento original
numero_ENTRADAS=numero_ENTRADAS_bkp

numero_ENTRADAS<-numero_ENTRADAS%>%
  arrange(PERCENTUAL)
SINAL <- paste(numero_ENTRADAS$PERCENTUAL, "%", sep=" ")#para plotar o sinal de porcentagem
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=numero_ENTRADAS, aes(x=ENTRADAS, y=PERCENTUAL, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(numero_ENTRADAS_rmark[1,3]+5)) +
  scale_x_discrete(limits = numero_ENTRADAS$ENTRADAS)+
  geom_text(aes(label = SINAL), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[97,],": Entradas")),
       subtitle = (str_c("Belo Horizonte, ", format(Sys.Date()-365*2, "%Y"))),
       caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL", 
       x = "ENTRADAS", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        plot.caption =element_text(hjust = 0.5)  ) 

ggsave("GRAFICO[97,].png", width=8, height=3, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/tester/imagens"))

#salvar png
ggpie(graf_pizza_MEDIDAS_LET, 
      x= "QUANTIDADE", label = "PERCENTUAL",
      lab.pos = "in", lab.font = list(color = "white", face = "bold"),
      lab.adjust = 0,
      fill = "MEDIDA PROTETIVA", color = "white", face="bold",
      palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  labs(caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL", 
       subtitle = "Letalidade") +
  ggtitle((str_c(GRAFICO[98,],": Aplicação de medida protetiva, Belo Horizonte, ", format(Sys.Date()-365*2, "%Y"))))
ggsave("GRAFICO[98,].png", width=8.5, height=5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
#GRAFICO BANCO_MEDIDAS_LET ALTERNATIVO
#BANCO_MEDIDAS_LET_original=BANCO_MEDIDAS_LET #salvando MEDIDAs atendimento original
BANCO_MEDIDAS_LET=BANCO_MEDIDAS_LET_bkp

BANCO_MEDIDAS_LET<-BANCO_MEDIDAS_LET%>%
  arrange((QUANTIDADE))
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=BANCO_MEDIDAS_LET, aes(x= reorder(MEDIDA, -QUANTIDADE), y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(BANCO_MEDIDAS_LET_rmark[1,2]+2)) +
  geom_text(aes(label = QUANTIDADE), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[99,],": Medidas Protetivas, Belo Horizonte, ", format(Sys.Date()-365*1, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL", 
       x = "MEDIDAS", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        plot.caption =element_text(hjust = 0.5)  ) +
  scale_x_discrete(limits = BANCO_MEDIDAS_LET$MEDIDA)

ggsave("GRAFICO[99,].png", width=7.2, height=3.5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
#GRAFICO BANCO_MEDSOC_LET ALTERNATIVO
#BANCO_MEDSOC_LET_original=BANCO_MEDSOC_LET #salvando MEDSOC atendimento original
BANCO_MEDSOC_LET=BANCO_MEDSOC_LET_bkp

BANCO_MEDSOC_LET<-BANCO_MEDSOC_LET%>%
  arrange((QUANTIDADE))
setwd(file.path("~/tester/imagens"))
#salvar png
ggplot(data=BANCO_MEDSOC_LET, aes(x= reorder(DECISAO_APLICADA2, -QUANTIDADE), y=QUANTIDADE, fill = NULL)) +
  geom_bar(stat="identity", fill="#bb1e23")+
  coord_flip()+
  ylim(0, sum(BANCO_MEDSOC_LET_rmark[1,2]+5)) +
  geom_text(aes(label = QUANTIDADE), hjust = 0, nudge_x = 0.05, colour= "#bb1e23") +
  labs(title = (str_c(GRAFICO[100,],": Medidas socioeducativas aplicadas, Belo Horizonte, ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL", 
       x = "MEDSOC", y = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        plot.caption =element_text(hjust = 0.5)  ) +
  scale_x_discrete(limits = BANCO_MEDSOC_LET$DECISAO_APLICADA2)

ggsave("GRAFICO[100,].png", width=10, height=3.5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
#GRAFICO IDADE/SEXO

library(ggplot2)
library(scales)

setwd(file.path("~/tester/imagens"))

#salvar png
ggplot(data = banco_EXTPUNIB_sexo_idade_LET, aes(x=IDADE, y=QUANTIDADE, group = SEXO)) +
  geom_col(aes(fill = SEXO), position = "dodge") +
  # ggtitle("GRÁFICO 35: Extinção de punibilidade, Belo Horizonte, 2021") +
  theme_bw() +
  labs(title = (str_c(GRAFICO[101,],": Extinção de punibilidade, Belo Horizonte, ", format(Sys.Date()-365*2, "%Y"))),
       subtitle = "Letalidade",
       caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0,  size = 12), 
        plot.caption =element_text(hjust = 0.5, vjust = 1, size = 12)  ) +
  geom_text(
    aes(label = QUANTIDADE),
    position = position_dodge(0.9),
    vjust = 0)+
  scale_color_brewer(palette="Set1")
#scale_fill_manual(values=c("#c0504d","#4f81bd"))
ggsave("GRAFICO[101,].png", width=8, height=6, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/tester/imagens"))

#salvar png
ggpie(banco_EXTPUNIB_sexo_LET, 
      x= "QUANTIDADE", label = "PERCENTUAL",
      lab.pos = "in", lab.font = list(color = "white", face = "bold"),
      lab.adjust = 0,
      fill = "SEXO", color = "white", face="bold",
      palette = "Set1") +
  theme(legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, face="bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.5, vjust = 0.5, size = 12)) +
  labs(caption = "FONTE: VARA INFRACIONAL/INSTITUTO MÉDICO LEGAL", subtitle = "Letalidade") +
  ggtitle((str_c(GRAFICO[102,],": Extinção de punibilidade, Belo Horizonte, ", format(Sys.Date()-365*2, "%Y"))))
ggsave("GRAFICO[102,].png", width=7.2, height=5, pointsize=12, dpi = 512)
dev.off()
#########################################################################################################
#########################################################################################################
setwd(file.path("~/tester/scripts_v_001/"))#configurar diretorio
#########################################################################################################