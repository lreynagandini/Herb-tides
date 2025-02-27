library(ggrepel)
library(ggsci)
library(ggtext)

drywet<-read.csv("12 - Descomposicion - Hoja 5.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

str(drywet)
{
drywet$porc.peso.perdido.corr <- as.numeric(drywet$porc.peso.perdido.corr)
drywet$ID <- as.factor(drywet$ID)
drywet$Marisma <- as.factor(drywet$Marisma)
drywet$Especie <- as.factor(drywet$Especie)
drywet$Herbivoria <- as.factor(drywet$Herbivoria)
drywet$PSf <- as.numeric(drywet$`PS final`)
}

citation("stats")

# Room humidity ####

ggplot(data = drywet %>%dplyr::filter(Sobre=="CONT"), aes(x = Especie, y = porc.peso.perdido.corr)) + 
  geom_boxplot()+
  theme_cowplot()

drywetCONTmod<-aov(porc.peso.perdido ~Especie, data = drywet %>%dplyr::filter(Sobre=="CONT"))
check_normality(drywetCONTmod)
check_heteroscedasticity(drywetCONTmod)
summary(drywetCONTmod)

drywet %>% dplyr::filter(Sobre=="CONT") %>%
  summarise(mean=mean(porc.peso.perdido))

#Los controles NO difieren en PS por especie, ambos tienen el mismo contenido de humedad (7.84%)


# Experiment ####
## Litter dry weight remaining (%) (Corregido por humedad) ####

drywetmod<-glmmTMB(porc.peso.perdido.corr ~Herbivoria*Especie*Marisma, data = drywet %>%dplyr::filter(Sobre!="CONT", Sobre!="mufla"), family = gaussian, dispformula = ~Especie)

plot(simulateResiduals(drywetmod))

summary(drywetmod)

cld((lsmeans(drywetmod, c("Especie","Marisma","Herbivoria"))), Letters=letters)

drywet_summarya <- drywet %>%
  filter(Sobre != "CONT", Sobre != "mufla") %>%
  group_by(Herbivoria, Marisma, Especie) %>%
  summarise(
    mean_porc_PS = 100-mean(as.numeric(porc.peso.perdido.corr), na.rm = TRUE),
    se_porc_PS = sd(as.numeric(porc.peso.perdido.corr), na.rm = TRUE) / sqrt(n()))

ggplot(data = drywet_summarya, aes(x = Herbivoria, y = mean_porc_PS, fill = Marisma)) + 
  geom_col(alpha = .7, show.legend = T, size=.6, position = position_dodge(width = 1), col="gray33", width=.7) +
  geom_errorbar(aes(ymin = mean_porc_PS - se_porc_PS, 
                    ymax = mean_porc_PS + se_porc_PS), width = .2, 
                position = position_dodge(width = 1)) + 
  theme_cowplot() +
  scale_y_continuous(limits = c(0, 102)) +  guides(fill = guide_legend("Fooding frequency"),color = "none") +
  guides(fill = guide_legend("Flooding frequency"), color = "none" ) +
  labs(x = "Marisma", y = "Litter dry weight remaining (%)") +
  facet_wrap(~ Especie, strip.position = "bottom",
             labeller = labeller(Especie = c(
               "S.densi" = "italic('S. densiflora')",
               "S.alterni" = "italic('S. alterniflora')"),
               .default = label_parsed)) +
  scale_fill_manual(values = c("skyblue", "skyblue4"),
                    labels = c("Alta" = "Infrequently", 
                               "Baja" = "Frequently")) +
  scale_x_discrete(labels = c("Comido" = "With\nherbivory", 
                              "No comido" = "Without\nherbivory")) +
  theme(legend.position = "bottom",
        strip.text.x = element_text(),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.spacing = unit(0, "lines"),
        strip.text.y = element_text(angle = 0),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(vjust = 0.5),
        axis.line.y.right = element_blank(),    
        axis.ticks.y.right = element_blank(),   
        axis.text.y.right = element_blank())+
  geom_text(data = drywet_summarya, 
            aes(label = round(((drywet_summarya$mean_porc_PS)),digits=1), 
                y = (((drywet_summarya$mean_porc_PS)-10))), 
            position = position_dodge(width = 1), vjust = 0, size = 4, color = "black")+
  geom_text(data = drywet_summarya, 
            aes(label = c("c", "f", "a", "d", "c", "f", "b", "e"), 
                y = 100), 
            position = position_dodge(width = 1), vjust = 0, size = 5, color = "black")


## OM content (%)#### 

drywet22<-read.csv("12 - Descomposicion - Hoja 12.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

str(drywet22)
{
  drywet22$Marisma <- as.factor(drywet22$Marisma)
  drywet22$Especie <- as.factor(drywet22$Especie)
  drywet22$Herbivoria <- as.factor(drywet22$Herbivoria)
  drywet22$Tiempo <- as.factor(drywet22$Tiempo)
}

drywetmodporc.MOfinal <-glmmTMB(MO ~ Herbivoria*Especie*Marisma*Tiempo, data = drywet22, family = gaussian, dispformula = ~Especie)

plot(simulateResiduals(drywetmodporc.MOfinal))

summary(drywetmodporc.MOfinal)

cld((emmeans(drywetmodporc.MOfinal, c("Especie", "Tiempo","Marisma","Herbivoria"))), Letters=letters)

drywet_summaryMO <- drywet22 %>%
  filter(Tiempo!= "inicial") %>%
  group_by(Herbivoria, Marisma, Especie, Tiempo) %>%
  summarise(
    mean_finalMO = round(mean(MO),2), 
    se_finalMO = sd((MO), na.rm = TRUE) / sqrt(n()))
drywet_summaryMO

drywet_summaryInicial <- drywet22 %>%
  filter(Tiempo== "inicial", Marisma=="Alta", Herbivoria=="Comido") %>%
  group_by(Especie) %>%
  summarise(
    mean_MO = mean(MO), 
    se_MO = sd((MO), na.rm = TRUE) / sqrt(n()))
drywet_summaryInicial

ggplot(data = drywet_summaryMO, aes(x = Herbivoria, y = mean_finalMO, fill = Marisma)) + 
  geom_rect(data = subset(drywet_summaryMO, Especie == "S.alterni"),
            aes(xmin = 0.4, xmax = 2.6, ymin = 80.75 - 0.630, ymax = 80.75 + 0.630), 
            fill = "gray33", alpha = 0.1) +
  geom_rect(data = subset(drywet_summaryMO, Especie == "S.densi"), 
            aes(xmin = 0.4, xmax = 2.6, ymin = 93.96 - 0.303, ymax = 93.96 + 0.303), 
            fill = "gray33", alpha = 0.1) +
  geom_hline(data = drywet_summaryMO[drywet_summaryMO$Especie == "S.alterni", ], aes(yintercept = 80.75-0.630), color = "gray16", size=.4) +
  geom_hline(data = drywet_summaryMO[drywet_summaryMO$Especie == "S.alterni", ], aes(yintercept = 80.75+0.630), color = "gray16", size=.4) +
  geom_hline(data = drywet_summaryMO[drywet_summaryMO$Especie == "S.densi", ], aes(yintercept = 93.96-0.303), color = "gray16", size=.4) +
  geom_hline(data = drywet_summaryMO[drywet_summaryMO$Especie == "S.densi", ], aes(yintercept = 93.96+0.303), color = "gray16", size=.4) +
  geom_col(alpha = .7, show.legend = T, size=.6, position = position_dodge(width = 1), col="gray33", width=.7) +
  geom_errorbar(aes(ymin = mean_finalMO - se_finalMO, 
                    ymax = mean_finalMO + se_finalMO), width = .2, 
                position = position_dodge(width = 1))  +
  theme_cowplot() +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100),
                     limits = c(0, 120)) +  
  guides(fill = guide_legend("Flooding frequency"),
         color = "none") +
  labs(x = "Marisma", 
       y = "OM content (%)") +
  facet_wrap(~ Especie, strip.position = "bottom",
             labeller = labeller(Especie = c(
               "S.densi" = "italic('S. densiflora')",
               "S.alterni" = "italic('S. alterniflora')"),
               .default = label_parsed)) +
  scale_fill_manual(values = c("skyblue", "skyblue4"),
                    labels = c("Alta" = "Infrequently", 
                               "Baja" = "Frequently")) +
  scale_x_discrete(labels = c("Comido" = "With\nherbivory", 
                              "No comido" = "Without\nherbivory")) +
  theme(
    legend.position = "bottom",
    strip.text.x = element_text(),
    strip.background = element_blank(),
    strip.placement = "outside",
    panel.spacing = unit(0, "lines"),
    strip.text.y = element_text(angle = 0),
    axis.title.x = element_blank(), 
    axis.text.x = element_text(vjust = 0.5),
    axis.line.y.right = element_blank(),    
    axis.ticks.y.right = element_blank(),   
    axis.text.y.right = element_blank())+
  geom_text(data = drywet_summary, 
            aes(label = round(((drywet_summaryMO$mean_finalMO)),digits=2), 
                y = (((drywet_summary$mean_finalMO)+5))), 
            position = position_dodge(width = 1),  
            vjust = 0, size = 4, color = "black") +
  geom_label(data = drywet_summaryMO[drywet_summaryMO$Especie == "S.alterni", ], 
             aes(x = 1.5, y = 70, label = "80.75 (0.63)"), 
             inherit.aes = FALSE, 
             size = 3.5, vjust = -0.5, label.size = 0, fill = "white", alpha=.3) +
  geom_label(data = drywet_summaryMO[drywet_summaryMO$Especie == "S.densi", ], 
             aes(x = 1.5, y = 83, label = "93.96 (0.3)"), 
             inherit.aes = FALSE, 
             size = 3.5, vjust = -0.5, label.size = 0, fill = "white", alpha=.3) +
  geom_text(aes(label = c("ab","cd","cde","de","b","cde","cde","e"),
                y = 115),
            position = position_dodge(width = 1),
            vjust = 0, size = 5, color = "black")
