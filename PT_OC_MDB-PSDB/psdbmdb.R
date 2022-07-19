library(haven)
library(ggplot2)
library(ggtext)
library(ragg)
library(showtext)
library(tidyverse)

PREPPS_Latam_V2 <- read_dta('PREPPS Latam V2.dta')

#Data Wrangling
PREPPS <- PREPPS_Latam_V2 %>%
          filter(partyname_en == "Brazilian Democratic Movement" | partyname_en == "Brazilian Social Democracy Party") %>%
          filter(dimensionname == "Left-Right" |
                 #dimensionname == "Social Policy" | 
                 dimensionname == "Privatization" | 
                 dimensionname == "Deregulation") %>%
          summarise(partyname_en, dimensionname, score_pos) %>%
          mutate(nome = case_when(partyname_en == "Brazilian Democratic Movement" ~ "MDB",
                                  partyname_en == "Brazilian Social Democracy Party" ~ "PSDB")) %>%
          mutate(dimensao = case_when(dimensionname == "Left-Right" ~ "Esquerda-Direita",
                                      #dimensionname == "Social Policy" ~ "Política Social",
                                      dimensionname == "Privatization" ~ "Privatização",
                                      dimensionname == "Deregulation" ~ "Desregulamentação")) %>%
          summarise(nome, dimensao, score_pos)


PREPPS2 <- PREPPS %>%
          filter(dimensao == "Política Social")

PREPPS2$dimension <- (PREPPS2$score_pos-20)*-1

PREPPS2 <- PREPPS2 %>%
          summarise(nome, dimensao, dimension) %>%
          rename(score_pos = dimension)

PREPPINHO <- rbind(PREPPS, PREPPS2)

PREPPINHO <- PREPPINHO %>%
            mutate(across(dimensao, factor, levels=c("Esquerda-Direita","Desregulamentação", "Política Social", "Privatização")))

##fonts 
font_add(family="regular", "NotoSansJP-Regular.otf")
font_add(family="bold", "Roboto-Bold.ttf")
showtext_auto()

## Theme 
trialtheme <- theme(
  #Title, Subtitle, Caption
  plot.title=element_text(family="bold", hjust=0, vjust = 1, size=45, color="black"),
  plot.title.position = "plot",
  plot.subtitle = element_markdown(family="bold", size=30, hjust=0, color="black"),
  plot.caption=element_text(family="bold", size=30, color="black", hjust=1),
  plot.caption.position = "plot",
  #Panel and Background
  panel.border=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "#Ebebad"),
  plot.background = element_rect(fill = "#Ebebad"),
  plot.margin=ggplot2::margin(0.5, 0.5, 0.5, 0.5, "in"),
  panel.spacing = unit(2, "lines"),
  #Axes
  axis.ticks.length=unit(0.15, "cm"),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  axis.title = element_blank(),
  axis.text.y = element_text(size=30, family="regular", color="black"),
  axis.text.x = element_blank(),
  #MultiPlot 
  strip.text = element_text(family="regular", size=35, color="black"),
  strip.background = element_blank(),
  #Legend
  legend.position = "none")



##ggplot
psdbmdb <- PREPPINHO %>%
  ggplot(aes(nome, score_pos, fill = nome)) +
  geom_bar(stat = "identity", width = 0.9) + coord_cartesian(expand=FALSE) +
  scale_fill_manual(values=c("#1b7736", "#4245b6")) +
  geom_hline(yintercept=20, linetype="dotted",color = "black", size=0.3) +
  geom_hline(yintercept=15, linetype="dotted",color = "black", size=0.3) +
  geom_hline(yintercept=10, linetype="dotted",color = "black", size=0.3) +
  geom_hline(yintercept= 5, linetype="dotted",color = "black", size=0.3) +
  geom_hline(yintercept= 0, linetype="dotted",color = "black", size=0.3) +
  facet_wrap(~dimensao) + 
  ylim(c(0,20)) +
  labs(title = "Diferentes mas... nem tanto assim?",
       subtitle = "Comparação entre o <span style='color:#1b7736'>MDB</span> e o <span style='color:#4245b6'>PSDB</span> em 4 diferentes dimensões",
       x = "",
       y = "",
       caption = "Dados: Wiesehomeier et al. (2021) | @oc_ipolunb") +
  trialtheme


ggsave("psdbmdb.png",
       plot=psdbmdb,
       device = agg_png(width = 6, height = 6, units = "in", res = 300))