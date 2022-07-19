#looad packages
library(gghighlight)
library(ggplot2)
library(ragg)
library(showtext)
library(tidyverse)

#load data
camara = 
  tribble(
    ~party,~stance, ~votes, 
    "PP", "Liberado", 73.17,
    "AVANTE", "Não", 75,
    "PSL", "Sim", 96.23,
    "PT", "Não", 96.23,
    "PL", "Não", 82.93,
    "PSD", "Não", 88.57,
    "MDB", "Não", 75.76,
    "PSDB", "Não", 81.25,
    "REPUBLICANOS", "Sim", 90.62,
    "PSB", "Não", 90.32, 
    "DEM", "Não", 75,
    "PDT", "Não", 96, 
    "SOLIDARIEDADE", "Não", 85.71,
    "PSC", "Liberado", 100, 
    "PROS", "Liberado",81.82,
    "PTB", "Liberado", 80,
    "PODE", "Sim", 80,
    "PSOL", "Não", 88.89,
    "NOVO", "Liberado", 100,
    "PCdoB", "Não", 100,
    "CIDADANIA", "Não", 100, 
    "PATRIOTA", "Liberado", 100,
    "PV", "Não", 100,
    "REDE", "Não", 100,
    "a", "Sim", 0,
    "b", "Liberado", 0)

camara <- camara %>%
  arrange(desc(votes)) %>%
  arrange(desc(stance)) %>%
  mutate(party = factor(party, levels = c("PSL", "REPUBLICANOS", "PODE", "a",
                                          "NOVO", "PATRIOTA", "PSC", "PROS", "PTB", "PP", "b",
                                          "CIDADANIA", "PCdoB", "PV", "REDE", "PT", "PDT", "PSB", "PSOL", "PSD", "SOLIDARIEDADE", "PL", "PSDB", "MDB", "AVANTE", "DEM")))
    

#Media de participação    
mean(camara$votes)    
[1] 89.0625

bancada <- c("Sim" ="#51BB6F",
                 "Liberado" =  "#4E6E9B",
                 "Não" = "#Df1d31")


#load fonts -----
font_add(family="regular", "NotoSansJP-Regular.otf")
font_add(family="bold", "Roboto-Bold.ttf")
showtext_auto()

#Plot
impresso_voto <- camara %>% ggplot(aes(x = party, y = votes, fill = stance))+
  geom_bar(stat = "identity", width = 0.9) + coord_cartesian(expand=FALSE) +
  gghighlight(votes >= 89.0625, 
              unhighlighted_params = list(fill = NULL, alpha = 0.5)) +
  scale_x_discrete(breaks=camara$party[nchar(as.character(camara$party))!=1],
                   guide = guide_axis(n.dodge=3)) +
  geom_hline(yintercept = 88.73, color = "black", linetype="dashed", size = 1.0) +
  annotate("text", label = "Média da presença", x = 23, y = 91, size = 15, color = "black", family = "regular") +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 89, 100)) +
  #ylim(c(0,100)) +
  scale_fill_manual(values = bancada) +
  labs(title = "% de parlamentares presentes na votação do voto impresso por partido",
       subtitle = "Total de parlamentares de cada partido - ausentes e abstenções",
       x = "",
       y = "Presença por legenda",
       caption = "Dados: Poder360 | @oc_ipolunb") +
  guides(fill = guide_legend(#Title, Subtitle, Caption
                             title = "Orientção por Partido",
                             title.position = "left",
                             title.hjust = 0.5,
                             #Label
                             label.position = "bottom",
                             label.hjust = 0.5,
                             #Minor adjustments
                             nrow = 1,
                             reverse = F,
                             keyheight = 1,
                             keywidth = 5)) +
  theme(
    #Title, Subtitle, Caption
    plot.title = element_text(family="bold", hjust = 0.5),
    plot.subtitle = element_text(family="regular", hjust = 0.5),
    plot.caption = element_text(family="bold", size = 24, margin = margin(20,0,0,0)),
    #Axes
    axis.title.y = element_text(family="bold"),
    axis.text = element_text(family="regular"),
    #Legend
    legend.position = "top",
    legend.text = element_text(family="regular"),
    #Plus
    text = element_text(size = 30))
      
  
ggsave("voto_impresso.png",
       plot=impresso_voto,
       device = agg_png(width = 10, height = 8, units = "in", res = 300))
  

