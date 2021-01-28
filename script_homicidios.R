library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(geobr)
library(scales)
library(geofacet)

setwd("C:\\Users\\kahel\\OneDrive\\Documents\\Coisas do R\\scripts\\homicidios")

vitimas <- read_excel("vitimas.xlsx")

homicidio <- vitimas %>% 
  rename(tipo_crime = `Tipo Crime`) %>% 
  rename(vitimas = Vítimas) %>% 
  rename(mes = Mês) %>% 
  filter(tipo_crime == "Homicídio doloso") %>% 
  group_by(Ano,UF) %>% 
  summarise(mortes = sum(vitimas,na.rm = TRUE))

estados <- read_state(code_state = "all", year = 2018)

homicidio$UF[homicidio$UF%in% c("Rio de Janeiro", "Mato Grosso do Sul",
                                "Rio Grande do Norte", "Rio Grande do Sul")] <- c("Rio De Janeiro", "Mato Grosso Do Sul",
                                                                                  "Rio Grande Do Norte", "Rio Grande Do Sul")

pais <- left_join(estados, homicidio, by= c("name_state" = "UF"))

pais <- na.omit(pais)

# Gráfico 1

ggplot(pais) +
  geom_sf(aes(fill = mortes), color = "black") +
  facet_wrap(~Ano) +theme_minimal()+
  labs(fill = "Nº de Vitímas de homicídios", caption = "Até julho de 2020") +
  coord_sf(datum = NA) +
  theme(legend.position = "right", 
        legend.direction = "vertical", 
        legend.justification = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 5),
        strip.text = element_text(size=6.5, face="bold"),
        plot.caption = element_text(size = 5))+
  scale_fill_gradient(low="blue", high="red", na.value = "black")+
  guides(fill = guide_colorbar(barwidth = 0.35, barheight = 5 , title.position = "top"))

ggsave(filename = "grafico_1.png", width = 3, height = 2.75)


# Gráfico 2

homicidio <- vitimas %>% 
  rename(tipo_crime = `Tipo Crime`) %>% 
  rename(vitimas = Vítimas) %>% 
  rename(mes = Mês) %>% 
  filter(tipo_crime == "Homicídio doloso") %>% 
  group_by(Ano,UF) %>% 
  summarise(mortes = sum(vitimas))

ggplot(homicidio, aes(Ano, mortes)) +
  geom_line(color = "DarkRed",alpha=0.65, size= 1) +geom_point(color = "black")+
  facet_geo(~UF, grid = br_states_grid1) +
  labs(x = "Ano", y = "Nº de vitímas de homicídios",
       caption = "Até julho de 2020") +
  scale_x_continuous(breaks = 2015:2020, labels = 15:20)+
  theme(strip.text = element_text(size=12, face="bold"),
        panel.grid = element_blank(),
        axis.text = element_text(colour = "black"),
        panel.background = element_blank(),
        plot.caption = element_text(size = 10))

ggsave(filename = "grafico_2.png",width = 10, height = 9)
