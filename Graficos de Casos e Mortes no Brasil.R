library(readr)
library(tidyverse)

#### Dados do Brasil.io utilizados para calcular mortes e casos de covid nos municipios #####

caso_full <- as_tibble(read_csv(caso_full <- read_csv("Dados casos e Mortes Covid (Brasil.io)/caso_full.csv"))) 


casos_municipio <- caso_full %>%
  filter(place_type == 'city') %>%
  group_by(city_ibge_code, city, estimated_population) %>%
  summarize(casos = sum(new_confirmed, na.rm = TRUE), 
            mortes = sum(new_deaths), na.rm = TRUE)  %>%
  select(city, city_ibge_code, estimated_population, casos, mortes) %>%
  mutate(casos_100mil_hab = (casos / estimated_population) * 100000,
         mortes_100mil_hab = (mortes / estimated_population) * 100000) 
                    
  
#### Dados de casos e mortes de covid no Brasil por dia ####

casos_diarios <- caso_full %>%
  filter(place_type == 'city') %>%
  group_by(date) %>%
  summarize(casos = sum(new_confirmed, na.rm = TRUE), 
            mortes = sum(new_deaths), na.rm = TRUE)

casos_periodo <- caso_full %>%
  filter(place_type == 'city', date >= '2021-01-01' & date <= '2021-12-01') %>%
  group_by(date) %>%
  summarize(casos = sum(new_confirmed, na.rm = TRUE), 
            mortes = sum(new_deaths), na.rm = TRUE)


sum(casos_diarios$casos, na.rm = TRUE)
sum(casos_diarios$mortes, na.rm = TRUE) 

# Casos e Mortes no periodo de interesse
sum(casos_periodo$casos, na.rm = TRUE)
sum(casos_periodo$mortes, na.rm = TRUE) 


#### Grafico de casos

ggplot(casos_diarios, aes(x = date, y = casos)) +
  geom_line(color = "#1f77b4", size = 1.2) +
  geom_ribbon(aes(ymin = 0, ymax = casos), fill = "#f2f2f2") +
  ggtitle("Casos de COVID-19 por dia no Brasil") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE), breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000))  +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 18, face = "bold"),
        axis.line = element_line(color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 25)) +
  geom_segment(x = as.Date("2021-01-01"), xend = as.Date("2021-01-01"), y = 0, yend = Inf, color = "#FFA500", size = 1.5) +
  geom_segment(x = as.Date("2021-12-31"), xend = as.Date("2021-12-31"), y = 0, yend = Inf, color = "#FFA500", size = 1.5) +
  geom_hline(yintercept = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000), color = "#E0E0E0", linetype = "dashed", size = 0.5) 

#### Grafico de Mortes

ggplot(casos_diarios, aes(x = date, y = mortes)) +
  geom_line(color = "#FF3333", size = 1.2) +
  geom_ribbon(aes(ymin = 0, ymax = mortes), fill = "#f2f2f2") +
  ggtitle("Mortes de COVID-19 por dia no Brasil") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE), breaks = c(0, 2000, 4000, 6000, 8000, 10000)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 18, face = "bold"),
        axis.line = element_line(color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 25)) +
  geom_segment(x = as.Date("2021-01-01"), xend = as.Date("2021-01-01"), y = 0, yend = Inf, color = "#FFA500", size = 1.5) +
  geom_segment(x = as.Date("2021-12-31"), xend = as.Date("2021-12-31"), y = 0, yend = Inf, color = "#FFA500", size = 1.5) +
  geom_hline(yintercept = c(0, 2000, 4000, 6000, 8000, 10000), color = "#E0E0E0", linetype = "dashed", size = 0.5) 
  



