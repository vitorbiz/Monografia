library(readxl)
library(tidyverse)
library(knitr)
library(kableExtra)
library(stargazer)

ori_vot_19 <- read_excel("Dados de Votacoes (Dados Abertos da Câmara dos Deputados)/votacoesOrientacoes-2019.xlsx")
ori_vot_20 <- read_excel("Dados de Votacoes (Dados Abertos da Câmara dos Deputados)/votacoesOrientacoes-2020.xlsx")
vot_19 <- read_excel("Dados de Votacoes (Dados Abertos da Câmara dos Deputados)/votacoesVotos-2019.xlsx")
vot_20 <- read_excel("Dados de Votacoes (Dados Abertos da Câmara dos Deputados)/votacoesVotos-2020.xlsx")
votacoesVotos_2019 <- read_excel("Dados de Votacoes (Dados Abertos da Câmara dos Deputados)/votacoes-2019.xlsx")
votacoesVotos_2020 <- read_excel("Dados de Votacoes (Dados Abertos da Câmara dos Deputados)/votacoes-2020.xlsx")

ori_vot <- as_tibble(dplyr::union(ori_vot_19, ori_vot_20)) %>%
  select(idVotacao, siglaBancada, orientacao) %>%
  filter(siglaBancada %in% c("Governo", "GOV.")) %>%
  mutate(fl_orient_sim = ifelse(orientacao == "Sim", 1, 
                         ifelse(orientacao == "Não", 0, NA)))

votacoesVotos <- as_tibble(dplyr::union(votacoesVotos_2019, votacoesVotos_2020)) %>%
  select(id, votosSim, votosNao) %>%
  rename(idVotacao = id) %>%
  mutate(VotosTotal = votosSim + votosNao, 
         pct_sim = votosSim / (votosSim + votosNao),
         pct_nao = votosNao / (votosSim + votosNao)) %>%
  mutate(fl_competitiva = ifelse(pct_sim >= 0.1 & pct_sim <= 0.9, 1, 0)) %>%
  filter(is.na(fl_competitiva) == FALSE)

votos <- dplyr::union(vot_19, vot_20) %>% 
  left_join(ori_vot, by = "idVotacao") %>%
  left_join(votacoesVotos, by = "idVotacao") %>%
  select(idVotacao, deputado_id, voto, deputado_siglaPartido, deputado_siglaUf, 
         orientacao, fl_orient_sim, votosSim, votosNao, pct_sim, fl_competitiva) %>%
  mutate(fl_voto_sim = ifelse(voto == "Sim", 1, 
                       ifelse(voto == "Não", 0, NA))) %>%
  mutate(fl_alinhamento = ifelse(is.na(fl_voto_sim) == TRUE | is.na(fl_orient_sim) == TRUE, NA,
                          ifelse(fl_voto_sim == fl_orient_sim, 1,
                          ifelse(fl_voto_sim != fl_orient_sim, 0, NA)))) %>%
  filter(is.na(fl_alinhamento) == FALSE, fl_competitiva == 1)


partidos <- votos %>%
  mutate(partido = ifelse(deputado_siglaPartido == 'PPS', 'CIDADANIA',
                   ifelse(deputado_siglaPartido == 'PHS', 'PODE',
                   ifelse(deputado_siglaPartido == 'PATRI', 'PATRIOTA',
                   ifelse(deputado_siglaPartido == 'PR', 'PL',      
                   ifelse(deputado_siglaPartido == 'PRB', 'REPUBLICANOS',
                   ifelse(deputado_siglaPartido == 'PRP', 'PATRIOTA', deputado_siglaPartido))))))) %>%
  group_by(partido) %>% 
  summarize(num_votos = n(), votos_alinhados = sum(fl_alinhamento), 
            alinhamento = sum(fl_alinhamento) / n()) %>%
  filter(num_votos >= 100, partido != 'S.PART.') %>%
  arrange(desc(alinhamento)) 


ggplot(partidos, aes(x = reorder(deputado_siglaPartido, -num_votos), y = num_votos, fill = votos_alinhados)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Partido", y = "Número de votos", fill = "Proporção alinhada\ncom o governo")

##### APRESENTACAO EM TABELA #####

partidos %>%
  kable("html", 
        col.names = c("Partido Político", "Número de Votos", "Votos Alinhados ao Governo", "Proporção de Votos Alinhados"), 
        align = c('l', 'r', 'r', 'r')) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, bold = TRUE)

##### STARGAZER #####


partidos$alinhamento <- round(partidos$alinhamento, 3)
partidos$alinhamento <- paste0(round(partidos$alinhamento * 100, 2), "%")

stargazer(partidos, type = "html", 
          title = "Proporção de Votos Alinhados ao Governo Federal por Partido", 
          covariate.labels = c("Partido", "Total de Votos", "Votos Alinhados ao Governo", "% Votos Alinhados"),
          out = "tabela_alinhamento.html",
          summary = FALSE,
          rownames = FALSE,
          digits = 2)




