library(tidyverse)
library(basedosdados)
library(readr)
library(readxl)
library(rdrobust)
library(rddensity)
library(rddtools)
library(rdd)
library(gridExtra)
library(stargazer)
library(scales)

##### DADOS ELEITORAIS (TSE) #####

# Definir id do projeto no BigQuery
basedosdados::set_billing_id("DADOS PESSOAIS DE CADASTRO")

query_tse <- "WITH be AS (
              SELECT ano, turno, id_municipio, sigla_partido, id_candidato_bd, resultado, QTD_CANDIDATOS_MUN, FL_SITUACAO, FL_OPOSICAO,
                     ROUND(CAST((VOTOS / NULLIF(SUM(VOTOS) OVER(PARTITION BY id_municipio_tse), 0)) AS FLOAT64), 10) AS PORC_VOTOS,
                     ROUND(CAST(COUNT(*) OVER(PARTITION BY id_municipio) AS FLOAT64), 2) AS QTD_CANDIDATOS_MUN,
                     SUM(FL_SITUACAO) OVER(PARTITION BY id_municipio) AS SOMA_OPOSICAO,
                     SUM(FL_OPOSICAO) OVER(PARTITION BY id_municipio) AS SOMA_SITUACAO   
              FROM (SELECT *, 
                         CAST(COUNT(*) OVER(PARTITION BY id_municipio_tse) AS FLOAT64) AS QTD_CANDIDATOS_MUN, 
                         CASE WHEN sigla_partido IN ('PSL', 'PL', 'PSC', 'DEM', 'PATRIOTA', 'PP', 'PSD', 'PTB', 'MDB', 'DEM', 'REPUBLICANOS', 'PSDB', 'SOLIDARIEDADE') THEN 1 ELSE 0 END AS FL_SITUACAO,
                         CASE WHEN sigla_partido IN ('PT', 'PSOL', 'PCdoB', 'REDE', 'PSB', 'PDT', 'PV') THEN 1 ELSE 0 END AS FL_OPOSICAO       
                   FROM `basedosdados.br_tse_eleicoes.resultados_candidato` 
                   WHERE tipo_eleicao = 'eleicao ordinaria'
                   AND cargo = 'prefeito'
                   AND ano = 2020
                   AND turno = 1) AS A1
               WHERE CAST(QTD_CANDIDATOS_MUN AS FLOAT64) = 2
               AND (FL_SITUACAO = 1 OR FL_OPOSICAO = 1))
        
               SELECT *
               FROM be
               WHERE id_municipio IN (SELECT id_municipio
                                       FROM be
                                       GROUP BY id_municipio
                                       HAVING SUM(FL_OPOSICAO) = 1 
                                       AND SUM(FL_SITUACAO) = 1)"

basedosdados::download(query_tse,
                       path = "dadostse.csv")

# Dados do TSE com resultado de eleicoes ja filtrados para disputas situacao x oposicao
dados_tse <- as_tibble(read_csv("dadostse.csv")) 


##### DADOS CENSO 2010 PESSOA (IBGE) #####


query_censo2010_pessoa <- 
  "SELECT id_municipio, 
       SUM(peso_amostral) AS populacao, 
       SUM(idade * peso_amostral) / SUM(PESO_AMOSTRAL) AS IDADE_MEDIA,
       SUM(FL_HOMEM * PESO_AMOSTRAL) / SUM(PESO_AMOSTRAL) AS PCT_HOMENS,
       SUM(FL_BRANCO * PESO_AMOSTRAL) / SUM(PESO_AMOSTRAL) AS PCT_BRANCOS,
       SUM(FL_ANALFABETO * PESO_AMOSTRAL) / SUM(CASE WHEN FL_ANALFABETO IS NOT NULL THEN PESO_AMOSTRAL END) AS TX_ANALF
FROM (SELECT id_municipio, peso_amostral,  
            CASE WHEN v0601 = 1 THEN 1 
                 WHEN v0601 = 2 THEN 0 
                 ELSE NULL END AS FL_HOMEM,
            v6033 AS idade,
            CASE WHEN v0606 = 1 THEN 1
                 ELSE 0 END AS FL_BRANCO,
            --- ANALFABETISMO É CALCULADO PARA INDIVIDUOS MAIORES QUE 15 ANOS ---     
            CASE WHEN v0627 = 1 AND v6036 >= 15 THEN 0
                 WHEN v0627 = 2 AND v6036 >= 15 THEN 1
                 ELSE NULL END AS FL_ANALFABETO                              
      FROM `basedosdados.br_ibge_censo_demografico.microdados_pessoa_2010`) AS bs
WHERE id_municipio IN (
                        WITH be AS (
                        SELECT ano, turno, id_municipio, sigla_partido, id_candidato_bd, resultado, QTD_CANDIDATOS_MUN, FL_SITUACAO, FL_OPOSICAO,
                              ROUND(CAST((VOTOS / NULLIF(SUM(VOTOS) OVER(PARTITION BY id_municipio_tse), 0)) AS FLOAT64), 2) AS PORC_VOTOS,
                              ROUND(CAST(COUNT(*) OVER(PARTITION BY id_municipio) AS FLOAT64), 2) AS QTD_CANDIDATOS_MUN,
                              SUM(FL_SITUACAO) OVER(PARTITION BY id_municipio) AS SOMA_OPOSICAO,
                              SUM(FL_OPOSICAO) OVER(PARTITION BY id_municipio) AS SOMA_SITUACAO   
                        FROM (SELECT *, 
                                    CAST(COUNT(*) OVER(PARTITION BY id_municipio_tse) AS FLOAT64) AS QTD_CANDIDATOS_MUN, 
                                    CASE WHEN sigla_partido IN ('PSL', 'PL', 'PSC', 'DEM', 'PATRIOTA', 'PP', 'PSD', 'PTB', 'MDB', 'DEM', 'REPUBLICANOS', 'PSDB', 'SOLIDARIEDADE') THEN 1 ELSE 0 END AS FL_SITUACAO,
                                    CASE WHEN sigla_partido IN ('PT', 'PSOL', 'PCdoB', 'REDE', 'PSB', 'PDT') THEN 1 ELSE 0 END AS FL_OPOSICAO       
                              FROM `basedosdados.br_tse_eleicoes.resultados_candidato` 
                              WHERE tipo_eleicao = 'eleicao ordinaria'
                              AND cargo = 'prefeito'
                              AND ano = 2020
                              AND turno = 1) AS A1
                        WHERE CAST(QTD_CANDIDATOS_MUN AS FLOAT64) = 2
                        AND (FL_SITUACAO = 1 OR FL_OPOSICAO = 1)
                        )

                        SELECT DISTINCT id_municipio
                        FROM be
                        WHERE id_municipio IN (SELECT id_municipio
                                              FROM be
                                              GROUP BY id_municipio
                                              HAVING SUM(FL_OPOSICAO) = 1 
                                              AND SUM(FL_SITUACAO) = 1))
GROUP BY id_municipio"

basedosdados::download(query_censo2010_pessoa,
                       path = "dadoscenso2010_pessoa.csv")

dados_censo_pessoa <- as_tibble(read_csv("dadoscenso2010_pessoa.csv")) 

##### DADOS CENSO 2010 DOMICILIO (IBGE) #####

query_censo2010_domicilio <- 
  "SELECT id_municipio, 
       SUM(PESO_AMOSTRAL) AS DOMICILIOS,
       SUM(FL_URBANO * PESO_AMOSTRAL) / SUM((CASE WHEN FL_URBANO IS NOT NULL THEN PESO_AMOSTRAL END) * 1.00) AS TX_URBANI,
       SUM(FL_EE * PESO_AMOSTRAL) / SUM(CASE WHEN FL_EE IS NOT NULL THEN PESO_AMOSTRAL END) * 1.00 AS TX_EE,
       SUM(FL_TV * PESO_AMOSTRAL) / SUM(CASE WHEN FL_TV IS NOT NULL THEN PESO_AMOSTRAL END) AS TX_TV,
       SUM(FL_GELADEIRA * PESO_AMOSTRAL) / SUM(CASE WHEN FL_GELADEIRA IS NOT NULL THEN PESO_AMOSTRAL END) AS TX_GELAD,
       SUM(FL_COMPUTADOR * PESO_AMOSTRAL) / SUM(CASE WHEN FL_COMPUTADOR IS NOT NULL THEN PESO_AMOSTRAL END) AS TX_COMPUTADOR,
       SUM(FL_ESGOTO * PESO_AMOSTRAL) / SUM(CASE WHEN FL_ESGOTO IS NOT NULL THEN PESO_AMOSTRAL END) AS TX_ESGOTO,
       SUM(FL_DIST_AGUA * PESO_AMOSTRAL) / SUM(CASE WHEN FL_DIST_AGUA IS NOT NULL THEN PESO_AMOSTRAL END) AS TX_DIST_AGUA
FROM (
      SELECT id_municipio, peso_amostral, V0401 AS NUMERO_MORADORES, V6531 AS REND_DOMICILIAR_PC, 
            CASE WHEN situacao_domicilio = 1 THEN 1
                  WHEN situacao_domicilio = 2 THEN 0
                  ELSE NULL END AS FL_URBANO, 
              CASE WHEN v0211 IN (1, 2) THEN 1
                  WHEN v0211 = 3 THEN 0
                  ---- BRANCO PARA DOMICILIO IMPROVISADO ----
                  ELSE NULL END AS FL_EE,
                  --- BRANCO PARA DOMICILIO IMPROVISADO ----
              CASE WHEN v0214 = 1 THEN 1
                  WHEN v0214 = 2 THEN 0
                  ELSE NULL END AS FL_TV, 
                  --- BRANCO PARA DOMICILIO IMPROVISADO ----
              CASE WHEN v0216 = 1 THEN 1
                  WHEN v0216 = 2 THEN 0
                  ELSE NULL END AS FL_GELADEIRA,
                  --- COMPUTADOR C/ ACESSO A INTERNET ----
              CASE WHEN v0220 = 1 THEN 1
                   WHEN v0220 = 2 THEN 0
                   ELSE NULL END AS FL_COMPUTADOR,
                  --- ACESSO A ESGOTAMENTO SANITARIO = COLETORA OU FOSSA SEPTICA (1 e 2)
              CASE WHEN v0207 IN (1, 2) THEN 1
                   WHEN v0207 IN (3, 4, 5, 6) THEN 0
                   ELSE NULL END AS FL_ESGOTO, 
                   --- ACESSO A REDE GERAL DE DISTRIBUICAO DE AGUA (1)
              CASE WHEN v0208 = 1 THEN 1
                   WHEN v0208 IN (2, 3, 4, 5, 6, 7, 8, 9, 10) THEN 0
                   ELSE NULL END AS FL_DIST_AGUA                                   
      FROM `basedosdados.br_ibge_censo_demografico.microdados_domicilio_2010`)
WHERE id_municipio IN (
                        WITH be AS (
                        SELECT ano, turno, id_municipio, sigla_partido, id_candidato_bd, resultado, QTD_CANDIDATOS_MUN, FL_SITUACAO,     FL_OPOSICAO,
                               ROUND(CAST((VOTOS / NULLIF(SUM(VOTOS) OVER(PARTITION BY id_municipio_tse), 0)) AS FLOAT64), 2) AS PORC_VOTOS,
                               ROUND(CAST(COUNT(*) OVER(PARTITION BY id_municipio) AS FLOAT64), 2) AS QTD_CANDIDATOS_MUN,
                               SUM(FL_SITUACAO) OVER(PARTITION BY id_municipio) AS SOMA_OPOSICAO,
                               SUM(FL_OPOSICAO) OVER(PARTITION BY id_municipio) AS SOMA_SITUACAO   
                        FROM (SELECT *, 
                                    CAST(COUNT(*) OVER(PARTITION BY id_municipio_tse) AS FLOAT64) AS QTD_CANDIDATOS_MUN, 
                                    CASE WHEN sigla_partido IN ('PSL', 'PL', 'PSC', 'DEM', 'PATRIOTA', 'PP', 'PSD', 'PTB', 'MDB', 'DEM', 'REPUBLICANOS', 'PSDB', 'SOLIDARIEDADE') THEN 1 ELSE 0 END AS FL_SITUACAO,
                                    CASE WHEN sigla_partido IN ('PT', 'PSOL', 'PCdoB', 'REDE', 'PSB', 'PDT') THEN 1 ELSE 0 END AS FL_OPOSICAO       
                              FROM `basedosdados.br_tse_eleicoes.resultados_candidato` 
                              WHERE tipo_eleicao = 'eleicao ordinaria'
                              AND cargo = 'prefeito'
                              AND ano = 2020
                              AND turno = 1) AS A1
                        WHERE CAST(QTD_CANDIDATOS_MUN AS FLOAT64) = 2
                        AND (FL_SITUACAO = 1 OR FL_OPOSICAO = 1)
                        )

                        SELECT DISTINCT id_municipio
                        FROM be
                        WHERE id_municipio IN (SELECT id_municipio
                                              FROM be
                                              GROUP BY id_municipio
                                              HAVING SUM(FL_OPOSICAO) = 1 
                                              AND SUM(FL_SITUACAO) = 1))
GROUP BY id_municipio"

basedosdados::download(query_censo2010_domicilio,
                       path = "dadoscenso2010_domicilio.csv")

dados_censo_domicilio <- as_tibble(read_csv("dadoscenso2010_domicilio.csv")) 

##### CASOS E MORTES POR COVID (brasil.io) #####

caso_full <- as_tibble(read_csv("Dados casos e Mortes Covid (Brasil.io)/caso_full.csv"))

casos_municipio <- caso_full %>%
  group_by(city_ibge_code, city, estimated_population) %>%
  summarize(casos = sum(new_confirmed, na.rm = TRUE), 
            mortes = sum(new_deaths), na.rm = TRUE)  %>%
  select(city, city_ibge_code, estimated_population, casos, mortes) %>%
  mutate(casos_100mil_hab = (casos / estimated_population) * 100000,
         mortes_100mil_hab = (mortes / estimated_population) * 100000) 


##### PIB PER CAPITA MUNICIPIOS 2020 (IBGE) #####

tabela_pib_pc_mun <- as_tibble(read_excel("PIB per Capita Municipios 2010-2020 (IBGE)/PIB per Capita dos Municipios (IBGE).xlsx"))

pib_pc_municipios <- tabela_pib_pc_mun %>%
                rename(ano = Ano, id_municipio = `Código do Município`, 
                       nm_municipio = `Nome do Município`, 
                       pib_pc = `Produto Interno Bruto per capita, \r\na preços correntes\r\n(R$ 1,00)`, 
                       pib = `Produto Interno Bruto, \r\na preços correntes\r\n(R$ 1.000)`) %>%
                select(ano, nm_municipio, id_municipio, pib_pc, pib) %>%
                filter(ano == 2020)

###### ESTIMATIVA POPULACAO MUNICIPIOS 2020 (IBGE) ######

populacao_estimada_20 <- as_tibble(POP2020_20220905 <- read_excel("Estimativa Populacao Municipios 2020 (IBGE)/POP2020_20220905.xls", 
                                                             sheet = "Municípios",
                                                             skip = 1))

###### DIRETORIOS MUNICIPIOS ######

query_diretorios <-
  "SELECT nome, id_municipio, id_municipio_6, id_municipio_tse, sigla_uf, nome_regiao
   FROM `basedosdados.br_bd_diretorios_brasil.municipio` "

basedosdados::download(query_diretorios,
                       path = "diretorios_municipios.csv")


diretorios_municipios <- as_tibble(read_csv("diretorios_municipios.csv"))


############ JOIN DOS DADOS EM 1 TABELA ##############

tabela_completa <- dados_tse %>%
  # Adiciona todos os codigos de identificacao do municipio
  left_join(diretorios_municipios, by = "id_municipio") %>%
  # Adiciona dados do censo domiciliar
  left_join(dados_censo_domicilio, by = "id_municipio") %>%
  # Adiciona dados do censo pessoa
  left_join(dados_censo_pessoa, by = "id_municipio") %>%
  # Adiciona dados de Pib per Capita do municipio
  left_join(pib_pc_municipios, by = "id_municipio") %>%
  # Adiciona dados de casos e mortes de covid do municipio 
  left_join(casos_municipio, by = c("id_municipio" = "city_ibge_code"))

         
tabela_completa_filtrada <- tabela_completa %>%
    select(id_municipio, sigla_partido, resultado, QTD_CANDIDATOS_MUN, FL_SITUACAO, FL_OPOSICAO, PORC_VOTOS,
           TX_URBANI, TX_EE, TX_TV, TX_GELAD, TX_COMPUTADOR, TX_ESGOTO, TX_DIST_AGUA,
           IDADE_MEDIA, PCT_HOMENS, PCT_BRANCOS, TX_ANALF, 
           pib, pib_pc,
           estimated_population, casos, mortes, casos_100mil_hab, mortes_100mil_hab,
           sigla_uf, nome_regiao)

#### Estatisticas Descritivas ###


colunas_a_excluir <- c("id_municipio", "sigla_partido", "resultado", "QTD_CANDIDATOS_MUN", "FL_SITUACAO", "FL_OPOSICAO", "PORC_VOTOS", 
                       "margem_centralizada", "resulado_eleicao", "sigla_uf", "nome_regiao")

tabela_variaveis <- tabela_completa_filtrada %>%
              mutate(margem_centralizada = PORC_VOTOS - 0.5, 
                     resulado_eleicao = ifelse(resultado == "eleito", 1, 0))  %>%
              filter(FL_SITUACAO == 1) %>%
              select(-one_of(colunas_a_excluir))

df_variaveis_total <- tabela_variaveis


stats <- data.frame(
  N = sapply(df_variaveis_total, function(x) sum(!is.na(x))),
  mean = sapply(df_variaveis_total, function(x) mean(x, na.rm = TRUE)),
  sd = sapply(df_variaveis_total, function(x) sd(x, na.rm = TRUE)),
  min = sapply(df_variaveis_total, function(x) min(x, na.rm = TRUE)),
  median = sapply(df_variaveis_total, function(x) median(x, na.rm = TRUE)),
  max = sapply(df_variaveis_total, function(x) max(x, na.rm = TRUE))
)

stats_matrix <- as.matrix(stats)

stargazer(stats_matrix, type = "html", 
          title = "Estatísticas Descritivas", 
          rownames = TRUE,
          out = "tabela_estats_desc.html")


############# TABELA POR REGIAO #################

populacao_estimada_20 %>%
  left_join(diretorios_municipios, by = c("COD. UF" = "id_municipio"))

municipios <- casos_municipio %>%
            distinct(city_ibge_code, estimated_population) %>%
            left_join(diretorios_municipios, by = c("city_ibge_code" = "id_municipio")) %>%
            filter(city_ibge_code > 1000)

municipios_total <- municipios %>%
                  group_by(nome_regiao) %>%
                  summarize(quantidade_total = n())

municipios_menos_200milhab <- municipios %>%
                 filter(estimated_population < 200000) %>%
                 group_by(nome_regiao) %>%
                 summarize(quantidade_menos_200milhab = n()) 
                 

tabela_regioes <- tabela_completa_filtrada %>%
  filter(FL_SITUACAO == 1) %>%
  select(id_municipio, sigla_uf, nome_regiao) %>%
  group_by(nome_regiao) %>%
  summarize(quantidade = n()) %>%
  arrange(desc(quantidade)) %>%
  left_join(municipios_total, by = "nome_regiao") %>%
  left_join(municipios_menos_200milhab, by = "nome_regiao") %>%
  mutate("% Total Municipios" = percent(quantidade / quantidade_total, accuracy = 0.01),
         "% Municipios < 200 mil hab" = percent(quantidade / quantidade_menos_200milhab, accuracy = 0.01)) %>%
  select(nome_regiao, quantidade, `% Total Municipios`, `% Municipios < 200 mil hab`)

stargazer(tabela_regioes , type = "html", 
          title = "Distribuição dos Municípios por Região", 
          covariate.labels = c("Região", "Quantidade", "% Total Municipios", "% Municipios < 200 mil hab"),
          out = "tabela_regioes.html",
          summary = FALSE,
          rownames = FALSE,
          style = list(css.cell = "padding: 8px;"))

html_output <- stargazer(tabela_regioes,
                         type = "html",
                         title = "Distribuição dos Municípios por Região",
                         covariate.labels = c("Região", "Quantidade", "% Total Municípios", "% Municípios < 200 Mil hab."),
                         summary = FALSE,
                         rownames = FALSE)


html_output <- paste0("<style>
                      table {
                        border-collapse: separate;
                        border-spacing: 0px 8px;
                      }
                      th, td {
                        padding: 8px;
                      }
                      </style>", html_output)

writeLines(html_output, "tabela_regioes.html")

############# TABELA REGRESSAO DISCONTINUA #################

tabela_rdd <-  tabela_completa_filtrada %>%
  mutate(margem_centralizada = PORC_VOTOS - 0.5, 
         resulado_eleicao = ifelse(resultado == "eleito", 1, 0))  %>%
  filter(FL_SITUACAO == 1)


########### REGRESSOES Cattaneo and Titiunik (2014a) ############

rdd_casos_p1 <- rdrobust(tabela_rdd$casos_100mil_hab, tabela_rdd$margem_centralizada, c = 0, p = 1)
rdd_casos_p2 <- rdrobust(tabela_rdd$casos_100mil_hab, tabela_rdd$margem_centralizada, c = 0, p = 2)


rdd_mortes_p1 <- rdrobust(tabela_rdd$mortes_100mil_hab, tabela_rdd$margem_centralizada, c = 0,  p = 1)
rdd_mortes_p2 <- rdrobust(tabela_rdd$mortes_100mil_hab, tabela_rdd$margem_centralizada, c = 0,  p = 2)

###### Sumario Resultados RDD Casos

summary(rdd_casos_p1)
rdd_casos_p1$coef 
rdd_casos_p1$se 
rdd_casos_p1$pv 
rdd_casos_p1$N_h 
rdd_casos_p1$ci
rdd_casos_p1$bws

summary(rdd_casos_p2)
rdd_casos_p2$coef 
rdd_casos_p2$se
rdd_casos_p2$pv 
rdd_casos_p2$N_h 
rdd_casos_p2$ci 
rdd_casos_p2$bws 


##### Sumario Resultados RDD Mortes

summary(rdd_mortes_p1)
rdd_mortes_p1$coef
rdd_mortes_p1$se 
rdd_mortes_p1$pv 
rdd_mortes_p1$N_h
rdd_mortes_p1$ci 
rdd_mortes_p1$bws 

summary(rdd_mortes_p2)
rdd_mortes_p2$coef 
rdd_mortes_p2$se 
rdd_mortes_p2$pv 
rdd_mortes_p2$N_h 
rdd_mortes_p2$ci 
rdd_mortes_p2$bws 


##### GRAFICOS MORTES E CASOS 

casos_grafico1 <- rdplot(tabela_rdd$casos_100mil_hab, tabela_rdd$margem_centralizada,
            p = rdd_mortes_p1$p, h = rdd_mortes_p1$bws[1], scale = 50,
            col.dots = "#808080",
            col.lines = "blue")
casos_grafico1 <- casos_grafico1$rdplot +
              xlim(-0.12, 0.12) +
              ylim(2500, 22500) +
              ggtitle("RDD Casos Covid 100 Mil Habit (Linear) ") + 
              labs(x = "Margem Vitoria Cand. Alinhado", y = "") +
  theme(axis.text.x = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.line = element_line(color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 15))

casos_grafico2 <- rdplot(tabela_rdd$casos_100mil_hab, tabela_rdd$margem_centralizada,
                        p = rdd_casos_p2$p, h = rdd_casos_p2$bws[1], scale = 60,
                        col.dots = "#808080",
                        col.lines = "blue")
casos_grafico2 <- casos_grafico2$rdplot +
  xlim(-0.15, 0.15) +
  ylim(2500, 22500) +
  ggtitle("RDD Casos Covid 100 Mil Habit (Quadratico)") + 
  labs(x = "Margem Vitoria Cand. Alinhado", y = "") +
  theme(axis.text.x = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.line = element_line(color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 15))



mortes_grafico1 <- rdplot(tabela_rdd$mortes_100mil_hab, tabela_rdd$margem_centralizada,
                        p = rdd_mortes_p1$p, h = rdd_mortes_p1$bws[1], scale = 60,
                        col.dots = "#808080",
                        col.lines = "#FF0000")
mortes_grafico1 <- mortes_grafico1$rdplot +
            xlim(-0.12, 0.12) +
            ylim(0, 400) +
            ggtitle("RDD Mortes Covid 100 Mil Habit (Linear)") + 
            labs(x = "Margem Vitoria Cand. Alinhado", y = "") +
  theme(axis.text.x = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.line = element_line(color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 15))

mortes_grafico2 <- rdplot(tabela_rdd$mortes_100mil_hab, tabela_rdd$margem_centralizada,
                          p = rdd_mortes_p2$p, h = rdd_mortes_p2$bws[1], scale = 60,
                          col.dots = "#808080",
                          col.lines = "#FF0000")
mortes_grafico2 <- mortes_grafico2$rdplot +
  xlim(-0.12, 0.12) +
  ylim(0, 400) +
  ggtitle("RDD Mortes Covid 100 Mil Habit (Quadratico)") + 
  labs(x = "Margem Vitoria Cand. Alinhado", y = "") +
  theme(axis.text.x = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.line = element_line(color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 15)) 

grid.arrange(mortes_grafico1, mortes_grafico2, ncol = 2)
grid.arrange(casos_grafico1, casos_grafico2, ncol = 2)

########### REGRESSOES VARIAVEIS DE CONTROLE (COVARIAVEIS) ##############
## Lee, Moretti, and Butler (2004)
## Guido W. Imbens and Lemieux (2008)


rdd_TX_URBANI <- rdrobust(tabela_rdd$TX_URBANI, tabela_rdd$margem_centralizada, c = 0)
rdd_TX_EE <- rdrobust(tabela_rdd$TX_EE, tabela_rdd$margem_centralizada, c = 0)
rdd_TX_TV <- rdrobust(tabela_rdd$TX_TV, tabela_rdd$margem_centralizada, c = 0)
rdd_TX_GELAD <- rdrobust(tabela_rdd$TX_GELAD, tabela_rdd$margem_centralizada, c = 0)
rdd_TX_COMPUTADOR <- rdrobust(tabela_rdd$TX_COMPUTADOR, tabela_rdd$margem_centralizada, c = 0)
rdd_TX_ESGOTO <- rdrobust(tabela_rdd$TX_ESGOTO, tabela_rdd$margem_centralizada, c = 0)
rdd_TX_DIST_AGUA <- rdrobust(tabela_rdd$TX_DIST_AGUA, tabela_rdd$margem_centralizada, c = 0)
rdd_IDADE_MEDIA <- rdrobust(tabela_rdd$IDADE_MEDIA, tabela_rdd$margem_centralizada, c = 0)
rdd_PCT_HOMENS <- rdrobust(tabela_rdd$PCT_HOMENS, tabela_rdd$margem_centralizada, c = 0)
rdd_TX_ANALF <- rdrobust(tabela_rdd$TX_ANALF, tabela_rdd$margem_centralizada, c = 0)
rdd_pib_pc <- rdrobust(tabela_rdd$pib_pc, tabela_rdd$margem_centralizada, c = 0)
rdd_estimated_population <- rdrobust(tabela_rdd$estimated_population, tabela_rdd$margem_centralizada, c = 0)

summary(rdd_TX_URBANI)
summary(rdd_TX_EE)
summary(rdd_TX_TV)
summary(rdd_TX_GELAD)
summary(rdd_TX_COMPUTADOR)
summary(rdd_TX_ESGOTO)
summary(rdd_TX_DIST_AGUA)
summary(rdd_IDADE_MEDIA)
summary(rdd_PCT_HOMENS)
summary(rdd_TX_ANALF)
summary(rdd_pib_pc)


rdd_TX_URBANI$coef
rdd_TX_EE$coef
rdd_TX_TV$coef
rdd_TX_GELAD$coef
rdd_TX_COMPUTADOR$coef
rdd_TX_ESGOTO$coef
rdd_TX_DIST_AGUA$coef
rdd_IDADE_MEDIA$coef
rdd_PCT_HOMENS$coef
rdd_TX_ANALF$coef
rdd_pib_pc$coef

rdd_TX_URBANI$pv
rdd_TX_EE$pv
rdd_TX_TV$pv
rdd_TX_GELAD$pv
rdd_TX_COMPUTADOR$pv
rdd_TX_ESGOTO$pv
rdd_TX_DIST_AGUA$pv
rdd_IDADE_MEDIA$pv
rdd_PCT_HOMENS$pv
rdd_TX_ANALF$pv
rdd_pib_pc$pv




###### McCrary’s density test [Cattaneo, Jansson and Ma (2020)]
# local polynomial density estimators
# Hipotese nula de quebra (logo para ser rejeitada exige p ou t significante)
# McCrary, J. (2008)


DCdensity(tabela_rdd$margem_centralizada, cutpoint = 0) # p = 0,48103


myDCdensity <- function(runvar, cutpoint, my_abline = 0, my_title = "Default"){
  
  # get the default plot
  myplot <- DCdensity(runvar, cutpoint)
  
  # 'additional graphical options to modify the plot'
  abline(v = my_abline)
  title(main = my_title)http://127.0.0.1:32771/graphics/plot_zoom_png?width=1200&height=886
  
  # return
  return(myplot)
}

myDCdensity(tabela_rdd$margem_centralizada, 0, my_abline = 0, my_title = "Teste de McCrary’s   (p = 0,48103)")


##### Placebo tests around the threshold and sensitivity to the bandwidth
## plotPlacebo [Placebo Test]
## plotSensi [Sensitivity to the bandwidth]


rd1_data <- rdd_data(casos_100mil_hab, margem_centralizada, data = tabela_rdd, cutpoint = 0)
rdd1 <- rdd_reg_lm(rd1_data, order = 1, bw = 0.1095185)

rd1_placebo <- plotPlacebo(rdd1, from = 0.1, to = 0.8, by = 0.05, level = 0.95, plot = TRUE)
rd1_sensitivity <- plotSensi(rdd1, order = 1, level = 0.95, device = "ggplot", output = "ggplot") 

rd1_placebo_tibble <- as_tibble(rd1_placebo)
rd1_placebo_tibble$cutpoint <- round(as.numeric(as.character(rd1_placebo_tibble$cutpoint)), digits = 2)


grafico_placebo_rdd1 <- ggplot(rd1_placebo_tibble, aes(x = factor(cutpoint), y = LATE)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2, size = 0.75, color = "blue") +
  geom_point(data = rd1_placebo_tibble[which(rd1_placebo_tibble$LATE >= rd1_placebo_tibble$CI_low & rd1_placebo_tibble$LATE <= rd1_placebo_tibble$CI_high), ],
             aes(x = factor(cutpoint), y = LATE), color = "blue", size = 1.5, shape = 16) +
  geom_line(aes(group = 1), size = 0.3, color = "blue") +
  geom_hline(yintercept = 0, color = "orange", linetype = "solid", size = 1) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "orange", size = 0.7) +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "Teste de Placebo RDD Casos Covid 100 Mil Habit (Linear)",
       x = "Ponto de Corte",
       y = "LATE") + 
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.line = element_line(color = "black"),
        plot.title = element_text(size = 15),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13)) 


  
grafico_sensibilidade_rdd1 <- rd1_sensitivity +
  geom_line(color = "blue", size = 2) +
  labs(title = "Teste de Sensibilidade do Bandwidth RDD Casos (Linear)",
       x = "Largura de Banda",
       y = "LATE") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.line = element_line(color = "black"),
        plot.title = element_text(size = 15),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13))

  
rd2_data <- rdd_data(mortes_100mil_hab, margem_centralizada, data = tabela_rdd, cutpoint = 0)
rdd2 <- rdd_reg_lm(rd2_data, order = 1, bw = 0.1149777)  

rd2_placebo <- plotPlacebo(rdd2, device = "ggplot", from = 0.1, to = 0.8, by = 0.05, level = 0.95, plot = TRUE, output = "ggplot")
rd2_sensitivity <- plotSensi(rdd2, order = 1, level = 0.95, device = "ggplot", output = "ggplot")

grafico_sensibilidade_rdd2 <- rd2_sensitivity +
  geom_line(color = "red", size = 2) +

  labs(title = "Teste de Sensibilidade do Bandwidth RDD Mortes (Linear)",
       x = "Largura de Banda",
       y = "LATE") +
  theme_minimal() + 
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.line = element_line(color = "black"),
        plot.title = element_text(size = 15),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13))


rd2_placebo_tibble <- as_tibble(rd2_placebo)
rd2_placebo_tibble$cutpoint <- as.numeric(as.character(rd2_placebo_tibble$cutpoint))

grafico_placebo_rdd2 <- ggplot(rd2_placebo_tibble, aes(x = factor(cutpoint), y = LATE)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2, size = 0.75, color = "red") +
  geom_line(aes(group = 1), size = 0.3, color = "red") +
  geom_point(data = rd2_placebo_tibble, aes(x = factor(cutpoint), y = LATE), color = "red", size = 1.5) +
  theme_minimal() +
  ylim(-300, 300) +
  scale_x_discrete(labels = c("-0.20", "-0.15", "-0.10", "-0.05", "0", "0.5", "0.10", "0.15", "0.20", "0.25")) + 
  geom_hline(yintercept = 0, color = "orange", linetype = "solid") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "orange", size = 0.7) +
  labs(title = "Teste de Placebo RDD Casos Covid 100 Mil Habit (Linear)",
       x = "Ponto de Corte",
       y = "LATE") + 
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.line = element_line(color = "black"),
        plot.title = element_text(size = 15),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13)) 

grid.arrange(grafico_sensibilidade_rdd1, grafico_sensibilidade_rdd2, ncol = 2)
grid.arrange(grafico_placebo_rdd1, grafico_placebo_rdd2, ncol = 2)
