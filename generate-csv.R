library(tidyverse)
library(Amelia)
library(leaps)
library(ggplot2)
library(corrplot)

eleicoes <- readr::read_csv(
  here::here('data/preprocessed/eleicoes_2014.csv'), 
  local=readr::locale("br"),
  col_types = cols(
    ano = col_integer(),
    sequencial_candidato = col_character(),
    quantidade_doacoes = col_integer(),
    quantidade_doadores = col_integer(),
    total_receita = col_double(),
    media_receita = col_double(),
    recursos_de_outros_candidatos.comites = col_double(),
    recursos_de_pessoas_fisicas = col_double(),
    recursos_de_pessoas_juridicas = col_double(),
    recursos_proprios = col_double(),
    `recursos_de_partido_politico` = col_double(),
    quantidade_despesas = col_integer(),
    quantidade_fornecedores = col_integer(),
    total_despesa = col_double(),
    media_despesa = col_double(),
    votos = col_integer(),
    .default = col_character()))

eleicoes <- as.data.frame(eleicoes)

missmap(eleicoes)

# Setando valores  NA para a média dos valores da coluna recursos_de_pessoas_juridicas
eleicoes$recursos_de_pessoas_juridicas[is.na(eleicoes$recursos_de_pessoas_juridicas)]<- 0

eleicoes$recursos_de_partido_politico[is.na(eleicoes$recursos_de_partido_politico)]<- 0

#Setando valores NA para a média dos valores da coluna recursos_proprios
eleicoes$recursos_proprios[is.na(eleicoes$recursos_proprios)]<- 0

#Setando valores NA para a média dos valores da coluna recursos_de_outros_candidatos.comites
eleicoes$`recursos_de_outros_candidatos.comites`[is.na(eleicoes$`recursos_de_outros_candidatos.comites`)]<- 0

#Setando valores NA para a média dos valores da coluna recursos_de_pessoas_físicas.
eleicoes$recursos_de_pessoas_fisicas[is.na(eleicoes$recursos_de_pessoas_fisicas)]<- 0

eleicoes$quantidade_doacoes[is.na(eleicoes$quantidade_doacoes)]<- 0

eleicoes$quantidade_doadores[is.na(eleicoes$quantidade_doadores)]<- 0

eleicoes$quantidade_fornecedores[is.na(eleicoes$quantidade_fornecedores)]<- 0

eleicoes$quantidade_despesas[is.na(eleicoes$quantidade_despesas)]<- 0

eleicoes$total_despesa[is.na(eleicoes$total_despesa)] < median(eleicoes$total_despesa, na.rm = T)

eleicoes <- eleicoes %>% 
  group_by(ano, sequencial_candidato, nome) %>% 
  mutate(total_receita = sum(recursos_proprios, recursos_de_pessoas_fisicas, `recursos_de_outros_candidatos.comites`, recursos_de_pessoas_juridicas, 
                             recursos_de_partido_politico),
         media_receita = total_receita/quantidade_doacoes,
         media_despesa = total_despesa/quantidade_despesas) %>% ungroup()
eleicoes <- eleicoes %>% 
  mutate(media_receita = round(media_receita, 2),
         media_despesa = round(media_despesa, 2),
         total_despesa = round(total_despesa, 2),
         total_receita = round(total_receita, 2))

eleicoes$sequencial_candidato <- as.character(eleicoes$sequencial_candidato)

missmap(eleicoes)

write.csv(, here::here('data/test_hidden.csv'), row.names=FALSE)

