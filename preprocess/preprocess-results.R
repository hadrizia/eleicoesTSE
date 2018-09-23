library(functional)
library(plyr)
library(tidyverse)


source(here::here("utils/constants.R"))

#eleicoes2014 <- read.csv("data/eleicoes2014.csv", fileEncoding = "latin1")

# Recupera os nomes das colunas correspondentes ao ano 
get_resultado_columns <- function(ano) {
  if (ano <= 2012) {
    column_names <- col_names_candidatos_munzona_ate_2012
  } else {
    column_names <- col_names_candidatos_munzona_2014_em_diante
  }
}

summarise_votos <- function(df) {
  df %>% 
    filter(!str_detect(desc_sit_cand_tot, regex('2º TURNO')) & descricao_cargo %in% cargos) %>% 
    dplyr::group_by(ano, sigla_uf, nome_municipio, 
                    sq_candidato, nome_candidato,
                    descricao_cargo, sigla_partido, desc_sit_cand_tot) %>%
    dplyr::summarize(total_votos = sum(tot_votos))
}

# Retorna um dataframe único para os resultados dos candidatos em um ano.
get_resultados_por_ano <- function(ano = 2014) {
  print(paste('Processando resultados do ano: ', ano))
  # Nome de todos os arquivos necessários
  filenames <- list.files(paste0(here::here("data/resultados/"), ano), pattern="votacao_candidato_munzona_*", full.names=TRUE)
  
  # Lendo todos os arquivos e sumarizando em um único arquivo
  read_latin <- Curry(read_delim, delim = ";", col_names=FALSE, local = locale("br", encoding = "latin1"))
  ldf <- lapply(filenames, read_latin)
  df <- ldply(ldf, data.frame)
  
  # Renomeia as colunas do dataframe
  names(df) <- get_resultado_columns(ano)
  
  # Sumarizando votos (filtrando segundo turno e cargos desejados (deputados federais))
  df <- df %>% 
    summarise_votos()
  
  write.csv(df, paste0(here::here("data/resultados/Resultados-"), ano, ".csv"), row.names=FALSE)
  
  return(df)
}

preprocess_resultados_total <- function(ano_inicial, ano_final) {
  df <- do.call(rbind, lapply(seq(ano_inicial, ano_final, by=4), get_resultados_por_ano))
  
  write.csv(df, here::here("data/resultados/resultados_total.csv"), row.names=FALSE)
}
