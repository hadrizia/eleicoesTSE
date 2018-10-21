library(tidyverse)
library(data.table)

source(here::here("utils/constants.R"))

merge_candidatos_resultados <- function(resultados_df, candidatos_df){
  merged_df <- left_join(data.table(resultados_df), data.table(candidatos_df),
                     by= c("ano", "sq_candidato", "sigla_uf", "descricao_cargo", "sigla_partido")) %>% 
    select(-nome_candidato.y)
  return(merged_df)
}

merge_por_ano <- function(ano=2014) {
  resultados_df <- read_csv(paste0(here::here('data/candidatos/candidatos_eleitos_'), ano, '.csv'))
  candidatos_df <- read_csv(paste0(here::here('data/candidatos/candidatos_'), ano, '.csv'))
  
  df <- merge_candidatos_resultados(resultados_df, candidatos_df)
  
  write.csv(df, paste0(here::here('data/preprocessed/eleicoes_'), ano, '.csv'), row.names=FALSE)
  
  return (df)
}

build_all_data <- function() {
  anos <- seq(2014, 2018, by=4)
  df <- do.call(rbind, lapply(anos, merge_por_ano))

  write.csv(df, paste0(here::here("data/preprocessed/eleicoes_2014_e_2018.csv")), row.names=FALSE)
  print('Done! :)')
}

build_all_data()
