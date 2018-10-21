library(tidyverse)

filtra_candidatos_eleitos <- function(df) {
  df <- df %>% 
    filter(
      grepl("^ELEITO", toupper(desc_sit_cand_tot))
    )  %>% group_by(ano, sq_candidato, nome_candidato, descricao_cargo, sigla_partido, desc_sit_cand_tot, sigla_uf) %>% 
    summarise(total_votos = sum(total_votos))
  return(df)
}

preprocessa_candidatos_eleitos <- function(ano) {
  resultados_df <- read_csv(paste0(here::here('data/resultados/Resultados-'), ano, '.csv'))
  resultados_df <- filtra_candidatos_eleitos(resultados_df)
  write.csv(resultados_df, paste0(here::here('data/candidatos/candidatos_eleitos_'), ano, '.csv'), row.names = F)
}
