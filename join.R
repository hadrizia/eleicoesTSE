library(tidyverse)
library(data.table)

merge_despesas_receitas <- function(despesas_df, receitas_df) {
  df_merged <- merge(data.table(despesas_df), data.table(receitas_df), 
                     by= c("ano", "sq_candidato", "nome_candidato", "sigla_uf", "descricao_cargo"))
  return(df_merged)
}

merge_despesas_receitas_resultados <- function(despesas_df, receitas_df, resultados_df){
  df_merged <- merge(data.table(merge_despesas_receitas(despesas_df, receitas_df)),
                     data.table(resultados_df), 
                     by= c("ano", "sq_candidato", "nome_candidato", "sigla_uf", "descricao_cargo", "sigla_partido"))
}