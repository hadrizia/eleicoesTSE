library(tidyverse)
library(data.table)

source(here::here("utils/constants.R"))

merge_despesas_receitas <- function(despesas_df, receitas_df) {
  merged_df <- merge(data.table(despesas_df), data.table(receitas_df),
                     by= c("ano", "sq_candidato", "nome_candidato", "sigla_uf", "descricao_cargo"))
  return(merged_df)
}

merge_candidatos_resultados <- function(resultados_df, candidatos_df){
  merged_df <- merge(data.table(resultados_df), data.table(candidatos_df),
                     by= c("ano", "sq_candidato", "nome_candidato", "sigla_uf", "descricao_cargo", "sigla_partido"))
  return(merged_df)
}
merge_all <- function(despesas_df, receitas_df, resultados_df, candidatos_df){
  merged_df <- merge(data.table(merge_candidatos_resultados(resultados_df, candidatos_df)),
                     data.table(merge_despesas_receitas(despesas_df, receitas_df)),
                     by= c("sq_candidato", "ano", "nome_candidato", "sigla_uf", "descricao_cargo"))
  return(merged_df)
}

process_dataframe <- function(df){
  df <-
    df %>%
    select(-nome_municipio) %>%
    group_by(ano, sq_candidato, nome_candidato,
             sigla_uf, sigla_partido, qtd_doacoes,
             qtd_doadores, total_receita, media_receita,
             `recursos de outros candidatos/comitês`,
             `recursos de pessoas físicas`, `recursos de pessoas jurídicas`, `recursos próprios`,
             qtd_despesas, qtd_fornecedores, total_despesa,
             media_despesa, descricao_cargo, sexo,
             grau_instrucao, estado_civil, descricao_ocupacao) %>%
    summarise(votos = sum(total_votos))

  return(df)
}

merge_all_por_ano <- function(ano=2014) {
  despesas_df <- read_csv(paste0(here::here('data/prestacao_contas/Despesas-'), ano, '.csv'))
  receitas_df <- read_csv(paste0(here::here('data/prestacao_contas/Receitas-'), ano, '.csv'))
  resultados_df <- read_csv(paste0(here::here('data/resultados/Resultados-'), ano, '.csv'))
  candidatos_df <- read_csv(paste0(here::here('data/candidatos/candidatos_'), ano, '.csv'))

  df <- merge_all(despesas_df, receitas_df, resultados_df, candidatos_df)

  df <-
    df %>%
    process_dataframe()

  #names(df) <- col_names_dataframe
  write.csv(df, paste0(here::here('data/preprocessed/eleicoes_'), ano, '.csv'), row.names=FALSE)

  return (df)
}

build_all_data <- function() {
  anos <- seq(2006, 2014, by=4)
  df <- do.call(rbind, lapply(anos, merge_all_por_ano))

  names(df) <- col_names_dataframe
  write.csv(df, paste0(here::here("data/preprocessed/eleicoes_2006_a_2014.csv")), row.names=FALSE)
  print('Done! :)')
}

build_all_data()
