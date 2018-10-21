library(functional)
library(plyr)
library(tidyverse)
library(lubridate)

source(here::here("utils/constants.R"))

# Recupera os nomes das colunas correspondentes ao ano 
get_despesas_columns <- function(ano) {
  if (ano == 2006) {
    column_names <- col_names_despesa_2006
  } else if (ano == 2010) {
    column_names <- col_names_despesa_2010
  } else if(ano == 2014) {
    column_names <- col_names_despesa_2014
  }
}

# Recupera colunas comuns
preprocess_despesa <- function (df){
  df <- df %>% 
    filter(toupper(descricao_cargo) %in% cargos) %>% 
    mutate(valor_despesa = gsub(',', '.', valor_despesa),
           valor_despesa = as.double(valor_despesa),
           descricao_cargo = toupper(descricao_cargo)) %>% 
    select(c(ano, sq_candidato, 
             nome_candidato, descricao_cargo,  sigla_uf, 
             sigla_partido, valor_despesa, 
             tipo_despesa, data_despesa, nome_fornecedor))
  
  df %>% 
    group_by(nome_candidato, sq_candidato, sigla_uf, descricao_cargo, ano, nome_fornecedor, valor_despesa) %>% 
    summarize(qtd_despesas_by_fornecedor = n(),
              qtd_fornecedores_by_fornecedor = n_distinct(nome_fornecedor),
              tot_receita = sum(valor_despesa)) %>%
    ungroup() %>% 
    group_by(nome_candidato, sq_candidato, sigla_uf, descricao_cargo, ano) %>% 
    summarize(qtd_despesas = sum(qtd_despesas_by_fornecedor),
              qtd_fornecedores = sum(qtd_fornecedores_by_fornecedor),
              total_despesa = sum(valor_despesa),
              media_despesa = mean(valor_despesa))
}

# Retorna um dataframe único para os despesas dos candidatos em um ano.
get_despesas_por_ano <- function(ano = 2014) {
  print(paste('Processando despesas do ano: ', ano))
  # Nome de todos os arquivos necessários
  filenames <- list.files(paste0(here::here("data/prestacao_contas/"), ano), pattern="Despesa(s|)Candidato*", full.names=TRUE)
  
  # Lendo todos os arquivos e sumarizando em um único arquivo
  if (ano == 2010){
    read_latin <- Curry(read_delim, delim = ";", col_names=TRUE, local = locale("br", encoding = "latin1"), quote = "")
  } else {
  read_latin <- Curry(read_delim, delim = ";", col_names=TRUE, local = locale("br", encoding = "latin1"))
  }
  
  ldf <- lapply(filenames, read_latin)
  df <- ldply(ldf, data.frame)
  
  # Renomeia as colunas do dataframe
  names(df) <- get_despesas_columns(ano)
  
  df <- data.frame(sapply(df, function(x) gsub("\"", "", x)), stringsAsFactors = F)
  
  df$ano = ano
  
  # Recupera as colunas comuns durante os anos
  df <- df %>% 
    preprocess_despesa()
  
  
  write.csv(df, paste0(here::here("data/prestacao_contas/"), "Despesas-", ano, ".csv"), row.names=FALSE)
  return(df)
}

preprocess_resultados_total <- function(ano_inicial, ano_final) {
  df <- do.call(rbind, lapply(seq(ano_inicial, ano_final, by=4), get_despesas_por_ano))
  
  write.csv(df, paste0(here::here("data/prestacao_contas/Despesas-"), ano, ".csv"), row.names=FALSE)
}