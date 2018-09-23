library(functional)
library(plyr)
library(tidyverse)
library(lubridate)

source(here::here("utils/constants.R"))

# Recupera os nomes das colunas correspondentes ao ano 
get_resultado_columns <- function(ano) {
  if (ano == 2006) {
    column_names <- col_names_receita_2006
  } else if (ano == 2010) {
    column_names <- col_names_receita_2010
  } else if(ano == 2014) {
    column_names <- col_names_receita_2014
  }
}


# Calcula o total de receita por candidato
get_total_receita <- function(df) {
  df %>% 
    group_by(nome_candidato, sq_candidato, sigla_uf, descricao_cargo, ano) %>% 
            summarize(total_receita = sum(valor_receita),
                      media_receita = mean(valor_receita))
}

# Agrega por tipo de receita
aggregate_by_tipo_receita <- function(df){
  df %>% 
    filter(str_detect(tolower(tipo_receita), 'recursos (de|)( pessoas| outros candidatos|próprios)')) %>%  
    mutate(tipo_receita = tolower(tipo_receita)) %>% 
    group_by(nome_candidato, sq_candidato, sigla_uf, descricao_cargo, ano, tipo_receita) %>% 
    summarize(total_receita = sum(as.numeric(valor_receita))) %>% 
    spread(key = tipo_receita, value = total_receita)
}

# Processa e sumariza receitas por tipo
preprocess_receita <- function (df){
  df <-
   df %>% 
    filter(toupper(descricao_cargo) %in% cargos) %>%
    mutate(valor_receita = sub(",", ".", valor_receita),
           valor_receita = as.double(valor_receita),
           descricao_cargo = toupper(descricao_cargo)) %>% 
    select(c(ano, sq_candidato, 
             nome_candidato, descricao_cargo, 
             numero_cand, sigla_uf, 
             sigla_partido, valor_receita, 
             tipo_receita, data_receita, nome_doador))
 
 receita_by_tipo = df %>% 
   aggregate_by_tipo_receita() %>% 
   data.table::data.table()
 
 df <- df %>% 
   group_by(nome_candidato, sq_candidato, sigla_uf, descricao_cargo, ano, nome_doador, valor_receita) %>% 
   summarize(qtd_doacoes_by_doador = n(),
             qtd_doadores_by_doador = n_distinct(nome_doador),
             tot_receita = sum(valor_receita)) %>%
   ungroup() %>% 
   group_by(nome_candidato, sq_candidato, sigla_uf, descricao_cargo, ano) %>% 
   summarize(qtd_doacoes = sum(qtd_doacoes_by_doador),
             qtd_doadores = sum(qtd_doadores_by_doador),
             total_receita = sum(valor_receita),
             media_receita = mean(valor_receita))
 
 df = merge(df %>% 
              data.table::data.table(), 
            receita_by_tipo, 
            by = c("nome_candidato", "sq_candidato",
                   "descricao_cargo", "sigla_uf", "ano"))
 return (df)
}

# Retorna um dataframe único para os receitas dos candidatos em um ano.
get_receitas_por_ano <- function(ano) {
  print(paste('Processando receitas do ano: ', ano))
  # Nome de todos os arquivos necessários
  filenames <- list.files(paste0(here::here("data/prestacao_contas/"), ano), pattern="Receita(s|)Candidato*", full.names=TRUE)
  
  # Lendo todos os arquivos e sumarizando em um único arquivo
  if (ano == 2010){
    read_latin <- Curry(read_delim, delim = ";", col_names=TRUE, local = locale("br", encoding = "latin1"), quote = "")
  } else {
    read_latin <- Curry(read_delim, delim = ";", col_names=TRUE, local = locale("br", encoding = "latin1"))
  }
  
  ldf <- lapply(filenames, read_latin)
  df <- ldply(ldf, data.frame)
  
  # Renomeia as colunas do dataframe
  names(df) <- get_resultado_columns(ano)
  
  df$ano = ano
  
  #df <- data.frame(sapply(df, function(x) gsub("\"", "", x)), stringsAsFactors = F)
  
  # Sumarizando receitas de acordo com o tipo da receita
  df <- df %>% 
    preprocess_receita()
  
  write.csv(df, paste0(here::here("data/prestacao_contas/Receitas-"), ano, ".csv"), row.names=FALSE)
  return(df)
}

preprocess_resultados_total <- function(ano_inicial, ano_final) {
  df <- do.call(rbind, lapply(seq(ano_inicial, ano_final, by=4), get_receitas_por_ano))
  
  write.csv(df, here::here("data/prestacao_contas/resultados_total.csv"), row.names=FALSE)
}