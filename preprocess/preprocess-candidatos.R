library(functional)
library(plyr)
library(tidyverse)
library(lubridate)

source(here::here("utils/constants.R"))

# Filtra os candidatos cuja situação é igual a 2º turno (já que está as info sobre o candidato estão
# disponíveis em outra linha com o resultado final do segundo turno) e seleciona apenas variáveis úteis.
preprocess_candidatos <- function(df) {
  df %>%
    filter(!str_detect(desc_sit_cand_tot, regex('2º TURNO')) & toupper(descricao_cargo) %in% cargos & grepl('^ELEITO', toupper(desc_sit_cand_tot))) %>% 
    select(ano, sq_candidato, sigla_uf, 
           nome_candidato,  sigla_partido,
           sexo, estado_civil, grau_instrucao, 
           descricao_ocupacao, descricao_cargo, cor_raca, data_nascimento, descricao_nacionalidade) %>% 
    mutate(descricao_cargo = toupper(descricao_cargo))
}

# Recupera os nomes das colunas correspondentes.
get_candidatos_columns <- function(ano) {
  if (ano <= 2010) {
    column_names <- col_names_candidatos_ate_2010
  } else if (ano == 2012){
    column_names <- col_names_candidatos_2012
  } else if (ano >= 2014 & ano < 2018){
    column_names <- col_names_candidatos_2014_em_diante
  } else if (ano == 2018){
    column_names <- col_names_candidatos_2018
  }
}

# Retorna um dataframe único para os resultados dos candidatos em um ano.
get_candidatos_por_ano <- function(ano = 2014) {
  # Nome de todos os arquivos necessários
  filenames <- list.files(paste0(here::here("data/candidatos/"), ano), pattern="consulta_cand_*", full.names=TRUE)
  
  # Lendo todos os arquivos e sumarizando em um único arquivo
  read_latin <- Curry(read_delim, delim = ";", col_names=FALSE, local = locale("br", encoding = "latin1"), quote = '')
  ldf <- lapply(filenames, read_latin)
  df <- ldply(ldf, data.frame)
  
  df <- data.frame(sapply(df, function(x) gsub('"', "", x)), stringsAsFactors = F)
  # Renomeia as colunas do dataframe
  names(df) <- get_candidatos_columns(ano)
  
  return(df)
}

# Preprocessa e salva o dataframe recuperado na função get_votacao_candidato_por_ano().
preprocess_candidatos_por_ano <- function (ano = 2014) {
  df <- get_candidatos_por_ano(ano)
  df <- preprocess_candidatos(df)
  
  # Salva o arquivo no diretório '../data/candidatos/<ano>/candidatos_<ano>.csv'
  write.csv(df, paste0(here::here("data/candidatos/"), ano, "candidatos_", ano, ".csv"), row.names = FALSE)
  
  return(df)
} 

get_candidatos_por_filename <- function(filename, ano){
  print(paste('Processando arquivo: ', filename))
  df <- readr::read_delim(filename, delim = ";", col_names=FALSE, local = readr::locale("br", encoding = "latin1"))
  df <- df %>% as.data.frame()
  
  # Renomeia as colunas do dataframe
  names(df) <- get_candidatos_columns(ano)
  
  # Remove cabeçalho no ano de 2018
  if(ano == 2018){
    df <- df %>%
      slice(2:nrow(df))
  }
  
  # Sumarizando votos (filtrando segundo turno e cargos desejados)
  df <- df %>% 
    preprocess_candidatos()
  
  return(df)
}

get_resultados_por_ano_otimizado <- function(ano){
  print(paste('Processando resultados do ano: ', ano))
  # Nome de todos os arquivos necessários
  filenames <- list.files(paste0(here::here("data/candidatos/"), ano), pattern="consulta_cand_*", full.names=TRUE)
  
  res <- purrr::pmap(list(filenames), function(x) get_candidatos_por_filename(x, ano))
  candidatos <- purrr::map_df(res, ~ .)
  
  write.csv(candidatos, paste0(here::here("data/candidatos/candidatos_"), ano, ".csv"), row.names=FALSE)
}

# Preprocessa e salva o dataframe recuperado na função get_votacao_candidato_por_ano() para os anos de um intervalo.
preprocess_candidatos_total <- function(ano_inicial, ano_final) {
  df <- lapply(seq(ano_inicial, ano_final, by=2), preprocess_candidatos_por_ano)
  df_f <- do.call(rbind, df)
  
  # Salva o arquivo no diretório '../data/candidatos/candidatos_<ano_inicial>_a_<ano_final>.csv'
  write_csv(df, paste0(here::here("data/candidatos/"), ano, "candidatos_", ano_inicial, "_a_", ano_final, ".csv"), row.names = FALSE)
  
  return(df)
}

#df_2000 <- preprocess_candidatos_por_ano(2000)
#df_2002 <- preprocess_candidatos_por_ano(2002)
#df_2004 <- preprocess_candidatos_por_ano(2004)
#df_2006 <- preprocess_candidatos_por_ano(2006)
#df_2008 <- preprocess_candidatos_por_ano(2008)
#df_2010 <- preprocess_candidatos_por_ano(2010)
#df_2012 <- preprocess_candidatos_por_ano(2012)
#df_2014 <- preprocess_candidatos_por_ano(2014)
#df_2016 <- preprocess_candidatos_por_ano(2016)

#df <- rbind(df_2000, df_2002)
#df <- rbind(df, df_2004)
#df <- rbind(df, df_2006)
#df <- rbind(df, df_2008)
#df <- rbind(df, df_2010)
#df <- rbind(df, df_2012)
#df <- rbind(df, df_2014)
#df <- rbind(df, df_2016)

#write.csv(df, paste0(here::here("data/candidatos/candidatos_"), "2000_a_2016.csv"), row.names = FALSE)


