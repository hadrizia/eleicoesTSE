# Dados das eleições 

Este repositório tem como objetivo prover os dados eleitorais dos candidatos ao cargo de deputado federal para auxiliar na disciplina de Ciência de Dados Preditiva no período de 2018.2.

## Os dados

Os dados originais estão disponíveis no site do [TSE](http://tse.jus.br/eleicoes/estatisticas/repositorio-de-dados-eleitorais-1/repositorio-de-dados-eleitorais) e estão divididos da seguinte forma:
 
 * **Candidatos**: contendo informações mais pessoais sobre os candidatos, como idade, estado civil, sexo, escolaridade, raça, etc.
 * **Resultados**: com as informaçes sobre o resultado das eleições, como número de votos, situação do candidato, etc.
 * **Prestação de contas**: possui informações sobre as receitas e despesas declaradas pelos candidatos referentes à campanha, como valor, data e tipo da receita (recurso próprio, de outros partidos, de pessoas fisicas, etc.), nome do doador, nome do fornecedor, valor, data e tipo da despesa, entre outros.

## O repositório

 Os arquivos deste repositrio estão organizados da seguinte forma:
 
  * `data`: Que está no .gitignore, mas pode ser baixado [aqui](https://drive.google.com/open?id=12ZgoKWbXcwEYhVrCKEI7nxoL4NxYcaC7);
    *  `candidatos`: onde estão tanto os arquivos baixados do TSE quanto os arquivos preprocessados dos candidatos;
    *  `preprocessed`: diretório onde estão os arquivos csv padronizados e com todas as informações necessárias para a realização das atividades;
    *  `prestacao_contas`: diretório onde estão tanto os arquivos baixados do TSE quanto os arquivos preprocessados das receitas e despesas declaradas pelos candidatos;
    *  `resultados`: estão os arquivos baixados do TSE e os arquivos preprocessados dos resultados das votações;
  * `preprocess`: Onde estão os scripts feitos para o preprocessamento dos dados (padronização das colunas, sumarização, etc.);
  * `utils`: Com os arquivos utilizados nos scripts, como de constantes; 
  * `process.R`: Script que gera o csv resultante contendo todos os dados dos anos de 2006 a 2014 (*build_all_data()*). Nele também é possível gerar os csv separadamente, com a função *merge_all_by_ano(<ano>)*.
  
  ## Como executar
  
   1. Baixe o arquivo `data.zip` que está disponível no [drive](https://drive.google.com/open?id=12ZgoKWbXcwEYhVrCKEI7nxoL4NxYcaC7), descompacte-o e cole o diretório data dentro da pasta do repositório;
   2. Na pasta do repositório, execute o script R através do comando abaixo. (É necessário baixar as bibliotecas **tidyverse** e **data.table** antes!)   
   ```
    Rscript process.R
   ```
   3. Se apareceu uma mensagem 'Done!', isso significa que deu tudo certo e que o csv com os anos de 2006 a 2014 estarão em `data/preprocessed/ eleicoes_2006_a_2014.csv`.
