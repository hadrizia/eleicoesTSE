library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(tidyverse)
library(reshape2)
library(ggplot2)


eleitorado <- read.csv(here::here("dados_sanja.csv"))
eleitorado$NR_SECAO <- as.factor(eleitorado$NR_SECAO)
eleitorado <- eleitorado %>% 
  group_by(DS_FAIXA_ETARIA) %>% 
  mutate(faixa_etaria = 
           case_when(
             DS_FAIXA_ETARIA %in% c("16 anos", "17 anos", "18 anos", "19 anos") ~ '16 a 19 anos',
             DS_FAIXA_ETARIA %in% c("20 anos", "21 a 24 anos", "25 a 29 anos") ~ '20 a 29 anos',
             DS_FAIXA_ETARIA %in% c("30 a 34 anos", "35 a 39 anos") ~ '30 a 39 anos',
             DS_FAIXA_ETARIA %in% c("40 a 44 anos", "45 a 49 anos") ~ '40 a 49 anos',
             DS_FAIXA_ETARIA %in% c("50 a 54 anos", "55 a 59 anos") ~ '50 a 59 anos',
             DS_FAIXA_ETARIA %in% c("60 a 64 anos", "65 a 69 anos") ~ '60 a 69 anos',
             DS_FAIXA_ETARIA %in% c("70 a 74 anos", "75 a 79 anos") ~ '70 a 79 anos',
             DS_FAIXA_ETARIA %in% c("80 a 84 anos", "85 a 89 anos") ~ '80 a 89 anos',
             TRUE ~ '90 a 94 anos'))

secoes_data <- eleitorado %>% 
  pull(NR_SECAO) %>% unique()
resultados <-read.csv(here::here("resultados.csv"))

resultados_sanja <- resultados %>% 
  filter(NM_MUNICIPIO == 'SÃO JOÃO DO SABUGI') %>% 
  mutate(votos_Sanja = QT_VOTOS_NOMINAIS)

resultados_sumarizados <- resultados %>% 
  group_by(NM_CANDIDATO, DS_SIT_TOT_TURNO, SG_PARTIDO, DS_CARGO) %>% 
  summarise(votos_RN = sum(QT_VOTOS_NOMINAIS))

resultados_sumarizados <- merge(
  resultados_sumarizados, 
  resultados_sanja %>% select(votos_Sanja, NM_CANDIDATO),
  by = "NM_CANDIDATO")

get_candidato_by_cargo <- function(cargo) {
  resultados_sumarizados %>% 
    filter(toupper(DS_CARGO) == cargo & votos_RN > 0) %>% 
    arrange(-votos_Sanja) %>% 
    pull(NM_CANDIDATO) %>% unique()
}

melted_data <- function(df){
  melted <- melt(df[,c("NM_CANDIDATO","votos_RN","votos_Sanja")], 
                 id.vars="NM_CANDIDATO")
  return(melted)
}


# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Eleitorado Sanja 2018"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Perfil Eleitorado", 
               tabName = "profile", 
               icon = icon("fas fa-user")),
      menuItem("Resultados", 
               icon = icon("calendar"), 
               tabName = "results"),
      menuItem("Sobre", 
               icon = icon("fas fa-info-circle"), 
               tabName = "about")
    )
  ),
  
  dashboardBody(
    tags$style(HTML("
    .box {
      border: none;
    }
    .box.box-solid.box-danger>.box-header {
  color:#fff;
  background:#d845bb
                    }

.box.box-danger{
border-bottom-color:#d845bb;
border-left-color:#d845bb;
border-right-color:#d845bb;
border-top-color:#d845bb;
}")),
    
    shinyDashboardThemes(
      theme = "onenote"
    ),
    
    tabItems(
      tabItem(tabName = "profile",
              h3("Perfil Eleitorado Sabugiense 2018"),
              h5("Segundo o IBGE, o número estimado de habitantes no município de São João do Sabugi para 2018 é de aproximadamente 6.179 pessoas. Dentre as quais 4.774 são eleitores aptos, representando 77.2% da população total. Todos os dados utilizados estão disponíveis no site do TSE."),
              br(),
              tabBox(height = "600px", 
                     width = "500px",
                     tabPanel("Sexo",
                        fluidRow(
                          column(width = 12,
                             box(
                                plotOutput("sexo_plot")),
                             box(
                               h4("Eleitores que solicitaram inclusão do nome social no cadastro"),
                               h5("A tabela abaixo exibe o número de eleitores aptos que solicitaram inclusão do nome social em seu cadastrado junto à Justiça Eleitoral. Em São João do Sabugi, não houveram solicitações nas eleicões de 2018."),
                               tableOutput("nome_social_plot")),
                             box(
                               h4("Sexo - proporção eleitoral"),
                                 tableOutput("sexo_table"))
                        ))),
                     
                       tabPanel("Faixa Etária",
                                fluidRow(
                                  column(width = 12,
                                    box(
                                      selectInput('nr_secao', 'Selecione a(s) seção(ões):', 
                                                  secoes_data, multiple = TRUE,
                                                  selected = secoes_data[1]),
                                      plotOutput("idade_plot"),
                                      br()),
                                    box(
                                      h4("Faixa etária - proporção eleitoral"),
                                      br(),
                                      tableOutput("idade_table"))
                                    ))
                                ),
                     
                       tabPanel("Escolaridade",
                                fluidRow(
                                  column(width = 12,
                                    box(
                                      selectInput('nr_secao_eleitorado', 'Selecione a(s) seção(ões):', 
                                        secoes_data, multiple = TRUE,
                                         selected = secoes_data[1]),
                                      plotOutput("escolaridade_plot"),
                                      br()),
                                    box(
                                      h4(" Grau de escolaridade - proporção eleitoral"),
                                      br(),
                                      tableOutput("escolaridade_table"))
                                    )
                                )
                              )
                            )
              ),
      
      tabItem(tabName = "results",
              h3("Resultados do 1º turno - São João do Sabugi e Rio Grande do Norte"),
              h5("Abaixo são exibidos os resultados do primeiro turno das eleições 2018, comparando os votos de Sanja com o restante dos votos no RN. Todos os dados utilizados estão disponíveis no site do TSE."),
              br(),
              fluidRow(
                tabBox(
                  id = "results_gourp_by", height = "600px", width = "500px",
                  tabPanel("Deputado Estadual",
                           #  SEÇÃO DEPUTADO ESTADUAL
                           selectInput('candidatos_dep_estadual', 'Selecione o(s) candidato(s):', 
                                       get_candidato_by_cargo('DEPUTADO ESTADUAL'), multiple = TRUE,
                                       selected = get_candidato_by_cargo('DEPUTADO ESTADUAL')[1:5]),
                           plotOutput("candidatos_dep_estadual_plot")),
                  
                  tabPanel("Deputado Federal",
                             #  SEÇÃO DEPUTADO FEDERAL
                             selectInput('candidatos_dep_federal', 'Selecione o(s) candidato(s):', 
                                         get_candidato_by_cargo('DEPUTADO FEDERAL'), multiple = TRUE,
                                         selected = get_candidato_by_cargo('DEPUTADO FEDERAL')[1:5]),
                             plotOutput("candidatos_dep_federal_plot")),
                  
                  tabPanel("Governador",
                              #  SEÇÃO GOVERNADOR
                              selectInput('candidatos_governador', 'Selecione o(s) candidato(s):', 
                                          get_candidato_by_cargo('GOVERNADOR'), multiple = TRUE,
                                          selected = get_candidato_by_cargo('GOVERNADOR')[1:2]),
                              plotOutput("candidatos_governador_plot")),
                  
                  tabPanel("Senador",
                           #  SEÇÃO Senador
                           selectInput('candidatos_senador', 'Selecione o(s) candidato(s):', 
                                       get_candidato_by_cargo('SENADOR'), multiple = TRUE,
                                       selected = get_candidato_by_cargo('SENADOR')[1:2]),
                           plotOutput("candidatos_senador_plot")),
                  
                tabPanel("Presidente",
                           #  SEÇÃO Senador
                           selectInput('candidatos_presidente', 'Selecione o(s) candidato(s):', 
                                       get_candidato_by_cargo('PRESIDENTE'), multiple = TRUE,
                                       selected = get_candidato_by_cargo('PRESIDENTE')[1:4]),
                           plotOutput("candidatos_presidente_plot"))
                )
              )
              ),
      tabItem(tabName = "about",
              h3("Sobre"),
              fluidRow(
                column(width = 4,
                       box(
                         title = "O que é?", width = NULL, solidHeader = TRUE, status = "primary",
                         "Um pequeno levantamento do perfil eleitorado sabugiense e seus votos no 1º turno das Eleições, comparando-os com o resto do estado para investigar se a população sabugiense vota de acordo com o resto do RN."
                       )
                ),
                
                column(width = 4,
                       box(
                         title = "Por quê?", width = NULL, solidHeader = TRUE, status = "warning",
                         "'Como será o perfil dos eleitores da cidade em que voto?' Esta pergunta me motivou a buscar dados sobre os eleitores da minha cidade natal (São João do Sabugi/RN) e como eles votam em relação ao estado do RN." 
                       )
                       
                ),
                
                column(width = 4,
                       box(
                         title = "Como?", width = NULL, solidHeader = TRUE, status = "success",
                         "Filtrei os dados do TSE, do perfil eleitorado da cidade em 2018 e pude observar como estão distribuídos alguns aspectos como sexo, faixa etária, escolaridade, etc.; além de comparar os votos entre Sanja e o estado do RN."
                       )
                ),
                column(width = 12,
                       box(
                         title = "Quem sou eu", width = NULL, status="danger", solidHeader = F,
                         fluidRow(column(width = 10, 
                                   "Meu nome é Hadrizia Santos, graduanda em Ciência da Computação da UFCG que gosta de passáros, gatos, temaki, análise de dados e de brincar com dados públicos disponibilizados na internet. ",
                                   br(),
                                   "Email: hadrizia@gmail.com.",
                                   br(),
                                   "Facebook/Twitter/LinkedIn/GitHub: @hadrizia")
                                  )
                         )
                         
                       )
              ))
          )
      )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$candidatos_dep_estadual_plot <- 
     renderPlot({
       melted_data(resultados_sumarizados) %>% 
         filter(NM_CANDIDATO %in% input$candidatos_dep_estadual) %>% 
         ggplot(aes(x = NM_CANDIDATO, y = value, color = variable, group=NM_CANDIDATO)) + 
         coord_flip() +
         geom_segment(aes(x = NM_CANDIDATO, xend = NM_CANDIDATO, y = 0, yend = value), size = 1.1, alpha = 0.6) +
         geom_point(size = 3.5) +
         geom_text(aes(label=value), size = 3,vjust=1.9) +
         scale_y_log10()+
         labs(y = 'Número de votos nominais', x = '') +
         scale_color_discrete(name="Votos") +
         scale_fill_brewer(palette="Set3")
   })
   
   output$candidatos_dep_federal_plot <- 
     renderPlot({
       melted_data(resultados_sumarizados) %>% 
         filter(NM_CANDIDATO %in% input$candidatos_dep_federal) %>% 
         ggplot(aes(x = NM_CANDIDATO, y = value, color = variable, group=NM_CANDIDATO)) + 
         coord_flip() +
         geom_segment(aes(x = NM_CANDIDATO, xend = NM_CANDIDATO, y = 0, yend = value), size = 1.1, alpha = 0.6) +
         geom_point(size = 3.5) +
         geom_text(aes(label=value), size = 3,vjust=1.9) +
         scale_y_log10()+
         labs(y = 'Número de votos nominais', x = '') +
         scale_color_discrete(name="Votos") +
         scale_fill_brewer(palette="Set3")
     })
   
   output$candidatos_governador_plot <- 
     renderPlot({
       melted_data(resultados_sumarizados) %>% 
         filter(NM_CANDIDATO %in% input$candidatos_governador) %>% 
         ggplot(aes(x = NM_CANDIDATO, y = value, color = variable, group=NM_CANDIDATO)) + 
         coord_flip() +
         geom_segment(aes(x = NM_CANDIDATO, xend = NM_CANDIDATO, y = 0, yend = value), size = 1.1, alpha = 0.6) +
         geom_point(size = 3.5) +
         geom_text(aes(label=value), size = 3,vjust=1.9) +
         scale_y_log10()+
         labs(y = 'Número de votos nominais', x = '') +
         scale_color_discrete(name="Votos") +
         scale_fill_brewer(palette="Set3")
     })
   
   output$candidatos_senador_plot <- 
     renderPlot({
       melted_data(resultados_sumarizados) %>% 
         filter(NM_CANDIDATO %in% input$candidatos_senador) %>% 
         ggplot(aes(x = NM_CANDIDATO, y = value, color = variable, group=NM_CANDIDATO)) + 
         coord_flip() +
         geom_segment(aes(x = NM_CANDIDATO, xend = NM_CANDIDATO, y = 0, yend = value), size = 1.1, alpha = 0.6) +
         geom_point(size = 3.5) +
         geom_text(aes(label=value), size = 3,vjust=1.9) +
         scale_y_log10()+
         labs(y = 'Número de votos nominais', x = '') +
         scale_color_discrete(name="Votos") +
         scale_fill_brewer(palette="Set3")
     })
   
   output$candidatos_presidente_plot <- 
     renderPlot({
       melted_data(resultados_sumarizados) %>% 
         filter(NM_CANDIDATO %in% input$candidatos_presidente) %>% 
         ggplot(aes(x = NM_CANDIDATO, y = value, color = variable, group=NM_CANDIDATO)) + 
         coord_flip() +
         geom_segment(aes(x = NM_CANDIDATO, xend = NM_CANDIDATO, y = 0, yend = value), size = 1.1, alpha = 0.6) +
         geom_point(size = 3.5) +
         geom_text(aes(label=value), size = 3,vjust=1.9) +
         scale_y_log10()+
         labs(y = 'Número de votos nominais', x = '') +
         scale_color_discrete(name="Votos")  +
         scale_fill_brewer(palette="Set3")
     })
   
   output$sexo_plot <- 
     renderPlot({
         eleitorado %>% 
           group_by(DS_GENERO, NR_SECAO) %>% 
           filter(QT_ELEITORES_PERFIL > 0) %>% 
           summarise(total_pessoas = sum(QT_ELEITORES_PERFIL)) %>% 
           ggplot(aes(x = NR_SECAO, y = total_pessoas, fill = DS_GENERO)) +
           geom_histogram(stat = 'identity', position = "dodge") + 
           labs(y = "Total de pessoas", x = "Número da seção", fill="Sexo")
     })
   
   output$sexo_table <-
     renderTable({
       eleitorado %>% 
         group_by(DS_GENERO) %>% 
         summarise(total_eleitores = sum(QT_ELEITORES_PERFIL),
                   proporcao = paste0(round((sum(QT_ELEITORES_PERFIL) / 4774) * 100, 2), '%')) %>% 
         arrange(desc(total_eleitores)) %>% 
         rename(sexo = DS_GENERO)
     })
   
   output$nome_social_plot <- renderTable({
     eleitorado %>% 
       group_by(QT_ELEITORES_INC_NM_SOCIAL) %>% 
       summarise(total_eleitores = sum(QT_ELEITORES_INC_NM_SOCIAL),
                 proporcao = paste0(sum(QT_ELEITORES_INC_NM_SOCIAL) / total_eleitores, '%')) %>% 
       rename(qt_eleit_incl_nm_social = QT_ELEITORES_INC_NM_SOCIAL)
   })
   
   output$idade_plot <- 
     renderPlot({
       eleitorado %>% 
         group_by(faixa_etaria, NR_SECAO) %>% 
         filter(QT_ELEITORES_PERFIL > 0 & NR_SECAO %in% input$nr_secao) %>% 
         summarise(total_pessoas = sum(QT_ELEITORES_PERFIL)) %>%
         ggplot(aes(x = NR_SECAO, y = total_pessoas, fill=faixa_etaria)) +
         geom_histogram(stat = 'identity', position = "dodge") + 
         labs(y = "Total de pessoas", x = "Número da seção", fill="Faixa Etária") +
         coord_flip() +
         scale_fill_brewer(palette="Set3")
     })
   
   
   output$escolaridade_plot <- 
     renderPlot({
       eleitorado %>% 
         group_by(DS_GRAU_ESCOLARIDADE, NR_SECAO) %>% 
         filter(QT_ELEITORES_PERFIL > 0 & NR_SECAO %in% input$nr_secao_eleitorado) %>% 
         summarise(total_pessoas = sum(QT_ELEITORES_PERFIL)) %>%
         ggplot(aes(x = NR_SECAO, y = total_pessoas, fill=DS_GRAU_ESCOLARIDADE)) +
         geom_histogram(stat = 'identity', position = "dodge") + 
         labs(y = "Total de pessoas", x = "Número da seção", fill="Faixa Etária") +
         coord_flip() +
         scale_fill_brewer(palette="Set3")
     })
   
   output$escolaridade_table <-
     renderTable({
       eleitorado %>% 
         group_by(DS_GRAU_ESCOLARIDADE) %>% 
         summarise(total_eleitores = sum(QT_ELEITORES_PERFIL),
                   proporcao = paste0(round((sum(QT_ELEITORES_PERFIL) / 4774) * 100, 2), '%')) %>% 
         arrange(desc(total_eleitores)) %>% 
         rename(grau_escolaridade = DS_GRAU_ESCOLARIDADE)
     })
   
   output$idade_table <-
     renderTable({
       eleitorado %>% 
         group_by(faixa_etaria) %>% 
         summarise(total_eleitores = sum(QT_ELEITORES_PERFIL),
                   proporcao = paste0(round((sum(QT_ELEITORES_PERFIL) / 4774) * 100, 2), '%')) %>% 
         arrange(desc(total_eleitores))
     })
}

# Run the application 
shinyApp(ui = ui, server = server)