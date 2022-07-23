exploracion<-{
  navlistPanel(
    tabPanel("Graficos con ggplot2", 
             h4("GGplot2"),selectInput("expGgplot","Consultas exploratorias graficas",choices = c("Consulta 1"='1',"Consulta 2"='2',"Consulta 3"='3')),
             plotOutput("resGgplot2")
             
             
    ),
    tabPanel("Consultas en plotly", 
             h4("Plotly"),selectInput("expPlotly","Consultas exploratorias graficas",choices = c("Consulta 1"='1',"Consulta 2"='2',"Consulta 3"='3',"Consulta 4"='4')),
             plotlyOutput("resPlotly")
             
    )
  )
}

