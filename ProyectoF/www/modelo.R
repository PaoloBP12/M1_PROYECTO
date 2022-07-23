
modelo<-{ fluidRow(column(width = 12),h3("Modelo"),
                   sliderInput(inputId = "idKNN",label="Seleccione K",min=3, max=10, value=2),
                   sliderInput(inputId = "idx",label="CANTIDAD_PRODUCTOS_PEDIDOS",min=3, max=10000, value=2),
                   sliderInput(inputId = "idy",label="TOTAL",min=3, max=1000000, value=1000),
                   verbatimTextOutput(outputId = "idknnOutput")
)
}



