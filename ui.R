#------------------------------------------------------------------------------#
#                                INTERFAZ DE USUARIO                           #
#------------------------------------------------------------------------------#

ui <- fluidPage(
  # Título de la aplicación,
  titlePanel("Evapotranspiración Potencial - Penman Modificado para Perú"),
  # Barra de entradas y salidas
  sidebarLayout(
    # Barra de entradas
    sidebarPanel(
      # Input 1: Latitud en grados decimales
      numericInput(inputId = "latitud",
                   label = "Latitud (grados):",
                   value = -16.64, 
                   min = -18.5,
                   max = 0,
                   step = 0.0001),
      # Input 2: Altitud en m.s.n.m.
      numericInput(inputId = "altitud",
                   label = "Altitud (m.s.n.m.):",
                   value = 44,
                   min = 0,
                   max = 7000,
                   step = 1),
      # Input 3: Albedo, de 0 a 1
      numericInput(inputId = "albedo",
                   label = "Albedo (0 a 1):",
                   value = 0.23,
                   min = 0,
                   max = 1,
                   step = 0.01),
      # Input 4: Zona
      selectInput(inputId = "zona",
                  label = "Zona:",
                  choices = c("costa norte", "costa central", "costa sur",
                              "sierra norte", "sierra central", "sierra sur",
                              "selva"),
                  selected = "costa sur"),
      # Input 5: Importar datos excel
      fileInput(inputId = "importar", 
                label = "Seleccione Excel de temperaturas:",
                accept=c('application/vnd.ms-excel',
                         'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                         '.xls',
                         '.xlsx')),
      # Input 6: Total de resultados a vizualizar
      numericInput(inputId = "ini",
                   label = "Número de inicio de observaciones:",
                   value = 1),
      # Input 6: Total de resultados a vizualizar
      numericInput(inputId = "obs",
                   label = "Número de observaciones a vizualizar:",
                   value = 10),
      # # Input 6: Actualizar gráfico
      # actionButton("update", "Actualizar gráfico"),
      # Descargar resultados
      downloadButton("downloadData", "Descargar resultados")
    ),
    # Panel de resultados
    mainPanel(
      # Output 1: Salida de región recomendada
      verbatimTextOutput("regrec"),
      # Output 2: Salida gráfica
      plotlyOutput(outputId = "grafico"),
      # Output 3: Salida de mensajes y advertencias
      verbatimTextOutput("mensaje"),
      # Output 4: Tabla de resultados
      tableOutput("vista"),
      # Output 5: Unidades
      verbatimTextOutput("notas")
    )
  )
)
