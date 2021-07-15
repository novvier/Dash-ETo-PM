server <- function(input, output){
  # Salida de zona recomendada
  # zonaRecomendada <- reactive({
  #   paste("Zona recomendada:", input$var)
  # })
  datasetInput <- reactive({
    req(input$importar$datapath)
    # datos <- leer_datos(input$importar$datapath)
    evapPM(arch = input$importar$datapath,
           alt = input$altitud,
           lat = input$latitud,
           alb = input$albedo,
           zon = input$zona)
  })
  # Generar gráfico
  output$regrec <- renderPrint({
    reg <- zonaRecF(lat = input$latitud,
                    alt = input$altitud)
    cat("Región recomendada:", reg)
  })
  output$grafico <-  renderPlotly({
    plot_ly(datasetInput()$resultados, x=~fecha, y=~ETo,
            type = 'scatter', mode = 'lines') %>% 
      layout(xaxis = xlabel, yaxis = ylabel)
  })
  output$mensaje <- renderPrint({
    cat(datasetInput()$textRnol)
    cat(datasetInput()$textEa)
  })
  output$vista <- renderTable({
    datos <- datasetInput()$resultados
    datos$fecha <-  as.character(datos$fecha)
    ini <- input$ini
    lng <- nrow(datos)
    head(datos[ini:lng,], n = input$obs)
  })
  output$downloadData <- downloadHandler(
    filename = "resultados.xlsx",
    content = function(file) {
      write_xlsx(datasetInput()$resultados, file)
    }
  )
  output$notas <- renderPrint({
    cat("NOTAS:\n",
        "tmax : Temperatura Máxima (ºC)\n",
        "tmin : Temperatura Mínima (ºC)\n",
        "Rnoc : Radiación de onda corta (MJ/m2/día)\n",
        "Rnol : Radiación de onda larga (MJ/m2/día)\n",
        "Rn   : Radiación neta (MJ/m2/día)\n",
        "Ea   : Término aerodinámico (mm/día)\n",
        "ETo  : Evapotranspiración Potencial (mm/día)")
  })
}