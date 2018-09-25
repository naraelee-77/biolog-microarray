library(DT)
source("visFunctions.R")
source("analyze.R")
source("transform.R")
source("run_opm.R")

function(input, output, session) {
  # analyze regular data
  result = eventReactive(input$rSubmit, analyze(
    input$repFile[["datapath"]], input$repHeaders, input$plate, input$type, 
    input$d1Start, input$d1End, input$d2Start, input$d2End
  ))
  
  # show data table
  output$resultTable = DT::renderDataTable({
    datatable(result(), extensions = c('FixedColumns', 'Scroller'),
              options = list(fixedColumns = list(leftColumns = 2), scrollX = TRUE, 
                             scrollY = input$dimension[2]-160, scroller = TRUE, deferRender = FALSE, 
                             pageLength = 100, searching = FALSE, bInfo = FALSE, bSort = FALSE), 
              rownames = FALSE)
  })
  
  # make .csv file to download
  output$download = downloadHandler(
    filename = function() {
      paste0(substr(input$repFile[["name"]], 1, nchar(input$repFile[["name"]]) - 4), "_analysis.csv") 
    }, 
    content = function(file) { write.csv(result(), file) }
  )

  # show download button
  output$downloadUI = renderUI(conditionalPanel(
    !is.null(result()), 
    fluidRow(column(2, strong("Export: "), align = "right", style = "font-size: 20px"),
             column(10, downloadButton("download", "Download CSV")))
  ))
  
  visFormat = eventReactive(input$kSubmit, {
    if (input$trp) {
      plotLayout = c(1, 8)
      plotNum = 8
      plotWidth = "8.8in"
      plotHeight = "1.5in"
    } else {
      plotLayout = c(8, 12)
      plotNum = 96
      plotWidth = "11in"
      plotHeight = "9.1in"
    }
    plotFormat = list(plotLayout, plotNum, plotWidth, plotHeight)
    names(plotFormat) = c("layout", "num", "width", "height")
    plotFormat
  })
  
  # analyze visual data
  visData = eventReactive(input$kSubmit, {
    pData = processCSV(input$pFile, input$pHeaders)
    cData = processCSV(input$cFile, input$cHeaders)
    
    validateData(pData, cData, input$trp, input$columns)
    if (input$trp && input$columns != "avg") {
      pData = transformTrp(pData, as.numeric(input$columns))
    }
    
    both = list(patient = pData, control = cData)
  })
  
  # combine 96 or 8 plots
  output$plotGrid = renderPlot({
    par(mfrow = visFormat()$layout, omi = c(0, 0, 0.5, 0))
    lapply(1:visFormat()$num, function(i) { wellPlot(visData(), i) })
    mtext(input$trpTitle, outer = TRUE, side = 3, cex = 2, line = 1, font = 2)
  })
  
  # output plots to interface
  output$plotUI = renderUI(tags$div(
    plotOutput("plotGrid", width = visFormat()$width, height = visFormat()$height), 
    style = paste0("overflow: scroll; height: ", input$dimension[2] - 200, "px"), align = "center"
  ))
  
  # legend
  output$legend = renderPlot({
    x = rep(1, times = 3)
    y = 3:1
    par(mar = c(0, 0, 0, 0), oma = c(0, 0, 2, 0))
    plot(x, y, pch = 22, bg = c("royalblue", "tan4", "khaki1"), axes = FALSE, xlab = "", ylab = "",
         ylim = c(0, 4), xlim = c(0, 4), cex = 2)
    text(x, y, labels = c("Patient", "Control", "Overlap"), pos = 4, cex = 1.5, offset = 1)
    mtext("Legend:", line = 0, outer = TRUE, cex = 1.5, font = 2)
  }, width = 130, height = 120)
  
  # make .csv file to download (opm)
  output$opmDownload = downloadHandler(
    filename = function() { paste0("opm.", input$opmFile[["name"]]) }, 
    content = function(file) { run_opm(input$opmFile, file) }
  )
}