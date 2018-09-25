navbarPage(
  "Biolog Microarray",
  tabPanel(
    "Regular Analysis",
    sidebarLayout(
      # input sidebar
      sidebarPanel(
        fileInput("repFile", "Upload Replicate Data (.csv)", accept = "text/csv"),
        checkboxInput("repHeaders", "Headers?", TRUE),
        
        selectInput("plate", "Plate #", c(1, 2, 3, 4, 5, 6, 7, 8, "trp" = 9)),
        selectInput("type", "Value Type", c("Absorbance" = 1, "Slope" = 2, "AUC" = 3)),
        
        strong("Patient Data Columns:"),
        fluidRow(column(5, numericInput("d1Start", NULL, 2, min = 1, step = 1), style = "padding-top: 5px"),
                 column(2, "to", style = "padding-top: 10px"),
                 column(5, numericInput("d1End", NULL, 5, min = 1, step = 1), style = "padding-top: 5px")),
        
        strong("Control Data Columns:"),
        fluidRow(column(5, numericInput("d2Start", NULL, 6, min = 1, step = 1), style = "padding-top: 5px"),
                 column(2, "to", style = "padding-top: 10px"),
                 column(5, numericInput("d2End", NULL, 9, min = 1, step = 1), style = "padding-top: 5px")),
        
        actionButton("rSubmit", "Submit"),
        
        width = 3),
      
      # data table output
      mainPanel(conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                 tags$div("Loading...", class = "loadmessage")), 
                uiOutput("downloadUI"),
                dataTableOutput("resultTable"), width = 9))
  ),
  
  tabPanel("Kinetic Analysis", sidebarLayout(
    # input sidebar
    sidebarPanel(fileInput("pFile", "Upload Patient Data (.csv)", accept = "text/csv"), 
                 checkboxInput("pHeaders", "Headers?", TRUE),
                 fileInput("cFile", "Upload Control Data (.csv)", accept = "text/csv"),
                 checkboxInput("cHeaders", "Headers?", TRUE),
                 
                 checkboxInput("trp", "trp", FALSE),
                 conditionalPanel("input.trp", radioButtons(
                   "columns", "Columns:", c("01 to 03" = 1, "04 to 06" = 2, "07 to 09" = 3, "10 to 12" = 4,
                                            "Averages File" = "avg")
                   )),
                 
                 textInput("trpTitle", "Plot Title"),
                 
                 actionButton("kSubmit", "Submit"),
                 width = 3),
    
    # plot visualizations output
    mainPanel(fluidRow(column(4, conditionalPanel(condition="$('html').hasClass('shiny-busy')", 
                                                  tags$div("Loading...", class = "loadmessage")), offset = 4),
                       column(4, plotOutput("legend", inline = TRUE))),
              uiOutput("plotUI"), width = 9)
  )),
  
  tabPanel(
    "OPM Conversion",
    sidebarLayout(
      sidebarPanel(
        fileInput("opmFile", "Upload Kinetic File (.csv)", accept = "text/csv"),
        downloadButton("opmDownload", "Download CSV")),
      mainPanel()
    )
  ),

  # css and javascript formatting
  tags$head(tags$style(".col-sm-1 { padding-right: 0px; padding-left: 0px }
                       .col-sm-9 { padding-right: 15px; padding-left: 0px; padding-bottom: 0px }
                       .progress { margin-bottom: 0px }
                       .checkbox { margin-top: 0px }
                       .loadmessage { text-align: center; font-weight: bold; font-size: 20px }
                       .dataTables_scrollBody { border-bottom: 0px !important }"),
            tags$script('var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                          dimension[0] = window.innerWidth;
                          dimension[1] = window.innerHeight;
                          Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                          dimension[0] = window.innerWidth;
                          dimension[1] = window.innerHeight;
                          Shiny.onInputChange("dimension", dimension);
                        });
                        $(document).ready(function(e) {
                          $("input[value=\'avg\']").parent().parent().before("OR");
                        })'))
)
