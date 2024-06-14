####################################### WELCOME TO THE SHINY APP ##################################
####################################### from Sandra K. (2023) #####################################
###################################################################################################

####################################### Scripts ###################################################

source("R/generator.R")
source("R/percentile.R")

####################################### Libraries #################################################

if("DT" %in% rownames(installed.packages())){
  library(DT)} else{
    install.packages("DT")
    library(DT)}

if("gamlss" %in% rownames(installed.packages())){
  library(gamlss)} else{
    install.packages("gamlss")
    library(gamlss)}

if("shinydashboard" %in% rownames(installed.packages())){
  library(shinydashboard)} else{
    install.packages("shinydashboard")
    library(shinydashboard)}

####################################### User Interface ############################################

ui <- dashboardPage(
  dashboardHeader(title = "AdRI_Generator", titleWidth = 200),
  dashboardSidebar(width = 200,
                   sidebarMenu(
                     id = "sidebarid",
                     ### Sidebar - Generator ###
                     menuItem(
                       "Generator",
                       tabName = "generator",
                       icon = icon("calculator"),
                       selected = TRUE
                     ),
                     ### Sidebar - Percentile ###
                     menuItem("Percentile",
                              tabName = "percentile",
                              icon = icon("folder"))
                   )),
  dashboardBody(tabItems(
    ### MainPanel - Generator ###
    
    tabItem(
      tabName = "generator",
      
      p(
        strong(
          "This Shiny App is a generator to create age-dependent data from labor analytes!"
        ),
        br(),
        br(),
        "The following distributions are available:
                     Normaldistribution (with μ and σ), Lognormaldistribution (with μ and σ),
                     Box-Cox Cole & Green Distribution (with μ, σ and ν),
                     Box-Cox t-Distribution (with μ, σ, ν and τ) and Box-Cox Power Exponential Distribution (with μ, σ, ν and τ).
                     The parameters μ, σ, ν and τ can be changed over the time with a linear or an exponentially function.
                     The linear function is",
        strong("y = m*x + b"),
        "and the exponentially",
        strong("y = a*e^(x*b)"),
        ". All negative values are deleted automatically
                     and the data is saved in the form needed for the Shiny App AdRI",
        a("AdRI", href = "https://github.com/SandraKla/AdRI"),
        ". The data is saved with no sex, with unique values and the station is named Generator."
      ),
      
      fluidRow(
        column(width = 6,
               fluidRow(
                 box(
                   width = 6,
                   title = tagList(shiny::icon("gear"), "Settings"),
                   status = "primary",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   sliderInput("age_generator", "Maximum age [years]:", 0, 100, 18),
                   sliderInput("age_generator_steps", "Age steps [days]:", 1, 365, 100),
                   hr(),
                   sliderInput("ill_factor", "Pathological cases [%]:", 0, 0.5, 0),
                   sliderInput("mu_factor_ill", "Factor added to mean (µ) for the pathological cases:", 0, 1000, 1),
                   hr(),
                   selectInput(
                     "family_generator",
                     "Distribution:",
                     choices = list(
                       "Normaldistribution" = "NO",
                       "Log-Normaldistribution" = "LOGNO",
                       "Box-Cole and Green Distribution" = "BCCG",
                       "Box-Cole Green Exp. Distribution" = "BCPE",
                       "Box-Cole Green t-Distribution" = "BCT"
                     )
                   ),
                   sliderInput("n_", "Number of observations:", 10, 1000, 10),
                   hr(),
                   textInput("text", "Name the Analyte:", value = "Analyte"),
                   textInput("text_unit", "Unit of the Analyte:", value = "Unit"),
                   hr(),
                   downloadButton("download_settings", icon = icon("download"), "Settings"),
                   downloadButton("download_plot", "Plot"),
                   downloadButton("download_data", "Data")
                 ),
                 
                 box(
                   ######################## Mu Simulation ###################################
                   title = tagList(shiny::icon("gear"), "Settings for the distribution parameters"),
                   width = 6,
                   status = "primary",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   
                   selectInput(
                     "trend_mu",
                     "Trend for µ:",
                     c(Linear = "linear", Exponentially = "exponentially")
                   ),
                   conditionalPanel(
                     condition = "input.trend_mu == 'linear'",
                     div(style = "display:inline-block", numericInput("intercept_mu", "Intercept:", 1)),
                     div(style = "display:inline-block", numericInput("slope_mu", "Slope:", 0))
                   ),
                   conditionalPanel(
                     condition = "input.trend_mu == 'exponentially'",
                     div(style = "display:inline-block", numericInput("a_mu", "A:", 1)),
                     div(style = "display:inline-block", numericInput("b_mu", "B:", 0))
                   ),
                   hr(),
                   
                   ######################## Sigma Simulation ################################
                   selectInput(
                     "trend_sigma",
                     "Trend for σ:",
                     c(Linear = "linear", Exponentially = "exponentially")
                   ),
                   conditionalPanel(
                     condition = "input.trend_sigma == 'linear'",
                     div(style = "display:inline-block", numericInput("intercept_sigma", "Intercept:", 1)),
                     div(style = "display:inline-block", numericInput("slope_sigma", "Slope:", 0))
                   ),
                   conditionalPanel(
                     condition = "input.trend_sigma == 'exponentially'",
                     div(style = "display:inline-block", numericInput("a_sigma", "A:", 1)),
                     div(style = "display:inline-block", numericInput("b_sigma", "B:", 0))
                   ),
                   
                   conditionalPanel(
                     condition = "input.family_generator == 'BCCG' ||
                                        input.family_generator == 'BCPE' ||
                                        input.family_generator == 'BCT'",
                     hr()
                   ),
                   
                   ######################## Nu Simulation ###################################
                   conditionalPanel(
                     condition = "input.family_generator == 'BCCG' ||
                                        input.family_generator == 'BCPE' ||
                                        input.family_generator == 'BCT'",
                     selectInput(
                       "trend_nu",
                       "Trend for ν:",
                       c(Linear = "linear", Exponentially = "exponentially")
                     )
                   ),
                   
                   conditionalPanel(
                     condition = "(input.family_generator == 'BCCG' ||
                                        input.family_generator == 'BCPE' ||
                                        input.family_generator == 'BCT') &&
                                        input.trend_nu == 'linear'",
                     div(style = "display:inline-block", numericInput("intercept_nu", "Intercept:", 1)),
                     div(style = "display:inline-block", numericInput("slope_nu", "Slope:", 0))
                   ),
                   
                   
                   conditionalPanel(
                     condition = "(input.family_generator == 'BCCG' ||
                                        input.family_generator == 'BCPE' ||
                                        input.family_generator == 'BCT') &&
                                        input.trend_nu == 'exponentially'",
                     div(style = "display:inline-block", numericInput("a_nu", "A:", 1)),
                     div(style = "display:inline-block", numericInput("b_nu", "B:", 0))
                   ),
                   
                   
                   conditionalPanel(condition = "input.family_generator == 'BCPE' ||
                                        input.family_generator == 'BCT'", hr()),
                   
                   ######################## Tau Simulation ##################################
                   conditionalPanel(
                     condition = "input.family_generator == 'BCPE' ||
                                        input.family_generator == 'BCT'",
                     selectInput(
                       "trend_tau",
                       "Trend for τ:",
                       c(Linear = "linear", Exponentially = "exponentially")
                     )
                   ),
                   
                   conditionalPanel(
                     condition = "(input.family_generator == 'BCPE' ||
                                        input.family_generator == 'BCT') &&
                                        input.trend_tau == 'linear'",
                     div(style = "display:inline-block", numericInput("intercept_tau", "Intercept:", 1)),
                     div(style = "display:inline-block", numericInput("slope_tau", "Slope:", 0))
                   ),
                   
                   conditionalPanel(
                     condition = "(input.family_generator == 'BCPE' ||
                                        input.family_generator == 'BCT') &&
                                        input.trend_tau == 'exponentially'",
                     div(style = "display:inline-block", numericInput("a_tau", "A:", 1)),
                     div(style = "display:inline-block", numericInput("b_tau", "B:", 0))
                   )
                 )
               )),
        column(
          width = 6,
          box(
            title =  tagList(shiny::icon("chart-line"), "Plot"),
            status = "warning",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("plot_generator", height = "550px")
          ),
          column(width = 6),
          box(
            title =  tagList(shiny::icon("table"), "Table"),
            status = "warning",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            DT::dataTableOutput("table_generator")
          )
        )
      )
    ),
    
    ### Percentile ###
    tabItem(
      tabName = "percentile",
      
      p(
        strong(
          "This Shiny App is a generator to create age-dependent data from labor analytes!"
        ),
        br(),
        br(),
        "New data can be generated from normally distributed data with specified 95% reference intervals.
                     The example dataset is from Zierk et.al. (2019): Next-generation reference intervals for pediatric
                     hematology. In blue is the upper limit and in red is the given lower limit of the reference intervals, the grey dots are the generated data points.
        The data can be used in the Shiny App",
        a("AdRI", href = "https://github.com/SandraKla/AdRI"),
        ". The data is saved with no sex, with unique values and the station is named Generator!"
      ),
      
      box(
        title = tagList(shiny::icon("gear"), "Settings"),
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        selectInput(
          "data",
          "Select preinstalled dataset:",
          choice = list.files(pattern = ".csv", recursive = TRUE)
        ),
        uiOutput("dataset_file"),
        actionButton('reset', 'Reset Input', icon = icon("trash")),
        hr(),
        
        numericInput(
          "n_percentile",
          "Number of observations:",
          1,
          min = 1,
          max = 100
        ),
        textInput("text_percentile", "Name the Analyte:", value = "Analyt"),
        textInput("text_unit_percentile", "Unit of the Analyte:", value = "Unit"),
        hr(),
        downloadButton("download_percentileplot", "Plot"),
        downloadButton("download_precentile", "Data")
      ),
      
      box(
        title = tagList(shiny::icon("chart-line"), "Plot"),
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("percentile", height = "500px")
      )
    )
  ))
)

####################################### Server ####################################################

server <- function(input, output){
  
  options(shiny.plot.res=128)
  options(shiny.sanitize.errors = TRUE)
  
  ##################################### Reactive Expressions ######################################
  
  values <- reactiveValues(
    upload_state = NULL
  )
  
  observeEvent(input$dataset_file1, {
    values$upload_state <- 'uploaded'
  })
  
  observeEvent(input$reset, {
    values$upload_state <- 'reset'
  })
  
  dataset_input <- reactive({
    if (is.null(values$upload_state)) {
      return(NULL)
    } else if (values$upload_state == 'uploaded') {
      return(input$dataset_file1)
    } else if (values$upload_state == 'reset') {
      return(NULL)
    }
  })
  
  output$dataset_file <- renderUI({
    input$reset ## Create a dependency with the reset button
    fileInput('dataset_file1', label = NULL)
  })
  
  data_generator <- reactive({
    # Composition of users settings
    if(input$trend_mu == "linear"){
      formula_mu <- paste("linear(i,",input$slope_mu,",",input$intercept_mu,")")}
    
    if(input$trend_mu == "exponentially"){
      formula_mu <- paste("expo(i,", input$a_mu,",",input$b_mu,")")}
    
    if(input$trend_sigma == "linear"){
      formula_sigma <- paste("linear(i,",input$slope_sigma,",",input$intercept_sigma,")")}
    
    if(input$trend_sigma == "exponentially"){
      formula_sigma <- paste("expo(i,", input$a_sigma,",",input$b_sigma,")")}
    
    if(input$trend_nu == "linear"){
      formula_nu <- paste("linear(i,",input$slope_nu,",",input$intercept_nu,")")}
    
    if(input$trend_nu == "exponentially"){
      formula_nu <- paste("expo(i,", input$a_nu,",",input$b_nu,")")}
    
    if(input$trend_tau == "linear"){
      formula_tau <- paste("linear(i,",input$slope_tau,",",input$intercept_tau,")")}
    
    if(input$trend_tau == "exponentially"){
      formula_tau <- paste("expo(i,", input$a_tau,",",input$b_tau,")")}
    
    progress <- shiny::Progress$new()
    progress$set(message = "Generate new data...", detail = "", value = 2)
    
    generate_data <- make_data(input$age_generator, input$age_generator_steps,input$family_generator, 
                               input$n_, input$text, formula_mu, formula_sigma,
                               formula_nu, formula_tau, input$ill_factor, input$mu_factor_ill)
    on.exit(progress$close())
    generate_data
  })
  
  ##################################### Output ####################################################
  ##################################### Data-Generator ############################################
  
  output$table_generator <- DT::renderDataTable({
    data_generator <- data_generator()
    colnames(data_generator) <- c("Age [years]","Age [days]", "Value", "Id", "Sex", "Origin", "Analyte")
    
    DT::datatable(data_generator, caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;',
      'Table: Dataset'), extensions = 'Buttons', options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')))
  })
  
  output$summary <- renderPrint({
    summary(data_generator())
  })
  
  output$plot_generator <- renderPlot({
    plot(data_generator()[,3] ~ data_generator()[,2], xlab = "Age [Days]", ylab =  paste0(data_generator()[1,7]," [",input$text_unit,"]"), 
         pch = 20, cex = 0.75, col = "grey")
  })
  
  output$settings <- DT::renderDataTable({
    data_settings <- t(data.frame("Age" = input$age_generator,
                                  "Age steps" = input$age_generator_steps,
                                  "Distribution" = input$family_generator,
                                  "Number of observations" = input$n_,
                                  "Name" = input$text,
                                  "Unit" = input$text_unit,
                                  "Trend of mu" = input$trend_mu,
                                  "Trend of sigma" = input$trend_sigma,
                                  "Trend of nu" = input$trend_nu,
                                  "Trend of tau" = input$trend_tau,
                                  "Linear (mu): Intercept of mu" = input$intercept_mu,
                                  "Linear (mu): Slope of mu" = input$slope_mu,
                                  "Exponentially (mu): A of mu" = input$a_mu, 
                                  "Exponentially (mu): B of mu" = input$b_mu,
                                  "Linear (sigma): Intercept of sigma" = input$intercept_sigma,
                                  "Linear (sigma): Slope of sigma" = input$slope_sigma,
                                  "Exponentially (sigma): A of sigma" = input$a_sigma, 
                                  "Exponentially (sigma): B of sigma" = input$b_sigma,
                                  "Linear (nu): Intercept of nu" = input$intercept_nu,
                                  "Linear (nu): Slope of nu" = input$slope_nu,
                                  "Exponentially (nu): A of nu" = input$a_nu, 
                                  "Exponentially (nu): B of nu" = input$b_nu,
                                  "Linear (tau): Intercept of tau" = input$intercept_tau,
                                  "Linear (tau): Slope of tau" = input$slope_tau,
                                  "Exponentially (tau): A of tau" = input$a_tau, 
                                  "Exponentially (tau): B of tau" = input$b_tau, 
                                  check.names = FALSE))
    colnames(data_settings) <- c("Setting")
    DT::datatable(data_settings, extensions = 'Buttons', caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Settings'),
                  options = list(dom = 'Blfrtip', pageLength = 30, buttons = c('copy', 'csv', 'pdf', 'print')))
  })
  
  ################################ Generator (Percentile) ##########################
  
  output$percentile <- renderPlot({
    progress <- shiny::Progress$new()
    progress$set(message = "Generate new data...", detail = "", value = 2)
    
    if(is.null(dataset_input())){
      percentile_function(input$data, input$n_percentile, input$text_percentile, input$text_unit_percentile)
    } else{
      percentile_function(dataset_input()[["datapath"]], input$n_percentile, input$text_percentile, input$text_unit_percentile)
    }
    on.exit(progress$close())
  }) 

  ################################ Download ########################################
  
  output$download_data <- downloadHandler(
    filename = function(){
      paste0("Generator_",input$family_generator,"_",input$age_generator,"_",input$age_generator_steps ,"_", Sys.Date(),".csv")
    },
    content = function(file) {
      write.csv2(data_generator(), file, row.names = FALSE)
  })

  output$download_settings <- downloadHandler(
    filename = function(){
      paste0("Generator_Settings_",input$family_generator,"_",input$age_generator ,"_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data_settings <- t(data.frame("Age" = input$age_generator,
                                    "Age steps" = input$age_generator_steps,
                                    "Distribution" = input$family_generator,
                                    "Number of observations" = input$n_,
                                    "Name" = input$text,
                                    "Unit" = input$text_unit,
                                    "Trend of mu" = input$trend_mu,
                                    "Trend of sigma" = input$trend_sigma,
                                    "Trend of nu" = input$trend_nu,
                                    "Trend of tau" = input$trend_tau,
                                    "Linear: Intercept of mu" = input$intercept_mu,
                                    "Linear: Slope of mu" = input$slope_mu,
                                    "Exponentially: A of mu" = input$a_mu, 
                                    "Exponentially: B of mu" = input$b_mu,
                                    "Linear: Intercept of sigma" = input$intercept_sigma,
                                    "Linear: Slope of sigma" = input$slope_sigma,
                                    "Exponentially: A of sigma" = input$a_sigma, 
                                    "Exponentially: B of sigma" = input$b_sigma,
                                    "Linear: Intercept of nu" = input$intercept_nu,
                                    "Linear: Slope of nu" = input$slope_nu,
                                    "Exponentially: A of nu" = input$a_nu, 
                                    "Exponentially: B of nu" = input$b_nu,
                                    "Linear: Intercept of tau" = input$intercept_tau,
                                    "Linear: Slope of tau" = input$slope_tau,
                                    "Exponentially: A of tau" = input$a_tau, 
                                    "Exponentially: B of tau" = input$b_tau,
                                    check.names = FALSE))
      colnames(data_settings) <- c("Setting")
      write.csv2(data_settings, file)
    })
  
  output$download_plot <- downloadHandler(
    filename = function(){
      paste0("Generator_",input$family_generator,"_",input$age_generator,".eps")
    },
    content = function(file) {
      setEPS()
      postscript(file)
      plot(data_generator()[,3]~data_generator()[,2], xlab = "Age [Days]", ylab =  paste0(data_generator()[1,7]," [",input$text_unit,"]"), 
           pch = 20, cex = 0.75, col = "lightgrey")
      dev.off()
    })
  
  output$download_precentile <- downloadHandler(
    filename = function(){
      paste0("Percentile_", Sys.Date(),".csv")
    },
    content = function(file) {
      progress <- shiny::Progress$new()
      progress$set(message = "Save new data...", detail = "", value = 2)
      
      if(is.null(dataset_input())){
        table_percentile <- percentile_function(input$data, input$n_percentile, input$text_percentile, input$text_unit_percentile)
      } else{
        table_percentile <- percentile_function(dataset_input()[["datapath"]], input$n_percentile, input$text_percentile, input$text_unit_percentile)
      }
      on.exit(progress$close())
      write.csv2(table_percentile, file, row.names = FALSE)
  })
  
  output$download_percentileplot <- downloadHandler(
    filename = function(){
      paste0("Percentile_", Sys.Date(), ".eps")
    },
    content = function(file) {
      setEPS()
      postscript(file)
      progress <- shiny::Progress$new()
      progress$set(message = "Save new data...", detail = "", value = 2)
      
      if(is.null(dataset_input())){
        percentile_function(input$data, input$n_percentile, input$text_percentile, input$text_unit_percentile)
      } else{
        percentile_function(dataset_input()[["datapath"]], input$n_percentile, input$text_percentile, input$text_unit_percentile)
      }
      on.exit(progress$close())
      dev.off()
    })
}
####################################### Run the application #######################################
shinyApp(ui = ui, server = server)