####################################### Scripts ###################################################

source("generator.R")
source("percentile.R")

####################################### Libraries #################################################
# 
# if("DT" %in% rownames(installed.packages())){
#  library(DT)} else{
#    install.packages("DT")}
# 
# 
# if("shiny" %in% rownames(installed.packages())){
#  library(shiny)} else{
#    install.packages("shiny")}
# 
# if("gamlss" %in% rownames(installed.packages())){
#  library(gamlss)} else{
#    install.packages("gamlss")}

####################################### User Interface ############################################

ui <- fluidPage(
  
  theme = "style.css", 
  navbarPage("Generator for Age-dependent-Reference-Intervals (AdRI)",
             
  ##################################### Data-Generator ############################################
  
    tabPanel("Generator", icon = icon("calculator"),
             
      sidebarLayout(      
        sidebarPanel(width = 3,
                    
          sliderInput("age_generator", "Maximum age in years:", 0, 100, 18),
          sliderInput("age_generator_steps", "Age steps in days:", 1, 365, 100),
          sliderInput("ill_factor", "Select the pathological cases in %:", 0, 0.2, 0),
          numericInput("mu_factor_ill", "Factor added to mu for the pathological cases:", 1 , min = 0, max = 10000),
          
          selectInput("family_generator", "Distribution", choices = list("Normaldistribution" = "NO", 
                                                                         "Log-Normaldistribution" = "LOGNO",
                                                                         "Box-Cole and Green Distribution" = "BCCG",
                                                                         "Box-Cole Green Exp. Distribution" = "BCPE",
                                                                         "Box-Cole Green t-Distribution" = "BCT")),
          numericInput("n_", "Number of observations:", 100, min = 10, max = 10000),
          textInput("text", "Name the Analyte:", value = "Analyte"),
          textInput("text_unit", "Unit of the Analyte:", value = "Unit"),
          hr(), helpText("The linear function is: y = Slope*x + Intercept and the exponentially y = A*e^(x*B)."),
          
          ######################## Mu Simulation ###################################
          
          selectInput("trend_mu", "Trend for mu:", c(Linear = "linear", Exponentially = "exponentially")),
          conditionalPanel(condition = "input.trend_mu == 'linear'",  numericInput("intercept_mu", "Intercept:", 1),
                           numericInput("slope_mu", "Slope:", 0)),
          conditionalPanel(condition = "input.trend_mu == 'exponentially'",  numericInput("a_mu", "A:", 1),
                           numericInput("b_mu", "B:", 0)), 
          
          ######################## Sigma Simulation ################################
          
          selectInput("trend_sigma", "Trend for sigma", c(Linear = "linear", Exponentially = "exponentially")),
          conditionalPanel(condition = "input.trend_sigma == 'linear'",  numericInput("intercept_sigma", "Intercept:", 1),
                           numericInput("slope_sigma", "Slope:", 0)),
          conditionalPanel(condition = "input.trend_sigma == 'exponentially'",  numericInput("a_sigma", "A:", 1),
                           numericInput("b_sigma", "B:", 0)),
          
          ######################## Nu Simulation ###################################
          
          selectInput("trend_nu", "Trend for nu", c(Linear = "linear", Exponentially = "exponentially")),
          conditionalPanel(condition = "input.trend_nu == 'linear'",  numericInput("intercept_nu", "Intercept:", 1),
                           numericInput("slope_nu", "Slope:", 0)),
          conditionalPanel(condition = "input.trend_nu == 'exponentially'",  numericInput("a_nu", "A:", 1),
                           numericInput("b_nu", "B:", 0)),
          
          ######################## Tau Simulation ##################################
          
          selectInput("trend_tau", "Trend for Tau", c(Linear = "linear", Exponentially = "exponentially")),
          conditionalPanel(condition = "input.trend_tau == 'linear'",  numericInput("intercept_tau", "Intercept:", 1),
                           numericInput("slope_tau", "Slope:", 0)),
          conditionalPanel(condition = "input.trend_tau == 'exponentially'",  numericInput("a_tau", "A:", 1),
                           numericInput("b_tau", "B:", 0))),              
            
        mainPanel(width = 9,
          
          tabsetPanel(
            tabPanel("Home", icon = icon("chart-line"), 
                     p(strong("This Shiny App is a generator to create age-dependent data from labor analytes."), "Available are following distributions:
                     Normaldistribution (μ and σ), Lognormaldistribution (μ and σ), Box-Cox Cole and Green Distribution (μ, σ and ν),
                     Box-Cox t-Distribution (μ, σ, ν and τ) and Box-Cox Power Exponential Distribution (μ, σ, ν and τ). 
                     The parameters μ, σ, ν and τ are changing over the time with a linear or an exponentially function.
                     The linear function is: y = m*x + b and the exponentially y = a*e^(x*b). All negative values are deleted automatically 
                     and save the data in the form needed for Shiny App", strong("Age-dependent-Reference-Intervals"),"(AdRI). The data is saved
                     with no gender, unique values and the station is named Generator!"),
                     downloadButton("download_plot","Plot"), plotOutput("plot_generator", height = "500px")),
            
            tabPanel("Table", icon = icon("table"), downloadButton("download_data", "Data"),
                     DT::dataTableOutput("table_generator"), verbatimTextOutput("summary")),
            tabPanel("Settings", icon = icon("cogs"), downloadButton("download_settings", "Settings"), 
                     DT::dataTableOutput("settings")))
        )
      )
    ),
      
    ################################### Generator with given Percentiles ##########################
  
    tabPanel("Percentile", icon = icon("folder"),
      
      sidebarLayout( 
        sidebarPanel(width = 3,
          
          selectInput("data", "Select data with reference intervals:", choice = list.files(pattern = ".csv", recursive = TRUE)), hr(),
          numericInput("n_percentile", "Number of observations:", 1, min = 1, max = 1000),
          textInput("text_percentile", "Name of the Value:", value = "Analyt"),
          textInput("text_unit_percentile", "Unit of the Value:", value = "Unit")
        ),
               
        mainPanel(width = 9, 
          tabsetPanel(
            tabPanel("Home", icon = icon("home"),
                     p(strong("With given 95% Reference Intervals from normally distributed data new data can be generated!")," For this 
                     the data from the publication:", strong("Next-generation reference intervals for pediatric 
                     hematology [Zierk et.al. (2019)]"), "is used for Hemoglobindata (Loading the examples and the download takes a while!
                     The downloaded data contains 10 datapoints per day). 
                     Hemoglobin is an important iron-containing oxygen-transport protein in the erythrocytes and the changes of the value 
                     by newborn are important to prevent jaundice in babies and to prevent anemia. The data is smoothed with smooth.spline()."),
                    downloadButton("download_precentile", "Data"), plotOutput("percentile", height = "500px")),
            
            tabPanel("Example - Hemoglobin", icon = icon("venus"), 
                     downloadButton("download_hem_women", "Data"), plotOutput("hemoglobin_women", height = "800px")),
            tabPanel("Example - Hemoglobin", icon = icon("mars"), 
                     downloadButton("download_hem_men", "Data"), plotOutput("hemoglobin_men", height = "800px"))
          )
        )
      )
    )
  )
)

####################################### Server ####################################################

server <- function(input, output){
  
  options(shiny.sanitize.errors = TRUE)
  
  ##################################### Reactive Expressions ######################################
  
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
    
    generate_data <- make_data(input$age_generator, input$age_generator_steps,input$family_generator, 
                               input$n_, input$text, formula_mu, formula_sigma,
                               formula_nu, formula_tau, input$ill_factor, input$mu_factor_ill)
    generate_data
  })
  
  ##################################### Output ####################################################
  ##################################### Data-Generator ############################################
  
  output$table_generator <- DT::renderDataTable({
    DT::datatable(data_generator(), caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;',
      'Table: Dataset'))
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
    DT::datatable(data_settings, caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Settings'),
                  options = list(lengthMenu = c(6,30), pageLength = 30))
  })
  
  ################################ Generator (Percentile) ##########################
  
  output$percentile <- renderPlot({
    percentile_function(input$data, input$n_percentile, input$text_percentile, input$text_unit_percentile)
  }) 
  

  ################################ Data from Hemoglobin ############################
  
  output$hemoglobin_women <- renderPlot({
    
    data_hemoglobin_women <- read.csv2("data/Hemoglobin_Women.csv", header = TRUE, sep = ";", dec = ",", na.strings = "", 
                                       stringsAsFactors = FALSE)
    percentile_hemoglobin(data_hemoglobin_women, 10)
  })
  
  
  output$hemoglobin_men <- renderPlot({
    
    data_hemoglobin_men <- read.csv2("data/Hemoglobin_Men.csv", header = TRUE, sep = ";", dec = ",", na.strings = "", 
                                     stringsAsFactors = FALSE)
    percentile_hemoglobin(data_hemoglobin_men, 10)
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
      table_percentile <- percentile_function(input$data, input$n_percentile, input$text_percentile, input$text_unit_percentile)
      write.csv2(table_percentile, file, row.names = FALSE)
  })
  
  output$download_hem_women <- downloadHandler(
    filename = function(){
      paste0("Percentile_hemoglobin_women_", Sys.Date(),".csv")
    },
    content = function(file) {
      
      data_hemoglobin_women <- read.csv2("data/Hemoglobin_Women.csv", header = TRUE, sep = ";", dec = ",", na.strings = "", 
                                         stringsAsFactors = FALSE)
      table_save <- percentile_hemoglobin(data_hemoglobin_women, 10)
      
      rows_table_ <- nrow(table_save) 
      table_save <- table_save[table_save$value > 0,] 
      
      row.names(table_save) <- 1 : nrow(table_save)
      
      table_women <- data.frame(ALTER = table_save$age/365, ALTERTAG = table_save$age, ERGEBNIST1 = table_save$value)
      table_women["PATISTAMMNR"] <- seq(1, nrow(table_women))
      table_women["SEX"] <- "W"
      table_women["EINSCODE"] <- "Generator"
      table_women["CODE1"] <- "Hemoglobin"
      
      write.csv2(table_women, file, row.names = FALSE)
  })
  
  output$download_hem_men <- downloadHandler(
    filename = function(){
      paste0("Percentile_hemoglobin_men_", Sys.Date(),".csv")
    },
    content = function(file) {

      data_hemoglobin_men <- read.csv2("data/Hemoglobin_Men.csv", header = TRUE, sep = ";", dec = ",", na.strings = "", 
                                       stringsAsFactors = FALSE)
      table_save <- percentile_hemoglobin(data_hemoglobin_men, 10)
      
      rows_table_ <- nrow(table_save) 
      table_save <- table_save[table_save$value > 0,] 
      
      row.names(table_save) <- 1 : nrow(table_save)
      
      table_men <- data.frame(ALTER = table_save$age/365, ALTERTAG = table_save$age, ERGEBNIST1 = table_save$value)
      table_men["PATISTAMMNR"] <- seq(1, nrow(table_men))
      table_men["SEX"] <- "M"
      table_men["EINSCODE"] <- "Generator"
      table_men["CODE1"] <- "Hemoglobin"
      
      write.csv2(table_men, file, row.names = FALSE)
  })
}
####################################### Run the application #######################################
shinyApp(ui = ui, server = server)