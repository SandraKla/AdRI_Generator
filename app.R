################################## Scripts #########################################

source("generator.R")
source("percentile.R")

################################## Libraries #######################################

if("DT" %in% rownames(installed.packages())){
 library(DT)} else{
   install.packages("DT")}


if("shiny" %in% rownames(installed.packages())){
 library(shiny)} else{
   install.packages("shiny")}

################################## User Interface ##################################

ui <- fluidPage(
  
  theme = "style.css", 
  navbarPage("Generator for Age-dependent Analyte Data",
             
  ################################ Data-Generator ##################################
  
    tabPanel("Generator", icon = icon("calculator"),
             
      sidebarLayout(      
        sidebarPanel(width = 4,
                    
          sliderInput("age_generator", "Maximum age in years:", 0, 123, 18),
          sliderInput("age_generator_steps", "Age steps in days:", 1, 365, 100),
          sliderInput("ill_factor", "Select the pathological cases in %:", 0, 0.2, 0),
          numericInput("mu_factor_ill", "Factor added to mu for sick patients:", 1 , min = 0, max = 1000),
          
          selectInput("family_generator", "Distribution", choices = list("Normal-Distribution" = "NO", 
                                                                         "Log-Normal-Distribution" = "LOGNO",
                                                                         "Box-Cole and Green Distribution" = "BCCG", 
                                                                         "Box-Cole Green Distribution orig." = "BCCGo",
                                                                         "Box-Cole Green Exp. Distribution" = "BCPE",
                                                                         "Box-Cole Green Exp. Distribution orig." = "BCPEo",
                                                                         "Box-Cole Green t-Distribution" = "BCT", 
                                                                         "Box-Cole Green t-Distribution orig." = "BCTo")),
          numericInput("n_", "Number of observations:", 100, min = 10, max = 10000),
          textInput("text", "Name the Analyte:", value = "Analyte"),
          textInput("text_unit", "Unit of the Analyte:", value = "Unit"),
          hr(),
          
          ######################## Mu Simulation ###################################
          
          selectInput("trend_mu", "Trend for mu:", c(Linear = "linear", Exponentially = "exponentially")),
          conditionalPanel(condition = "input.trend_mu == 'linear'",  numericInput("intercept_mu", "Intercept:", 0),
                           numericInput("slope_mu", "Slope:", 0)),
          conditionalPanel(condition = "input.trend_mu == 'exponentially'",  numericInput("a_mu", "A:", 0),
                           numericInput("b_mu", "B:", 0)), hr(),
          
          ######################## Sigma Simulation ################################
          
          selectInput("trend_sigma", "Trend for sigma", c(Linear = "linear", Exponentially = "exponentially")),
          conditionalPanel(condition = "input.trend_sigma == 'linear'",  numericInput("intercept_sigma", "Intercept:", 1),
                           numericInput("slope_sigma", "Slope:", 0)),
          conditionalPanel(condition = "input.trend_sigma == 'exponentially'",  numericInput("a_sigma", "A:", 1),
                           numericInput("b_sigma", "B:", 0)), hr(),
          
          ######################## Nu Simulation ###################################
          
          selectInput("trend_nu", "Trend for nu", c(Linear = "linear", Exponentially = "exponentially")),
          conditionalPanel(condition = "input.trend_nu == 'linear'",  numericInput("intercept_nu", "Intercept:", 0),
                           numericInput("slope_nu", "Slope:", 0)),
          conditionalPanel(condition = "input.trend_nu == 'exponentially'",  numericInput("a_nu", "A:", 0),
                           numericInput("b_nu", "B:", 0)), hr(),
          
          ######################## Tau Simulation ##################################
          
          selectInput("trend_tau", "Trend for Tau", c(Linear = "linear", Exponentially = "exponentially")),
          conditionalPanel(condition = "input.trend_tau == 'linear'",  numericInput("intercept_tau", "Intercept:", 0),
                           numericInput("slope_tau", "Slope:", 0)),
          conditionalPanel(condition = "input.trend_tau == 'exponentially'",  numericInput("a_tau", "A:", 0),
                           numericInput("b_tau", "B:", 0))),              
            
        mainPanel(width = 8,
          
          tabsetPanel(
            tabPanel("Home", icon = icon("chart-line"), 
                     p("Generator for age-dependent data. Negative values are deleted automatically and save the data in 
                     the form needed for Shiny App", strong("Age-dependent-Reference-Intervals"),". The data is saved
                     with no gender, unique and the station is Generator!"), br(), 
                     downloadButton("download_plot","Plot"), plotOutput("plot_generator", height = "500px")),
            
            tabPanel("Table", icon = icon("table"), downloadButton("download_data", "Data"), verbatimTextOutput("summary"),
                     DT::dataTableOutput("table_generator")),
            tabPanel("Settings", icon = icon("cogs"), downloadButton("download_settings", "Settings"), 
                     DT::dataTableOutput("settings")))
        )
      )
    ),
      
    ############################## Generator with given Percentiles ################
  
    tabPanel("Percentile", icon = icon("folder"),
      
      sidebarLayout( 
        sidebarPanel(width = 2,
          
          selectInput("data", "Select data with reference intervals:", choice = list.files(pattern = ".csv", recursive = TRUE)), hr(),
          numericInput("n_percentile", "Number of observations:", 10, min = 10, max = 1000),
          textInput("text_percentile", "Name of the Value:", value = "Analyt"),
          textInput("text_unit_percentile", "Unit of the Value:", value = "Unit")
        ),
               
        mainPanel(width = 10, 
          tabsetPanel(
            tabPanel("Home", icon = icon("home"),
                     p("Generate normally distributed data with given reference intervals, examples are from Hemoglobin (0-18 Years)
                       from the publication Next-generation reference intervals for pediatric 
                       hematology [Zierk et.al. (2019)]. Hemoglobin is an important 
                       iron-containing oxygen-transport protein in the erythrocytes and the changes of the value by newborn are 
                       important to prevent jaundice in babies and to prevent anemia, this important reference intervals for babys and 
                       children can be modelled in the Shiny App",strong("Age-dependent-Reference-Intervals"),". The data is smoothed 
                       with the function smooth.spline()."), br(),
                    downloadButton("download_precentile", "Data"), plotOutput("percentile", height = "500px")),
            
            tabPanel("Hemoglobin", icon = icon("venus"), 
                     downloadButton("download_hem_women", "Data"), plotOutput("hemoglobin_women", height = "1000px")),
            tabPanel("Hemoglobin", icon = icon("mars"), 
                     downloadButton("download_hem_men", "Data"), plotOutput("hemoglobin_men", height = "1000px"))
          )
        )
      )
    ),
      
    tabPanel("About", icon = icon("info"), includeHTML("www/about.html"))
  )
)

################################## Server ##########################################

server <- function(input, output){
  
  options(shiny.sanitize.errors = TRUE)
  
  ################################ Reactive Expressions ############################
  
  data_generator <- reactive({

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
  
  ################################ Output ##########################################
  ################################ Data-Generator ##################################
  
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
                                   "Intercept of mu" = input$intercept_mu,
                                   "Slope of mu" = input$slope_mu,
                                   "A of mu" = input$a_mu, 
                                   "B of mu" = input$b_mu,
                                   "Intercept of sigma" = input$intercept_sigma,
                                   "Slope of sigma" = input$slope_sigma,
                                   "A of sigma" = input$a_sigma, 
                                   "B of sigma" = input$b_sigma,
                                   "Intercept of nu" = input$intercept_nu,
                                   "Slope of nu" = input$slope_nu,
                                   "A of nu" = input$a_nu, 
                                   "B of nu" = input$b_nu,
                                   "Intercept of tau" = input$intercept_tau,
                                   "Slope of tau" = input$slope_tau,
                                   "A of tau" = input$a_tau, 
                                   "B of tau" = input$b_tau, 
                                   check.names = FALSE))
    colnames(data_settings) <- c("Setting")
    DT::datatable(data_settings, caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;',
                                                                   'Table: Settings'),
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
    percentile_hemoglobin(data_hemoglobin_women, input$n_percentile)
  })
  
  
  output$hemoglobin_men <- renderPlot({
    
    data_hemoglobin_men <- read.csv2("data/Hemoglobin_Men.csv", header = TRUE, sep = ";", dec = ",", na.strings = "", 
                                     stringsAsFactors = FALSE)
    percentile_hemoglobin(data_hemoglobin_men, input$n_percentile)
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
                                     "Intercept of mu" = input$intercept_mu,
                                     "Slope of mu" = input$slope_mu,
                                     "A of mu" = input$a_mu, 
                                     "B of mu" = input$b_mu,
                                     "Intercept of sigma" = input$intercept_sigma,
                                     "Slope of sigma" = input$slope_sigma,
                                     "A of sigma" = input$a_sigma, 
                                     "B of sigma" = input$b_sigma,
                                     "Intercept of nu" = input$intercept_nu,
                                     "Slope of nu" = input$slope_nu,
                                     "A of nu" = input$a_nu, 
                                     "B of nu" = input$b_nu,
                                     "Intercept of tau" = input$intercept_tau,
                                     "Slope of tau" = input$slope_tau,
                                     "A of tau" = input$a_tau, 
                                     "B of tau" = input$b_tau, 
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
      write.csv2(table_percentile, file, row.names = FALSE)
  })
  
  output$download_hem_women <- downloadHandler(
    filename = function(){
      paste0("Percentile_hemoglobin_women_", Sys.Date(),".csv")
    },
    content = function(file) {
      
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
################################## Run the application #############################
shinyApp(ui = ui, server = server)