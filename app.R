# shiny App TP


library(shiny)
library(ggplot2)
library(cluster)
library(factoextra)
library(dplyr)



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Shiny Web app by : EL MAJJODI Ayoub"),
   br(),  
    
   
   sidebarLayout(
     
     sidebarPanel(
       # Select data csv file    
       fileInput(
         inputId = "data",
         label = "Select csv data :",
         multiple = FALSE,
         accept = c("text/csv",".csv")
       ),
       tags$hr(),
       
       
       
       # Number of observation
       uiOutput("N_observation"),

       
       # select feature x and y
       uiOutput("x_axis"),   uiOutput("y_axis"),
       
       # Plot type
       uiOutput("plot_type"),
       
       
       
       # Algorithme 
       uiOutput("algo"),
       
       #cluster number
       uiOutput("nbr_cluster")
       
         
      ),
       
      
      # main Panel
      mainPanel(
        h2(textOutput("data_summary")),
        verbatimTextOutput("summary",placeholder = FALSE),
        
        h2(textOutput("some_observation")),
        tableOutput("observation"),
        
        h2(textOutput("data_plot")),
        plotOutput("distPlot"),
        
        h2(textOutput("Algorithm")),
        plotOutput("kmeansPlot"),
        
        h3(textOutput("PCA_result")),
        verbatimTextOutput("PCA_summary")
        
        
        
      )
     
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # Test file data is readed
  
  filedata <- reactive({
    my_file <- input$data
    if(is.null(my_file)){
      return(NULL)
    }
    temp <- read.csv(my_file$datapath)

  })

  # dataSummaty 
  output$summary <- renderPrint({
    df <- filedata()
  if(is.null(df)) {return(NULL)}
    summary(df)
  })
  
  # display Input to select observation

  output$N_observation <- renderUI({
    df <- filedata()
  
    if(is.null(df)){ return("")}
    
    numericInput(inputId = "N_obs",label = "Number of observation :", value = "4", min = 0, max = 6)
  })


  # Select x-axis and y-axis
  
  output$x_axis <- renderUI({
    df <- filedata()
    
    if(is.null(df)){ return(NULL)  }
    
    selectInput( inputId = "x_ax",label = "x var:" ,  choices = names(df))
    
  })
  
  output$y_axis <- renderUI({
    
    df <- filedata()
    if(is.null(df)){  return(NULL)}
    
    selectInput( inputId = "y_ax", label = "y var:", choices = names(df))
  })
  
  
  
  
  
  # Plot type
  
  output$plot_type <- renderUI({
    df <- filedata()
    
    if(is.null(df)) { return(NULL)}
    
    selectInput(inputId = "plotType", label= "Ploting type:", choices = c("ScatterPlot","Histogramme","BoxPlot"), selected = "Scatterplot")

        })
  
  
  
  
  
  # Kmeans or PCA
  
  output$algo <- renderUI({
    df <- filedata()
    if(is.null(df)) { return(NULL)}
    
    radioButtons(inputId = "selected_algo", label = "Analyes data :", choices = c("K-means","PCA"))
    
    
  })
  
  # Display observation table
  output$observation <- renderTable({
    df <- filedata()
    if(is.null(df)) {return(NULL)}
    
    head(df,input$N_obs)
    
  })

  # Ploting
  
  output$distPlot <- renderPlot({
    
    df <- filedata()
    if(is.null(df)) { return(NULL)}
    

    if(input$plotType == "Histogramme"){
      
      colo <- as.numeric(df[,input$x_ax])
      
      hist(colo, col="blue",  main="Colored histogram")

    }
  
    
    if(input$plotType == "BoxPlot"){

      
      boxplot(df[,c(input$x_ax)] ~ df[,c(input$y_ax)], xlab = input$x_ax , ylab = input$y_lab,
              border = "black")
      
    }
  

    
    if(input$plotType == "ScatterPlot"){
      ggplot(df, aes_string(x = input$x_ax , y = input$y_ax)) + geom_point()
    }
    
  })
  
  # get the number of cluster from the numeric input
  
  output$nbr_cluster <- renderUI({
    df <- filedata()
    
    if(is.null(df) ){return(NULL)}
    
    if( input$selected_algo == "PCA"){return(NULL)}
    
    numericInput(inputId = "cluster_number", label = "number of cluster :", value = "3")
    
    
    
  })
  
  # Kmeans function
  
  clusters <- reactive({
    df <- filedata()

    if(is.null(df) ){return(NULL)}

    
    val <- kmeans(df[,c(input$x_ax,input$y_ax)], input$cluster_number , iter.max = 10, nstart = 1,
           algorithm = c("Hartigan-Wong"), trace=FALSE)
    return(val)
  })
  
  # Plot kmeans result or PCA results
  
  output$kmeansPlot <- renderPlot({
    df <- filedata()
    if(is.null(df) ){return(NULL)}
    if( input$selected_algo == "K-means"){   # for kmeans result
      plot(df[,c(input$x_ax,input$y_ax)],
           col = clusters()$cluster,
           pch = 20, cex = 3)
      points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
      
    }else{     # for PCA result 
      
     
     
      #  extract onyl the numeric variable in given data
      val_num <- select_if(df,is.numeric)  
       
      #  compute PCA for data
      pca_algo <- prcomp(val_num , scale  = TRUE)
      pca_var <- pca_algo$sdev^2
      pca_var_per <- round(pca_var/sum(pca_var)*100, 1)
      
      # plot the standart variation 
      
       barplot(pca_var_per, 
               xlab="Principal Component", ylab="Percent Variation")
      
      
    }
    
  })

  # Plot summary about our PCA
  
  pca_static <- reactive({
    
    df <- df <- filedata()
    if(is.null(df) ){return(NULL)}
    
    #  extract onyl the numeric variable in given data
    val_num <- select_if(df,is.numeric)  
    
    #  compute PCA 
    pca_algo <- prcomp(val_num , scale  = TRUE)
    
    return(pca_algo)
    
    
    
  })
  
  #  Display the summary
  
  output$PCA_summary <- renderPrint({
    
    df <- filedata()
    if(is.null(df) ){return(NULL)}
    
    summary_pca <- pca_static()
    if(input$selected_algo == "PCA"){
    return(summary_pca)}
  })
  
  # The titles
  
  output$data_summary <- renderText({
    
    df <- filedata()
    if(is.null(df) ){return(NULL)}
   
    val <-"Statistics :"
    
    return(val)
  })
  
  output$some_observation <- renderText({
    
    df <- filedata()
    if(is.null(df) ){return(NULL)}
    
    val <- "Data :"
    
    return(val)
    
  })
  
  
  
  
  output$data_plot <- renderText({
    
    df <- filedata()
    if(is.null(df) ){return(NULL)}
    
    val <- "Plotting :"
    return(val)
    
    
  })
  
  output$Algorithm <- renderText({
    
    df <- filedata()
    if(is.null(df) ){return(NULL)}
    
    selectedAlgo <- 
    
    if(input$selected_algo == "K-means"){ 
      val <- "K-means:" 
      return(val)
   }
    else{ 
      val <- " PCA :" 
      return(val)
     }
    
  })
  
  
  output$PCA_result <- renderText({
    df <- filedata()
    if(is.null(df) ){return(NULL)}
    
    if(input$selected_algo == "PCA"){
      val <- "The PCA result"
      return(val)
    }
    
    
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)



p <- PCA(select_if(data(), is.numeric),graph = FALSE)
      var <- get_pca_var(p)
      
      return(corrplot(var$cos2, is.corr = FALSE))  
      
    }
    if(input$pp2 == "ch_corr")
    {
      return(chart.Correlation(select_if(data(), is.numeric), histogram=TRUE, pch=19))
    }
    
    if(input$pp2 == "pca_p")
    {
      pca <- PCA(select_if(data(), is.numeric), graph = FALSE)
      return(plot(pca, choix = "var"))
    }
  })