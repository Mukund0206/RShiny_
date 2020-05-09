library(shiny)
library(data.table)
library(wordspace)
library(MASS)
library(knitr)
library(shinythemes)

Keyword_dt <- read.csv("Keyword_dataset.csv", stringsAsFactors = FALSE)
Vector.Obj <- dsm(target=Keyword_dt$KW1, feature=Keyword_dt$KW2, score= Keyword_dt$Frequency.of.Co.occurance,
                  raw.freq=TRUE, sort=TRUE)
VObj <- Vector.Obj 
VObj <- dsm.score(VObj, score="simple-ll", transform="log", normalize=TRUE, method="euclidean")
VObj300 <- dsm.projection(VObj, method="svd", n=300)

# title = "APP"
# position = "fixed-top" 

ui <- navbarPage(title=div(img(src="www/images/springer_logo.png", height = "20 px", width = "135 px"), ""), theme = shinytheme("sandstone"),
                 tabPanel("Find Similarity",
                        fluidRow(
                           column(width = 3, wellPanel(
                            selectInput('input_1', 'select Keyword', c(Choose='',Keyword_dt$KW1), selectize=TRUE)
                          )), 
                          
                          column(width = 6, plotOutput(outputId = "distPlot")), 
                          column(width = 3, div(dataTableOutput("TableResult"), style = "font-size:70%"))
                        ), 
                        fluidRow(
                          column(width = 3, wellPanel(
                            selectInput('input_pair_1', 'select Keyword from KW1', c(Choose='',Keyword_dt$KW1), selectize=TRUE),
                            selectInput('input_pair_2', 'select Keyword from KW2', c(Choose='',Keyword_dt$KW2), selectize=TRUE)
                          )), 
                          column(width= 5, verbatimTextOutput('pairResult'))
                        ) 
                    ), 
                   tabPanel('Documentation',   mainPanel(includeMarkdown("distributedSemanticModel.Rmd"))
                   ),
                 fluid = TRUE )


server <- function(input, output, sesion) {
  
  output$distPlot <- renderPlot({
    
    nn.mat <- nearest.neighbours(VObj300, input$input_1, n=10, dist.matrix=TRUE)
    # par(mar=c(1,1,1,1))
    plot(nn.mat, main= "Plotting cosine similarity")
    
  })
  
  output$TableResult <- renderDataTable({
    nn.mat <- nearest.neighbours(VObj300, input$input_1, n=10, dist.matrix=TRUE, convert = FALSE)
    data.table(keywords = rownames(nn.mat) , cosine = nn.mat[1,1:nrow(nn.mat)])
    }) 
 
  output$pairResult <- renderPrint({
    pair.distances(input$input_pair_1, input$input_pair_2, VObj300 ,method='cosine', convert = FALSE)  
  })
   
  
}

shinyApp(ui = ui, server = server)           
