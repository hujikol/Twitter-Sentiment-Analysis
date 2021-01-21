if(interactive()){
  library(shiny)
  library(shinycustomloader)
  library(shinythemes)
  library(textclean)
  library(SnowballC)
  library(twitteR)
  library(tm)
  library(NLP)
  library(plyr)
  library(ggplot2)
  library(RColorBrewer)
  library(wordcloud)
  
  #library sentiment bayes
  source("D:/MATERI KULIAH/Semester V/Data Science/ProjectAkhir/create_matrix.R")
  source("D:/MATERI KULIAH/Semester V/Data Science/ProjectAkhir/classify_polarity.R")
  source("D:/MATERI KULIAH/Semester V/Data Science/ProjectAkhir/classify_emotion.R")
  
  source("D:/MATERI KULIAH/Semester V/Data Science/ProjectAkhir/film.R")
  
  wordlist <<- list("covid", "vaccine", "pandemic", "coronavirus")
  
  ### CREATING UI WITH SHINY ###
  
  ui<-navbarPage(theme=shinytheme("superhero"),
                 
                 tags$head(
                   tags$style(HTML("
                      @import url('//fonts.googleapis.com/css?family=Poppins:200,300,400,600,700,800');
                      "))
                 ),
                 
                 windowTitle="Analisis Sentiment COVID-19",fluid=TRUE,inverse=FALSE,
                 
                 tabPanel(strong("Home"),
                          headerPanel(
                            h1("Visualization", 
                               style = "font-family: 'Poppins', cursive;
                                        font-weight: 500; line-height: 1.1; 
                                        color: #FF5733;")),
                          sidebarPanel(width=3,
                                       checkboxGroupInput(inputId = "keyword",
                                                          label = "Choose the keyword :",
                                                          choices = wordlist,
                                                          selected = "covid"),
                                       hr(),
                                       sliderInput(inputId = "slider",
                                                   label = "Data count:",
                                                   min = 1,  max = 100,   value =  50),
                                       actionButton("update", "Change", class = "btn-primary")
                          ),
                          mainPanel(h4("Application Description"),
                                    splitLayout(
                                      style = "border: 1px solid silver;",
                                      cellWidths = 270,
                                      cellArgs = list(style = "padding: 6px"),
                                      plotOutput("plot1"),
                                      plotOutput("plot2")
                                    ),
                                    splitLayout(
                                      style = "border: 1px solid silver;",
                                      plotOutput("plot")))
                          #            h6("Menggunakan keyword data "covid, coronavirus, pandemic, vaccine". Dengan rentang waktu dari 1 Desember 2019 sampai 15 Januari 2021."),
                          #            plotOutput("plot"))
                 ),
                 tabPanel(strong("Tweet Data"),
                          mainPanel(tabsetPanel(id = 'dataset',
                                                tabPanel("Real Tweets", dataTableOutput("table_real")),
                                                tabPanel("Cleaned Tweets", dataTableOutput("table_cleaned"))
                          ))
                 ),
                 tabPanel(strong("Classification"),
                          mainPanel(h4("A table of the sentiment scores across four dictionaries"),withLoader(dataTableOutput("table"),loader="dnaspin")))
                 
  )
  
  server<-function(input,output,session){
    
    # Define a reactive expression for the document term matrix
    terms <- reactive({
      # Change when the "checkbox" button is pressed...
      input$update
      
      twData <- searchTwitter(input$keyword,  
                                  since='2019-12-01', until='2021-01-15', n=1000, lang="en") #mendefinisikan keyword
      no_retweets = strip_retweets(twData)
      twText <- sapply(no_retweets, function(x) x$getText()) 
      
      #cleaning data
      twText = sapply(no_retweets, function(x) x$getText())
      twText = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", twText)
      twText = gsub("@\\w+", "", twText)
      twText = gsub("[[:punct:]]", "", twText) 
      twText = gsub("[[:digit:]]", "", twText)
      twText = gsub("http\\w+", "", twText)
      twText = gsub("[ \t]{2,}", "", twText) 
      twText = gsub("^\\s+|\\s+$", "", twText)
      twText = gsub("note", "", twText)
      
      # define "tolower error handling" function
      try.error = function(x) {
        # create missing value
        y = NA
        # tryCatch error
        try_error = tryCatch(tolower(x), error=function(e) e)
        # if not an error
        if (!inherits(try_error, "error"))
          y = tolower(x)
        # result
        return(y)
      }
      # lower case using try.error with sapply 
      twText = sapply(twText, try.error)
      
      # remove NAs in tweetsText
      twText = twText[!is.na(twText)]
      names(twText) = NULL
      
      isolate({
        withProgress({
          setProgress(message = "Processing corpus...")
          #getTermMatrix(input$keyword)
          
          wc_file <- tweetsText
          
          corpus = Corpus(VectorSource(tweetsText))                          # convert tweets to corpus
          
          #some more cleaning
          corpus = tm_map(corpus, removeWords, stopwords("english"))   #remove stopwords like "and","the" and"that"
          corpus = tm_map(corpus, stripWhitespace)                     # remove whitespace
          
          # word frequency
          uniqwords = as.matrix(TermDocumentMatrix(corpus, control = list(minWordLength = 1)))            # convert corpus to Term Document matrix 
          sort(rowSums(uniqwords), decreasing = TRUE)
          
        })
      })
    })
    
    
    
    # Make the wordcloud drawing predictable during a session
    wordcloud_rep <- repeatable(wordcloud)
    
    output$plot <- renderPlot({
      v <- terms()
      wordcloud(names(v), v, scale=c(4,0.5),
                min.freq = input$slider, max.words=100,random.order=FALSE, rot.per=0.1,
                colors=brewer.pal(8, "Dark2"))
    })
    
    
    
    #plotting emotions
    #plotSentiments1(sent_df, "Analisis Sentiment COVID-19")
    output$plot1 <- renderPlot(plot(plotSentiments1(sent_df, "Analisis Sentiment COVID-19")))
    
    
    
    #plotting polarity
    #plotSentiments2(sent_df, "Analisis Polaritas COVID-19")
    output$plot2 <- renderPlot(plot(plotSentiments2(sent_df, "Analisis Polaritas COVID-19")))
    
    
    
    output$table_real <- renderDataTable(tweets)
    output$table_cleaned <- renderDataTable(cleanedtweets)
    output$table<-renderDataTable(sent_df)
  }
  
}

shinyApp(ui = ui, server = server)