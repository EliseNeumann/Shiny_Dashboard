library(shinydashboard)

ui <- dashboardPage(
    
    
    # customizing the look of the app
    skin = "purple",
    
    
    # defining title
    dashboardHeader(title = "Are you normal?"),
   
    
    # list of tabs in sidebar menu
    dashboardSidebar(
        sidebarMenu(
            
            # landing page
            menuItem("Study Overview",
                     tabName = "home"),
         
            # sentiment analysis
            menuItem("Sentiment Analysis",
                     tabName = "sentiment"),
            
            # tf_idf
            menuItem("Relative term frequency",
                     tabName = "tf_idf"),
            
            # pwcor
            menuItem("Pair-wise correlation",
                     tabName = "pw_cor_tab"),
            
            
            # ngrams
            menuItem("N-Grams",
                     tabName = "ngrams"),
            
            # naive Bayes
            menuItem("Prediction",
                     tabName = "my_prediction")
               
        )
    ),
    
    
    # filling body of the dashboard
    dashboardBody(
        
        tabItems(
            
            # home
            tabItem(tabName = "home",
                fluidRow(
                    
                    box(
                        width = 12,
                        title = "Context",
                        uiOutput("home_context")
                    ),
                    
                    box(
                        width = 6,
                        title = "Questions",
                        uiOutput("home_questions")
                    ),
                    
                    box(
                        width = 6,
                        title = "Limitations",
                        uiOutput("home_limitations")
                    )
                )
            ),
            
            
            
            # sentiment analysis
            tabItem(tabName = "sentiment",
                fluidRow(
                    box(
                        width = 6,
                        sliderInput("plumbus","Select Minimum Word Frequency",
                                    min = 0, max = 15, value = 3)
                    ),
                    box(
                        status="success",
                        width = 6,
                        title = "INSIGHTS",
                        uiOutput("fleeb_myList")
                    ),
                    box(
                        width = 12,
                        title = "Contribution to Sentiment",
                        plotly::plotlyOutput("grumbo_afinn") 
                    )
                )
            ),
            
            
            # tf_idf
            tabItem(tabName = "tf_idf",
                fluidRow(
                    
                    box(
                        width = 6,
                        selectInput("tf_idf_select","Select Question",selected = 1,choices = c(1,2,3,4,5)),
                        sliderInput("tf_slider","Select tf_idf Range",value= 0.005, min= 0.005, max = 0.08)
                    ),
                    
                    box(
                        status="success",
                        width = 6,
                        title = "INSIGHTS",
                        uiOutput("myList")
                    ),
                    
                    box(
                        width = 12,
                        title = "Proportionate Term Frequency for Survey Questions",
                        plotly::plotlyOutput("tf_graph"))
                    )
                    
                ),
            
            # pw_cor
            tabItem(tabName = "pw_cor_tab",
                fluidRow(
                    
                    box(
                        width = 6,
                        selectInput("pw_cor_select","Select word for analysis",
                                    selected = c("tiger","police"),
                                    choices = c("tiger", "police", "party", "drinking", "super", "panda"), 
                                    multiple = T)
                    ),
                    
                    box(
                        status="success",
                        width = 6,
                        title = "INSIGHTS",
                        uiOutput("pwcor_myList")
                    ),
                    
                    box(
                        width = 12,
                        title = "Pair-wise word correlation",
                        plotly::plotlyOutput("pwcor"))
                    )
                    
            ),
            
            # ngrams
            tabItem(tabName = "ngrams",
                fluidRow(
                    box(
                        width = 6,
                        sliderInput("ngram_n", "How many combinations to see:",
                                    min = 1, max = 10, value = 4),
                        selectInput("ngram_normal", "Which business outcome to focus on:",
                                    choices = c("yes", "no"), selected = c("yes", "no"), multiple = T)
                    ),
                    box(
                        status="success",
                        width = 6,
                        title = "INSIGHTS",
                        "The only bigrams re-occuring at least twice are key phrases used to formulate the questions. As such, this analysis does not help identify one group of customers from another"
                    ),
                    box(
                        width = 12,
                        title = "Most Frequent Bigrams",
                        tableOutput("ngram_table")
                    )
                    
                )
            ),
            
            
            
            
            #Naive Bayes
            tabItem(tabName = "my_prediction",
                    fluidRow(
                        box(
                            width = 6,
                            title = "Model Prediction",
                            tableOutput("model")
                        ),
                        box(
                            width = 6,
                            title = "Top Chi Scores",
                            numericInput("k_input", "Select number of rows to view", max=10, min=1, value=3),
                            tableOutput("top_k")
                            
                        ),
                        box(
                            status="success",
                            width = 12,
                            title = "INSIGHTS",
                            uiOutput("nb_myList")
                        )
                    )
            )
        )
    )
)


