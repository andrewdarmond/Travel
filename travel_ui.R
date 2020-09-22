#########################################################
# Initialization                                        #
#########################################################
# The aim is to create a shiny UI with the 4 questions 
library(shinyBS)
library(rintrojs)
library(shinydashboard)
library(shiny)
library(shinyWidgets)
questions <- c("1.	What do you like to do when you travel ?", 
               "2.	Mention some of your favorite places cities countries to visit ?",
               "3.	Where would you like to travel next ?",
               "4.	Can you remember the best trip you have ever had and why was it so good ?")
first <- questions[1]   #assigning the first question to an object called first 

#########################################################
#Header code                                            #
#########################################################
header <- dashboardHeader(title = "Andrew D'Armond", titleWidth = 200, 
    dropdownMenu(type="messages", messageItem(from="Larry", message = "Click here for awesome deals", href ="https://www.kayak.com/deals?og=ASFO&sort=popularity_b")),
    dropdownMenu(type="notifications", notificationItem(text="check out datacamp", href="http://www.datacamp.com")),
    dropdownMenu(type="tasks", taskItem(text="booking progress", value=20, href = "https://www.kayak.com/flights/SFO-DXB/2020-05-11?sort=bestflight_a"))
)
#########################################################
#Sidebar code                                           #
#########################################################
sidebar <- dashboardSidebar(
    sidebarMenu(menuItem("OVERVIEW", tabName = "overview", icon = icon("table")),
                menuItem("CHARTS", tabName = "chart", icon = icon("bar-chart-o")),
                menuItem("LDA MODEL", tabName = "LDA", icon = icon("bar-chart-o")),
                #menuItem("CORRELOGRAM", tabName = "corr",  icon = icon("retweet", lib = 'font-awesome')),
                #menuItem("INSIGHTS", tabName = "Business", icon = icon("lightbulb")),
                tags$head(tags$style(HTML(".skin-blue .main-sidebar {background-color:  grey;}"))) #color
    )
)
#########################################################
# Dashboard body code                                   #
#########################################################

b_intro1 <- infoBoxOutput("problem",width = 14)
b_intro2 <- infoBoxOutput("sur", width = 14)

# Select question, print number of respondet and questions
b_question <- box(title = "Travel Survey questions", status = "primary", 
                  width = 6, height = 125, solidHeader = TRUE, 
                  selectInput(inputId = "question", 
                              label = "Select a question:", choices = questions)
                 )
b_respondent <- valueBoxOutput("respondent", width = 4)
b_questions <- valueBoxOutput("questions", width = 4)
row_q <- fluidRow(b_question)
row1 <- fluidRow(b_respondent, b_questions)



b_choice_L <- box(title = "Latent Dirichlet Allocation (LDA)", status = "primary", 
                  width = 5, height = 120, solidHeader = TRUE,
                  fluidRow(
                      column(
                          width = 12,
                          introBox(
                              bsButton("beta", 
                                       label = "BETA", 
                                       icon = icon("spinner"),
                                       style = "danger"),
                              bsButton("gamma", 
                                       label = "GAMMA", 
                                       icon = icon("spinner", class = "spinner-box"), 
                                       style = "danger")
                          )
                      )
                  )
)

b_select_B_G <- box(title = "Travel Survey questions", status = "success", 
                    width = 10, height = 150, solidHeader = TRUE, 
                    selectInput(inputId = "Choice", 
                                label = "Select:", choices = questions))



b_frequencies <- box(title = "Most frequent words", width = 7,
                     status = "warning",  solidHeader = TRUE, collapsible = TRUE,
                     background = "black",
                     sliderInput("sldr", "Most Frequent Words", 5, 20, 10),
                     plotOutput("frequencies", height = 250)
                     ) 
b_sentiments <- box(title = "Sentiment analysis", width = 9,
                    status = "warning", solidHeader = TRUE, collapsible = TRUE,
                    background = "black",
                    plotOutput("sentiment", height = 400)
                    )
b_wordcloud <- box(title = "Sentiment Word Cloud", width = 10,
                   status = "warning", solidHeader = TRUE, collapsible = TRUE,
                   background = "black",
                   plotOutput("wordcloud", height = 500)
                   )

#b_corr <- box(title = "CORRELOGRAM", width = 4,
                   #status = "warning", solidHeader = TRUE, collapsible = TRUE,
                   #background = "black",
                   #plotOutput("corr", height = 250)
#)


#b_charts <- box(title = "charts", status = "success", 
                #width = 4, height = 125, solidHeader = TRUE, 
                #plotOutput("charts", height = 250),
                #selectInput(inputId = "charts", 
                            #label = "Select a chart:", choices = questions, 
                            #selected = first)
#)

#row2 <- fluidRow(b_charts)
row2 <- fluidRow(b_frequencies, b_sentiments, b_wordcloud) #b_corr
    
# Word netword and survey it-idf analysis

b_network <- box(title = "Words Network", width = 10,
                 status = "warning", solidHeader = TRUE, collapsible = TRUE,
                 background = "black",
                 plotOutput("network", height = 500)
                 )
b_tfidf <- box(title = "Travel Survey TF_IDF Analysis", width = 10,
               status = "warning", solidHeader = TRUE, collapsible = TRUE,
               background = "black",
               sliderInput('slider', 'Top Words', 5, 25, 10),
               plotOutput("tfidf", height = 500)
               )

b_beta <- box(title = "LDA Analysis",
              status = "warning", width = 12, solidHeader = TRUE, collapsible = TRUE,
              background = "black",
              plotOutput("topic", height = 500)
)

b_gamma <- box(title = "LDA Analysis (Gamma)",
               status = "warning", width = 12, solidHeader = TRUE, collapsible = TRUE,
               background = "black",
               plotOutput("gamma", height = 500))

des_row1 <- fluidRow(b_choice_L)
des_row2 <- fluidRow(b_beta)

row3 <- fluidRow(b_network, b_tfidf)

body <- dashboardBody(
    tabItems(tabItem(tabName = "overview", b_intro1,b_intro2,row1),
             tabItem(tabName = "chart", row_q, row2, row3),
             tabItem(tabName = "LDA", des_row1, des_row2)))   

#Main code
UI <- dashboardPage(header, sidebar, body, skin="yellow")
shinyApp(UI, server)
