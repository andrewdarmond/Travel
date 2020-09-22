#Load required libraries
library(textreadr)
library(tidyverse)
library(tidytext)
library(tidyr)
library(scales)
library(wordcloud)
library(igraph)
library(ggraph)
library(reshape2)
library(topicmodels)
library(quanteda)
library(tm)
library(ROCR)
library(naivebayes)
library(e1071)
library(klaR)
library(caret)
library(gmodels)
library(knitr)

#function for tokenizing
tokenizing <- function (question, custom=NULL){
    if(is.null(custom)) {custom <- head(stop_words,1)} #if custom is null select first record from stop_words
    # Tokenizing question
    frequency <- question %>%
        unnest_tokens(word, text) %>%   #tokenizing
        anti_join(stop_words) %>%       #remove common tokens
        anti_join(custom) %>%           #remove custom tokens
        count(word, sort=TRUE) %>%      #count frequency of words
        ungroup()
    #return dataframe with frequencies
    return (frequency)
}

#Function for getting bigrams
bi_grams <- function (data, custom=NULL) {
    if(is.null(custom)) {custom <- head(stop_words,1)} #if custom is null select first record from stop_words
    tokens <- data %>%
        unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word) %>%
        filter(!word1 %in% custom$word) %>%
        filter(!word2 %in% custom$word) %>%
        count(word1, word2, sort = TRUE) %>%
        arrange(-n)
    return (tokens)
}

#function to plot frequencies
plot_freq <- function (data, m, top=NULL, color='navy') { # "m" is a created parameter 
    if (is.null(top)){top<-nrow(frequencies)} #evaluate arg top
freq <- data %>%
        unnest_tokens(word, text) %>%   #tokenizing
        anti_join(stop_words) %>%       #remove common tokens
        anti_join(custom) %>%          #remove custom tokens
        count(word, sort=TRUE) %>%      #count frequency of words
        ungroup()    
plot <- freq %>%  
        arrange(desc(n),word) %>%
        #top_n(m) %>%
        #head(top) %>%
        head(m) %>%   #to increase the number of frequent words 
        ggplot(aes(reorder(word,-n), n))+
        geom_bar(stat='identity', fill=color)+
        xlab(NULL)+
        coord_flip()+
        theme_light()
return(plot)
}


#function to analyze sentiments
sentiments_lex <- function (survey) {
    #tokenizing
    question <- survey %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        count(word, sort=T) %>%
        ungroup()
    
    nrc   <- get_sentiments ("nrc") #%>% filter(sentiment %in% c("positive", "negative"))
    bing  <- get_sentiments ("bing")
    afinn <- get_sentiments ("afinn")
    
    #create a index
    question <- question %>%
        mutate(linenumber = row_number())
    
    #create a dataframe with affin lexicon
    q_affin <- question %>%
        inner_join(afinn) %>%
        group_by(index=linenumber) %>%
        summarise(sentiment=sum(value)) %>%
        mutate(method = "AFINN")
    
    #create a dataframe with nrc and bing lexicon
    q_nrcbing <- bind_rows (
        question %>%
            inner_join(nrc) %>% 
            mutate(method = "NRC"),
        question %>%
            inner_join(bing) %>% 
            mutate(method = "BING")) %>%
        count(method, index=linenumber, sentiment) %>%
        spread(sentiment, n, fill=0) %>%
        mutate(sentiment = positive-negative)
    #Plot sentiments based in each lexicon
    plot <- bind_rows (q_nrcbing, q_affin) %>%
        ggplot(aes(index, sentiment, fill=method))+
        geom_col(show.legend=FALSE)+
        facet_wrap(~method, ncol =1, scales= "free_y")
    return(plot)
}

#function to analyze sentiments (positives and negatives)
sentiments <- function (survey, top=NULL) {
    if (is.null(top)) {top=nrow(survey)}
    sentiment <- survey %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments("nrc")) %>%
        count(word, sentiment, sort=T) %>%
        ungroup()
    
    plot <- sentiment %>%
        group_by(sentiment) %>%
        top_n(5) %>%
        ungroup() %>%
        mutate(word=reorder(word, n)) %>%
        ggplot(aes(word, n, fill=sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y")+
        labs(y="Contribution to sentiment", x=NULL)+
        coord_flip()
    return(plot)
}  



#function to analyze sentiments
wordcloud <- function (survey, top=NULL) {
    if (is.null(top)) {top=nrow(survey)} 
    plot <- survey %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments("nrc")) %>%
        count(word, sentiment, sort=T) %>%
        acast(word ~sentiment, value.var="n", fill=0) %>%
        comparison.cloud(colors = c("red", "blue"),
                         max.words=50, scale = c(0.9,0.9), #try NRC and change up the scales
                         title.size=2,
                         title.colors = c("red", "Navy"))
return(plot)
}  

#Function for TF-IDF analysis
# other words arent really showing 

tf_idf <- function(survey,m, top=NULL) {
    if (is_null(top)) {top=nrow(survey)}
    plot<-  survey %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        count(question, word, sort = TRUE) %>%
        bind_tf_idf(word, question, n) %>%
        group_by(question) %>%
        top_n(10) %>%
        mutate(rank = row_number()) %>%
        filter (rank <= m) %>%   #to increase the number of words in the tf_idf charts
        ggplot(aes(reorder(word, tf_idf) ,tf_idf, fill=question))+
        geom_col(show.legend=FALSE)+
        labs(x=NULL, y="tf-idf", title='TF-IDF Analysis')+
        facet_wrap(~question, ncol=2, scales="free")+
        coord_flip()
return(plot)
}


#function to plot a word network
network <- function(data, lower=1) {
    set.seed(1234)
    data_graph <- data %>%
        filter(n >= lower) %>%
        graph_from_data_frame()
    #Show words network
    plot <- ggraph(data_graph, layout = "fr") +  
        geom_edge_link()+
        geom_node_point(size = 3, shape = 21, stroke = 1,
                        fill = 'green', color = 'black') +
        geom_node_text(aes(label=name), vjust=1.5, hjust=1.5) +
        theme(legend.position="none")
return(plot)
}

#topic beta analysis
top_beta <- function (survey_dtm, top=10) { 
    survey_lda <- LDA(survey_dtm,k=2, control = list(seed=123)) #Get topics    
    survey_topics <- tidy(survey_lda, matrix="beta") # Running LDA per token - beta matrix
    
    # Looking at the probabilities of the top terms
    plot <- survey_topics %>%
        group_by(topic) %>%
        top_n(top, beta) %>%
        ungroup() %>%
        # Lets plot the term frequencies by topic
        mutate(term=reorder(term, beta)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend=FALSE) +
        facet_wrap(~topic, scales = "free") +
        coord_flip()
    return(plot)
}

#topic gamma analysis
top_gamma <- function (survey_dtm) {
    survey_lda <- LDA(survey_dtm,k=2, control = list(seed=123)) #Get topics
    plot <- tidy(survey_lda, matrix="gamma") %>%
        arrange(document, desc(gamma)) %>%
        ggplot(aes(document, gamma, fill = factor(topic))) +
        geom_col(show.legend=FALSE) +
        facet_wrap(~topic, scales = "free") +
        labs (x="questions") +
        coord_flip()
    return(plot)
}



get_df <- function(survey, number) {
    if (number != "9") {
        survey <- survey %>%
        filter (str_detect(question, number))
    }
    print(c("rows:",nrow(survey)))
    return(survey)
}



############################################################
# Initialization                                           #
############################################################

# Loading the .txt file into R
survey_ans <- read_document(file="SurveyAnwers_160_Clean[4751].docx")

#Define parameters and create a empty dataframe
rows <- 40 #how many observations to you have - how many people you have
cols <- 4 #how many variables do you have - how many answers per person
my_df <- as.data.frame(matrix(nrow=rows, ncol=cols))

# Creating a nested for loop to fill in dataframe with corresponding line item
for(z in 1:cols){
    for(i in 1:rows){
        my_df[i,z]<- survey_ans[i*cols+z-cols]
    }#closing z loop
}#closing i loop

#Custom stop_words
custom <- data_frame(word=c("that's", "i'm", "lot", "it's", "favorite", "i'd",
                             "world", "i've", "time", "trip", "remember", "visit",
                             "cities"), lexicon='custom')
#custom2 <- data_frame(word=c('shoes', 'shoe'), lexicon=c('cust', 'cust'))

#Create a dataframe for each question
q1 <- data_frame(text=my_df$V1)
q2 <- data_frame(text=my_df$V2)
q3 <- data_frame(text=my_df$V3)
q4 <- data_frame(text=my_df$V4)
questions <- c("Question1", "Question2", "Question3", 
               "Question4")
colnames(my_df) <- questions
survey <- bind_rows(mutate(q1, question = questions[1]),
                    mutate(q2, question = questions[2]),
                    mutate(q3, question = questions[3]),
                    mutate(q4, question = questions[4]))

#DTM Survey
survey_dtm <- survey %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    anti_join(custom) %>%
    count(question, word, sort=TRUE) %>%
    cast_dtm(question, word, n)      


#######################################
server <- function(input, output) {
    df <- reactive({ get_df(survey, substr(input$question,1,1)) })
    number <- reactive({substr(input$question,1,1)})
    #Number of respondents
    output$respondent <-  renderValueBox({
        valueBox(formatC(40, format="d", big.mark=',')
                 ,'Number of respondents'
                 ,icon = icon("user",lib='glyphicon')
                 ,color = "light-blue"
                ) }) 
    
    output$questions <-  renderValueBox({
        valueBox(formatC(4, format="d", big.mark=',')
                 ,'Number of questions'
                 ,icon = icon("menu-hamburger",lib='glyphicon')
                 ,color = "light-blue"
        ) })
    
    
    output$problem <-  renderInfoBox({
        infoBox(title=NULL, 
                value = tags$p("Where do Hult students enjoy travelling to the most?"
                               , style = "font-size: 150%;"), 
                subtitle = NULL,
                icon = shiny::icon("question"), color = "orange", 
                href = NULL, fill = FALSE)
    })
       
    output$sur <-  renderInfoBox({
        infoBox(title=NULL, 
                value = tags$p("To find out we asked a total of..."
                               , style = "font-size: 150%;"), 
                subtitle = NULL,
                icon = shiny::icon("lightbulb"), color = "orange", 
                href = NULL, fill = FALSE)
    })
    
   
    
    #plot word frequencies
    output$frequencies <- renderPlot({ plot_freq(df(),input$sldr, top=10,color='blue') })
    
    #plot positive and negative sentiment
    output$sentiment <- renderPlot({ sentiments(df()) }) #bing sentiment is used switch to nrc
    
    #plot word network 
    output$network <-   renderPlot({ network(bi_grams(df())) })
    
    #plot word cloud
    output$wordcloud <- renderPlot({ wordcloud(df()) })
    
    #plot word cloud
    output$tfidf <- renderPlot({ tf_idf(survey,input$slider, top = 10) })
    
    #plot beta analysis
    observeEvent(input$beta, {
        output$topic <- renderPlot({ top_beta(survey_dtm) })
    })
    
    observeEvent(input$gamma, {
        output$topic <- renderPlot({ top_gamma(survey_dtm) })
    }
    )
  
                   
                   
}
