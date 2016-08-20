
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

function(input, output, session) {
    
    statuses <- reactive({
        # Change when the "update" button is pressed...
        input$update
        # ...but not for anything else
        isolate({
            withProgress({
                setProgress(message = "Accumulating tweets..")
                getTweets(input$searchString, input$numTweets, 
                          input$rt_remove, input$isUser)
            })
        })
    })
    
    textdata <- reactive({
        # Change when the "update" button is pressed...
        input$update
        # ...but not for anything else
        getTextData(statuses())
    })
    
    sentiments <- reactive({
        # Change when the "update" button is pressed...
        input$update
        # ...but not for anything else
        #isolate({
            withProgress({
                setProgress(message = "Gathering sentiments...")
                sentiments <- getSentiments(textdata())
            })
        #})
    })
    
    runpca <- reactive({
        # Change when the "update" button is pressed...
        #input$update
        # ...but not for anything else
        #isolate({
            withProgress({
                setProgress(message = "Running PCA...")
                doPCA(textdata(), statuses(), sentiments())
            })
        #})
    })
    
    # Make the wordcloud drawing predictable during a session
    wordcloud_rep <- repeatable(wordcloud)
    
    output$plot <- renderPlot({
        wordcloud_rep(textdata(), scale=c(4,0.5),
                      min.freq=3, max.words=100,
                      colors=brewer.pal(8, "RdBu"), random.order=F, 
                      rot.per=0.1, use.r.layout=F)
    })
    
    output$tweetCount  <- renderText({
        df <- statuses()
        if(input$isUser){
            paste("Number of Tweets Found: ", as.character(nrow(statuses())),
                  "\nUser: ",as.character(df$user[1]),
                  "\nDescription: ",as.character(getUser(df$user[1])$description)
                  )
        }else{
            paste("Number of Tweets Found: ", as.character(nrow(df)))
        }
        
    })
    
    output$sentiment <- renderPlot({
        v <- sentiments()
        emotions <- data.frame("count"=colSums(v[,c(1:8)]))
        emotions <- cbind("sentiment" = rownames(emotions), emotions)
        ggplot(data = emotions, aes(x = sentiment, y = count)) +
            geom_bar(aes(fill = sentiment), stat = "identity") +
            xlab("Sentiment") + ylab("Total Count") + 
            scale_fill_brewer(palette='RdBu') + 
            theme_bw() + theme(legend.position='none')
    })
    
    output$pcaplot <- renderRbokeh({
        df <- runpca()[1]$statuses

        # Ensure tweets and user name are encoded properly to work with Bokeh
        df <-
            df %>%
            mutate(text = iconv(text, from='ASCII', to='UTF-8', sub='byte'),
                   user = iconv(user, from='ASCII', to='UTF-8', sub='byte'))

        # Old plotting style, requires using renderPlot
#         ggplot(df, aes_string(x=input$xvar, y=input$yvar)) + 
#             geom_point(aes_string(fill=input$colvar), size=4, alpha=0.7, pch=21, stroke=1.3) + 
#             scale_fill_gradientn(colours = brewer.pal(10,"RdBu")) + theme_bw()
        
        figure(lod_threshold = 100, logo=NULL, 
               tools = c("pan", "wheel_zoom", "box_zoom", "resize", "reset", "save"), 
               legend_location = NULL) %>%
             ly_points(input$xvar, input$yvar, data=df, 
                       hover=list(user, text)) 
    })
    
    output$tweet_table <- DT::renderDataTable({
        df <- runpca()[1]$statuses
        text <- df$text
        pc <- df[,input$pc]
        
        cuts <- cut(pc, 10)
        temp <- data.frame(text=text, pc=pc, pc_val=cuts)
        temp <- temp %>%
            group_by(pc_val) %>%
            summarise(text=iconv(sample(text,1), from='ASCII', to='UTF-8', sub='byte')) %>%
            filter(!is.na(pc_val))
        
        DT::datatable(temp)
    })
    
    output$loadingFactors <- renderText({
        df <- runpca()[2]$pca
        
        # Additional PCA checks
        loads <- data.frame(df$rotation^2)
        temp <- data.frame('term'=rownames(loads))
        loads <- cbind(temp, loads)
        loads <- loads[,c("term",input$pc)]
        colnames(loads) <- c("term","PC")
        loads <-
            loads %>%
            arrange(desc(PC)) %>%
            select(term) %>%
            head(10)
        loads <- paste(loads$term, collapse=', ')
        
        paste("Most prominent terms: ", loads)
    })
    
    output$timeplot <- renderRbokeh({
        df <- runpca()[1]$statuses
        df <-
            df %>%
            mutate(time = as.POSIXct(time, format='%F %T')) %>%
            mutate(text = iconv(text, from='ASCII', to='UTF-8', sub='byte'),
                   user = iconv(user, from='ASCII', to='UTF-8', sub='byte')) %>%
            arrange(time)
        
#         ggplot(df, aes_string(x='time', y=input$yvar_time)) + 
#             geom_line(col='darkblue', alpha=0.8) +
#             scale_x_datetime(labels = date_format('%F %H:%M:%S')) +
#             labs(x='Date') + theme_bw() +
#             theme(axis.text.x = element_text(angle=90))
        
        figure(lod_threshold = 100, logo=NULL, 
               tools = c("pan", "wheel_zoom", "box_zoom", "resize", 
                         "reset", "save")) %>%
            ly_points(time, input$yvar_time, data=df, 
                      hover=list(user, text)) %>%
            x_axis(label='Date')
    })
}