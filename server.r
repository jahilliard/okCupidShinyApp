library(shiny)
library(ggplot2)
library(tm)
library(wordcloud)
library(gridExtra)
library(ggExtra)
setwd("/Users/justinhilliard/Documents/16Spring/36315/BartendersShinyApp/")

profiles <- read.csv("profiles.csv", 
                     header=TRUE)
drinkers <- profiles[which(profiles$drinks== c("very often", "often")),]
drinkersIncome <- drinkers[which(drinkers$income != -1),]
males <- drinkers[which(drinkers$sex == "m"),]
females <- drinkers[which(drinkers$sex == "f"),]
profiles$drinker <- ifelse(profiles$drinks == "often" | profiles$drinks == "very often", TRUE, FALSE)
profiles$drinker <- as.factor(profiles$drinker)
sentiment_df <- read.csv("sentimentdf.csv")
hate_love_df <- read.csv("hatelovedf.csv")

indices1 <- which(profiles$drinks=="rarely")
indices2 <- which(profiles$drinks=="socially")
indices3 <- which(profiles$drinks=="not at all")
indicesFinal <- c(indices1,indices2,indices3)
nonDrinkers <- profiles[indicesFinal,]
ggovindt_315_theme <- theme(
  panel.background = element_rect(fill = "white"),
  axis.title.x = element_text(family = "Garamond", size = 14, color = "black"),
  axis.title.y = element_text(family = "Garamond", size = 14, color = "black"),
  plot.title = element_text(family = "Garamond", size = 18, color = "black"),
  legend.text = element_text(family = "Garamond", size = 10, color = "black"),
  legend.position = "right",
  legend.title = element_text(family = "Garamond", size = 12, color = "black"),
  axis.ticks = element_line(color = "black"),
  axis.text = element_text(family = "Garamond", size = 10, color = "black"),
  panel.border = element_blank()
)

# Shiny Server Implementation
shinyServer(function(input, output) {
  
    output$genderIncome <- renderPlot({    
      
      bar <- ggplot(data = drinkersIncome, aes(x = as.factor(income), 
                                               fill=sex)) + 
        geom_bar() + 
      labs(x = "Income (USD)") + ggtitle("Income of Drinkers") + 
        ggovindt_315_theme + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_fill_discrete(name = "Type")
      if (input$side) {
        bar <- ggplot(data = drinkersIncome, aes(x = as.factor(income), 
                                                 fill=sex)) + 
        geom_bar(position="dodge") + 
        labs(x = "Income (USD)") + ggtitle("Income of Drinkers") + 
          ggovindt_315_theme + 
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
          scale_fill_discrete(name = "Type")
      }
      bar 
    })
    
    output$ageSex <- renderPlot({
      ageSex <- ggplot(data = drinkers, aes(x=age)) + 
      geom_histogram(binwidth=1, aes(y=..density.., fill=sex)) + 
        labs(x="Age of Drinkers") +
      ggtitle("Age of Drinkers by Sex") + ggovindt_315_theme + 
        scale_fill_discrete(name = "Gender")
      if (input$densities) {
        ageSex <- ageSex+
          geom_density(data=males, size=1.5, alpha=0.4, colour="blue")+
          geom_density(data=females, size=1.5, alpha=0.4, colour="red")
      }
      ageSex
    })

    
    output$genderUni <- renderPlot({    
      plot_sample <- ggplot(data = drinkers , aes(x= body_type))
      if (input$genderBreakdown) {
        genderPlot <- plot_sample + geom_bar() + coord_cartesian() +
          aes(fill = as.factor(sex)) +
          ggtitle("Body Types of Heavy Drinkers, by Gender")  + 
          xlab("Body Type") + ylab("Frequency") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
          ggovindt_315_theme + 
          scale_fill_discrete(name = "Gender")
      }
      else{
        genderPlot <-  plot_sample + geom_bar(aes(fill = "purple")) + coord_cartesian() +
          ggtitle("Body Types of Heavy Drinkers")  + 
          xlab("Body Type") + ylab("Frequency") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
          scale_fill_discrete(guide = FALSE) + ggovindt_315_theme
      }
      genderPlot
    })
    
    output$drugsHeavy <- renderPlot({
      plot_sample <- ggplot(data = drinkers , aes(x=drugs))
      drugsHeavyPlot <- plot_sample+geom_bar(fill="maroon")+coord_cartesian()+ 
        ggtitle("Drug Use of Heavy Drinkers") + 
        xlab("Drug Use") + ylab("Frequency") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        ggovindt_315_theme
      drugsNonHeavyPlot <- plot_sample + geom_bar(fill="blue") + 
        coord_cartesian()   + 
        ggtitle("Drug Use of Non-Heavy Drinkers") + 
        xlab("Drug Use") + ylab("Frequency") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        ggovindt_315_theme
      grid.arrange(drugsHeavyPlot, drugsNonHeavyPlot, ncol = 2)
    })
    
    output$text_mining_drinkers <- renderPlot({
      essaystr <- input$essay
      essay_plain <- tm_map(Corpus(VectorSource(drinkers[essaystr])), PlainTextDocument)
      #  Remove punctuation
      essay_plain <- tm_map(essay_plain, removePunctuation)
      #  Remove stop words
      essay_plain <- tm_map(essay_plain, removeWords, stopwords('english'))
      #  Stem the words in each document
      essay_plain <- tm_map(essay_plain, stemDocument)
      wordcloud(essay_plain, max.words = 100, random.order = FALSE)
    })
    
    output$text_mining_all <- renderPlot({
      essaystr_all <- input$essay
      essay_all_plain <- tm_map(Corpus(VectorSource(profiles[essaystr_all])), 
                                PlainTextDocument)
      #  Remove punctuation
      essay_all_plain <- tm_map(essay_all_plain, removePunctuation)
      #  Remove stop words
      essay_all_plain <- tm_map(essay_all_plain, removeWords, stopwords('english'))
      #  Stem the words in each document
      essay_all_plain <- tm_map(essay_all_plain, stemDocument)
      wordcloud(essay_all_plain, max.words = 100, random.order = FALSE)
    })
    
    output$heatmap <- renderPlot({
      bw_1 <- input$bw1
      bw_2 <- input$bw2
      map_all <- ggplot(data = nonDrinkers, aes(x = age, y = height)) +
        stat_density2d(geom = "polygon", h = c(bw_1, bw_2), aes(fill = ..level..)) + 
        ggovindt_315_theme + 
        ggtitle("Contour Plot of Age and Heights, with Marginal Distributions") + 
        labs(x = "Age", y = "Height") + 
        scale_fill_continuous(name = "Level")
      map_drinkers <- ggplot(data = drinkers, aes(x = age, y = height)) +
        stat_density2d(geom = "polygon", h = c(bw_1, bw_2), aes(fill = ..level..)) + 
        ggovindt_315_theme + 
        ggtitle("Contour Plot of Age and Heights, with Marginal Distributions") + 
        labs(x = "Age", y = "Height") + 
        scale_fill_continuous(name = "Level")
      map_all2 <- ggExtra::ggMarginal(map_all, type = "density", margins = "both", size = 3)
      map_drinkers2 <- ggExtra::ggMarginal(map_drinkers, type = "density", margins = "both", size = 3)
      if (input$radio) {
        map_drinkers2
      } else {
        map_all2
      }
    })
    
    index <- eventReactive(input$button, {
      sample(1:length(drinkers$age), 1000, replace = F)
    })
    
    output$mds_plot <- renderPlot({
      if (input$radio2) {
        inds <- index()
        sub <- drinkers[inds, ]
        sub <- subset(sub, select = c(age, height, income))
        sub <- scale(sub)
        sub_dist <- dist(sub)
        sub_mds <- cmdscale(sub_dist, k = 2)
        sub_mds <- as.data.frame(sub_mds)
        colnames(sub_mds) <- c("mds_1", "mds_2")
        ggplot(data = sub_mds, aes(x = mds_1, y = mds_2)) + 
          geom_density2d() + ggovindt_315_theme + 
          ggtitle("Contour Plot of MDS Coordinates") + 
          labs(x = "MDS Coordinate 1", y = "MDS Coordinate 2")
      } else {
        inds <- index()
        sub <- nonDrinkers[inds, ]
        sub <- subset(sub, select = c(age, height, income))
        sub <- scale(sub)
        sub_dist <- dist(sub)
        sub_mds <- cmdscale(sub_dist, k = 2)
        sub_mds <- as.data.frame(sub_mds)
        colnames(sub_mds) <- c("mds_1", "mds_2")
        ggplot(data = sub_mds, aes(x = mds_1, y = mds_2)) + 
          geom_density2d() + ggovindt_315_theme + 
          ggtitle("Contour Plot of MDS Coordinates") + 
          labs(x = "MDS Coordinate 1", y = "MDS Coordinate 2")
      }
    })
    
    output$sentplot <- renderPlot({
      ggplot(sentiment_df, aes(x = type, y = (word_sum/total), fill = type)) + 
        geom_bar(stat = "identity") + 
        labs(x="Type of Word", y = "Type Frequency") + 
        ggtitle("Heavy Drinkers Vs. Non-Heavy Drinkers Sentiment in Essay Questions") + 
        facet_wrap(~is_drinking) + ggovindt_315_theme +
        scale_fill_discrete(name = "Type")
    })
    
    output$hatelove <- renderPlot({
      cols <- c("love" = "blue", "hate" = "red")
      ggplot(data = hate_love_df) +
        geom_density(alpha = 0.4, bw=.34, aes(x = hate, colour = "hate")) +
        geom_density(alpha = 0.4, bw=.34, aes(x = love, colour = "love")) +
        facet_wrap(~type, ncol = 1) + 
        scale_color_manual(name = "Dimension", values = cols) + 
        labs(x = "Frequency", y = "Density") +
        ggtitle("Incidence of 'hate' and 'love' in Essays, by Drinking Status") + 
        ggovindt_315_theme
    })
    
    output$cloudP <- renderPlot({
      wordcloud(sentiment_df[which(sentiment_df$type == "positive"),]$words, 
                sentiment_df[which(sentiment_df$type == "positive"),]$word_sum, 
                max.words = 50)
    })
    
    output$cloudN <- renderPlot({
      wordcloud(sentiment_df[which(sentiment_df$type == "negative"),]$words, 
                sentiment_df[which(sentiment_df$type == "negative"),]$word_sum, 
                max.words = 50)
    })
    
})