library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(shinycssloaders)
library(stringr)


# Load necessary data frames from the RData file
load("papers.RData")

#Create a vector with colors as many as the topic labels
colors <- scales::hue_pal()(length(unique(paper_data$topicLabel)))
names(colors) <- unique(paper_data$topicLabel)

# Function to wrap text without breaking words
wrap_text <- function(text, width) {
  words <- unlist(strsplit(text, " "))
  wrapped_text <- ""
  current_line_length <- 0
  
  for (word in words) {
    if (nchar(word) + current_line_length > width) {
      wrapped_text <- paste0(wrapped_text, "<br>", word)
      current_line_length <- nchar(word)
    } else {
      if (current_line_length != 0) {
        wrapped_text <- paste0(wrapped_text, " ")
      }
      wrapped_text <- paste0(wrapped_text, word)
      current_line_length <- current_line_length + nchar(word) + 1
    }
  }
  
  return(wrapped_text)
}


ui <- dashboardPage(
  dashboardHeader(title = "LLM Paper Tracker"),
  dashboardSidebar(
                   #css to hide permanent error messages which happen between functions
                   tags$style(type="text/css",
                              ".shiny-output-error { visibility: hidden; }",
                              ".shiny-output-error:before { visibility: hidden; }"),
    # Sidebar content
    conditionalPanel(
      
      condition = "input.tabs === 'Papers at a Glance'",
      #ui object for topic label selections
      uiOutput("p1"),
      
      #ui object for display in 2D or 3D
      radioButtons("d23", "Display on:",
                   choices = c("2D","3D"),
                   selected = "2D"),
      #min max dates
      uiOutput("inputs"),
      
      #min max cit counts
      uiOutput("inputs2"),
      
      #UI message for available min and max inpouts
      uiOutput("minmax"),
      
      #UI message for error that occurs when max<min
      uiOutput("error"),
      tags$hr(),
      
      
      #UI messAge when min.date>max.date
      uiOutput("error2"),
      
    ),
    conditionalPanel(
      condition = "input.tabs === 'Paper Topic Category Statistics'",
      #UI object for x avriable
      uiOutput("input1"),
      
      #UI object for y variable
      uiOutput("input2"),
      
      #UI object for size variable
      uiOutput("input3"),
      
      #UI object for color variable
      uiOutput("input4")
      
    ),
    conditionalPanel(
      condition = "input.tabs === 'Analysis of Author Influence'",
      #UI object for topic label selection
      uiOutput("p3"),
      
      # slider input for citationCount
      
      #UI object for min numeric citation count
      uiOutput("num3.1"),
      #UI object for max numeric citation count
      uiOutput("num3.2"),
      
      #UI message that displays min,max available values
      uiOutput("minmax3"),
      
      #UI message that is displayed when min>max
      uiOutput("error3")
    )
  ),
  dashboardBody(
    # Body content with tabsetPanel
    tabsetPanel(
      type = "tabs",
      id = "tabs",
      tabPanel("Papers at a Glance",
               #UI object for scatterplot of 1st tab
               withSpinner(plotlyOutput(outputId = "scatterPlot3D")),
               tags$hr(),
               #UI object for paragraph below 
               withSpinner(uiOutput(outputId = "par1"))
               
      ),
      tabPanel("Paper Topic Category Statistics",
               #UI object for plot of 2nd tab
               withSpinner(plotlyOutput(outputId = "Plot2")),
               tags$hr(),
               
               #UI object for paragraph below
               withSpinner(uiOutput(outputId = "par2"))
      ),
      tabPanel("Analysis of Author Influence",
               
               #UI object for plot of 3rd tab
               withSpinner(plotlyOutput(outputId = "Plot3")),
               tags$hr(),
               #UI object for paragraph below
               withSpinner(uiOutput(outputId = "par3"))
      )
    )
  )
)

server <- function(input, output,session) {
  
  #server object for topic labels selection of 1st tab
  output$p1<-renderUI({
    words=unique(paper_data$topicLabel)
    words <- subset(words, words != "Unassigned Papers")
    pickerInput(inputId = "selectedTopics",
                label = "Choose Topic Labels:",
                choices = list(Topics = as.list(sort(unique(paper_data$topicLabel)))),
                
                #choices = list(Topics = as.list(unique(df2$topicLabel))),
                selected = words,
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "None...",
                  `select-all-text` = "Yeah, all !",
                  `none-selected-text` = "zero"
                ))
    
    
  })
  #server object for topic labels selection of 3rd tab
  output$p3<-renderUI({
    # Exclude a certain word
    word_to_exclude <- "Unassigned Papers"
    choices<-unique(author_influence$topicLabel)
    filtered_words <- choices[choices != word_to_exclude]
    
    pickerInput(inputId = "selectedTopics3",
                label = "Choose Topic Labels:",
                choices = list(Topics = as.list(sort(filtered_words))),
                
                #choices = list(Topics = as.list(unique(df2$topicLabel))),
                selected = filtered_words[1]
    )
    
    
  })
  #server object for min citation count of 3rd tab
  output$num3.1<-renderUI({
    
    
    numericInput("min3.1", "Minimum Topic Specialization Score", unique(min(df_authors2()$topicSpecializationScore,na.rm = T)), min = min(df_authors2()$topicSpecializationScore,na.rm = T), max = max(df_authors2()$topicSpecializationScore,na.rm = T))
    
    
  })
  
  #server object for max citation count of 3rd tab
  output$num3.2<-renderUI({
    
    
    numericInput("max3.1", "Maximum Topic Specialization Score", unique(max(df_authors2()$topicSpecializationScore,na.rm = T)), min = min(df_authors2()$topicSpecializationScore,na.rm = T), max = max(df_authors2()$topicSpecializationScore,na.rm = T))
    
    
  })
  
  #server object for message displaying min and max available values on 1st tab
  output$minmax <- renderUI({
    tags$div(
      class = "shiny-input-container",
      textOutput("minmax_text")  
    )
  })
  

  #server object for message displaying min and max available values on 3rd tab
  output$minmax3 <- renderUI({
    tags$div(
      class = "shiny-input-container",
      htmlOutput("minmax_text3")  
    )
  })
  
  output$minmax_text3 <- renderUI({

    HTML(paste('Minimum for Selected Topic:',min(df_authors2()$topicSpecializationScore,na.rm = T),"<br>",
          'Maximum for Selected Topic:',max(df_authors2()$topicSpecializationScore)
          
    ))
  })
  #server error message when min >max on 1st tab

  output$error <- renderUI({
    tags$div(
      class = "shiny-input-container",
      textOutput("error_text")
    )
  })

  output$error_text <- renderText({
    if(input$minNumeric>input$maxNumeric){
      "Minimum value cannot overlap maximum value"
    }else{
      return(NULL)
    }

  })
  #server error message when min date > max date in 1st tab
  output$error2<-renderUI({
    if(input$minDate=="NA"){
      return(NULL)
    }
    else if(input$maxDate=="NA"){
      return(NULL)
    }
    else if(input$minDate>input$maxDate){
      "Min. date cannot ba later than max. date"
    }else{
      return(NULL)
    }
  })
  output$error2 <- renderUI({
    tags$div(
      class = "shiny-input-container",
      textOutput("error_text2")
    )
  })
  
  output$error_text2 <- renderText({
    if(input$minDate>input$maxDate){
      "Min. date cannot ba later than max. date"
    }else{
      return(NULL)
    }

  })
  #server error message when min>max on 3rd tab
  
  output$error3 <- renderUI({
    tags$div(
      class = "shiny-input-container",
      textOutput("error_text3")  
    )
  })
  
  output$error_text3 <- renderText({
    if(input$min3.1>input$max3.1){
      "Minimum value cannot overlap maximum value"
    }else{
      return(NULL)
    }
  })
  #server object for min date input 
  
  
  df_authors2<-reactive({
    user_input1 <- input$selectedTopics3
    # Filter the data for the specific topic
    filtered_data <- author_influence %>%
      filter(topicLabel == user_input1)
    filtered_data
  })
  
  #reactive dataset for 3rd tab, filtered by min and max citation count
  df_authors3<-reactive({
    d<-df_authors2()
    d<-d%>%
      filter(topicSpecializationScore >= input$min3.1&topicSpecializationScore<=input$max3.1)
    d$plotlyTopicsAndCounts <- gsub("&nbsp;", "", d$plotlyTopicsAndCounts)
    #View(d)
    d
  })
  
  # Filter data based on selected names without nas
  filtered_data <- reactive({
    df_subset <- paper_data[paper_data$topicLabel %in% input$selectedTopics, ]
    df_subset
  })
  
  
  # Render numeric range inputs dynamically
  output$inputs <- renderUI({
    choice_dates <- sort(unique(filtered_data()$publicationDate))
    
    tagList(
      pickerInput(inputId = "minDate",
                  label = "Min Date:",
                  choices = choice_dates,
                  selected = min(choice_dates)
      ),
      pickerInput(inputId = "maxDate",
                  label = "Max. Date:",
                  choices = choice_dates,
                  selected = max(choice_dates)
      )
      
     
    )
  })
  
  
  
  #here the data for 1st plot is subseted with the inputs above-date range and cit count without nas
  filtered_data_b <- reactive({
    
    # Subset the dataframe based on the selected date range
    
      df_subset<-tibble(filtered_data())
      validate(need(input$minDate <= input$maxDate, "min Date has to be smaller or equal than max Date"))
      # Convert date columns to Date class using ymd() from lubridate
      df_subset <- df_subset %>%
        mutate(publicationDate = ymd(publicationDate))
      
      # Filter based on date range or NA
      df_subset <- df_subset %>%
        filter((publicationDate >= ymd(input$minDate) & publicationDate <= ymd(input$maxDate)) | is.na(publicationDate))
      df_subset
    }
  )
  
  output$inputs2<-renderUI({
    tagList(
      numericInput( 
        "minNumeric",
        "Min. Inf.Citation Count:",
        value = min(filtered_data_b()$influentialCitationCount),
        min = min(filtered_data_b()$influentialCitationCount),
        max = max(filtered_data_b()$influentialCitationCount)
      ),
      numericInput(
        "maxNumeric",
        "Max. Inf. Citation Count:",
        value = max(filtered_data_b()$influentialCitationCount),
        min = min(filtered_data_b()$influentialCitationCount),
        max = max(filtered_data_b()$influentialCitationCount)
      )
    )
  })
  
  #here the data for 1st plot is subseted with the inputs above-date range and cit count
  filtered_data_full <- reactive({
    
      validate(need(input$minNumeric <= input$maxNumeric, "min Numeric has to be smaller or equal than max Numeric"))
      df_subset <- subset(filtered_data_b(), influentialCitationCount >= input$minNumeric & influentialCitationCount <= input$maxNumeric)
      df_subset
      
    }
  )
  
  
  
  #plot of 1st tab
  output$scatterPlot3D <- renderPlotly({
    f<-filtered_data_full()

    if(input$d23=="3D"){
      # Create the 3D scatter plot with bolded hover text
      scatter3Dplot <- plot_ly(data = f, x = ~x3D, y = ~y3D, z = ~z3D, color = ~as.character(topicLabel),
                               type = "scatter3d", mode = "markers",
                               marker = list(size = 3),
                               colors = colors,
                               text = ~paste("<b>Title:</b> ", title,
                                             "<br><b>Topic:</b> ", topicLabel,
                                             "<br><b>Publication Date:</b> ", publicationDate,
                                             "<br><b>Influential Citation Count:</b> ", influentialCitationCount,
                                             "<br><b>TLDR:</b> ", tldr),
                               hoverinfo = "text") %>%
        layout(title = "LLM Papers Overview",
               scene = list(
                 xaxis = list(title = "X Axis"),
                 yaxis = list(title = "Y Axis"),
                 zaxis = list(title = "Z Axis")
               ),
          margin = list(l = 0, r = 0, b = 50, t = 100),
          hoverlabel = list(align = "left")) %>%
        layout(legend = list(orientation = "h"),
               showlegend = FALSE)
      
      
      
      return(scatter3Dplot)
    }
    else{
        # Create the 3D scatter plot with bolded hover text
        scatter2Dplot <- plot_ly(data = f, x = ~x2D, y = ~y2D, color = ~as.character(topicLabel),
                                 type = "scatter", mode = "markers",
                                 marker = list(size = 3),
                                 colors = colors,
                                 text = ~paste("<b>Title:</b> ", title,
                                               "<br><b>Topic:</b> ", topicLabel,
                                               "<br><b>Publication Date:</b> ", publicationDate,
                                               "<br><b>Influential Citation Count:</b> ", influentialCitationCount,
                                               "<br><b>TLDR:</b> ", tldr),
                                 hoverinfo = "text") %>%
          layout(title = "LLM Papers Overview",
                 scene = list(
                   xaxis = list(title = "X Axis"),
                   yaxis = list(title = "Y Axis")
                   
                 ),
                 margin = list(l = 0, r = 0, b = 50, t = 100),
                 hoverlabel = list(align = "left")) %>%
          layout(legend = list(orientation = "h"),
                 showlegend = FALSE)
        return(scatter2Dplot)
    }
  })
  
  #server object for x cariable on 2nd tab
  output$input1 <- renderUI({
    pickerInput(inputId = "Pr",
                label = "Choose x variable:",
                choices = c("Semantic X Location" = "xOfCentroid",
                            "Semantic Y Location" = "yOfCentroid",
                            "Number of Papers" = "numberPapers", 
                            "HHI" = "HHI", 
                            "Average Citation Count" = "avgCitationCount", 
                            "Average Influential Citation Count" = "avgInfluentialCitationCount",
                            "Total Citation Count" = "totalCitationCount", 
                            "Total Influential Citation Count" = "totalInfluentialCitationCount", 
                            "Average Reference Count" = "avgReferenceCount",
                            "Proportion of Papers with No Citations" = "proportionPapersWith0Citations", 
                            "Proportion of Papers with No Influential Citations" = "proportionPapersWith0InfluentialCitations"),
                selected = "xOfCentroid"
    )
  })
  
  #server object on y variable on 2nd tab
  output$input2 <- renderUI({
    pickerInput(inputId = "Pr2",
                label = "Choose y variable:",
                choices = c("Semantic X Location" = "xOfCentroid",
                            "Semantic Y Location" = "yOfCentroid",
                            "Number of Papers" = "numberPapers", 
                            "HHI" = "HHI", 
                            "Average Citation Count" = "avgCitationCount", 
                            "Average Influential Citation Count" = "avgInfluentialCitationCount",
                            "Total Citation Count" = "totalCitationCount", 
                            "Total Influential Citation Count" = "totalInfluentialCitationCount", 
                            "Average Reference Count" = "avgReferenceCount",
                            "Proportion of Papers with No Citations" = "proportionPapersWith0Citations", 
                            "Proportion of Papers with No Influential Citations" = "proportionPapersWith0InfluentialCitations"),
                selected = "yOfCentroid"
    )
  })
  
  #sever object for size variable on 3rd tab
  output$input3 <- renderUI({
    pickerInput(inputId = "Pr3",
                label = "Choose point size variable:",
                choices = c("None",
                            "Number of Papers" = "scaledNumberPapers",
                            "HHI" = "scaledHHI",
                            "Average Citation Count" = "scaledAvgCitationCount",
                            "Average Influential Citation Count" = "scaledAvgInfluentialCitationCount",
                            "Total Citation Count" = "scaledTotalCitationCount",
                            "Total Influential Citation Count" = "scaledTotalInfluentialCitationCount",
                            "Average Reference Count" = "scaledAvgReferenceCount",
                            "Proportion of Papers with No Citations" = "scaledProportionPapersWith0Citations",
                            "Proportion of Papers with No Influential Citations" = "scaledProportionPapersWith0InfluentialCitations"),
                selected = "None",
                multiple = T,
                options = pickerOptions(maxOptions = 1)
    )
  })
  
  #server object for color variable on 4th tab
  output$input4 <- renderUI({
    pickerInput(inputId = "Pr4",
                label = "Choose color intensity variable:",
                
                choices = c("None",
                            "Number of Papers" = "scaledNumberPapers",
                            "HHI" = "scaledHHI",
                            "Average Citation Count" = "scaledAvgCitationCount",
                            "Average Influential Citation Count" = "scaledAvgInfluentialCitationCount",
                            "Total Citation Count" = "scaledTotalCitationCount",
                            "Total Influential Citation Count" = "scaledTotalInfluentialCitationCount",
                            "Average Reference Count" = "scaledAvgReferenceCount",
                            "Proportion of Papers with No Citations" = "scaledProportionPapersWith0Citations",
                            "Proportion of Papers with No Influential Citations" = "scaledProportionPapersWith0InfluentialCitations"),
                selected = "None",
                multiple = T,
                options = pickerOptions(maxOptions = 1)
    )
  })

  
  
   choices = c("X Coordinate of Centroid" = "xOfCentroid",
              "Y Coordinate of Centroid" = "yOfCentroid",
              "Number of Papers" = "numberPapers", 
              "HHI" = "HHI", 
              "Average Citation Count" = "avgCitationCount", 
              "Average Influential Citation Count" = "avgInfluentialCitationCount",
              "Total Citation Count" = "totalCitationCount", 
              "Total Influential Citation Count" = "totalInfluentialCitationCount", 
              "Average Reference Count" = "avgReferenceCount",
              "Proportion of Papers with No Citations" = "proportionPapersWith0Citations", 
              "Proportion of Papers with No Influential Citations" = "proportionPapersWith0InfluentialCitations")
  
  
  #plot of 2nd tab
  output$Plot2<-renderPlotly({
    if(input$Pr3=="None"){
      if(input$Pr4=="None"){
        plot <- plot_ly(data = topic_stats, x = ~get(input$Pr), y = ~get(input$Pr2),
                        type = 'scatter', mode = 'markers',
                        marker = list(
                          sizemode = 'diameter',
                          sizeref = 0.1,
                          size = 8,  # Adjust this value to change the size of the points
                          opacity = 0.75,  # Adjust this value to change the transparency (0 to 1)
                          showscale = FALSE
                        ),
                        hoverinfo = 'text',
                        text = ~paste(
                          '<b>Topic:</b>', topicLabel,
                          '<br><b>Num Papers:</b>', numberPapers,
                          '<br><b>HHI:</b>', HHI,
                          '<br><b>Average Citation Count:</b>', avgCitationCount,
                          '<br><b>Average Influential Citation Count:</b>', avgInfluentialCitationCount,
                          '<br><b>Total Citation Count:</b>', totalCitationCount,
                          '<br><b>Total Influential Citation Count:</b>', totalInfluentialCitationCount,
                          '<br><b>Average References Count:</b>', avgReferenceCount,
                          '<br><b>Proportion of Papers with 0 Citations:</b>', proportionPapersWith0Citations,
                          '<br><b>Proportion of Papers with 0 Influential Citations:</b>', proportionPapersWith0InfluentialCitations
                        ))
        # Customize layout
        plot <- plot %>% layout(title = "Statistics of Different LLM Research Paper Topics",
                                xaxis = list(title = names(choices[choices == input$Pr])),
                                yaxis = list(title = names(choices[choices == input$Pr2])),
                                margin = list(l = 100, r = 0, b = 100, t = 100),
                                hoverlabel = list(align = "left")
                                
        )
        # Render the plot
        plot
      }
      else{
        plot <- plot_ly(data = topic_stats, x = ~get(input$Pr), y = ~get(input$Pr2),
                        type = 'scatter', mode = 'markers',
                        marker = list(
                          #size = ~get(input$Pr3), 
                          sizemode = 'diameter', sizeref = 0.1,
                          color = ~get(input$Pr4),
                          colorscale = c("lightcoral", "darkred"),
                          showscale = FALSE,
                          colorbar = list(title = wrap_text(names(choices[choices == input$Pr4]), 20))
                        ),
                        hoverinfo = 'text',
                        text = ~paste(
                          '<b>Topic:</b>', topicLabel,
                          '<br><b>Num Papers:</b>', numberPapers,
                          '<br><b>HHI:</b>', HHI,
                          '<br><b>Average Citation Count:</b>', avgCitationCount,
                          '<br><b>Average Influential Citation Count:</b>', avgInfluentialCitationCount,
                          '<br><b>Total Citation Count:</b>', totalCitationCount,
                          '<br><b>Total Influential Citation Count:</b>', totalInfluentialCitationCount,
                          '<br><b>Average References Count:</b>', avgReferenceCount,
                          '<br><b>Proportion of Papers with 0 Citations:</b>', proportionPapersWith0Citations,
                          '<br><b>Proportion of Papers with 0 Influential Citations:</b>', proportionPapersWith0InfluentialCitations
                        ))
        # Customize layout
        plot <- plot %>% layout(title = "Statistics of Different LLM Research Paper Topics",
                                xaxis = list(title = names(choices[choices == input$Pr])),
                                yaxis = list(title = names(choices[choices == input$Pr2])),
                                margin = list(l = 100, r = 0, b = 100, t = 100),
                                hoverlabel = list(align = "left"),
                                showlegend = FALSE
        )
        # Render the plot
        plot
      }
    }
    else{
      if(input$Pr4=="None"){
        plot <- plot_ly(data = topic_stats, x = ~get(input$Pr), y = ~get(input$Pr2),
                        type = 'scatter', mode = 'markers',
                        marker = list(
                          sizemode = 'diameter', sizeref = 0.1,
                          size = ~get(input$Pr3),
                          showscale = FALSE
                        ),
                        hoverinfo = 'text',
                        text = ~paste(
                          '<b>Topic:</b>', topicLabel,
                          '<br><b>Num Papers:</b>', numberPapers,
                          '<br><b>HHI:</b>', HHI,
                          '<br><b>Average Citation Count:</b>', avgCitationCount,
                          '<br><b>Average Influential Citation Count:</b>', avgInfluentialCitationCount,
                          '<br><b>Total Citation Count:</b>', totalCitationCount,
                          '<br><b>Total Influential Citation Count:</b>', totalInfluentialCitationCount,
                          '<br><b>Average References Count:</b>', avgReferenceCount,
                          '<br><b>Proportion of Papers with 0 Citations:</b>', proportionPapersWith0Citations,
                          '<br><b>Proportion of Papers with 0 Influential Citations:</b>', proportionPapersWith0InfluentialCitations
                        ))
        # Customize layout
        plot <- plot %>% layout(title = "Statistics of Different LLM Research Paper Topics",
                                xaxis = list(title = names(choices[choices == input$Pr])),
                                yaxis = list(title = names(choices[choices == input$Pr2])),
                                margin = list(l = 100, r = 0, b = 100, t = 100),
                                hoverlabel = list(align = "left"),
                                showlegend = FALSE
                                
        )
        # Render the plot
        plot
      }
      else{
        plot <- plot_ly(data = topic_stats, x = ~get(input$Pr), y = ~get(input$Pr2),
                        type = 'scatter', mode = 'markers',
                        marker = list(
                          size = ~get(input$Pr3), 
                          sizemode = 'diameter', sizeref = 0.1,
                          color = ~get(input$Pr4),
                          colorscale = c("lightcoral", "darkred"),
                          showscale = FALSE,
                          colorbar = list(title = wrap_text(names(choices[choices == input$Pr4]), 20))
                        ),
                        hoverinfo = 'text',
                        text = ~paste(
                          '<b>Topic:</b>', topicLabel,
                          '<br><b>Num Papers:</b>', numberPapers,
                          '<br><b>HHI:</b>', HHI,
                          '<br><b>Average Citation Count:</b>', avgCitationCount,
                          '<br><b>Average Influential Citation Count:</b>', avgInfluentialCitationCount,
                          '<br><b>Total Citation Count:</b>', totalCitationCount,
                          '<br><b>Total Influential Citation Count:</b>', totalInfluentialCitationCount,
                          '<br><b>Average References Count:</b>', avgReferenceCount,
                          '<br><b>Proportion of Papers with 0 Citations:</b>', proportionPapersWith0Citations,
                          '<br><b>Proportion of Papers with 0 Influential Citations:</b>', proportionPapersWith0InfluentialCitations
                        ))
        # Customize layout
        plot <- plot %>% layout(title = "Statistics of Different LLM Research Paper Topics",
                                xaxis = list(title = names(choices[choices == input$Pr])),
                                yaxis = list(title = names(choices[choices == input$Pr2])),
                                margin = list(l = 100, r = 0, b = 100, t = 100),
                                hoverlabel = list(align = "left"),
                                showlegend=FALSE
        )
        # Render the plot
        plot
      }
    }
    
    
  })
  
  
  
  #plot of 3rd tab
  output$Plot3<-renderPlotly({
     
    plot <- plot_ly(data = df_authors3(), x = ~jitter(authorTopicPaperCount), y = ~topicAvgInfluentialCitationCount,
                    type = 'scatter', mode = 'markers',
                    marker = list(
                      color = ~topicSpecializationScore, 
                      colorscale = 'Bluered',
                      colorbar = list(title = "Topic Specialization Score<br>"),
                      size = 10,  # Adjust this value as needed
                      opacity = 0.5  # Adjust this value as needed (range: 0 to 1)
                    ),
                    hoverinfo = 'text',
                    text = ~paste('<b>Name:</b>', authorName,
                                  '<br><b>Topic Specialization Score:</b>', topicSpecializationScore,
                                  '<br><b>Publications in Current Topic:</b>', authorTopicPaperCount,
                                  '<br><b>Average Influential Citations in Current Topic:</b>', topicAvgInfluentialCitationCount,
                                  '<br><b>Average Citation Count in Current Topic:</b>', topicAvgCitationCount,
                                  '<br><b>Author ID:</b>', authorId,
                                  '<br><b>Number of Papers in Other Topics',paste0("(", authorSumOtherPapers, ")"),':</b><br>',str_squish(plotlyTopicsAndCounts)))

    # Customize layout with wrapped titles
    plot <- plot %>% layout(
      title = wrap_text(paste('Influence of Authors in "', input$selectedTopics3, '"', sep=""), 65),
      xaxis = list(title = 'Publications in Current Topic'),
      yaxis = list(title = wrap_text(paste('Average Influential Citation Count in Current Topic'), 30)),
      margin = list(l = 70, r = 50, b = 90, t = 80),
      hoverlabel = list(align = "left")
    )
    # Render the plot
    plot
  })
  
  #server object for paragraph of 1st tab
  output$par1 <- renderUI({
    HTML(paste0(
      "<ul style='font-size: large; line-height: 200%;'>",
      "<li>", "Influential Citation Count for selected Topics", "</li>",
      "<div style='margin-left: 20px;'><strong>Min:</strong> ", min(filtered_data()$influentialCitationCount), "<br />",
      "<strong>Max:</strong> ", max(filtered_data()$influentialCitationCount), "</div>",
      "<li>", "Influential Citation Count for selected Topics AND date range", "</li>",
      "<div style='margin-left: 20px;'><strong>Min:</strong> ", min(filtered_data_b()$influentialCitationCount), "<br />",
      "<strong>Max:</strong> ", max(filtered_data_b()$influentialCitationCount), "</div>",
      "<li>", "Points that are closer together represent papers that are closer in the semantic meaning of their abstracts.", "</li>",
      "</ul>"
    ))
  })
  
  
  #server object for paragraph of 2nd tab
  output$par2 <- renderUI({
    HTML(paste0(
      "<ul style='font-size: large; line-height: 200%;'>", 
      "<li>", "Bigger HHI value = less author diversity - a higher concentration of papers written by a smaller number of authors", "</li>",
      "<li>", "Smaller HHI value = more author diversity", "</li>",
      "<li>", "Semantic X/Y Location = the X and Y values of the Centroid of the 2-Dimensional Vector Embeddings of Papers in a Particular Topic.", "</li>",
      "</ul>"
    ))
  })
  
  #server object for paragraph of 3rd tab
  output$par3 <- renderUI({
    HTML(paste0(
      "<ul style='font-size: large; line-height: 200%;'>", 
      "<li>", "Topic Specilization Score = Degree to which an author is localized to a particular topic. It is the ratio of the number of papers the author published in the current topic to their total number of papers published in all topics", "</li>",
      "</ul>"
    ))
  })
}

shinyApp(ui = ui, server = server)
