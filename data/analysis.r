library(ggplot2)
#Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)

setwd ("C:/Users/landm/Documents/GitHub/User_Models/data")
dir()

subject_0 <- read.csv(file="subject-0-esther.csv", header=TRUE, sep=",")
subject_0$ID = 0
subject_0$age = 24
subject_0$play_sing = 0
subject_0$read_notes = 1
subject_0$listen_music = 3
subject_0$typing = 2
table(subject_0$fact_id)

subject_1<- read.csv(file="subject-1-aaron.csv", header=TRUE, sep=",")
subject_1$ID = 1
subject_1$age = 23
subject_1$play_sing = 1
subject_1$read_notes = 1
subject_1$listen_music = 5
subject_1$typing = 4
table(subject_1$fact_id)

subject_2<- read.csv(file="subject-2-tianyi.csv", header=TRUE, sep=",")
subject_2$age = 23
subject_2$play_sing = 1
subject_2$read_notes = 1
subject_2$listen_music = 4
subject_2$typing = 4
subject_2$ID = 2
table(subject_2$fact_id)

subject_3<- read.csv(file="subject-3-vilem.csv", header=TRUE, sep=",")
subject_3$age = 23
subject_3$play_sing = 0
subject_3$read_notes = 0
subject_3$listen_music = 5
subject_3$typing = 4
subject_3$ID = 3
table(subject_3$fact_id)

subject_4<- read.csv(file="subject-4-stephanie.csv", header=TRUE, sep=",")
subject_4$ID = 4
subject_4$age = 27
subject_4$play_sing = 1
subject_4$read_notes = 1
subject_4$listen_music = 5
subject_4$typing = 5
table(subject_4$fact_id)

subject_5<- read.csv(file="subject-5-loran.csv", header=TRUE, sep=",")
subject_5$ID = 5
subject_5$age = 23
subject_5$play_sing = 1
subject_5$read_notes = 1
subject_5$listen_music = 5
subject_5$typing = 5
table(subject_5$fact_id)

subject_6<- read.csv(file="subject-6-klemen.csv", header=TRUE, sep=",")
subject_6$ID = 6
subject_6$age = 24
subject_6$play_sing = 0
subject_6$read_notes = 1
subject_6$listen_music = 5
subject_6$typing = 5
table(subject_6$fact_id)

subject_7<- read.csv(file="subject-7-liv.csv", header=TRUE, sep=",")
subject_7$ID = 7
subject_7$age = 23
subject_7$play_sing = 0
subject_7$read_notes = 0
subject_7$listen_music = 5
subject_7$typing = 5
table(subject_7$fact_id)

subject_8<- read.csv(file="subject-8-maxine.csv", header=TRUE, sep=",")
subject_8$ID = 8
subject_8$age = 24
subject_8$play_sing = 0
subject_8$read_notes = 0
subject_8$listen_music = 4
subject_8$typing = 4
table(subject_8$fact_id)

subject_9<- read.csv(file="subject-9-davidh.csv", header=TRUE, sep=",")
subject_9$ID = 9
subject_9$age = 25
subject_9$play_sing = 1
subject_9$read_notes = 1
subject_9$listen_music = 5
subject_9$typing = 5
table(subject_9$fact_id)

subject_10<- read.csv(file="subject-10-hidde.csv", header=TRUE, sep=",")
subject_10$ID = 10
subject_10$age = 25
subject_10$play_sing = 0
subject_10$read_notes = 0
subject_10$listen_music = 5
subject_10$typing = 1
table(subject_10$fact_id)

subject_11<- read.csv(file="subject-11-kyriakos.csv", header=TRUE, sep=",")
subject_11$ID = 11
subject_11$age = 29
subject_11$play_sing = 1
subject_11$read_notes = 1
subject_11$listen_music = 5
subject_11$typing = 5

table(subject_11$fact_id)
subject_12<- read.csv(file="subject-12-tim.csv", header=TRUE, sep=",")
subject_12$ID = 12
subject_12$age = 20
subject_12$play_sing = 0
subject_12$read_notes = 0
subject_12$listen_music = 2
subject_12$typing = 3
table(subject_12$fact_id)

subject_13<- read.csv(file="subject-13-ethelbert.csv", header=TRUE, sep=",")
subject_13$ID = 13
subject_13$age = 33
subject_13$play_sing = 0
subject_13$read_notes = 0
subject_13$listen_music = 4
subject_13$typing = 4
table(subject_13$fact_id)

subject_14<- read.csv(file="subject-14-davidk.csv", header=TRUE, sep=",")
subject_14$ID = 14
subject_14$age = 26
subject_14$play_sing = 0
subject_14$read_notes = 0
subject_14$listen_music = 5
subject_14$typing = 5

table(subject_14$fact_id)
training_data = rbind(subject_0,subject_1,subject_2,subject_3,subject_4,subject_5,subject_6,subject_7,subject_8,subject_9,subject_10,subject_11,subject_12,subject_13,subject_14)
is.na(training_data)<-sapply(training_data, is.infinite)
training_data[is.na(training_data)]<-0
training_data$read_notes<-as.logical(training_data$read_notes)

subject_0_test <- read.csv(file="subject-0-test.csv", header=TRUE, sep=",")
subject_0_test<-subset(subject_0_test,fact_id!=1&fact_id!=4&fact_id!=7*fact_id!=14&fact_id!=16&fact_id!=21)
subject_0_test$ID = 0
subject_0_test$age = 24
subject_0_test$play_sing = 0
subject_0_test$read_notes = 1
subject_0_test$listen_music = 3
subject_0_test$typing = 2

subject_1_test<- read.csv(file="subject-1-test.csv", header=TRUE, sep=",")
subject_1_test$age = 23
subject_1_test$play_sing = 1
subject_1_test$read_notes = 1
subject_1_test$listen_music = 5
subject_1_test$typing = 4
subject_1_test$ID = 1
subject_1_test_subset<-subset(subject_1_test,fact_id!=2&fact_id!=3&fact_id!=6&fact_id!=13&fact_id!=14&fact_id!=18&fact_id!=20&fact_id!=23)

subject_2_test<- read.csv(file="subject-2-test.csv", header=TRUE, sep=",")
subject_2_test$age = 23
subject_2_test$play_sing = 1
subject_2_test$read_notes = 1
subject_2_test$listen_music = 4
subject_2_test$typing = 4
subject_2_test$ID = 2
subject_2_test_subset<-subset(subject_2_test,fact_id!=3&fact_id!=4&fact_id!=6&fact_id!=9&fact_id!=11&fact_id!=21&fact_id!=24&fact_id!=25)

subject_3_test<- read.csv(file="subject-3-test.csv", header=TRUE, sep=",")
subject_3_test$age = 23
subject_3_test$play_sing = 0
subject_3_test$read_notes = 0
subject_3_test$listen_music = 5
subject_3_test$typing = 4
subject_3_test$ID=3
subject_3_test_subset<-subset(subject_3_test,fact_id!=3&fact_id!=4&fact_id!=5&fact_id!=6&fact_id!=10&fact_id!=12&fact_id!=13&fact_id!=14&fact_id!=15&fact_id!=20&fact_id!=23&fact_id!=23&fact_id!=25)

subject_4_test<- read.csv(file="subject-4-test.csv", header=TRUE, sep=",")
subject_4_test$ID = 4
subject_4_test$age = 27
subject_4_test$play_sing = 1
subject_4_test$read_notes = 1
subject_4_test$listen_music = 5
subject_4_test$typing = 5
subject_4_test_subset<-subset(subject_4_test,fact_id!=2&fact_id!=5&fact_id!=7&fact_id!=9&fact_id!=11&fact_id!=13&fact_id!=14&fact_id!=15&fact_id!=17&fact_id!=18&fact_id!=20&fact_id!=25&fact_id!=25)

subject_5_test<- read.csv(file="subject-5-test.csv", header=TRUE, sep=",")
subject_5_test$ID = 5
subject_5_test$age = 23
subject_5_test$play_sing = 1
subject_5_test$read_notes = 1
subject_5_test$listen_music = 5
subject_5_test$typing = 5
subject_5_test_subset<-subset(subject_5_test,fact_id!=1&fact_id!=5&fact_id!=12&fact_id!=13&fact_id!=14&fact_id!=15&fact_id!=17&fact_id!=19&fact_id!=22&fact_id!=23)

subject_6_test<- read.csv(file="subject-6-test.csv", header=TRUE, sep=",")
subject_6_test$ID = 6
subject_6_test$age = 24
subject_6_test$play_sing = 0
subject_6_test$read_notes = 1
subject_6_test$listen_music = 5
subject_6_test$typing = 5
subject_6_test_subset<-subset(subject_6_test,fact_id!=3&fact_id!=5&fact_id!=9&fact_id!=17&fact_id!=20&fact_id!=21&fact_id!=24&fact_id!=25)

subject_7_test<- read.csv(file="subject-7.test.csv", header=TRUE, sep=",")
subject_7_test$ID = 7
subject_7_test$age = 23
subject_7_test$play_sing = 0
subject_7_test$read_notes = 0
subject_7_test$listen_music = 5
subject_7_test$typing = 5
subject_7_test_subset<-subset(subject_7_test,fact_id!=1&fact_id!=3&fact_id!=5&fact_id!=6&fact_id!=7&fact_id!=11&fact_id!=15&fact_id!=22&fact_id!=23&fact_id!=25)

subject_8_test<- read.csv(file="subject-8-test.csv", header=TRUE, sep=",")
subject_8_test$ID = 8
subject_8_test$age = 24
subject_8_test$play_sing = 0
subject_8_test$read_notes = 0
subject_8_test$listen_music = 4
subject_8_test$typing = 4
subject_8_test_subset<-subset(subject_8_test,fact_id!=1&fact_id!=3&fact_id!=5&fact_id!=7&fact_id!=8&fact_id!=10&fact_id!=11&fact_id!=12&fact_id!=15&fact_id!=16&fact_id!=23&fact_id!=25)

subject_9_test<- read.csv(file="subject-9-test.csv", header=TRUE, sep=",")
subject_9_test$ID = 9
subject_9_test$age = 25
subject_9_test$play_sing = 1
subject_9_test$read_notes = 1
subject_9_test$listen_music = 5
subject_9_test$typing = 5
subject_9_test_subset<-subset(subject_9_test,fact_id!=1&fact_id!=2&fact_id!=3&fact_id!=5&fact_id!=6&fact_id!=9&fact_id!=12&fact_id!=15&fact_id!=19&fact_id!=20&fact_id!=22&fact_id!=25)

subject_10_test<- read.csv(file="subject-10-test.csv", header=TRUE, sep=",")
subject_10_test$ID = 10
subject_10_test$age = 25
subject_10_test$play_sing = 0
subject_10_test$read_notes = 0
subject_10_test$listen_music = 5
subject_10_test$typing = 1
subject_10_test_subset<-subset(subject_10_test,fact_id!=3&fact_id!=5&fact_id!=3&fact_id!=8&fact_id!=9&fact_id!=10&fact_id!=12&fact_id!=14&fact_id!=17&fact_id!=24&fact_id!=25)

subject_11_test<- read.csv(file="subject-11-test.csv", header=TRUE, sep=",")
subject_11_test$ID = 11
subject_11_test$age = 29
subject_11_test$play_sing = 1
subject_11_test$read_notes = 1
subject_11_test$listen_music = 5
subject_11_test$typing = 5
subject_11_test_subset<-subset(subject_11_test,fact_id!=1&fact_id!=2&fact_id!=7&fact_id!=8&fact_id!=9&fact_id!=10&fact_id!=11&fact_id!=13&fact_id!=14&fact_id!=18&fact_id!=20&fact_id!=25)

subject_12_test<- read.csv(file="subject-12-test.csv", header=TRUE, sep=",")
subject_12_test$ID = 12
subject_12_test$age = 20
subject_12_test$play_sing = 0
subject_12_test$read_notes = 0
subject_12_test$listen_music = 2
subject_12_test$typing = 3
subject_12_test_subset<-subset(subject_12_test,fact_id!=3&fact_id!=4&fact_id!=7&fact_id!=10&fact_id!=12&fact_id!=13&fact_id!=14&fact_id!=16&fact_id!=19&fact_id!=20&fact_id!=23)

subject_13_test<- read.csv(file="subject-13-test.csv", header=TRUE, sep=",")
subject_13_test$ID = 13
subject_13_test$age = 33
subject_13_test$play_sing = 0
subject_13_test$read_notes = 0
subject_13_test$listen_music = 4
subject_13_test$typing = 4
subject_13_test_subset<-subset(subject_13_test,fact_id!=1&fact_id!=4&fact_id!=5&fact_id!=17&fact_id!=18&fact_id!=25)

subject_14_test<- read.csv(file="subject-14-test.csv", header=TRUE, sep=",")
subject_14_test$ID = 14
subject_14_test$age = 26
subject_14_test$play_sing = 0
subject_14_test$read_notes = 0
subject_14_test$listen_music = 5
subject_14_test$typing = 5
subject_14_test_subset<-subset(subject_14_test,fact_id!=2&fact_id!=5&fact_id!=8&fact_id!=13&fact_id!=20&fact_id!=21&fact_id!=22)


testing_data =rbind(subject_0_test,subject_1_test,subject_2_test,subject_3_test,subject_4_test,subject_5_test,subject_6_test,subject_7_test,subject_8_test,subject_10_test,subject_11_test,subject_12_test,subject_13_test,subject_14_test)
is.na(testing_data)<-sapply(testing_data, is.infinite)
testing_data[is.na(testing_data)]<-0
testing_data$correct<-as.logical(testing_data$correct)
testing_data$correct<-as.integer(testing_data$correct)
testing_data$read_notes<-as.logical(testing_data$read_notes)

remove_NA <- function(data) {
  is.na(data)<-sapply(data, is.infinite)
  data[is.na(data)]<-0
}
  # 
# p<-ggplot(data=fact1, aes(x=trial, y=rt,fill=correct)) +
#   geom_bar(stat="identity")
# p + labs(title="Reaction time of the first fact during training",x="Trial",y="Reaction time (ms)")
# 
# p<-ggplot(data=fact1, aes(x=trial, y=alpha,fill=correct)) +
#   geom_bar(stat="identity")
# p + labs(title="Alpha of the first fact during training",x="Trial",y="Alpha)")
# 
# 
# test_data <-read.csv(file="subject-0_test.csv", header=TRUE, sep=",")
# test<- ggplot(data=test_data, aes(x=answer, y=correct,fill=correct)) +
#   geom_bar(stat="identity")
# test + labs(title="Answer per composer at the test",x="Composer",y="Accuracy")
# test+theme(axis.text.y = element_blank())

mean.Alpha <- with(training_data,aggregate(list(alpha=alpha),list(answer=answer,read_notes=read_notes),mean))
mean.correct <- with(testing_data,aggregate(list(correct=correct),list(answer=answer,read_notes=read_notes),mean))


composers_alpha<- ggplot(data=mean.Alpha, aes(x=answer, y=alpha,fill=answer)) +
  geom_bar(stat="identity")
composers_alpha + labs(title="Alpha per composer during training",x="Composer",y="Alpha")
composers_correct<- ggplot(data=mean.correct, aes(x=answer, y=correct,fill=correct)) +
  geom_bar(stat="identity")
composers_correct + labs(title="Score per composer during testing")
  histogram<-qplot(training_data$fact_id, geom="histogram")
# table(training_data$fact_id)


# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Classical music Learning"),
              
                  # Output: Description, lineplot, and reference
                    tabsetPanel(
                    tabPanel(title = "Training data: facts",
                    selectInput(inputId = "fact_ID", label = strong("Fact ID"),
                    choices = unique(training_data$fact_id),
                    selected = training_data$fact_id[1]),
                             
                    plotOutput(outputId = "lineplot", height = "300px"),
                    plotOutput(outputId = "lineplot2", height = "300px"),
                    
                    textOutput(outputId = "desc"),
                    ),
                    tabPanel(title = "Training data: Trials",
                             selectInput(inputId = "ID", label = strong("Participant ID"),
                                         choices = unique(training_data$ID),
                                         selected = training_data$ID[1]),
                             
                             plotOutput(outputId = "lineplot5", height = "300px"),
                             plotOutput(outputId = "lineplot6", height = "300px")
                             
                    ),
                  tabPanel(title = "Testing data",
                             selectInput(inputId = "ID2", label = strong("Participant ID"),
                                         choices = unique(testing_data$ID),
                                         selected = testing_data$ID[0]),
                             
                             plotOutput(outputId = "lineplot3", height = "300px")
                    ),
                  tabPanel(title = "Overall result",
                           
                           plotOutput(outputId = "lineplot4", height = "300px"),
                           plotOutput(outputId = "lineplot7", height = "300px")
                           
                  ),
                    ),
)


# Define server function
server <- function(input, output) {
  
  # Subset data
  selected_trends <- reactive({
    req(inputId)
    })
  
  # 
  
  
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    data = subset(training_data,training_data$fact_id==input$fact_ID)
    max <-length(data$trial)
    data$trial <-1:max
    ggplot(data=data, aes(x=trial, y=alpha,fill=correct))+ggtitle("Mean alpha of fact during training") +
         geom_bar(stat="identity")
     
    })
  output$lineplot2 <- renderPlot({
    data = subset(training_data,training_data$fact_id==input$fact_ID)
    max <-length(data$trial)
    data$trial <-1:max
    ggplot(data=data, aes(x=trial, y=rt,fill=correct))+ggtitle("Mean RT of fact during training") +
      geom_bar(stat="identity")
    
  })
  output$lineplot3 <- renderPlot({
    data = subset(testing_data,testing_data$ID==input$ID2)
    max <-length(data$trial)
    data$trial <-1:max
    ggplot(data=data, aes(x=answer, y=correct))+ggtitle("Accuracy per participants") +
      geom_bar(stat="identity",fill="#56B4E9")
  })
  
  output$lineplot4 <- renderPlot({
    
    mean.correct <- with(testing_data,aggregate(list(correct=correct),list(answer=answer,read_notes=read_notes),mean))
    composers_correct<- ggplot(data=mean.correct, aes(x=read_notes, y=correct,fill=answer)) +
      geom_bar(stat="identity")
    composers_correct + labs(title="Mean score per composer during testing")
  })
  output$lineplot5 <- renderPlot({
    data = subset(training_data,training_data$ID==input$ID)
    ggplot(data=data, aes(x=trial, y=alpha,fill=answer))+ggtitle("Alpha of  all facts during one training trail") +
      geom_bar(stat="identity")
    
  })
  output$lineplot6 <- renderPlot({
    data = subset(training_data,training_data$ID==input$ID)
    ggplot(data=data, aes(x=trial, y=rt,fill=answer))+ggtitle("RT of  all facts during one training trail") +
      geom_bar(stat="identity")
    
  })
  output$lineplot7 <- renderPlot({
    mean.Alpha <- with(training_data,aggregate(list(alpha=alpha),list(answer=answer,read_notes=read_notes),mean))
    ggplot(data=mean.Alpha, aes(x=read_notes, y=alpha,fill=answer))+ggtitle("Mean alpha of  all facts during all training trails") +
      geom_bar(stat="identity")
    
  })
  
  }

# Create Shiny object
shinyApp(ui = ui, server = server)

