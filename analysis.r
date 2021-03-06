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
table(subject_0$fact_id)
subject_1<- read.csv(file="subject-1-aaron.csv", header=TRUE, sep=",")
subject_1$ID = 1
table(subject_1$fact_id)
subject_2<- read.csv(file="subject-2-tianyi.csv", header=TRUE, sep=",")
subject_2$ID = 2
table(subject_2$fact_id)

subject_5<- read.csv(file="subject-5-loran.csv", header=TRUE, sep=",")
subject_5$ID = 5
table(subject_5$fact_id)

subject_6<- read.csv(file="subject-6-klemen.csv", header=TRUE, sep=",")
subject_6$ID = 6
table(subject_6$fact_id)
subject_7<- read.csv(file="subject-7-liv.csv", header=TRUE, sep=",")
subject_7$ID = 7
table(subject_7$fact_id)
training_data = rbind(subject_0,subject_1,subject_2,subject_5,subject_6,subject_7)
is.na(training_data)<-sapply(training_data, is.infinite)
training_data[is.na(training_data)]<-0
subject_0_test <- read.csv(file="subject-0-test.csv", header=TRUE, sep=",")
subject_0_test<-subset(subject_0_test,fact_id!=1&fact_id!=4&fact_id!=7*fact_id!=14&fact_id!=16&fact_id!=21)
subject_0_test$ID = 0
subject_1_test<- read.csv(file="subject-1-test.csv", header=TRUE, sep=",")
subject_1_test<-subset(subject_1_test,fact_id!=2&fact_id!=3&fact_id!=6&fact_id!=13&fact_id!=14&fact_id!=18&fact_id!=20&fact_id!=23)
subject_1_test$ID = 1
subject_2_test<- read.csv(file="subject-2-test.csv", header=TRUE, sep=",")
subject_2_test<-subset(subject_2_test,fact_id!=3&fact_id!=4&fact_id!=6&fact_id!=9&fact_id!=11&fact_id!=21&fact_id!=24&fact_id!=25)
subject_2_test$ID = 2
subject_5_test<- read.csv(file="subject-5-test.csv", header=TRUE, sep=",")
subject_5_test$ID = 5
subject_5_test<-subset(subject_5_test,fact_id!=1&fact_id!=5&fact_id!=12&fact_id!=13&fact_id!=14&fact_id!=15&fact_id!=17&fact_id!=19&fact_id!=22&fact_id!=23)

subject_6_test<- read.csv(file="subject-6-test.csv", header=TRUE, sep=",")
subject_6_test$ID = 6
subject_6_test<-subset(subject_6_test,fact_id!=3&fact_id!=5&fact_id!=9&fact_id!=17&fact_id!=20&fact_id!=21&fact_id!=24&fact_id!=25)

subject_7_test<- read.csv(file="subject-7.test.csv", header=TRUE, sep=",")
subject_7_test$ID = 7
subject_7_test<-subset(subject_7_test,fact_id!=1&fact_id!=3&fact_id!=5&fact_id!=6&fact_id!=7&fact_id!=11&fact_id!=15&fact_id!=22&fact_id!=23&fact_id!=25)

testing_data =rbind(subject_0_test,subject_1_test,subject_2_test,subject_5_test,subject_6_test,subject_7_test)
is.na(testing_data)<-sapply(testing_data, is.infinite)
testing_data[is.na(testing_data)]<-0
testing_data$correct<-as.logical(testing_data$correct)
testing_data$correct<-as.integer(testing_data$correct)

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

mean.Alpha <- with(training_data,aggregate(list(alpha=alpha),list(answer=answer),mean))
mean.correct <- with(testing_data,aggregate(list(correct=correct),list(answer=answer),mean))


composers_alpha<- ggplot(data=mean.Alpha, aes(x=answer, y=alpha,fill=answer)) +
  geom_bar(stat="identity")
composers_alpha + labs(title="Alpha per composer during training",x="Composer",y="Alpha")
composers_correct<- ggplot(data=mean.correct, aes(x=answer, y=correct,fill=correct)) +
  geom_bar(stat="identity")
composers_correct + labs(title="Score per composer during testing")
training_data$answer<-as.factor(training_data$answer)
training_data$answer<-as.integer(training_data$answer)
histogram<-qplot(training_data$fact_id, geom="histogram")
table(training_data$fact_id)


# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Classical music Learning"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select type of trend to plot
                    selectInput(inputId = "fact_ID", label = strong("Fact ID"),
                                choices = unique(training_data$fact_id),
                                selected = training_data$fact_id[1]),
                    
                    
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot", height = "300px"),
                    plotOutput(outputId = "lineplot2", height = "300px"),
                    
                    textOutput(outputId = "desc"),
                    
                  )
                )
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
}

# Create Shiny object
shinyApp(ui = ui, server = server)


