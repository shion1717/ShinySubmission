library(shiny)
library(tidyverse)

data(ToothGrowth)
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
# Define UI ----
ui <- fluidPage(
    titlePanel("Effect of Vitamin C on Tooth Growth"),

# Sidebar helptext and input widgets
    sidebarLayout(
        sidebarPanel("This dashboard enables the user to explore the ",a("ToothGrowth",href = "https://stat.ethz.ch/R-manual/R-patched/library/datasets/html/ToothGrowth.html"),
                     "dataset. In the first option, the user can isolate the exploration to the type of method (Orange Juice or Ascorbic Acid or Both). ",br(),
                     "In the subsequent options, the user can set the relevant parameters to perform a t-test to determine if the change in dosage leads to a significant change in Tooth Growth length.",br(),
                     "The results of the t test is shown in the second tab, Student's t test.", p(),
                     selectInput("supp","Choose a Vitamin C administration method ",c("Both (OJ and VC)","Orange Juice","Ascorbic Acid")),br(),
                     h3("T test parameters"),
                     sliderInput("conf","Choose a desired confidence level",min=0.5,max=1,value=0.95),br(),
                     selectInput("dose1","Choose the first dosage level to compare ",c(0.5,1.0,2.0)),br(),
                     selectInput("dose2","Choose the second dosage level to compare ",c(0.5,1.0,2.0),selected=1.0)
                     
        ),
# Main panel tabs and output
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Boxplot","Displaying box plots for:",strong(textOutput("selected_supp")),plotOutput("box")),
                                 
                        tabPanel("Student's t test", 
                                 textOutput("conf_chosen"),
                                 textOutput("testperform"),
                                 verbatimTextOutput("ttest")))
        )
    )
)

# Define server logic ----
server <- function(input, output) {
    
# To set plaintext outputs
    output$selected_supp <- renderText({
        input$supp
    })
    output$conf_chosen <- renderText({
        paste("Confidence level: ",input$conf*100,"%")
    })
    output$testperform <- renderText({
        paste("Performing a t-test on dosage =",input$dose1, "and dosage =",input$dose2)
    })
    
# Plotting the boxplot    
    output$box <- renderPlot({
        
        # Choose which subset of data to use
        data <- switch(input$supp, 
                       "Both (OJ and VC)" = ToothGrowth,
                       "Orange Juice" = subset(ToothGrowth,supp=="OJ"),
                       "Ascorbic Acid" = subset(ToothGrowth,supp=="VC"))
        ggplot(data=data,aes(x=dose,y=len))+
                geom_boxplot(aes(color=supp))+ggtitle("Boxplot of Tooth Growth Length against Dosage")+
                xlab("Dosage (mg/day)")+ylab("Length")
    })
    
# To set server logic to perform t-tests between the different dosages
    output$ttest <- renderPrint({
        # Choose which subset of data to use
        data <- switch(input$supp, 
                       "Both (OJ and VC)" = ToothGrowth,
                       "Orange Juice" = subset(ToothGrowth,supp=="OJ"),
                       "Ascorbic Acid" = subset(ToothGrowth,supp=="VC"))
        # t test
        tmod <- t.test(subset(data,dose == input$dose1)$len,subset(data,dose==input$dose2)$len,
                       paired=FALSE,var.equal=FALSE,conf.level=input$conf)
        print(tmod)
        if(tmod$p.value < (1-input$conf)) print("Reject the null hypothesis (The means of the two groups are significantly different)") else print("Accept the null hypothesis (The means of the two groups are not significantly different)")
    })
    
}

# Run the app ----
shinyApp(ui = ui, server = server)