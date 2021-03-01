library(shiny)

shinyUI(
  fluidPage(
    navbarPage("Kategorija",
               tabPanel(titlePanel(title = h5("BDP", align="center")),
                        sidebarLayout(
                          sidebarPanel(
                            selectizeInput('drzave','Država', choices = NULL),
                          ),
                          mainPanel(
                            plotOutput(outputId = "grafdrzav"))
                        )), 
               tabPanel(titlePanel(title = h5("Potrošnja gospodinjstev", align="center")),
                        sidebarLayout(
                          sidebarPanel(
                            selectizeInput('drzave','Država', choices = NULL),
                          ),
                          mainPanel(
                            plotOutput(outputId = "grafpotrosnja"))
                        )), 
               tabPanel(titlePanel(title = h5("Državni izdatki", align="center")),
                        sidebarLayout(
                          sidebarPanel(
                            selectizeInput('drzave','Država', choices = NULL),
                          ),
                          mainPanel(
                            plotOutput(outputId = "grafdrzava"))
                        )) 
    ) 
  ) 
) 

