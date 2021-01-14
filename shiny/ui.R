library(shiny)

shinyUI(
  fluidPage(
    navbarPage("Kategorija",
               tabPanel(titlePanel(title = h5("BDP", align="center")),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "drzave",
                                        selected = "Austria",
                                        label = "Država",
                                        choices = c(sort(unique(zdruzeni$Drzave)))),
                          ), #konec drugega sidebar panel
                          mainPanel(
                            plotOutput(outputId = "grafdrzav"))
                        )), 
               tabPanel(titlePanel(title = h5("Potrošnja gospodinjstev", align="center")),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "drzave",
                                               selected = "Austria",
                                               label = "Država",
                                               choices = c(sort(unique(zdruzeni$Drzave)))),
                          ), #konec drugega sidebar panel
                          mainPanel(
                            plotOutput(outputId = "grafpotrosnja"))
                        )), #konec drugega tab panela
               tabPanel(titlePanel(title = h5("Državni izdatki", align="center")),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "drzave",
                                               selected = "Austria",
                                               label = "Država",
                                               choices = c(sort(unique(zdruzeni$Drzave)))),
                          ), #konec drugega sidebar panel
                          mainPanel(
                            plotOutput(outputId = "grafdrzava"))
                        )) #konec drugega tab panela
    ) #konec navbarPage
  ) #konec fluid page
) #konec shinyUI
