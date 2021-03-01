shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          'drzave', 'Država',
          choices=list(
            Eastern=c('Latvija', 'Litva', 'Estonija'),
            Western=c('Velika Britanija', 'Portugalska', 'Francija', 'Irska', 'Španija'),
            Middle=c('Madžarska', 'Poljska', 'Češka', 'Italija', 'Slovenija', 'Belgija', 'Nemčija', 'Avstrija', 'Luksemburg', 'Švica'),
            Northern=c('Švedska', 'Finska', 'Nemčija', 'Danska', 'Norveška'),
            Southern=c('Bolgarija', 'Romunija', 'Hrvaška', 'Grčija', 'Ciper', 'Srbija')
          ),
          selected='Slovenija', multiple=TRUE
        ),
      ),
      mainPanel(
        navbarPage(
          "Kategorija",
          tabPanel(titlePanel(title=h5("BDP", align="center")),
                   mainPanel(width=50,plotOutput(outputId="grafdrzav"))),
          tabPanel(titlePanel(title=h5("Potrošnja gospodinjstev", align="center")),
                   mainPanel(width=50,plotOutput(outputId="grafpotrosnja"))),
          tabPanel(titlePanel(title=h5("Državni izdatki", align="center")),
                   mainPanel(width=50,plotOutput(outputId="grafdrzava")))
        )
      )
    )
  )
)