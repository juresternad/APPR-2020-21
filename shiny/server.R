library(shiny)

shinyServer(function(input, output, session) {
  
  updateSelectizeInput(session, 'drzave', choices = list(
    Eastern = c('Latvija','Litva','Estonija'),
    Western = c('Velika Britanija', 'Portugalska','Francija','Irska','Španija'),
    Middle = c('Madžarska','Poljska','Češka','Italija','Slovenija','Belgija','Nemčija','Avstrija','Luksemburg','Švica'),
    Northern =c('Švedska','Finska','Nemčija','Danska','Norveška'),
    Southern = c('Bolgarija','Romunija','Hrvaška','Grčija','Ciper','Srbija')
  ), selected = 'Slovenija',server=TRUE)
  
  zdruzeni <- zdruzeni %>% mutate(Drzave=slovar[Drzave])
  potrosnja <- potrosnja %>% mutate(Drzave=slovar[Drzave])
  drzava <- drzava %>% mutate(Drzave=slovar[Drzave])
  
  output$grafdrzav <- renderPlot({
    podatki1 <- zdruzeni %>% filter(Drzave==input$drzave)
    ggplot(podatki1, aes(x=Kvartal, y=Vrednost*1000000/Populacija, col=Drzave))+
      geom_point()+geom_line() + facet_wrap(facets = vars(Leto))+
      scale_x_continuous(breaks =c(1,2,3),labels=c("1.", "2.", "3."))+
      ylab('Vrednost BDP na prebivalca v evrih')+
      scale_colour_discrete("Država")
  })
  output$grafpotrosnja <- renderPlot({
    podatki1 <- potrosnja %>% filter(Drzave==input$drzave)
    ggplot(podatki1, aes(x=Kvartal, y=Vrednost, col=Drzave))+
      geom_point()+geom_line() + facet_wrap(facets = vars(Leto))+
      scale_x_continuous(breaks =c(1,2,3),labels=c("1.", "2.", "3."))+
      ylab('Vrednost potrošnje gospodinjstev v (mio) evrih')+
      scale_colour_discrete("Država")
  })
  output$grafdrzava <- renderPlot({
    podatki1 <- drzava %>% filter(Drzave==input$drzave)
    ggplot(podatki1, aes(x=Kvartal, y=Vrednost, col=Drzave))+
      geom_point()+geom_line() + facet_wrap(facets = vars(Leto))+
      scale_x_continuous(breaks =c(1,2,3),labels=c("1.", "2.", "3."))+
      ylab('Vrednost državnih izdatkov v (mio) evrih')+
      scale_colour_discrete("Država")
  })
})

