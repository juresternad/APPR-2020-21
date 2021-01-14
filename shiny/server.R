library(shiny)

shinyServer(function(input, output, session) {
  output$grafdrzav <- renderPlot({
    podatki1 <- zdruzeni %>% filter(Drzave==input$drzave)
    ggplot(podatki1, aes(x=Kvartal, y=Vrednost*1000000/Populacija, col=Drzave))+
      geom_point()+geom_line() + facet_wrap(facets = vars(Leto))+
      scale_x_continuous(breaks =c(1,2,3),labels=c("1.", "2.", "3."))+
      ylab('Vrednost BDP na prebivalca')
  })
  output$grafpotrosnja <- renderPlot({
    podatki1 <- potrosnja %>% filter(Drzave==input$drzave)
    ggplot(podatki1, aes(x=Kvartal, y=Vrednost, col=Drzave))+
      geom_point()+geom_line() + facet_wrap(facets = vars(Leto))+
      scale_x_continuous(breaks =c(1,2,3),labels=c("1.", "2.", "3."))+
      ylab('Vrednost potrošnje gospodinjstev')
  })
  output$grafdrzava <- renderPlot({
    podatki1 <- drzava %>% filter(Drzave==input$drzave)
    ggplot(podatki1, aes(x=Kvartal, y=Vrednost, col=Drzave))+
      geom_point()+geom_line() + facet_wrap(facets = vars(Leto))+
      scale_x_continuous(breaks =c(1,2,3),labels=c("1.", "2.", "3."))+
      ylab('Vrednost državnih izdatkov')
  })
})

