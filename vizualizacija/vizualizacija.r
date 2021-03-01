###grafi za Slovenijo###########################################################

slovenijaalfa <- slovenija[c(grep("Finančne in zavarovalniške dejavnosti|
                                  |Druge storitvene dejavnosti|
                                  |Informacijske in komunikacijske dejavnosti|
                                  |Poslovanje z nepremičninami|Gradbeništvo|
                                  |Kmetijstvo, lov, gozdarstvo, ribištvo", slovenija$Panoge)),]

slovenijabeta <- slovenija[-c(grep("Finančne in zavarovalniške dejavnosti|
                                  |Druge storitvene dejavnosti|
                                  |Informacijske in komunikacijske dejavnosti|
                                  |Poslovanje z nepremičninami|Gradbeništvo|
                                  |Kmetijstvo, lov, gozdarstvo, ribištvo|Vse", slovenija$Panoge)),]

graf1 <- ggplot(data=slovenijaalfa, aes(x=Kvartal, y=Stevilo, color=str_wrap(Panoge,20)))+
  geom_point()+geom_line() + ylab('Sredstva za zaposlene v mio €') + facet_wrap(facets = vars(Leto)) + 
  scale_x_continuous(breaks =c(1,2,3),labels=c("1.", "2.", "3."))  + 
  theme(legend.key.height=unit(1.5, "cm"))+ labs(colour="Panoga")
ggtitle("Panoge s sredstvi nad 5 milijard €")


graf2 <- ggplot(data=slovenijabeta, aes(x=Kvartal, y=Stevilo, color=str_wrap(Panoge,20)))+
  geom_point()+geom_line() + ylab('Sredstva za zaposlene v mio €') + facet_wrap(facets = vars(Leto)) + 
  scale_x_continuous(breaks =c(1,2,3),labels=c("1.", "2.", "3."))  + 
  theme(legend.key.height=unit(2, "cm"))+ labs(colour="Panoga")
ggtitle("Panoge s sredstvi nad 5 milijard €")

###grafi za BDP na prebivalca###################################################

zdruzeni$Na_prebivalca <- zdruzeni$Vrednost * 1000000 / zdruzeni$Populacija

zdruzenidve <- zdruzeni[c(grep("Luxembourg|Ireland|Norway|Switzerland|Denmark", zdruzeni$Drzave)),]
zdruzenitri <- zdruzeni[c(grep("Serbia|Romania|Bulgaria|Croatia|Hungary|Poland", zdruzeni$Drzave)),]
zdruzenistiri <- zdruzeni[c(grep("Greece|Cyprus|Lithuania|Latvia", zdruzeni$Drzave)),]
zdruzenipet <- zdruzeni[c(grep("Slovenia|Spain|Portugal|Estonia|Czechia", zdruzeni$Drzave)),]
zdruzenisest <- zdruzeni[c(grep("France|Belgium|Germany|Austria|Netherlands|Finland|Sweden", zdruzeni$Drzave)),]

zdruzenidve <- zdruzenidve %>% mutate(Drzave=slovar[Drzave])
zdruzenitri <- zdruzenitri %>% mutate(Drzave=slovar[Drzave])
zdruzenistiri <- zdruzenistiri %>% mutate(Drzave=slovar[Drzave])
zdruzenipet <- zdruzenipet %>% mutate(Drzave=slovar[Drzave])
zdruzenisest <- zdruzenisest %>% mutate(Drzave=slovar[Drzave])


graf3.1 <- ggplot(data=zdruzenidve, aes(x=Kvartal, y=Vrednost*1000000/Populacija, col=Drzave))+
  geom_point()+geom_line() + facet_wrap(facets = vars(Leto))+
  scale_x_continuous(breaks =c(1,2,3),labels=c("1.", "2.", "3."))+
  ylab('Vrednost BDP na prebivalca') +
  ggtitle("Primerjava držav z najvišjim BDP na prebivalca")+
  scale_colour_discrete("Država")

graf3.2 <- ggplot(data=zdruzenitri, aes(x=Kvartal, y=Vrednost*1000000/Populacija, col=Drzave))+
  geom_point()+geom_line() + facet_wrap(facets = vars(Leto))+
  scale_x_continuous(breaks =c(1,2,3),labels=c("1.", "2.", "3."))+
  ylab('Vrednost BDP na prebivalca') +
  ggtitle("Države z BDP na prebivalca do 4000 €")+
  scale_colour_discrete("Država")

graf3.3 <- ggplot(data=zdruzenistiri, aes(x=Kvartal, y=Vrednost*1000000/Populacija, col=Drzave))+
  geom_point()+geom_line() + facet_wrap(facets = vars(Leto))+
  scale_x_continuous(breaks =c(1,2,3),labels=c("1.", "2.", "3."))+
  ylab('Vrednost BDP na prebivalca') +
  ggtitle("Države z BDP na prebivalca med 4000 € in 6000 €")+
  scale_colour_discrete("Država") 


graf3.4 <- ggplot(data=zdruzenipet, aes(x=Kvartal, y=Vrednost*1000000/Populacija, col=Drzave))+
  geom_point()+geom_line() + facet_wrap(facets = vars(Leto))+
  scale_x_continuous(breaks =c(1,2,3),labels=c("1.", "2.", "3."))+
  ylab('Vrednost BDP na prebivalca') +
  ggtitle("Primerjava držav s Sloveniji podobnim BDP na prebivalca")+
  scale_colour_discrete("Država")


graf3.5 <- ggplot(data=zdruzenisest, aes(x=Kvartal, y=Vrednost*1000000/Populacija, col=Drzave))+
  geom_point()+geom_line() + facet_wrap(facets = vars(Leto))+
  scale_x_continuous(breaks =c(1,2,3),labels=c("1.", "2.", "3."))+
  ylab('Vrednost BDP na prebivalca') +
  ggtitle("Države z BDP na prebivalca med 9000 € in 12000 €")+
  scale_colour_discrete("Država")



###grafi za delež BDP###########################################################

#1. urejanje tabel##############################################################
bdp1 <- bdp[-c(100:108),c(1,2,4)]
bdp18 <- bdp1[c(grep("2018",bdp$Leto)),]
bdp19 <- bdp1[c(grep("2019",bdp$Leto)),]
bdp20 <- bdp1[c(grep("2020",bdp$Leto)),]
bdp18 <- aggregate(x = bdp18$Vrednost,               
                   by = list(bdp18$Drzave),              
                   FUN = sum) 
bdp19 <- aggregate(x = bdp19$Vrednost,               
                   by = list(bdp19$Drzave),              
                   FUN = sum) 
bdp20 <- aggregate(x = bdp20$Vrednost,               
                   by = list(bdp20$Drzave),              
                   FUN = sum) 

potrosnja1 <- potrosnja[-c(100:108),c(1,2,4)]
potrosnja18 <- potrosnja1[c(grep("2018",potrosnja$Leto)),]
potrosnja19 <- potrosnja1[c(grep("2019",potrosnja$Leto)),]
potrosnja20 <- potrosnja1[c(grep("2020",potrosnja$Leto)),]
potrosnja18 <- aggregate(x = potrosnja18$Vrednost,               
                         by = list(potrosnja18$Drzave),              
                         FUN = sum) 
potrosnja19 <- aggregate(x = potrosnja19$Vrednost,               
                         by = list(potrosnja19$Drzave),              
                         FUN = sum) 
potrosnja20 <- aggregate(x = potrosnja20$Vrednost,               
                         by = list(potrosnja20$Drzave),              
                         FUN = sum) 

drzava1 <- drzava[-c(100:108),c(1,2,4)]
drzava18 <- drzava1[c(grep("2018",drzava$Leto)),]
drzava19 <- drzava1[c(grep("2019",drzava$Leto)),]
drzava20 <- drzava1[c(grep("2020",drzava$Leto)),]
drzava18 <- aggregate(x = drzava18$Vrednost,               
                      by = list(drzava18$Drzave),              
                      FUN = sum) 
drzava19 <- aggregate(x = drzava19$Vrednost,               
                      by = list(drzava19$Drzave),              
                      FUN = sum) 
drzava20 <- aggregate(x = drzava20$Vrednost,               
                      by = list(drzava20$Drzave),              
                      FUN = sum) 

tabela18 <- join(bdp18,potrosnja18, by = "Group.1")
tabela18 <- join(tabela18,drzava18, by = "Group.1")
colnames(tabela18) = c("Drzave","bdp18","potrosnja18","drzava18")
tabela18 <- inner_join(tabela18,okuzbe[,c(1,3)])

colnames(tabela18) = c("Drzave","bdp18","potrosnja18","drzava18","populacija")
tabela18$bdp18 <- (tabela18$bdp18/tabela18$populacija)*1000000
tabela18$potrosnja18 <- (tabela18$potrosnja18/tabela18$populacija)*1000000
tabela18$drzava18 <- (tabela18$drzava18/tabela18$populacija)*1000000
tabela18$bdp18 <- tabela18$bdp18-(tabela18$potrosnja18+tabela18$drzava18)

tabela18 <- tabela18[,c(1,2,3,4)] %>% pivot_longer(!Drzave, names_to="Kvartal", values_to="Vrednost")
tabela18 <- tabela18 %>% mutate(Drzave=slovar[Drzave])
#
tabela19 <- join(bdp19,potrosnja19, by = "Group.1")
tabela19 <- join(tabela19,drzava19, by = "Group.1")
colnames(tabela19) = c("Drzave","bdp19","potrosnja19","drzava19")
tabela19 <- inner_join(tabela19,okuzbe[,c(1,3)])

colnames(tabela19) = c("Drzave","bdp19","potrosnja19","drzava19","populacija")
tabela19$bdp19 <- (tabela19$bdp19/tabela19$populacija)*1000000
tabela19$potrosnja19 <- (tabela19$potrosnja19/tabela19$populacija)*1000000
tabela19$drzava19 <- (tabela19$drzava19/tabela19$populacija)*1000000
tabela19$bdp19 <- tabela19$bdp19-(tabela19$potrosnja19+tabela19$drzava19)

tabela19 <- tabela19[,c(1,2,3,4)] %>% pivot_longer(!Drzave, names_to="Kvartal", values_to="Vrednost")
tabela19 <- tabela19 %>% mutate(Drzave=slovar[Drzave])
#
tabela20 <- join(bdp20,potrosnja20, by = "Group.1")
tabela20 <- join(tabela20,drzava20, by = "Group.1")
colnames(tabela20) = c("Drzave","bdp20","potrosnja20","drzava20")
tabela20 <- inner_join(tabela20,okuzbe[,c(1,3)])

colnames(tabela20) = c("Drzave","bdp20","potrosnja20","drzava20","populacija")
tabela20$bdp20 <- (tabela20$bdp20/tabela20$populacija)*1000000
tabela20$potrosnja20 <- (tabela20$potrosnja20/tabela20$populacija)*1000000
tabela20$drzava20 <- (tabela20$drzava20/tabela20$populacija)*1000000
tabela20$bdp20 <- tabela20$bdp20-(tabela20$potrosnja20+tabela20$drzava20)

tabela20 <- tabela20[,c(1,2,3,4)] %>% pivot_longer(!Drzave, names_to="Kvartal", values_to="Vrednost")
tabela20 <- tabela20 %>% mutate(Drzave=slovar[Drzave])

#2. grafi#######################################################################

graf4.1 <- ggplot(data=tabela18, aes(x=reorder(Drzave,Vrednost),y=Vrednost, fill=Kvartal)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_bar(stat="identity") + ylab('Vrednost v mio €') + 
  theme(axis.title.x = element_blank()) + ggtitle('Sestava BDP v letu 2018') +
  scale_fill_discrete(name = "",labels = c("ostalo", "Državni izdatki", "Potrošnja gospodinjstev"))
  

graf4.2 <- ggplot(data=tabela19, aes(x=reorder(Drzave,Vrednost),y=Vrednost, fill=Kvartal)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_bar(stat="identity") + ylab('Vrednost v mio € ') + 
  theme(axis.title.x = element_blank()) + ggtitle('Sestava BDP v letu 2019') + 
  scale_fill_discrete(name = "",labels = c("ostalo", "Državni izdatki", "Potrošnja gospodinjstev"))

graf4.3 <- ggplot(data=tabela20, aes(x=reorder(Drzave,Vrednost),y=Vrednost, fill=Kvartal)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_bar(stat="identity") + ylab('Vrednost v mio € ') +
  theme(axis.title.x = element_blank()) + ggtitle('Sestava BDP v letu 2020')+
  scale_fill_discrete(name = "",labels = c("osatlo", "Državni izdatki", "Potrošnja gospodinjstev"))

###graf za rasti BDP############################################################
Imena <- c("rast_iz_2018_na_2019" = "Rast iz 2018 na 2019",
           "rast_iz_2019_na_2020" = "Rast iz 2019 na 2020")

bdp12 <- bdp %>% mutate(Drzave=slovar[Drzave])
bdp12 <- bdp12[!duplicated(bdp$rast_iz_2019_na_2020),]
bdp12 <- bdp12[-c(12),]
bdp12 <- bdp12[,c(1,5,6)] %>% gather("Rast", "Vrednost", -Drzave)
bdp12 <- bdp12 %>% mutate(Rast=Imena[Rast])

graf5 <- ggplot(data=bdp12,aes(x=reorder(Drzave,Vrednost),y=Vrednost,fill=Rast)) +
  geom_bar(position="dodge",stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Graf rasti")+
  ylab('Vrednost v % ')+
  xlab('')
  

###graf za gostoto##############################################################



rt <- bdp[,c(1,5,6)]
rt <- rt[!duplicated(rt$rast_iz_2018_na_2019),]
rt <- rt %>% pivot_longer(!Drzave, names_to="Rast", values_to="vr")
rt <- rt %>% mutate(Rast=Imena[Rast])

graf6 <-  ggplot(data=rt, aes(x=vr,color=Rast,fill=Rast))+
  geom_density(alpha=0.4,) + ylab('Gostota') + xlab('Rast BDP v %') + ggtitle('Gostota rasti BDP')+
  guides(color = FALSE)+scale_colour_discrete("Leto")
  
####zemljevid###################################################################
  
  zemljevid <- uvozi.zemljevid(
    "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip", "ne_50m_admin_0_countries", encoding="UTF-8")
  zemljevid <- zemljevid[zemljevid$CONTINENT == "Europe",]
  zemljevid@polygons <- zemljevid@polygons[-c(2,3,4,37,40)]
  zemljevid@data <- zemljevid@data[-c(2,3,4,37,40),]
  
  kt <- bdp[,-c(2:5)]
  kt <- inner_join(kt,zdruzeni[,c(1,9)])
  kt <- kt[!duplicated(kt$rast_iz_2019_na_2020),]
  kt$`Drzave`[kt$`Drzave` == "Serbia"] <- iconv("Republic of Serbia")
  kt[13,3] <- NaN
  colnames(kt)[3] <- "Okužbe"
  
  zemljevid1 <- tm_shape(merge(zemljevid,
                               kt %>% group_by(Drzave),
                               by.x="SOVEREIGNT", by.y="Drzave"),xlim=c(-25,32), ylim=c(32,72))+
    tm_polygons("rast_iz_2019_na_2020",palette = "RdYlGn", n=8,style = "equal",title = "Rast iz 2019 na 2020") +
    tm_bubbles(size = "Okužbe", scale=2.5,style="kmeans", col = "red", 
               size.lim=c(60,4200)) +
    tm_layout(main.title = "Zemljevid okuženosti in rasti BDP iz 2018 na 2019")
  

  
  