#uvoz podatkov za Slovenijo
uvozi.slovenija <- function() {
  tabela <- read_csv2("podatki/slovenija.csv", locale=locale(encoding="Windows-1250"),skip=1)
  colnames(tabela) <- c("Panoge", "2018-1.Kvartal","2018-2.Kvartal",
                        "2018-3.Kvartal","2019-1.Kvartal","2019-2.Kvartal",
                        "2019-3.Kvartal","2020-1.Kvartal","2020-2.Kvartal",
                        "2020-3.Kvartal")
  return(tabela)
}
slovenija <- uvozi.slovenija()

#uvoz podatkov za okuzenost
uvozi.okuzbe <- function() {
  link <- "https://www.worldometers.info/coronavirus/?utm_campaign=homeAdvegas1?%22%20%5Cl%20%22countries"
  stran <- html_session(link) %>% read_html()
  tabela <- stran %>% html_nodes(xpath="//table[@id='main_table_countries_today']") %>%
    .[[1]] %>% html_table(dec=",")
  tabela <- tabela[,-c(1)]
  tabela[15,1] <- "United Kingdom"
  tabela[,(3:13)] <- NULL
  tabela[,4:7] <- NULL
  colnames(tabela) <- c("Drzave", "Stevilo zabelezenih okuzb", "Populacija")
  return(tabela)
}
okuzbe <- uvozi.okuzbe()

#uvoz ekselove tabele za spremebo kratic v polna imena drzav v csv datotekah
eksel <- read_excel("podatki/prevod.xlsx", sheet = 3, skip=9)
eksel <- eksel[-c(1,36,37,38,39,40,41), ]

#uvoz podatkov za drzavno potrosnjo
uvozi.drzava <- function() {
  tabela <- read_csv("podatki/drzava.csv", locale=locale(encoding="Windows-1250")) 
  tabela$TIMEPERIOD <- eksel$TIME
  tabela$TIMEPERIOD[tabela$TIMEPERIOD == "Germany (until 1990 former territory of the FRG)"] <- iconv("Germany")
  tabela$TIMEPERIOD[tabela$TIMEPERIOD == "European Union - 27 countries (from 2020)"] <- iconv("European Union")
  return(tabela)
}
drzava <- uvozi.drzava()

#uvoz podatkov za bdp
uvozi.bdp <- function() {
  tabela <- read_csv("podatki/bdp.csv", locale=locale(encoding="Windows-1250")) 
  tabela$TIMEPERIOD <- eksel$TIME
  tabela$TIMEPERIOD[tabela$TIMEPERIOD == "Germany (until 1990 former territory of the FRG)"] <- iconv("Germany")
  tabela$TIMEPERIOD[tabela$TIMEPERIOD == "European Union - 27 countries (from 2020)"] <- iconv("European Union")
  return(tabela)
}
bdp <- uvozi.bdp()

#uvoz podatkov za potrosnjo gospodinjstev 
uvozi.potrosnja <- function() {
  tabela <- read_csv("podatki/potrosnja.csv", locale=locale(encoding="Windows-1250")) 
  tabela$TIMEPERIOD <- eksel$TIME
  tabela$TIMEPERIOD[tabela$TIMEPERIOD == "Germany (until 1990 former territory of the FRG)"] <- iconv("Germany")
  tabela$TIMEPERIOD[tabela$TIMEPERIOD == "European Union - 27 countries (from 2020)"] <- iconv("European Union")
  return(tabela)
}
potrosnja <- uvozi.potrosnja()


#podatki, ki jih ne bom uporabljal (razna EU območja glede na čas...)
a <- -c(9,10,15)


#odstranitev vrstic 
odstranitev <- function(x) {
  x <- x[a, ]
}

tabele <- list(bdp,potrosnja,drzava)
tabele <- lapply(tabele,odstranitev)

bdp <- data.frame(tabele[1])
potrosnja <- data.frame(tabele[2])
drzava <- data.frame(tabele[3])

#urejanje podatkov v tabeli
urejanje.podatkov <- function(x) {
  for (i in 1:nrow(x)) {
    for (j in 2:ncol(x)){
      x[i,j] <- x[i,j] %>% str_extract("\\d+") %>% as.integer()
    }
  }
  return(x)
}

tabele <- list(bdp,potrosnja,drzava,Zdruzeni)
tabele <- lapply(tabele,urejanje.podatkov)

bdp <- data.frame(tabele[1])
potrosnja <- data.frame(tabele[2])
drzava <- data.frame(tabele[3])
zdruzeni <- data.frame(tabele[4])

#preimenovanje stolpcev
stolpci <- c("Drzave", "2017-4.Kvartal", "2018-1.Kvartal","2018-2.Kvartal",
             "2018-3.Kvartal","2018-4.Kvartal","2019-1.Kvartal","2019-2.Kvartal",
             "2019-3.Kvartal","2019-4.Kvartal","2020-1.Kvartal","2020-2.Kvartal",
             "2020-3.Kvartal")
colnames(potrosnja) <- stolpci
colnames(bdp) <- stolpci
colnames(drzava) <- stolpci

#Združena tabela za bdp in okuzbe
Zdruzeni <- inner_join(bdp,okuzbe)
