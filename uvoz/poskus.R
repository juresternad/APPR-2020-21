#uvoz podatkov za Slovenijo
uvozi.slovenija <- function() {
  tabela <- read_csv2("podatki/slovenija.csv", locale=locale(encoding="Windows-1250"),skip=1)
  colnames(tabela) <- c("Panoge","Q1_2018","Q2_2018","Q3_2018","Q1_2019",
                        "Q2_2019","Q3_2019","Q1_2020","Q2_2020","Q3_2020")
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
  colnames(tabela) <- c("Drzave", "Stevilo_zabelezenih_okuzb", "Populacija")
  return(tabela)
}
okuzbe <- uvozi.okuzbe()

#uvoz ekselove tabele za spremebo kratic v polna imena drzav v csv datotekah
eksel <- read_excel("podatki/prevod.xlsx", sheet = 3, skip=9)
eksel <- eksel[-c(1,36,37,38,39,40,41), ]

#funkcija za uvazanje podatkov iz eurostat-a
uvozi <- function(ime_datoteke) {
  ime <- paste0("podatki/",ime_datoteke,".csv")
  tabela <- read_csv(ime, locale=locale(encoding="Windows-1250")) 
  tabela$TIMEPERIOD <- eksel$TIME
  tabela$TIMEPERIOD[tabela$TIMEPERIOD == "Germany (until 1990 former territory of the FRG)"] <- iconv("Germany")
  tabela$TIMEPERIOD[tabela$TIMEPERIOD == "European Union - 27 countries (from 2020)"] <- iconv("European Union")
  tabela <- tabel[,c(9,10,15)]
  return(tabela)
}

#uvoz tabel
drzava <- uvozi("drzava")
bdp <- uvozi("bdp")
potrosnja <- uvozi("potrosnja")

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
stolpci <- c("Drzave", "Q4_2017","Q1_2018","Q2_2018","Q3_2018","Q4_2018",
             "Q1_2019","Q2_2019","Q3_2019","Q4_2019","Q1_2020","Q2_2020",
             "Q3_2020")
colnames(potrosnja) <- stolpci
colnames(bdp) <- stolpci
colnames(drzava) <- stolpci

#Združena tabela za bdp in okuzbe
Zdruzeni <- inner_join(bdp,okuzbe)
