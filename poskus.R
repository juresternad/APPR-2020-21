require(dplyr)
require(rvest)
require(readr)
require(tidyr)
require(readxl)

#uvoz podatkov za Slovenijo
slovenija <- read_csv2("podatki/slovenija.csv", locale=locale(encoding="Windows-1250"),skip=1) 

#uvoz podatkov za okuzenost
uvozi.okuzbe <- function() {
  link <- "https://www.worldometers.info/coronavirus/?utm_campaign=homeAdvegas1?%22%20%5Cl%20%22countries"
  stran <- html_session(link) %>% read_html()
  tabela <- stran %>% html_nodes(xpath="//table[@id='main_table_countries_today']") %>%
    .[[1]] %>% html_table(dec=",")
  return(tabela)
}
okuzbe <- uvozi.okuzbe()

#uvoz ekselove tabele za spremebo kratic v polna imena drzav v csv datotekah
eksel <- read_excel("podatki/prevod.xlsx", sheet = 3, skip=9)
eksel <- eksel[-c(1,36,37,38,39,40,41), ]

#podatki, ki jih ne bom uporabljal (razna EU območja glede na čas, kvartal 2017,...)
a <- -c(9,10,15)
b <- -c(2,14)

#uvoz podatkov za drzavno potrosnjo
drzava <- read_csv("podatki/drzava.csv", locale=locale(encoding="Windows-1250")) 
drzava$imena <- eksel$TIME
drzava$TIMEPERIOD <- drzava$imena
drzava <- drzava[a, b]
drzava$TIMEPERIOD[drzava$TIMEPERIOD == "Germany (until 1990 former territory of the FRG)"] <- iconv("Gemany")
drzava$TIMEPERIOD[drzava$TIMEPERIOD == "European Union - 27 countries (from 2020)"] <- iconv("European Union")


#uvoz podatkov za bdp
bdp <- read_csv("podatki/bdp.csv", locale=locale(encoding="Windows-1250")) 
bdp$imena <- eksel$TIME
bdp$TIMEPERIOD <- bdp$imena
bdp <- bdp[a, b]
bdp$TIMEPERIOD[bdp$TIMEPERIOD == "Germany (until 1990 former territory of the FRG)"] <- iconv("Gemany")
bdp$TIMEPERIOD[bdp$TIMEPERIOD == "European Union - 27 countries (from 2020)"] <- iconv("European Union")



#uvoz podatkov za potrosnjo gospodinjstev 
potrosnja <- read_csv("podatki/potrosnja.csv", locale=locale(encoding="Windows-1250")) 
potrosnja$imena <- eksel$TIME
potrosnja$TIMEPERIOD <- potrosnja$imena
potrosnja <- potrosnja[a, b]
potrosnja$TIMEPERIOD[potrosnja$TIMEPERIOD == "Germany (until 1990 former territory of the FRG)"] <- iconv("Gemany")
potrosnja$TIMEPERIOD[potrosnja$TIMEPERIOD == "European Union - 27 countries (from 2020)"] <- iconv("European Union")


#preimenovanje stolpcev
preimenovanje <- function(x) {
  x <- x %>%
    rename(
      Drzave = TIMEPERIOD
    )
}

tabele <- list(bdp,potrosnja,drzava)
tabele <- lapply(tabele,preimenovanje)

bdp <- data.frame(tabele[1])
potrosnja <- data.frame(tabele[2])
drzava <- data.frame(tabele[3])