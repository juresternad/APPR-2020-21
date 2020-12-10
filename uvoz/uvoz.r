# 2. faza: Uvoz podatkov

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

# Funkcija, ki uvozi občine iz Wikipedije
uvozi.obcine <- function() {
  link <- "http://sl.wikipedia.org/wiki/Seznam_ob%C4%8Din_v_Sloveniji"
  stran <- html_session(link) %>% read_html()
  tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
    .[[1]] %>% html_table(dec=",")
  for (i in 1:ncol(tabela)) {
    if (is.character(tabela[[i]])) {
      Encoding(tabela[[i]]) <- "UTF-8"
    }
  }
  colnames(tabela) <- c("obcina", "povrsina", "prebivalci", "gostota", "naselja",
                        "ustanovitev", "pokrajina", "regija", "odcepitev")
  tabela$obcina <- gsub("Slovenskih", "Slov.", tabela$obcina)
  tabela$obcina[tabela$obcina == "Kanal ob Soči"] <- "Kanal"
  tabela$obcina[tabela$obcina == "Loški potok"] <- "Loški Potok"
  for (col in c("povrsina", "prebivalci", "gostota", "naselja", "ustanovitev")) {
    if (is.character(tabela[[col]])) {
      tabela[[col]] <- parse_number(tabela[[col]], na="-", locale=sl)
    }
  }
  for (col in c("obcina", "pokrajina", "regija")) {
    tabela[[col]] <- factor(tabela[[col]])
  }
  return(tabela)
}

# Funkcija, ki uvozi podatke iz datoteke druzine.csv
uvozi.druzine <- function(obcine) {
  data <- read_csv2("podatki/druzine.csv", col_names=c("obcina", 1:4),
                    locale=locale(encoding="Windows-1250"))
  data$obcina <- data$obcina %>% strapplyc("^([^/]*)") %>% unlist() %>%
    strapplyc("([^ ]+)") %>% sapply(paste, collapse=" ") %>% unlist()
  data$obcina[data$obcina == "Sveti Jurij"] <- iconv("Sveti Jurij ob Ščavnici", to="UTF-8")
  data <- data %>% pivot_longer(`1`:`4`, names_to="velikost.druzine", values_to="stevilo.druzin")
  data$velikost.druzine <- parse_number(data$velikost.druzine)
  data$obcina <- parse_factor(data$obcina, levels=obcine)
  return(data)
}

# Zapišimo podatke v razpredelnico obcine
obcine <- uvozi.obcine()

# Zapišimo podatke v razpredelnico druzine.
druzine <- uvozi.druzine(levels(obcine$obcina))

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.

#######################################################################################################
#uvoz podatkov za Slovenijo
uvozi.slovenija <- function() {
  tabela <- read_csv2("podatki/slovenija.csv", locale=locale(encoding="Windows-1250"),skip=1)
  colnames(tabela) <- c("Panoge","Q1_2018","Q2_2018","Q3_2018","Q1_2019",
                        "Q2_2019","Q3_2019","Q1_2020","Q2_2020","Q3_2020")
  tabela <- tabela[c(1:12),]
  tabela <-tabela[-c(4),]
  tabela$Panoge <- c("Vse","Kmetijstvo, lov, gozdarstvo, ribištvo","Rudarstvo, predelovalne dejavnosti, elektrika, voda",
                     "Gradbeništvo", "Trgovina in popravila vozil, promet in skladiščenje, gostinstvo",
                     "Informacijske in komunikacijske dejavnosti", "Finančne in zavarovalniške dejavnosti",
                     "Poslovanje z nepremičninami","Strokovne, znanstvene, tehnične dejavnosti, poslovne dejavnosti",
                     "Uprava in obramba, izobraževanje, zdravstvo in socialno varstvo","Druge storitvene dejavnosti")
  tabela$rast_iz_2018_na_2019_ <- round(100*((tabela$Q1_2019-tabela$Q1_2018)/tabela$Q1_2018 +
                                               (tabela$Q2_2019-tabela$Q2_2018)/tabela$Q2_2018 +(tabela$Q3_2019-tabela$Q3_2018)/tabela$Q3_2018)/3,2)
  tabela$rast_iz_2019_na_2020 <- round(100*((tabela$Q1_2020-tabela$Q1_2019)/tabela$Q1_2019 +
                                              (tabela$Q2_2020-tabela$Q2_2019)/tabela$Q2_2019 +(tabela$Q3_2020-tabela$Q3_2019)/tabela$Q3_2019)/3,2)
  prva_tabela <- tabela[,-c(11,12)] %>% pivot_longer(!Panoge, names_to="Kvartal", values_to="Stevilo")
  prva_tabela$Stevilo <- as.numeric(prva_tabela$Stevilo)
  tabela <- inner_join(prva_tabela,tabela[,c(1,11,12)])
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
  tabela$Stevilo_zabelezenih_okuzb <- gsub(",", "", tabela$Stevilo_zabelezenih_okuzb)
  tabela$Populacija <- gsub(",", "", tabela$Populacija)
  tabela$Stevilo_zabelezenih_okuzb <- as.numeric(tabela$Stevilo_zabelezenih_okuzb)
  tabela$Populacija <- as.numeric(tabela$Populacija)
  tabela$Stevilo_okuzb_na_sto_tisoc_prebivalcev <- round(tabela$Stevilo_zabelezenih_okuzb/tabela$Populacija*100000)
  return(tabela)
}
okuzbe <- uvozi.okuzbe()

#uvoz ekselove tabele za spremebo kratic v polna imena drzav v csv datotekah
eksel <- read_excel("podatki/prevod.xlsx", sheet = 3, skip=9)
eksel <- eksel[-c(1,36,37,38,39,40,41), ]

#funkcija za uvazanje podatkov iz eurostat-a
uvozi <- function(ime_datoteke) {
  ime <- paste0("podatki/",ime_datoteke,".csv")
  tabela <- read_csv(ime, locale=locale(encoding="Windows-1250"), na = c(":"))
  tabela$TIMEPERIOD <- eksel$TIME
  tabela$TIMEPERIOD[tabela$TIMEPERIOD == "Germany (until 1990 former territory of the FRG)"] <- iconv("Germany")
  tabela$TIMEPERIOD[tabela$TIMEPERIOD == "European Union - 27 countries (from 2020)"] <- iconv("European Union")
  tabela <- tabela[-c(9,10,15),]
  stolpci <- c("Drzave", "Q4_2017","Q1_2018","Q2_2018","Q3_2018","Q4_2018",
               "Q1_2019","Q2_2019","Q3_2019","Q4_2019","Q1_2020","Q2_2020",
               "Q3_2020")
  colnames(tabela) <- stolpci
  tabela <- tabela %>% pivot_longer(!Drzave, names_to="Kvartal", values_to="Stevilo")
  tabela$Stevilo <- str_extract(tabela$Stevilo, "\\d+\\.*\\d*")
  tabela$Stevilo <- as.numeric(tabela$Stevilo)
  tabela <- tabela %>% pivot_wider(!Kvartal, names_from="Kvartal",values_from="Stevilo")
  tabela$rast_iz_2018_na_2019 <- round(100*((tabela$Q1_2019-tabela$Q1_2018)/tabela$Q1_2018 +
                                              (tabela$Q2_2019-tabela$Q2_2018)/tabela$Q2_2018 +(tabela$Q3_2019-tabela$Q3_2018)/tabela$Q3_2018)/3,2)
  tabela$rast_iz_2019_na_2020 <- round(100*((tabela$Q1_2020-tabela$Q1_2019)/tabela$Q1_2019 +
                                              (tabela$Q2_2020-tabela$Q2_2019)/tabela$Q2_2019 +(tabela$Q3_2020-tabela$Q3_2019)/tabela$Q3_2019)/3,2)
  prva_tabela <- tabela[,-c(14,15)] %>% pivot_longer(!Drzave, names_to="Kvartal", values_to="Vrednost")
  druga_tabela <- tabela[,c(1,14,15)]
  tabela <- inner_join(prva_tabela,druga_tabela)
  return(tabela)
}

#uvoz tabel
drzava <- uvozi("drzava")
bdp <- uvozi("bdp")
potrosnja <- uvozi("potrosnja")


#Združena tabela za bdp in okuzbe
zdruzeni <- inner_join(bdp,okuzbe)
zdruzeni <- zdruzeni[,c(1,6,7,8,2,3,4,5)]

bdp %>% ggplot() + 
  geom_point(aes(y = Drzave, x = rast_iz_2018_na_2019), color = "blue") +
  geom_point(aes(y = Drzave, x = rast_iz_2019_na_2020), color = "red") +
  ylab('Drzave') +
  xlab('Rast bdp')
ggtitle("Sprememba rasti bdp")
