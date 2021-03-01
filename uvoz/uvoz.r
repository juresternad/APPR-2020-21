# 2. faza: Uvoz podatkov

slovar <- c("European Union" = "EU",
            "Belgium" = "Belgija",
            "Bulgaria" = "Bolgarija",
            "Czechia" = "Češka",
            "Denmark" = "Danska",
            "Germany" = "Nemčija",
            "Estonia" = "Estonija",
            "Ireland" = "Irska",
            "Greece" = "Grčija",
            "Spain" = "Španija",
            "France" = "Francija",
            "Croatia" = "Hrvaška",
            "Italy" = "Italija",
            "Cyprus" = "Ciper",
            "Latvia" = "Latvija",
            "Lithuania" = "Litva",
            "Luxembourg" = "Luksemburg",
            "Hungary" = "Madžarska",
            "Malta" = "Malta",
            "Netherlands" = "Nizozemska",
            "Austria" = "Avstrija",
            "Poland" = "Poljska",
            "Portugal" = "Portugalska",
            "Romania" = "Romunija",
            "Slovenia" = "Slovenija",
            "Slovakia" = "Slovaška",
            "Finland" = "Finska",
            "Sweden" = "Švedska",
            "United Kingdom" = "Velika Britanija",
            "Iceland" = "Islandija",
            "Norway" = "Norveška",
            "North Macedonia" = "Severna Makedonija",
            "Serbia" = "Srbija",
            "Switzerland" = "Švica")

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

#######################################################################################################
#uvoz podatkov za Slovenijo
uvozi.slovenija <- function() {
  tabela <- read_csv2("podatki/slovenija.csv", locale=locale(encoding="Windows-1250"),skip=1)
  tabela <- rename(tabela,c("TRANSAKCIJE" = "Panoge"))
  tabela <- tabela[c(1:12),]
  tabela <-tabela[-c(4),]
  tabela$Panoge <- c("Vse","Kmetijstvo, lov, gozdarstvo, ribištvo","Rudarstvo, predelovalne dejavnosti, elektrika, voda",
                     "Gradbeništvo", "Trgovina in popravila vozil, promet in skladiščenje, gostinstvo",
                     "Informacijske in komunikacijske dejavnosti", "Finančne in zavarovalniške dejavnosti",
                     "Poslovanje z nepremičninami","Strokovne, znanstvene, tehnične dejavnosti, poslovne dejavnosti",
                     "Uprava in obramba, izobraževanje, zdravstvo in socialno varstvo","Druge storitvene dejavnosti")
  prva_tabela <- tabela %>% pivot_longer(!Panoge, names_to="Kvartal", values_to="Stevilo") %>%
    separate(Kvartal, c("Leto", "Kvartal"), sep="Q") %>%
    mutate(Leto=parse_number(Leto), Kvartal=parse_number(Kvartal))
  return(prva_tabela)
}

slovenija <- uvozi.slovenija()


#uvoz podatkov za okuzenost
uvozi.okuzbe <- function() {
  link <- "https://www.worldometers.info/coronavirus/?utm_campaign=homeAdvegas1?%22%20%5Cl%20%22countries"
  stran <- html_session(link) %>% read_html()
  tabela <- stran %>% html_nodes(xpath="//table[@id='main_table_countries_today']") %>%
    .[[1]] %>% html_table(dec=",")
  tabela <- tabela[,-c(1)]
  tabela$`Country,Other`[tabela$`Country,Other` == "UK"] <- iconv("United Kingdom")
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
  tabela <- rename(tabela,c("TIMEPERIOD" = "Drzave"))
  tabela$Drzave <- eksel$TIME
  tabela$Drzave[tabela$Drzave == "Germany (until 1990 former territory of the FRG)"] <- iconv("Germany")
  tabela$Drzave[tabela$Drzave == "European Union - 27 countries (from 2020)"] <- iconv("European Union")
  tabela <- tabela[-c(9,10,15),-c(2,6,10)]
  stolpci <- c("Drzave","Q1_2018","Q2_2018","Q3_2018",
               "Q1_2019","Q2_2019","Q3_2019","Q1_2020","Q2_2020",
               "Q3_2020")
  orstolpci <- colnames(tabela)
  colnames(tabela) <- stolpci
  orstolpci <- append(orstolpci,"rast_iz_2018_na_2019")
  orstolpci <- append(orstolpci,"rast_iz_2019_na_2020")
  tabela <- tabela %>% pivot_longer(!Drzave, names_to="Kvartal", values_to="Stevilo")
  tabela$Stevilo <- str_extract(tabela$Stevilo, "\\d+\\.*\\d*")
  tabela$Stevilo <- as.numeric(tabela$Stevilo)
  tabela <- tabela %>% pivot_wider(!Kvartal, names_from="Kvartal",values_from="Stevilo") 
  tabela$rast_iz_2018_na_2019 <- round(100*((tabela$Q1_2019-tabela$Q1_2018)/tabela$Q1_2018 +
                                              (tabela$Q2_2019-tabela$Q2_2018)/tabela$Q2_2018 +(tabela$Q3_2019-tabela$Q3_2018)/tabela$Q3_2018)/3,2)
  tabela$rast_iz_2019_na_2020 <- round(100*((tabela$Q1_2020-tabela$Q1_2019)/tabela$Q1_2019 +
                                              (tabela$Q2_2020-tabela$Q2_2019)/tabela$Q2_2019 +(tabela$Q3_2020-tabela$Q3_2019)/tabela$Q3_2019)/3,2)
  colnames(tabela) <-  orstolpci
  prva_tabela <- tabela[,-c(11,12)] %>% pivot_longer(!Drzave, names_to="Kvartal", values_to="Vrednost")%>%
    separate(Kvartal, c("Leto", "Kvartal"), sep="Q") %>%
    mutate(Leto=parse_number(Leto), Kvartal=parse_number(Kvartal))
  druga_tabela <- tabela[,c(1,11,12)]
  tabela <- inner_join(prva_tabela,druga_tabela)
  return(tabela)
}

#uvoz tabel
drzava <- uvozi("drzava")
bdp <- uvozi("bdp")
potrosnja <- uvozi("potrosnja")

#Združena tabela za bdp in okuzbe
zdruzeni <- inner_join(bdp,okuzbe)




