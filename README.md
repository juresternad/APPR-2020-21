# Analiza podatkov s programom R, 2020/21

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2020/21

* [![Shiny](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/juresternad/APPR-2020-21/master?urlpath=shiny/APPR-2020-21/projekt.Rmd) Shiny
* [![RStudio](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/juresternad/APPR-2020-21/master?urlpath=rstudio) RStudio

## Vpliv COVIDA-19 na gospodarstvo v Sloveniji in po svetu

### Tematika

V svojem projektu bom analiziral vpliv COVIDA-19 na domače in tuje gospodarstvo. V Sloveniji se bom osredotočil na dohodkovno strukturo BDP-a, v primerjavi s svetom pa bom ločil potrošnjo gospodinjstev in potrošnjo države, ki sta po mojem mnenju eni izmed najboljših pokazateljic sprememb gospodarstva glede na trenutne razmere. Posamezne podatke bom primerjal po kvartalih, saj tako lahko še bolj natančno ocenimo razlike, kot če bi jih primerjal samo po letih, poleg tega pa gledamo še bolj sveže podatke. Na koncu bom razlike v gospodarstvu primerjal še s prisotnostjo virusa v posamezni državi in s populacijo te države, ter tako analiziral katere države je najhuje prizadelo, ne glede na njihovo velikost, in kako hudo je prizadelo nas v primerjavi z ostalimi.

### Skrčen plan dela 

1. Po uvozu vseh podatkov se bom najprej posvetil dohodkovni strukturi Slovenije, gleda na zadnja tri leta (po kvartalih).
2. Nato se bom lotil primerjave držav, prvo po potrošnji gospodinjstev, nato pa še po potrošnji države.
3. Potem bom primerjal še države po BDP, zraven pa dodal še podatke iz 2. in 3. tabele. Na koncu bom iz vseh zbranih podatkov sestavil novo tabelo, v katero bom vključil še populacijo in okuženost posamezne države.
4. Na koncu bom še s pomočjo zemljevida (že vgrajenega v R) prikazal rezultate. Zemljevid bo podan v interaktivni obliki.

### Tabele

- Tabela 1: **Primerjava v Sloveniji glede na dohodkovno strukturo**: Posamezne panoge (kmetijstvo, gradbeništvo...) glede na posamezne kvartale (2018-2020)  
- Tabela 2: **Primerjava držav po potrošnji gospodinjstev**:  Posamezni kvartali, glede na države, na koncu še izračunane razlike kvartalov 2020 in kvartalov 2018, 2019
- Tabela 3: **Primerjava držav po državnih izdatkih**:  Posamezni kvartali, glede na države, na koncu še izračunane razlike kvartalov 2020 in kvartalov 2018, 2019
- Tabela 4: **Primerjava držav po BDP**:  Posamezni kvartali, populacija in število zabeleženih primerov, glede na države 

### Viri

- Eurostat( https://ec.europa.eu/eurostat/databrowser/view/teina010/default/table?lang=en, https://ec.europa.eu/eurostat/databrowser/view/teina020/default/table?lang=en, https://ec.europa.eu/eurostat/databrowser/view/teina030/default/table?lang=en)
- SiStat( https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/0300240S.px/table/tableViewLayout2/)
- https://www.worldometers.info/coronavirus/?utm_campaign=homeAdvegas1?%22%20%5Cl%20%22countries


Podatke iz SiStata bom uvozil v obliki csv, vse ostale podatke pa bom naložil preko html.

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `rgdal` - za uvoz zemljevidov
* `rgeos` - za podporo zemljevidom
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `tidyr` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `mosaic` - za pretvorbo zemljevidov v obliko za risanje z `ggplot2`
* `maptools` - za delo z zemljevidi
* `tmap` - za izrisovanje zemljevidov
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)

## Binder

Zgornje [povezave](#analiza-podatkov-s-programom-r-202021)
omogočajo poganjanje projekta na spletu z orodjem [Binder](https://mybinder.org/).
V ta namen je bila pripravljena slika za [Docker](https://www.docker.com/),
ki vsebuje večino paketov, ki jih boste potrebovali za svoj projekt.

Če se izkaže, da katerega od paketov, ki ji potrebujete, ni v sliki,
lahko za sprotno namestitev poskrbite tako,
da jih v datoteki [`install.R`](install.R) namestite z ukazom `install.packages`.
Te datoteke (ali ukaza `install.packages`) **ne vključujte** v svoj program -
gre samo za navodilo za Binder, katere pakete naj namesti pred poganjanjem vašega projekta.

Tako nameščanje paketov se bo izvedlo pred vsakim poganjanjem v Binderju.
Če se izkaže, da je to preveč zamudno,
lahko pripravite [lastno sliko](https://github.com/jaanos/APPR-docker) z želenimi paketi.

Če želite v Binderju delati z git,
v datoteki `gitconfig` nastavite svoje ime in priimek ter e-poštni naslov
(odkomentirajte vzorec in zamenjajte s svojimi podatki) -
ob naslednjem zagonu bo mogoče delati commite.
Te podatke lahko nastavite tudi z `git config --global` v konzoli
(vendar bodo veljale le v trenutni seji).
