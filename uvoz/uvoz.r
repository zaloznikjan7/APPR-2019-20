# 2. faza: Uvoz podatkov
library(dplyr)
library(knitr)
library(rmarkdown)
library(shiny)
library(DT)
library(rgdal)
library(rgeos)
library(digest)
library(readr)
library(rvest)
library(tidyr)
library(dplyr)
library(gsubfn)
library(ggplot2)
library(mosaic)
library(maptools)
library(extrafont) 

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
                    locale=locale(encoding="CP1250"))
  data$obcina <- data$obcina %>% strapplyc("^([^/]*)") %>% unlist() %>%
    strapplyc("([^ ]+)") %>% sapply(paste, collapse=" ") %>% unlist()
  data$obcina[data$obcina == "Sveti Jurij"] <- "Sveti Jurij ob Ščavnici"
  data <- data %>% gather(`1`:`4`, key="velikost.druzine", value="stevilo.druzin")
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




# Ko uvoziš nov csv, ko delaš path kak prit do datoteke kak greš eno mapo nazaj? 
# Nove spremenljivke sam ne delajo, a jih sploh spremenim ker so tak dolge? 
# A lahk napišeš funkcijo, ko spremeni ime datoteke?, al rabim 10 različnih? 
# Kk je s ttimi knjižnicami? A je bajboljš da vedno vse poženem?? 
# Pojma nimam kak to prebere HTML, pač tte občine kak nastaviš da od neki naprej bere in kak veš od kje do kje more it? 
# A morem js zaj to združit skup al ne ker ne gre to tak ko na vajah, ker je miljon podatkov 
# A morem naredit parse_noumber in kak če je tega zelo velik?
# 
# nova_imena = c(Domača_ekipa = HomeTeam, Gostujoča_ekipa = AwayTeam, Zadetki_domača_ekipa = FTHG, Zadetki_gostujoca_ekipa = FTAG, Rezultat = FTR, Zadetki_domača_ekipa_polčas = HTHG,
#                Zadetki_gostujoca_ekipa_počas = HTAG, Rezultat_polčas= HTR, Sodnik = Referee, Streli_domači = HS, Streli_gosti = AS, Streli_domači_v_okvir = HST, Streli_gosti_v_okvir = AST,
#                Prekrški_domači = HF, Prekrški_gostje = AF, Koti_domači= HC, Koti_gostje= AF, Rumeni_karton_domači = HY, Rumeni_karton_gostje = AY, Rdeč_karton_domači= HR, Rdeč_karton_gostje = AR,
#                Kvota_zmaga_domačin= B365H, Kvota_neodločeno = B365D, Kvota_zamga_gost = B365A)
# kaj_obravnavam = c(HomeTeam,AwayTeam,FTHG,FTAG,FTR, HTHG, HTAG, HTR, Referee, HS, AS, HST, AST, HF, AF, HC, AC, HF, AF, HY, AY, HR, AR, B365H, B365D, B365A)

# Funkcija, ki uvozi podatke in jih počisti:
uvozi.podatke <- function() {
  Sezona1819 <- read_csv("podatki/season-1819.csv", locale=locale(encoding="utf8"))
  Počiščeni_podatki <- Sezona1819 %>% select(HomeTeam,AwayTeam,FTHG,FTAG,FTR, HTHG, HTAG, HTR, Referee, HS, AS, HST, AST, HF, AF, HC, AC, HF, AF, HY, AY, HR, AR, B365H, B365D, B365A) %>%
    rename(Domača_ekipa = HomeTeam, Gostujoča_ekipa = AwayTeam, Zadetki_domača_ekipa = FTHG, Zadetki_gostujoca_ekipa = FTAG, Rezultat = FTR, Zadetki_domača_ekipa_polčas = HTHG,
           Zadetki_gostujoca_ekipa_počas = HTAG, Rezultat_polčas= HTR, Sodnik = Referee, Streli_domači = HS, Streli_gosti = AS, Streli_domači_v_okvir = HST, Streli_gosti_v_okvir = AST,
           Prekrški_domači = HF, Prekrški_gostje = AF, Koti_domači= HC, Koti_gostje= AF, Rumeni_karton_domači = HY, Rumeni_karton_gostje = AY, Rdeč_karton_domači= HR, Rdeč_karton_gostje = AR,
           Kvota_zmaga_domačin= B365H, Kvota_neodločeno = B365D, Kvota_zamga_gost = B365A)
  Počiščeni_podatki %>% write.csv2("podatki/Očiščeni_podatki_1819.csv",fileEncoding = "utf8")
  Počiščeni_podatki
}

Sezona.1819 <- uvozi.podatke()
  
  
  