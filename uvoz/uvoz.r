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
library(plyr)

nova_imena = c("Domača_ekipa" = "HomeTeam", "Gostujoča_ekipa" = "AwayTeam", "Zadetki_domača_ekipa" = "FTHG", "Zadetki_gostujoca_ekipa" = "FTAG", "Rezultat" = "FTR", "Zadetki_domača_ekipa_polčas" = "HTHG",
               "Zadetki_gostujoca_ekipa_počas" = "HTAG", "Rezultat_polčas" = "HTR", "Sodnik" = "Referee", "Streli_domači" = "HS", "Streli_gostje" = "AS", "Streli_domači_v_okvir" = "HST", "Streli_gosti_v_okvir" = "AST",
               "Prekrški_domači" = "HF", "Prekrški_gostje" = "AF", "Koti_domači" = "HC", "Koti_gostje" = "AC", "Rumeni_karton_domači" = "HY", "Rumeni_karton_gostje" = "AY", "Rdeč_karton_domači"= "HR", "Rdeč_karton_gostje" = "AR",
               "Kvota_zmaga_domačin" = "B365H", "Kvota_neodločeno" = "B365D", "Kvota_zamga_gost" = "B365A")
kaj_obravnavam = c("HomeTeam","AwayTeam","FTHG","FTAG","FTR", "HTHG", "HTAG", "HTR", "Referee", "HS", "AS", "HST", "AST", "HF", "AF", "HC", "AC", "HF", "AF", "HY", "AY", "HR", "AR", "B365H", "B365D", "B365A")

popravljeno_ime = c("l Mason"= "L Mason")
# Funkcija, ki uvozi podatke in jih počisti:
uvozi.podatke <- function() {
  Počiščeni_podatki <- NULL
  for (i in 9:18) {
    datoteka <- sprintf("podatki/season-%02d%02d.csv", i, i+1)
    Sezona <- read_csv(datoteka, locale=locale(encoding="utf8")) %>%
      select_(.dots = kaj_obravnavam) %>%
      rename_(.dots = nova_imena)
    Sezona$Sezona <- 2000 + i
    Počiščeni_podatki <- rbind(Počiščeni_podatki, Sezona) 
  }
  Počiščeni_podatki %>% drop_na(1)
}

Sezone <- uvozi.podatke()
  
# Funkcija, ki uvozi občine iz Wikipedije
uvozi.tabelo1 <- function() {
  lin <- "https://en.wikipedia.org/wiki/Premier_League"
  stran <- html_session(lin) %>% read_html()
  tabel <- stran %>% html_nodes(xpath="//table[@class='wikitable']") %>%
    .[[8]] %>% html_table(dec=",")
  for (i in 1:ncol(tabel)) {
    if (is.character(tabel[[i]])) {
      Encoding(tabel[[i]]) <- "UTF-8"
    }
  }
  colnames(tabel) <- c("Trener", "Klub", "Zmage", "LetoZmage")
  Leto <- tabel$LetoZmage %>% strapplyc("[^, ]+") %>% lapply(parse_number) %>% unlist()
  Trener <- sapply(1:nrow(tabel),
                   function(i) rep(tabel$Trener[i], tabel$Zmage[i])) %>% unlist()
  Klub <- sapply(1:nrow(tabel),
                   function(i) rep(tabel$Klub[i], tabel$Zmage[i])) %>% unlist()
  return(data.frame(Leto, Trener, Klub) %>% arrange(Leto))
}
Najbolši_trenerji <- uvozi.tabelo1()

# Funkcija, ki uvozi tabelo trenutnih trenerjev Wikipedije
uvozi.trenerje <- function() {
  link <- "https://en.wikipedia.org/wiki/Premier_League"
  stran <- html_session(link) %>% read_html()
  tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
    .[[6]] %>% html_table(dec=",")
  tabela %>% transmute(Name, Club,
                       Appointed=parse_date(Appointed, "%d %B %Y",
                                            locale=locale("en")))
}
Trenutni_trenerji <- uvozi.trenerje()

# Dodat stolpec za datum #parse_date(as.character(Sys.Date())) - Trenutni_trenerji$Appointed

