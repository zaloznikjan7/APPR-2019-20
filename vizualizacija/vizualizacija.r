library(ggplot2)
library(dplyr)
library(plotrix)
library(plotly)

# # 3. faza: Vizualizacija podatkov
# 
# # Uvozimo zemljevid.
# zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
#                              pot.zemljevida="OB", encoding="Windows-1250")
# levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
#   { gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
# zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels=levels(obcine$obcina))
# zemljevid <- fortify(zemljevid)
# 
# # Izračunamo povprečno velikost družine
# povprecja <- druzine %>% group_by(obcina) %>%
#   summarise(povprecje=sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))

#=========================================================================================================================================================================================
#1. POVPREČNO ŠTEVILO DOSEŽENIH ZADETKOV DOMAČINOV IN GOSTOV

# povprecno_golov <- ggplot(Sezone %>% filter(Sezona == "2009")) + aes(x=1:380 ,y=Zadetki_domača_ekipa) + geom_point()
# povprecno_golov

# povprečje_golov_domači <- Sezone %>% group_by(Sezona) %>% summarise(Povprecje = mean(Zadetki_domača_ekipa, na.rm = TRUE))
# graf1 <-  ggplot(povprečje_golov_domači) %>% + aes(x = Sezona ,y = Povprecje) + geom_path()
# print(graf1 + theme(panel.background=element_rect(fill="white")))
# 
# povprecno_golov_gostje <- Sezone %>% group_by(Sezona) %>% summarise(Povprecje = mean(Zadetki_gostujoca_ekipa, na.rm = TRUE))
# graf2 <-  ggplot(povprecno_golov_gostje) %>% + aes(x = Sezona ,y = Povprecje) + geom_path()
# print(graf2 +theme(panel.background=element_rect(fill="white")))

#Ne dela ker ne najde Povprecje2


# povprečje_golov <- Sezone %>% group_by(Sezona) %>% summarise(Povprecje1 = mean(Zadetki_domača_ekipa, na.rm = TRUE), Povprecje2 = mean(Zadetki_gostujoca_ekipa,na.rm= TRUE))
# graf1 <- povprečje_golov %>%
#   ggplot(aes(x = Sezona ,y = Povprecje1)) + 
#   geom_path(color="blue") + 
#   geom_path(x = Povprecje2)
# graf2 <- ggplot(povprečje_golov) %>% + aes(x = Sezona , y= Povprecje2) + geom_path(color= "red")
# print(graf2 + theme(panel.background=element_rect(fill="white")))


novi <- ggplot(data = povprečje_golov, aes(x = Sezona, y = Povprecje1)) + geom_path()
#1 graf
novi2 <- ggplot(povprečje_golov, aes(Sezona)) + 
  geom_line(aes(y = Povprecje1), color = "red") + 
  geom_line(aes(y = Povprecje2), color = "blue") +
  xlab("Sezona") + ylab("Povprečje zadetkov") + ggtitle("Primerjava zadetkov domače in gostujoče ekipe") +
 # legend(x= 0.5,y = 1.5, legend = c("Koliko golov dosežejo domači", "Koliko golov dosežejo gostje"), fill = c(4,2)) +
  theme(panel.background=element_rect(fill="white"))
print(novi2)
# Kak spremeniš vrstice in stolpce? Kak spremeniš nek podatek naprimer iz A hočem js G
# Kaj vse je sploh mišljeno pod vizualizacijo? Se pravi grafe in histograme itd? Kak bi biu primer dobrega grafa? tak da se kr na Sezone sklicujem
# Sj je to mišljeno kot da tak analiziram jl? Pa da sm zaj opazu tut neki kar prej nism vedo in sicer to da narašča število golov v gosteh
# Kak se ggtitle prestavi na sredino? Kak se naredi legenda?
# Kak lahk dva grafa skup narišem? se pravi graf1 in graf2 skup na isti sliki? Kak se to naredi s dplyr, al pa da še več kot dva grafa skup daš



# Kak bi naredu da bi zbralo eno ekipo in pol pogledlo vrstico in verjetno z if stavkom torej če zadetki_domača_ekipa > zadetki_gostujoca_ekipa pol prištej ena
# Torej rad bi preštel vse ko so zmagali doma in kolk zgubili, ostalo so tak mogli igrat izenačeno
# torej na koncu bi rad dau na graf x os je vsak klub in pol nek histogram kolk so meli zmag, novi graf pa kolk so igrali izenačeno pa še en graf kok so zgubili 
#ista logika tut za sodnika da pogledaš pol kolk je dau kerih kartonov


# 3. graf
# Preštejem vse rumene kartone vsake domače ekipe in dam zraven vsoto vseh gostujočih ekip 
kartoni <- Sezone %>% summarise(kartoni_dodeljeni_domačinom = sum(Rumeni_karton_domači, na.rm = TRUE), kartoni_dodeljeni_gostom =sum(Rumeni_karton_gostje, na.rm = TRUE))
bar <- ggplot(kartoni) + aes(x=1, fill= kartoni_dodeljeni_gostom) +geom_histogram()
print(bar)
##### Kak se to naredi? da bi lahk predstavu da so dobili domači velik manj rumenih kartonov
vsi_kartoni <- sum(kartoni$kartoni_dodeljeni_domačinom + kartoni$kartoni_dodeljeni_gostom)


stevilo_vseh_rumenih_kartonov <-sum(Sezone$Rumeni_karton_domači, na.rm = TRUE) +sum(Sezone$Rumeni_karton_gostje, na.rm = TRUE)
stevilo_vseh_rumenih_kartonov_domaci <- sum(Sezone$Rumeni_karton_domači, na.rm = TRUE)
stevilo_vseh_rumenih_kartonov_gostje <- sum(Sezone$Rumeni_karton_gostje, na.rm = TRUE)

stevilo_tekem <- Sezone %>% select(Rumeni_karton_domači) %>% count()
stevilo_tekem$n
stevilo_tekem1 <- nrow(Sezone)

povprecno_rumenih_kartonov_domaci <- round(stevilo_vseh_rumenih_kartonov_domaci/stevilo_tekem1,2)
povprecno_rumenih_kartonov_gostje <- round(stevilo_vseh_rumenih_kartonov_gostje/stevilo_tekem1,2)
povprecno_rumenih_kartonov <- round(stevilo_vseh_rumenih_kartonov/stevilo_tekem1,2)



# kolk kartonov dajo vsi sodniki povprečno skup
povprečno_vsi_Sodniki <- data_frame(povprecno_rumenih_kartonov,povprecno_rumenih_kartonov_domaci,povprecno_rumenih_kartonov_gostje)

# kolk kartonov da ker sodnik
Sodniki_rumeni_kartoni <- Sezone %>% 
  group_by(Sodnik) %>%
  summarise(povprecno_domačim_rumen_karton= round(mean_(Rumeni_karton_domači, na.rm=TRUE),2), povprecno_gostom_rumeni_karton = round(mean_(Rumeni_karton_gostje, na.rm= TRUE),2), povprecno_rumeni_karton = round(mean_((Rumeni_karton_gostje+Rumeni_karton_domači), na.rm= TRUE),2)) %>% 
  arrange(povprecno_rumeni_karton)
# kak bi zaj lahk js to nariso oziroma predstavu? fajn bi blo da bi meu tut neko rdečo premico ki kaže povprečno_vsi_sodniki
# To še morem naredit za rdeč karton zraven
# enkrat mam l Mason drgač pa L Mason kak to spremenim? 
# mam nek NA noter sam ga ne najdem, moglo bi bit 3800 dolgo, pa zakaj tu ne dela na.rm?

apply(Sodniki_rumeni_kartoni,2, length)

#Tu sm preveru če sm prau zračuno
# c <- Sezone %>% filter(Sodnik == "L Mason") %>% summarise(stevilo=sum(Rumeni_karton_domači, na.rm = TRUE),kolk_tekm = n())
# c$stevilo/c$kolk_tekm





#Zemljevidi 
# Na vajah smo meli občine in je samo po sebi se izšlo ka pa naj js naredim ko mam angleško ligo? kak zemljevid je fajn uvozit? 

install.packages("maptools")
library(tmap)
source("https://raw.githubusercontent.com/jaanos/APPR-2019-20/master/lib/uvozi.zemljevid.r")


obcine <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
                          pot.zemljevida="OB", encoding="Windows-1250")

names(obcine)
tm_shape(obcine) + tm_polygons("OB_UIME") + tm_legend(show=FALSE)

tmap_options(max.categories=nrow(obcine))
# Iz kere strani je včeri potegno zemljevid Slonokoščene obale

##
#UK <- uvozi.zemljevid("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_GBR_0_sf.rds")




