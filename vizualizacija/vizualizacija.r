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

#1. graf
preimenovanje <- c("Zadetki_domača_ekipa"="Domači", "Zadetki_gostujoca_ekipa"="Gosti")
povprečje_golov <- Sezone %>% select(Sezona, Zadetki_domača_ekipa, Zadetki_gostujoca_ekipa) %>%
  gather(key=Gostovanje, value="Zadetki", -Sezona) %>%
  mutate(Gostovanje=preimenovanje[Gostovanje]) %>%
  group_by(Sezona, Gostovanje) %>%
  summarise(Povprecje = mean(Zadetki, na.rm = TRUE))

Primerjava_zadetkov_domace_gostujoce_ekipe_graf <- ggplot(povprečje_golov, aes(x=Sezona, y=Povprecje, color=Gostovanje)) + 
  geom_line() + 
  xlab("Sezona") + ylab("Povprečje zadetkov") + ggtitle("Primerjava zadetkov domače in gostujoče ekipe") +
  theme(panel.background=element_rect(fill="white"), plot.title=element_text(hjust=0.5))
print(Primerjava_zadetkov_domace_gostujoce_ekipe_graf)

Napoved_golov <- Primerjava_zadetkov_domace_gostujoce_ekipe_graf + geom_smooth(method="lm")
print(Napoved_golov)
# Kak naredit da maš dva različna grafa

# novi3 <- ggplot(povprečje_golov, aes(x=Sezona, y=Povprecje)) + 
#   geom_line() + 
#   xlab("Sezona") + ylab("Povprečje zadetkov") + ggtitle("Primerjava zadetkov domače in gostujoče ekipe") +
#   theme(panel.background=element_rect(fill="white"), plot.title=element_text(hjust=0.5)) +
#   facet_grid(cols=vars(Gostovanje)) # tu lahk daš al cols= al pa rows =
# print(novi3)

#=========================================================================================================================================================================================
# Drugi graf (Osvojene točke v zadnjih 10 sezonah)
zmage <- Sezone %>%
  transmute(Domača_ekipa, Gostujoča_ekipa,
            Zmaga_domaci=Zadetki_domača_ekipa > Zadetki_gostujoca_ekipa,
            Remi=Zadetki_domača_ekipa == Zadetki_gostujoca_ekipa,
            Zmaga_gosti=Zadetki_domača_ekipa < Zadetki_gostujoca_ekipa)
zmage.skupaj <- rbind(select(zmage, Ekipa=Domača_ekipa, Zmaga=Zmaga_domaci, Remi, Poraz=Zmaga_gosti),
                      select(zmage, Ekipa=Gostujoča_ekipa, Zmaga=Zmaga_gosti, Remi, Poraz=Zmaga_domaci)) %>%
                      group_by(Ekipa) %>% summarise(Zmage=sum(Zmaga), Remiji=sum(Remi), Porazi=sum(Poraz)) %>%
                      mutate(Tocke = 3*Zmage + Remiji)

ggplot(zmage.skupaj, aes(x=reorder(Ekipa, Tocke), y=Tocke)) + geom_col() + coord_flip() + xlab("Ekipa") + ylab("Točke")

tocke <- Sezone %>%
  transmute(Domača_ekipa, Gostujoča_ekipa,
            Tocke_domaci=ifelse(Zadetki_domača_ekipa > Zadetki_gostujoca_ekipa, 3,
                                ifelse(Zadetki_domača_ekipa == Zadetki_gostujoca_ekipa, 1, 0)),
            Tocke_gostujoci=ifelse(Zadetki_domača_ekipa < Zadetki_gostujoca_ekipa, 3,
                                   ifelse(Zadetki_domača_ekipa == Zadetki_gostujoca_ekipa, 1, 0)))
tocke.skupaj <- rbind(select(tocke, Ekipa=Domača_ekipa, Tocke=Tocke_domaci),
                      select(tocke, Ekipa=Gostujoča_ekipa, Tocke=Tocke_gostujoci)) %>%
  group_by(Ekipa) %>% summarise(Tocke=sum(Tocke))
#=========================================================================================================================================================================================
# 3. graf
# diagram, ki kaže da dobijo gostje veliko več rumenih kartonov kot domači // to bi še lahk naredu po sezonah in eno skupno bi blo bolj zanimivo  
########kartoni <- Sezone %>% summarise("Domači" = sum(Rumeni_karton_domači, na.rm = TRUE), "Gostje" =sum(Rumeni_karton_gostje, na.rm = TRUE))
#pripravljeni podatki
stevilo_vseh_rumenih_kartonov_domaci <- sum(Sezone$Rumeni_karton_domači, na.rm = TRUE)
stevilo_vseh_rumenih_kartonov_gostje <- sum(Sezone$Rumeni_karton_gostje, na.rm = TRUE)
vsi_kartoni <- sum(kartoni$Domači + kartoni$Gostje)
procent <- c(stevilo_vseh_rumenih_kartonov_domaci/vsi_kartoni, stevilo_vseh_rumenih_kartonov_gostje/ vsi_kartoni)
kartoni2 <- kartoni %>% gather(key= "Ekipa", value = "Število_kartonov")
##### 3 graf #####
stolpični_graf_prejetih_rumenih_kartonov <- ggplot(kartoni2,  aes(x= Ekipa, y = Število_kartonov, fill=Ekipa )) +
                              geom_col() + geom_text(aes(label = scales::percent(procent), y = procent, group = Število_kartonov),
                              position = position_dodge(width = 1.5), vjust = -0.5) +
                              ylab("Število kartonov") + ggtitle("Primerjava prejetih rumenih kartonov domače in gostujoče ekipe") +
                              theme(panel.background=element_rect(fill="white"), plot.title=element_text(hjust=0.5))
print(stolpični_graf_prejetih_rumenih_kartonov)




porazdelitvena_funkcija_rumenih_kartonov_domaci <- Sezone %>% select(Rumeni_karton_domači) %>% count()
stevilo_tekem <- nrow(Sezone)





# kolk kartonov dajo vsi sodniki povprečno skup
povprecno_rumenih_kartonov_domaci <- round(stevilo_vseh_rumenih_kartonov_domaci/stevilo_tekem1,2)
povprecno_rumenih_kartonov_gostje <- round(stevilo_vseh_rumenih_kartonov_gostje/stevilo_tekem1,2)
povprecno_rumenih_kartonov <- round(stevilo_vseh_rumenih_kartonov/stevilo_tekem1,2)

povprečno_vsi_Sodniki <- tibble(povprecno_rumenih_kartonov,povprecno_rumenih_kartonov_domaci,povprecno_rumenih_kartonov_gostje)

# kolk kartonov da ker sodnik
Sodniki_rumeni_kartoni <- Sezone %>% 
  group_by(Sodnik) %>%
  summarise(povprecno_domačim_rumen_karton= round(mean_(Rumeni_karton_domači, na.rm=TRUE),2), povprecno_gostom_rumeni_karton = round(mean_(Rumeni_karton_gostje, na.rm= TRUE),2), povprecno_rumeni_karton = round(mean_((Rumeni_karton_gostje+Rumeni_karton_domači), na.rm= TRUE),2)) %>% 
  arrange(povprecno_rumeni_karton)

# 4. graf, koliko rumenih kartonov dodeli sodnik na posamezni tekmi
ggplot(Sodniki_rumeni_kartoni,aes(x= Sodniki_rumeni_kartoni$Sodnik,y= Sodniki_rumeni_kartoni$povprecno_rumeni_karton))+ geom_col() + coord_flip()+
  geom_hline(aes(yintercept= povprecno_rumenih_kartonov, color = "red"))+ xlab("Sodnik") +
  ylab("Število rumenih kartonov") + ggtitle("Število dodeljenih rumenih kartonov v povprečju") + labs(color="Legenda")  
  


# kak bi zaj lahk js to nariso oziroma predstavu? fajn bi blo da bi meu tut neko rdečo premico ki kaže povprečno_vsi_sodniki
# To še morem naredit za rdeč karton zraven

apply(Sodniki_rumeni_kartoni,2, length)

#Tu sm preveru če sm prau zračuno
# c <- Sezone %>% filter(Sodnik == "L Mason") %>% summarise(stevilo=sum(Rumeni_karton_domači, na.rm = TRUE),kolk_tekm = n())
# c$stevilo/c$kolk_tekm







#prekrški 
# Prekrski <- Sezone %>% group_by(Prekrški_domači, Prekrški_gostje) %>% 
#   summarise(Prekskii= Prekrški_domači+Prekrški_gostje)




#=========================================================================================================================================================================================
#=========================================================================================================================================================================================

#Zemljevidi 
# Na vajah smo meli občine in je samo po sebi se izšlo ka pa naj js naredim ko mam angleško ligo? kak zemljevid je fajn uvozit? 


#library(tmap)
#source("https://raw.githubusercontent.com/jaanos/APPR-2019-20/master/lib/uvozi.zemljevid.r")


#obcine <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
#                          pot.zemljevida="OB", encoding="Windows-1250")

#names(obcine)
#tm_shape(obcine) + tm_polygons("OB_UIME") + tm_legend(show=FALSE)

#tmap_options(max.categories=nrow(obcine))
# Iz kere strani je včeri potegno zemljevid Slonokoščene obale

##
#UK <- uvozi.zemljevid("https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_GBR_shp.zip", "gadm36_GBR_1",
#                      encoding="UTF-8")

#tm_shape(UK) + tm_polygons("NAME_1") + tm_legend(show=FALSE)

#UK.ggplot <- fortify(UK)






