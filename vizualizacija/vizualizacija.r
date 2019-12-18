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

povprecno_golov <- ggplot(Sezone %>% filter(Sezona == "2009")) + aes(x=1:380 ,y=Zadetki_domača_ekipa) + geom_point()
povprecno_golov

povprečje_golov_domači <- Sezone %>% group_by(Sezona) %>% summarise(Povprecje = mean(Zadetki_domača_ekipa, na.rm = TRUE))
graf1 <-  ggplot(povprečje_golov_domači) %>% + aes(x = Sezona ,y = Povprecje) + geom_path()
print(graf1 + theme(panel.background=element_rect(fill="white")))

povprecno_golov_gostje <- Sezone %>% group_by(Sezona) %>% summarise(Povprecje = mean(Zadetki_gostujoca_ekipa, na.rm = TRUE))
graf2 <-  ggplot(povprecno_golov_gostje) %>% + aes(x = Sezona ,y = Povprecje) + geom_path()
print(graf2 +theme(panel.background=element_rect(fill="white")))


povprečje_golov <- Sezone %>% group_by(Sezona) %>% summarise(Povprecje1 = mean(Zadetki_domača_ekipa, na.rm = TRUE), Povprecje2 = mean(Zadetki_gostujoca_ekipa,na.rm= TRUE))
graf1 <- povprečje_golov %>%
  ggplot(aes(x = Sezona ,y = Povprecje1)) + 
  geom_path(color="blue") + 
  geom_path(x = Povprecje2)
graf2 <- ggplot(povprečje_golov) %>% + aes(x = Sezona , y= Povprecje2) + geom_path(color= "red")
print(graf2 + theme(panel.background=element_rect(fill="white")))


novi <- ggplot(data = povprečje_golov, aes(x = Sezona, y = Povprecje1)) + geom_path()

novi2 <- ggplot(povprečje_golov, aes(Sezona)) + 
  geom_line(aes(y = Povprecje1), color = "red") + 
  geom_line(aes(y = Povprecje2), color = "blue") +
  xlab("Sezona") + ylab("Povprečje zadetkov") + ggtitle("Primerjava zadetkov domače in gostujoče ekipe") +
  legend(x= 0.5,y = 1.5, legend = c("Koliko golov dosežejo domači", "Koliko golov dosežejo gostje"), fill = c(4,2)) +
  theme(panel.background=element_rect(fill="white"))
print(novi2)

# kak lahk kličem zaj js Sezone? Kak se ggtitle prestavi na sredino? Kak se naredi legenda?
# Kak lahk dva grafa skup narišem? se pravi graf1 in graf2 skup na isti sliki? Kak se to naredi s dplyr
# Sj je to mišljeno kot da tak analiziram jl? Pa da sm zaj opazu tut neki kar prej nism vedo in sicer to da narašča število golov v gosteh


# Kak bi naredu da bi zbralo eno ekipo in pol pogledlo vrstico in verjetno z if stavkom torej če zadetki_domača_ekipa > zadetki_gostujoca_ekipa pol prištej ena
# Torej rad bi preštel vse ko so zmagali doma in kolk zgubili, ostalo so tak mogli igrat izenačeno
# torej na koncu bi rad dau na graf x os je vsak klub in pol nek histogram kolk so meli zmag, novi graf pa kolk so igrali izenačeno pa še en graf kok so zgubili 



# 3. graf
# Preštejem vse rumene kartone vsake domače ekipe in dam zraven vsoto vseh gostujočih ekip 

