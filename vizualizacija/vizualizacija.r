#=========================================================================================================================================================================================
#1. POVPREcNO sTEVILO DOSEzENIH ZADETKOV DOMAcINOV IN GOSTOV

#1. graf
preimenovanje <- c("Zadetki_domaca_ekipa"="Domači", "Zadetki_gostujoca_ekipa"="Gosti")
povprecje_golov <- Sezone %>% select(Sezona, Zadetki_domaca_ekipa, Zadetki_gostujoca_ekipa) %>%
  gather(key=Gostovanje, value="Zadetki", -Sezona) %>%
  mutate(Gostovanje=preimenovanje[Gostovanje]) %>%
  group_by(Sezona, Gostovanje) %>%
  summarise(Povprecje = mean(Zadetki, na.rm = TRUE))

Primerjava_zadetkov_domace_gostujoce_ekipe_graf <- ggplot(povprecje_golov, aes(x=Sezona, y=Povprecje, color=Gostovanje)) + 
  geom_line() + 
  xlab("Sezona") + ylab("Povprečje zadetkov") + ggtitle("Primerjava zadetkov domače in gostujoče ekipe") +
  theme(panel.background=element_rect(fill="white"), plot.title=element_text(hjust=0.5))
print(Primerjava_zadetkov_domace_gostujoce_ekipe_graf)

Napoved_golov <- Primerjava_zadetkov_domace_gostujoce_ekipe_graf + geom_smooth(method="lm")
print(Napoved_golov)

#=========================================================================================================================================================================================
# Drugi graf (Osvojene tocke v zadnjih 10 sezonah)
zmage <- Sezone %>%
  transmute(Domaca_ekipa, Gostujoca_ekipa,
            Zmaga_domaci<-Zadetki_domaca_ekipa > Zadetki_gostujoca_ekipa,
            Remi<-Zadetki_domaca_ekipa == Zadetki_gostujoca_ekipa,
            Zmaga_gosti<-Zadetki_domaca_ekipa < Zadetki_gostujoca_ekipa)
zmage.skupaj <- rbind(select(zmage, Ekipa=Domaca_ekipa, Zmaga=Zmaga_domaci, Remi, Poraz=Zmaga_gosti),
                      select(zmage, Ekipa=Gostujoca_ekipa, Zmaga=Zmaga_gosti, Remi, Poraz=Zmaga_domaci)) %>%
                      group_by(Ekipa) %>% summarise(Zmage=sum(Zmaga), Remiji=sum(Remi), Porazi=sum(Poraz)) %>%
                      mutate(Tocke = 3*Zmage + Remiji)

print(ggplot(zmage.skupaj, aes(x=reorder(Ekipa, Tocke), y=Tocke)) + geom_col() + coord_flip() + xlab("Ekipa") + ylab("Tocke"))

tocke <- Sezone %>%
  transmute(Domaca_ekipa, Gostujoca_ekipa,
            Tocke_domaci=ifelse(Zadetki_domaca_ekipa > Zadetki_gostujoca_ekipa, 3,
                                ifelse(Zadetki_domaca_ekipa == Zadetki_gostujoca_ekipa, 1, 0)),
            Tocke_gostujoci=ifelse(Zadetki_domaca_ekipa < Zadetki_gostujoca_ekipa, 3,
                                   ifelse(Zadetki_domaca_ekipa == Zadetki_gostujoca_ekipa, 1, 0)))
tocke.skupaj <- rbind(select(tocke, Ekipa=Domaca_ekipa, Tocke=Tocke_domaci),
                      select(tocke, Ekipa=Gostujoca_ekipa, Tocke=Tocke_gostujoci)) %>%
  group_by(Ekipa) %>% summarise(Tocke=sum(Tocke))
#=========================================================================================================================================================================================
# 3. graf
# diagram, ki kaze da dobijo gostje veliko vec rumenih kartonov kot domaci // to bi se lahk naredu po sezonah in eno skupno bi blo bolj zanimivo  
kartoni <- Sezone %>% summarise("Domaci" = sum(Rumeni_karton_domaci, na.rm = TRUE), "Gostje" = sum(Rumeni_karton_gostje, na.rm = TRUE))
#pripravljeni podatki
stevilo_vseh_rumenih_kartonov_domaci <- sum(Sezone$Rumeni_karton_domaci, na.rm = TRUE)
stevilo_vseh_rumenih_kartonov_gostje <- sum(Sezone$Rumeni_karton_gostje, na.rm = TRUE)
vsi_kartoni <- sum(kartoni$Domaci + kartoni$Gostje)
procent <- c(stevilo_vseh_rumenih_kartonov_domaci/vsi_kartoni, stevilo_vseh_rumenih_kartonov_gostje/ vsi_kartoni)
kartoni2 <- kartoni %>% gather(key= "Ekipa", value = "stevilo_kartonov")
##### 3 graf #####
stolpicni_graf_prejetih_rumenih_kartonov <- ggplot(kartoni2,  aes(x= Ekipa, y = stevilo_kartonov, fill=Ekipa )) +
                              geom_col() + geom_text(aes(label = scales::percent(procent), y = procent, group = stevilo_kartonov),
                              position = position_dodge(width = 1.5), vjust = -0.5) +
                              ylab("stevilo kartonov") + ggtitle("Primerjava prejetih rumenih kartonov domace in gostujoce ekipe") +
                              theme(panel.background=element_rect(fill="white"), plot.title=element_text(hjust=0.5))
print(stolpicni_graf_prejetih_rumenih_kartonov)




# kolk kartonov dajo vsi sodniki povprecno skup
stevilo_tekem <- nrow(Sezone)
stevilo_vseh_rumenih_kartonov <- stevilo_vseh_rumenih_kartonov_domaci + stevilo_vseh_rumenih_kartonov_gostje
povprecno_rumenih_kartonov_domaci <- round(stevilo_vseh_rumenih_kartonov_domaci/stevilo_tekem,2)
povprecno_rumenih_kartonov_gostje <- round(stevilo_vseh_rumenih_kartonov_gostje/stevilo_tekem,2)
povprecno_rumenih_kartonov <- round(stevilo_vseh_rumenih_kartonov/stevilo_tekem,2)

povprecno_vsi_Sodniki <- tibble(povprecno_rumenih_kartonov,povprecno_rumenih_kartonov_domaci,povprecno_rumenih_kartonov_gostje)

# kolk kartonov da ker sodnik
Sodniki_rumeni_kartoni <- Sezone %>%
  group_by(Sodnik) %>%
  summarise(povprecno_domacim_rumen_karton= round(mean_(Rumeni_karton_domaci, na.rm=TRUE),2), povprecno_gostom_rumeni_karton = round(mean_(Rumeni_karton_gostje, na.rm= TRUE),2), povprecno_rumeni_karton = round(mean_((Rumeni_karton_gostje+Rumeni_karton_domaci), na.rm= TRUE),2)) %>% 
  arrange(povprecno_rumeni_karton)

#### 4. graf, koliko rumenih kartonov dodeli sodnik na posamezni tekmi ####
ggplot(Sodniki_rumeni_kartoni,aes(x= Sodniki_rumeni_kartoni$Sodnik,y= Sodniki_rumeni_kartoni$povprecno_rumeni_karton))+ geom_col() + coord_flip()+
  geom_hline(aes(yintercept= povprecno_rumenih_kartonov, color = "red"))+ xlab("Sodnik") +
  ylab("stevilo rumenih kartonov") + ggtitle("Število dodeljenih rumenih kartonov v povprečju") + labs(color="Legenda")  
  

####5 . graf (Povprecno rdeci kartoni) ####

stevilo_vseh_rdecih_kartonov_domaci <- sum(Sezone$Rdec_karton_domaci, na.rm = TRUE)
stevilo_vseh_rdecih_kartonov_gostje <- sum(Sezone$Rdec_karton_gostje, na.rm = TRUE)
rdeci_kartoni <- Sezone %>% summarise("Domaci" = sum(Rdec_karton_domaci, na.rm = TRUE), "Gostje" = sum(Rdec_karton_gostje, na.rm = TRUE))
vsi_kartoni2 <- sum(rdeci_kartoni$Domaci + rdeci_kartoni$Gostje)
rdec_procent <- c(stevilo_vseh_rdecih_kartonov_domaci/vsi_kartoni2, stevilo_vseh_rdecih_kartonov_gostje/ vsi_kartoni2)
rdec_kartoni2 <- rdeci_kartoni %>% gather(key= "Ekipa", value = "stevilo_kartonov")

##### 5 graf ##### RDECI KARTONI
stolpicni_graf_prejetih_rdecih_kartonov <- ggplot(rdec_kartoni2,  aes(x= Ekipa, y = stevilo_kartonov, fill=Ekipa )) +
  geom_col() + geom_text(aes(label = scales::percent(rdec_procent), group = stevilo_kartonov),
                         position = position_dodge(width = 1), vjust = -1) +
  ylab("Število kartonov") + ggtitle("Primerjava prejetih rdečih kartonov domače in gostujoče ekipe") +
  theme(panel.background=element_rect(fill="white"), plot.title=element_text(hjust=0.5))
print(stolpicni_graf_prejetih_rdecih_kartonov)




#prekrški 
Prekrski_domacini <- Sezone %>% select(Domaca_ekipa, PrekrSki_domaci) %>%
  gather(-Domaca_ekipa, key = ekipa, value = stevilo_prekrskov) %>% group_by(Domaca_ekipa) %>% 
  summarise(skupek = sum(stevilo_prekrskov))# %>% rename("Ekipa"= Domaca_ekipa)
Prekrski_gostjee <- Sezone %>% select(Gostujoca_ekipa, PrekrSki_gostje) %>%
  gather(-Gostujoca_ekipa, key = ekipa, value = stevilo_prekrskov) %>% group_by(Gostujoca_ekipa) %>% 
  summarise(skupek = sum(stevilo_prekrskov))# %>% rename("Ekipa"= Gostujoca_ekipa)

Prekrski_skupaj <- rbind(select(Prekrski_domacini, Ekipa=Domaca_ekipa, prek= skupek),
                        select(Prekrski_gostjee, Ekipa=Gostujoca_ekipa, prek= skupek)) %>%
                        group_by(Ekipa) %>% summarise("Prekrški"=sum(prek))
print(ggplot(Prekrski_skupaj, aes(x=reorder(Ekipa, Prekrški), y=Prekrški)) + geom_point() + xlab("Ekipa")+ coord_flip() + ylab("Število prekrškov"))



# Graf kolk dni je kdo trener

Trenutni_trenerji$razlika_v_dnevih <-  as.Date(as.character(Sys.Date()), format="%Y-%m-%d")-
  as.Date(as.character((Trenutni_trenerji$Appointed)), format="%Y-%m-%d")

print(ggplot(Trenutni_trenerji, aes(x=as.numeric(razlika_v_dnevih), y= Name)) + geom_point())+  ylab("Trener")+ xlab("Število dni")

# Graf od ustanovitve premier lige, najuspešnejši trenerji

stevilo_naslovov <- NajbolSi_trenerji %>% group_by(Trener) %>% tally()
print(ggplot(stevilo_naslovov, aes(x=Trener, y=n)) + geom_point())+ coord_flip() + ylab("Število naslovov")


# Vprašanja: Graf 2  in Graf 4 pri legendi 
#=========================================================================================================================================================================================
#=========================================================================================================================================================================================

#Zemljevidi 
# Na vajah smo meli obcine in je samo po sebi se izslo ka pa naj js naredim ko mam anglesko ligo? kak zemljevid je fajn uvozit? 


#library(tmap)
#source("https://raw.githubusercontent.com/jaanos/APPR-2019-20/master/lib/uvozi.zemljevid.r")


#obcine <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
#                          pot.zemljevida="OB", encoding="Windows-1250")

#names(obcine)
#tm_shape(obcine) + tm_polygons("OB_UIME") + tm_legend(show=FALSE)

#tmap_options(max.categories=nrow(obcine))
# Iz kere strani je vceri potegno zemljevid Slonokoscene obale

##
#UK <- uvozi.zemljevid("https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_GBR_shp.zip", "gadm36_GBR_1",
#                      encoding="UTF-8")

#tm_shape(UK) + tm_polygons("NAME_1") + tm_legend(show=FALSE)

#UK.ggplot <- fortify(UK)






