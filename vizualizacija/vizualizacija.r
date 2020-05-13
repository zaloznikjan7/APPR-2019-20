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


#=========================================================================================================================================================================================
# Drugi graf (Osvojene tocke v zadnjih 10 sezonah)
zmage <- Sezone %>%
  transmute(Domaca_ekipa, Gostujoca_ekipa,
            Zmaga_domaci = Zadetki_domaca_ekipa > Zadetki_gostujoca_ekipa,
            Remi=Zadetki_domaca_ekipa == Zadetki_gostujoca_ekipa,
            Zmaga_gosti= Zadetki_domaca_ekipa < Zadetki_gostujoca_ekipa)
zmage.skupaj <- rbind(select(zmage, Ekipa=Domaca_ekipa, Zmaga=Zmaga_domaci, Remi, Poraz=Zmaga_gosti),
                      select(zmage, Ekipa=Gostujoca_ekipa, Zmaga=Zmaga_gosti, Remi, Poraz=Zmaga_domaci)) %>%
                      group_by(Ekipa) %>% summarise(Zmage=sum(Zmaga), Remiji=sum(Remi), Porazi=sum(Poraz)) %>%
                      mutate(Tocke = 3*Zmage + Remiji)

Najuspesnejse_ekipe <- (ggplot(zmage.skupaj, aes(x=reorder(Ekipa, Tocke), y=Tocke)) + geom_col() + coord_flip() + xlab("Ekipa") + ylab("Tocke"))


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


#=========================================================================================================================================================================================


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
  geom_hline(aes(yintercept = povprecno_rumenih_kartonov), color = "red")+ xlab("Sodnik") +
  ylab("stevilo rumenih kartonov") + ggtitle("Število dodeljenih rumenih kartonov v povprečju")
  
#=========================================================================================================================================================================================
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



#=========================================================================================================================================================================================

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
graf_prekrskov <- ggplot(Prekrski_skupaj, aes(x=reorder(Ekipa, Prekrški), y=Prekrški)) + geom_point() + xlab("Ekipa")+ coord_flip() + ylab("Število prekrškov")


#=========================================================================================================================================================================================
# Graf kolk dni je kdo trener

Trenutni_trenerji$razlika_v_dnevih <-  as.Date(as.character(Sys.Date()), format="%Y-%m-%d")-
  as.Date(as.character((Trenutni_trenerji$Appointed)), format="%Y-%m-%d")

Trenutna_trenerska_doba <- (ggplot(Trenutni_trenerji, aes(x=as.numeric(razlika_v_dnevih), y= Name)) + geom_point())+  ylab("Trener")+ xlab("Število dni")

#=========================================================================================================================================================================================
# Graf od ustanovitve premier lige, najuspešnejši trenerji

stevilo_naslovov <- NajbolSi_trenerji %>% group_by(Trener) %>% tally()
Najuspesnejsi_trenerji <- (ggplot(stevilo_naslovov, aes(x=Trener, y=n)) + geom_point())+ coord_flip() + ylab("Število naslovov")




#=========================================================================================================================================================================================
#=========================================================================================================================================================================================

#Zemljevidi 




tocke <- Sezone %>%
  transmute(Domaca_ekipa, Gostujoca_ekipa,
            Tocke_domaci=ifelse(Zadetki_domaca_ekipa > Zadetki_gostujoca_ekipa, 3,
                                ifelse(Zadetki_domaca_ekipa == Zadetki_gostujoca_ekipa, 1, 0)),
            Tocke_gostujoci=ifelse(Zadetki_domaca_ekipa < Zadetki_gostujoca_ekipa, 3,
                                   ifelse(Zadetki_domaca_ekipa == Zadetki_gostujoca_ekipa, 1, 0)))
tocke.skupaj <- rbind(select(tocke, Ekipa=Domaca_ekipa, Tocke=Tocke_domaci),
                      select(tocke, Ekipa=Gostujoca_ekipa, Tocke=Tocke_gostujoci)) %>%
  group_by(Ekipa) %>% summarise(Tocke=sum(Tocke))

tocke_domacii <- tocke %>% group_by(Domaca_ekipa) %>%
  summarise(Domace_tocke=sum(Tocke_domaci)) %>% rename(Ekipa=Domaca_ekipa)
gostujoce_tocke <- tocke %>% group_by(Gostujoca_ekipa) %>%
  summarise(Gostujoce_tocke=sum(Tocke_gostujoci)) %>% rename(Ekipa=Gostujoca_ekipa)



UK <- uvozi.zemljevid("https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_GBR_shp.zip", "gadm36_GBR_2",
                      encoding="UTF-8")
klubi <- c("Man United"="Manchester",
           "Man City" = "Salford",
           "Chelsea"="Birmingham",
           "Tottenham"="Bolton",
           "Stoke City" = "Stoke-on-Trent",
           "Bournemouth" = "Bournemouth",
           "Leicester" = "Leicester",
           "Middlesbrough" = "Middlesbrough",
           "Southampton" = "Southampton",
           "Cardiff" = "Cardiff",
           "Swansea" = "Swansea",
           "Bolton" = "Bolton",
           "Brighton" = "Brighton and Hove",
           "Birmingham" = "Birmingham",
           "Blackpool" = "Blackpool",
           "Blackburn" ="Blackburn with Darwen",
           "Bolton" = "Bolton",
           "Reading" = "Reading",
           "Bristol" = "Bristol",
           "Blackpool" = "Blackpool",
           "Huddersfield" = "Calderdale",
           "Everton" = "Sefton",
           "Aston Villa" = "Coventry",
           "Newcastle" = "Newcastle upon Tyne",
           "Wigan" = "Wigan",
           "Wolverhampton" = "Wolverhampton",
           "Sunderland" = "Sunderland", 
           "West Brom" = "Dudley",
           "Liverpool" = "Saint Helens",
           "Hull" = "Kingston upon Hull",
           "Watford" = "Luton",
           "Burnley" = "Bury",
           "Norwich" = "Peterborough",
           "Arsenal" = "Greater London",
           "West Ham" = "Greater London",
           "Fulham" = "Greater London",
           "Chelsea"= "Greater London",
           "Crystal Palace" = "Greater London",
           "QPR" = "Greater London",
           "Tottenham" = "Greater London",
           "Watford" = "Greater London",
           "Wolves" = "Wolverhampton"
           
           )
klubi.regija <- data.frame(Ekipa=names(klubi),
                           Regija=parse_factor(klubi, levels(UK$NAME_2)),
                           stringsAsFactors=FALSE)
EngWal <- UK[UK$NAME_1 %in% c("England", "Wales"),]

tocke.regije <- tocke.skupaj %>% inner_join(klubi.regija) %>%
  group_by(Regija) %>% summarise(Tocke=mean(Tocke))

zem.tocke <- merge(EngWal, tocke.regije, by.x="NAME_2", by.y="Regija")

zemljevid_EngWal <- tm_shape(zem.tocke) + tm_polygons("Tocke") + tm_legend(show=TRUE)






# LONDON

UK2 <- uvozi.zemljevid("https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_GBR_shp.zip", "gadm36_GBR_3",
                      encoding="UTF-8")

klubi2 <- c(  "Arsenal" = "Hackney",
              "West Ham" = "Barking and Dagenham",
              "Fulham" = "Hammersmith and Fulham",
              "Chelsea"= "Kensington and Chelsea",
              "Crystal Palace" = "Croydon",
              "QPR" = "Ealing",
              "Tottenham" = "Enfield",
              "Watford" = "Harrow"
                )

Lond <- UK2[UK2$NAME_2 %in% c("Greater London"),]

klubi.regija2 <- data.frame(Ekipa=names(klubi2),
                           Regija=parse_factor(klubi2, levels(UK2$NAME_3)),
                           stringsAsFactors=FALSE)
tocke.regije2 <- tocke.skupaj %>% inner_join(klubi.regija2) #%>%
 #group_by(Regija) %>% summarise(Tocke=mean(Tocke))

zem.tocke2 <- merge(Lond, tocke.regije2, by.x="NAME_3", by.y="Regija")


zemljevi_london <- tm_shape(zem.tocke2) + tm_polygons("Tocke") + tm_legend(show=TRUE)






#### Moja ideja
# install.packages("rworldxtra")
# newmap <- getMap(resolution = "high")
# plot(newmap, xlim = c(-5, 0), ylim = c(50, 58), asp = 1)
# 
# stadioni
# iz_sezone_2018 <- Sezone %>%  filter(Sezona == 2018)
# Seznam_ekip <- iz_sezone_2018 %>% select(Domaca_ekipa) %>% group_by(Domaca_ekipa) %>% count() %>% select(Domaca_ekipa)
# mesta <- c("Liverpool", "Manchester United", "Newcastle")
# 
# tocke <- Sezone %>%
#   transmute(Domaca_ekipa, Gostujoca_ekipa,
#             Tocke_domaci=ifelse(Zadetki_domaca_ekipa > Zadetki_gostujoca_ekipa, 3,
#                                 ifelse(Zadetki_domaca_ekipa == Zadetki_gostujoca_ekipa, 1, 0)),
#             Tocke_gostujoci=ifelse(Zadetki_domaca_ekipa < Zadetki_gostujoca_ekipa, 3,
#                                    ifelse(Zadetki_domaca_ekipa == Zadetki_gostujoca_ekipa, 1, 0)))
# tocke.skupaj <- rbind(select(tocke, Ekipa=Domaca_ekipa, Tocke=Tocke_domaci),
#                       select(tocke, Ekipa=Gostujoca_ekipa, Tocke=Tocke_gostujoci)) %>%
#   group_by(Ekipa) %>% summarise(Tocke=sum(Tocke))
# 
# lon <- c( 53.400002, 52, 51 )
# lat <- c(-2.983333,-1, -2)
# Koordinate <- data.frame(mesta,lon, lat)
# 
# points(Koordinate$lat, Koordinate$lon, col = "red")


