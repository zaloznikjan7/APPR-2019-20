# # 4. faza: Analiza podatkov


#=========================================================================================================================================================================================
# 1. napredna analiza
Napoved_golov <- Primerjava_zadetkov_domace_gostujoce_ekipe_graf + geom_smooth(method="lm")



#=========================================================================================================================================================================================
# 2. napredna analiza
tocke_domaci_gosti <- inner_join(tocke_domacii, gostujoce_tocke) %>% data.frame()
rownames(tocke_domaci_gosti) <- tocke_domaci_gosti$Ekipa

k <- 5
razdelitev <- tocke_domaci_gosti %>% select(-Ekipa) %>% scale() %>% kmeans(k, nstart=1000)
skupine <- data.frame(Ekipa=tocke_domaci_gosti$Ekipa,
                      Skupina=factor(razdelitev$cluster,
                                     levels=unique(razdelitev$cluster),
                                     labels=1:k),
                      stringsAsFactors=FALSE)


tocke_regije3 <- skupine %>% inner_join(klubi.regija) %>%
  group_by(Regija) %>% summarise(Ekipa=unique(Ekipa) %>% sort() %>% paste(collapse=", "),
                                 Skupina=ifelse(n() == 1, paste(Skupina), "London"))


zem.tocke3 <- merge(EngWal, tocke_regije3, by.x="NAME_2", by.y="Regija")
tocke_brezLondona <- tocke_regije3 %>% filter(Skupina != "London")
zem.tocke3_brezLondona <- merge(EngWal, tocke_brezLondona, by.x="NAME_2", by.y="Regija")



zemljevid_AngWal <- tm_shape(zem.tocke3) + tm_polygons("Skupina") + tm_legend(show=TRUE)

zemljevid_severne_ANGLIJE <- tm_shape(zem.tocke3_brezLondona, xlim=c(-3.2, -1.7), ylim=c(53.3, 53.9)) + tm_polygons("Skupina") +
  tm_legend(show=TRUE) + tm_text("Ekipa", size=0.7)

zemljevid_srednje_ANGLIJE <- tm_shape(zem.tocke3_brezLondona, xlim=c(-2.2, -1), ylim=c(52.3, 52.9)) + tm_polygons("Skupina") +
  tm_legend(show=TRUE) + tm_text("Ekipa", size=0.7)


#=========================================================================================================================================================================================
# Samo v Londonu


tocke_regije4 <- skupine %>% inner_join(klubi.regija2)
zem.tocke4 <- merge(Lond, tocke_regije4, by.x="NAME_3", by.y="Regija")
zemljevid_london_skupin <- tm_shape(zem.tocke4) + tm_polygons("Skupina") + tm_legend(show=TRUE) +
                        tm_text("Ekipa", size=0.7)


 

