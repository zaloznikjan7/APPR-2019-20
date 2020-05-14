# # 4. faza: Analiza podatkov


#=========================================================================================================================================================================================
# 1. napredna analiza
Napoved_golov <- Primerjava_zadetkov_domace_gostujoce_ekipe_graf + geom_smooth(method="lm")



#=========================================================================================================================================================================================
# 2. napredna analiza
tocke_domaci_gosti <- inner_join(tocke_domacii, gostujoce_tocke) %>% data.frame()
rownames(tocke_domaci_gosti) <- tocke_domaci_gosti$Ekipa
razdelitev <- tocke_domaci_gosti %>% select(-Ekipa) %>% scale() %>% kmeans(5)


skupine <- data.frame(Ekipa=tocke_domaci_gosti$Ekipa, Skupina=factor(razdelitev$cluster),
                      stringsAsFactors=FALSE)
tocke_regije3 <- skupine %>% inner_join(klubi.regija) %>%
  group_by(Regija) %>% summarise(Ekipa=unique(Ekipa) %>% sort() %>% paste(collapse=", "),
                                 Skupina=unique(Skupina) %>% sort() %>% paste(collapse=", "),
                                 Stevilo=n())


zem.tocke3 <- merge(EngWal, tocke_regije3, by.x="NAME_2", by.y="Regija")
zemljevid_skupin <- tm_shape(zem.tocke3) + tm_polygons("Skupina")

zemljevid_severne_ANGLIJE <- tm_shape(zem.tocke3, xlim=c(-3.2, -1.7), ylim=c(53.3, 53.9)) + tm_polygons("Skupina") +
  tm_legend(show=TRUE) + tm_text("Ekipa", size=0.7)

zemljevid_srednje_ANGLIJE <- tm_shape(zem.tocke3, xlim=c(-2.2, -1), ylim=c(52.3, 52.9)) + tm_polygons("Skupina") +
  tm_legend(show=TRUE) + tm_text("Ekipa", size=0.7)


#=========================================================================================================================================================================================
# Samo v Londonu


tocke_regije4 <- skupine %>% inner_join(klubi.regija2)
zem.tocke4 <- merge(Lond, tocke_regije4, by.x="NAME_3", by.y="Regija")
zemljevid_london_skupin <- tm_shape(zem.tocke4) + tm_polygons("Skupina") + tm_legend(show=TRUE) +
                        tm_text("Ekipa", size=0.7)


 

