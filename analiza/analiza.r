# # 4. faza: Analiza podatkov
# 
# podatki <- obcine %>% transmute(obcina, povrsina, gostota,
#                                 gostota.naselij=naselja/povrsina) %>%
#   left_join(povprecja, by="obcina")
# row.names(podatki) <- podatki$obcina
# podatki$obcina <- NULL
# 
# # Število skupin
# n <- 5
# skupine <- hclust(dist(scale(podatki))) %>% cutree(n)


#=========================================================================================================================================================================================
# 1. napredna analiza
Napoved_golov <- Primerjava_zadetkov_domace_gostujoce_ekipe_graf + geom_smooth(method="lm")



#=========================================================================================================================================================================================
# 2. napredna analiza
tocke_domaci_gosti <- inner_join(tocke_domacii, gostujoce_tocke)
rownames(tocke.domaci.gosti) <- tocke_domaci_gosti$Ekipa
razdelitev <- tocke_domaci_gosti %>% select(-Ekipa) %>% scale() %>% kmeans(5)
skupine <- data.frame(Ekipa=tocke_domaci_gosti$Ekipa, Skupina=factor(razdelitev$cluster),
                      stringsAsFactors=FALSE)


tocke_regije3 <- skupine %>% inner_join(klubi.regija) %>% select(Regija, Skupina)
zem.tocke3 <- merge(EngWal, tocke_regije3, by.x="NAME_2", by.y="Regija")
zemljevid_skupin <- tm_shape(zem.tocke3) + tm_polygons("Skupine") + tm_legend(show=FALSE)

#=========================================================================================================================================================================================
# Samo v Londonu


tocke_regije4 <- skupine %>% inner_join(klubi.regija3)
zem.tocke4 <- merge(Lond, tocke_regije4, by.x="NAME_3", by.y="Regija")
zemljevid_london_skupin <- tm_shape(zem.tocke4) + tm_polygons("Skupine") + tm_legend(show=TRUE)


 

