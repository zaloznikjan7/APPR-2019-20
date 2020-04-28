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



Napoved_golov <- Primerjava_zadetkov_domace_gostujoce_ekipe_graf + geom_smooth(method="lm")
print(Napoved_golov)


tocke <- Sezone %>%
  transmute(Domaca_ekipa, Gostujoca_ekipa,
            Tocke_domaci=ifelse(Zadetki_domaca_ekipa > Zadetki_gostujoca_ekipa, 3,
                                ifelse(Zadetki_domaca_ekipa == Zadetki_gostujoca_ekipa, 1, 0)),
            Tocke_gostujoci=ifelse(Zadetki_domaca_ekipa < Zadetki_gostujoca_ekipa, 3,
                                   ifelse(Zadetki_domaca_ekipa == Zadetki_gostujoca_ekipa, 1, 0)))
tocke.skupaj <- rbind(select(tocke, Ekipa=Domaca_ekipa, Tocke=Tocke_domaci),
                      select(tocke, Ekipa=Gostujoca_ekipa, Tocke=Tocke_gostujoci)) %>%
                      group_by(Ekipa) %>% summarise(Tocke=sum(Tocke))

tocke_domacii <- tocke %>% group_by(Domaca_ekipa) %>%  summarise(Domače_točke=sum(Tocke_domaci))
gostujoce_tocke <- tocke %>% group_by(Gostujoca_ekipa) %>%  summarise(Domače_točke=sum(Tocke_gostujoci))

# Kak bi lahk iz tega zaj naredu napredno analizo? 