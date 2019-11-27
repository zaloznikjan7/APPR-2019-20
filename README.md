# Analiza podatkov s programom R, 2019/20
# Jan Založnik
Repozitorij z gradivi pri predmetu APPR v študijskem letu 2019/20

* [![Shiny](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/jaanos/APPR-2019-20/master?urlpath=shiny/APPR-2019-20/projekt.Rmd) Shiny
* [![RStudio](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/jaanos/APPR-2019-20/master?urlpath=rstudio) RStudio


## Tematika
# Analiza angleške prve lige

Pri tem projektu bom analizira prvo angleško ligo v zadnjih letih in dodal še podrobno analizo Manchester Uniteda. V analizi Premier league bom analiziral koliko boljše so ekipe na domači zelenici kot pa v gosteh, število golov na tekmo, število strelov v in izven okvira vrat, število kotov, število rumenih in rdečih kartonov in še več pomembnih statističnih podatkov. Dodal bom pa tudi sodnike, ki so na vsaki tekmi zelo pomemben dejavnik, čeprav so letos v ligi uvedli VAR(video assisting referee) so na koncu vse odločitve še vedno odvisne od glavnega sodnika tekme.

Poglobil se bom v ekipo Manchester Uniteda in analiziral osnovne nogometne statistike vsakega igralca ter dodal tržno vrednost posameznikov. Tako bom tudi ocenil vrednost kluba skozi leta in prikazal tudi nekaj najdražjih nakupov ter prodaj.

Cilj moje projektne naloge je analizirati "najboljšo" ligo na svetu in ugotoviti, zakaj je ravno ta liga po mnenju mnogih najboljša in najmanj predvidljiva. Podroben pogled bom pa namenil Manchester Unitedu, ki je v zadnjih nekaj letih naredil veliko strateških napak in posledično se to pozna na vrednosti ekipe, torej me zanima, če in koliko so padle cene igralcev. Zanima me tudi, če so na novo pripeljani igralci dobro vključili v ekipo, torej imeli veliko igralnih minut in dobro statistiko. 

Viri:
https://datahub.io/sports-data/english-premier-league
https://www.transfermarkt.com
https://en.wikipedia.org/wiki/Premier_League
https://github.com/ewenme/transfers/tree/master/data
https://www.kaggle.com/zaeemnalla/premier-league/version/1

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `rgdal` - za uvoz zemljevidov
* `rgeos` - za podporo zemljevidom
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `tidyr` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `mosaic` - za pretvorbo zemljevidov v obliko za risanje z `ggplot2`
* `maptools` - za delo z zemljevidi
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)

## Binder

Zgornje [povezave](#analiza-podatkov-s-programom-r-201819)
omogočajo poganjanje projekta na spletu z orodjem [Binder](https://mybinder.org/).
V ta namen je bila pripravljena slika za [Docker](https://www.docker.com/),
ki vsebuje večino paketov, ki jih boste potrebovali za svoj projekt.

Če se izkaže, da katerega od paketov, ki ji potrebujete, ni v sliki,
lahko za sprotno namestitev poskrbite tako,
da jih v datoteki [`install.R`](install.R) namestite z ukazom `install.packages`.
Te datoteke (ali ukaza `install.packages`) **ne vključujte** v svoj program -
gre samo za navodilo za Binder, katere pakete naj namesti pred poganjanjem vašega projekta.

Tako nameščanje paketov se bo izvedlo pred vsakim poganjanjem v Binderju.
Če se izkaže, da je to preveč zamudno,
lahko pripravite [lastno sliko](https://github.com/jaanos/APPR-docker) z želenimi paketi.

Če želite v Binderju delati z git,
v datoteki `gitconfig` nastavite svoje ime in priimek ter e-poštni naslov
(odkomentirajte vzorec in zamenjajte s svojimi podatki) -
ob naslednjem zagonu bo mogoče delati commite.
Te podatke lahko nastavite tudi z `git config --global` v konzoli
(vendar bodo veljale le v trenutni seji).
