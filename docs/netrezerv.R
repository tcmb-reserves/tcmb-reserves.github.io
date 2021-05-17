myCBRTKey <- "uKsYRjVuRs"

library(CBRT)

library(textreadr)

library(data.table)

library(tidytext)

library(dplyr)

library(zoo)

library(rmarkdown)

library(DT)

Sys.setlocale(category = "LC_ALL", locale = "Turkish")

# BRUT REZERVLER

## Veri Duzenlenmesi

BRUTR <- getDataSeries(c("TP.AB.C1", # Altin (altin)
                         "TP.AB.C2", # Doviz (doviz)
                         "TP.BL035", # SDR (SDR)
                         "TP.BL005", # Menkul Kiymetler (mk)
                         "TP.DK.USD.A.YTL"), startDate = "01-01-2021", CBRTKey = myCBRTKey, freq = 3)

BRUTR <- na.omit(BRUTR)

colnames(BRUTR) <- c("time", "altin", "doviz", "SDR", "mk", "USD")

BRUTR[, SDR := SDR/(USD*1000)]

BRUTR[, mk := mk/(USD*1000)]

## Alakali Diger Veriler

BRUTR[, doviz := doviz - SDR]

BRUTR[, tot := doviz - mk]

# Bilanco ici Yukumlulukler

## Veri Duzenlenmesi

BIY <- getDataSeries(c("TP.BL084", # Bankacilik Sektoru Mevduati (bsm)
                       "TP.BL085", # Yurtici Bankalar (yib)
                       "TP.BL129", # Nakit (nakit)
                       "TP.BL130", # Teminat (teminat)
                       "TP.BL136", # Altin (altin)
                       "TP.BL086", # Yurt Disi Bankalar (ydb)                   bankalarin ydb a yukumlulugu       bydby
                       "TP.BL087", # Zorunlu Karsiliklar Bloke Hesabi (zk)
                       "TP.BL088", # Zorunlu Karsilik Doviz Kismi (zkd)
                       "TP.BL089", # Zorunlu Karsiliklar Altin Kismi (zka)
                       "TP.BL091", # Diger Mevduat (dm)
                       "TP.BL097", # Yurt Disi Bankalar YP (fb)                 merkez bankasinin ydb a yukumlulugu mbydby
                       "TP.BL099", # SDR 
                       "TP.DK.USD.A.YTL"), startDate = "01-01-2021", CBRTKey = myCBRTKey, freq = 3)

BIY <- na.omit(BIY)

colnames(BIY) <- c("time", "bsm", "yib", "nakit", "teminat","altin", "ydb",
                   "zk", "zkd", "zka", "dm", "fb", "sdr", "USD")

BIY[, bsm := bsm/(USD*1000)]

BIY[, yib := yib/(USD*1000)]

BIY[, nakit := nakit/(USD*1000)]

BIY[, teminat := teminat/(USD*1000)]

BIY[, altin := altin/(USD*1000)]

BIY[, ydb := ydb/(USD*1000)]

BIY[, zk := zk/(USD*1000)]

BIY[, zkd := zkd/(USD*1000)]

BIY[, zka := zka/(USD*1000)]

BIY[, dm := dm/(USD*1000)]

BIY[, fb := fb/(USD*1000)]

BIY[, sdr := sdr/(USD*1000)]

# Bilanco Disi Yukumlulukler

### Yurtici Swap --------------

a <- data.table(read_pdf("https://www.tcmb.gov.tr/wps/wcm/connect/a6ffdb2f-47d9-4ae9-8c39-5075867aaec3/TCMB+Tarafli+Swap+Islemleri.pdf?MOD=AJPERES&CACHEID=ROOTWORKSPACE-a6ffdb2f-47d9-4ae9-8c39-5075867aaec3-nzm2YQt%22"))

swap <- a %>%
  unnest_tokens(word, text)

swap$time <- as.Date(swap$word, format = "%d.%m.%Y")

swap$rown <- c(1:nrow(swap)) 

sel <- swap[is.na(time) == F]

sel <- as.vector(sel$rown)

selp <- sel+15

selp

swap[, word := gsub(",", "", word, fixed = TRUE)]

as.numeric(swap$word)

mat <- data.table(
  "Valor Tarihi" = rep(as.Date("2020-10-10"),length(sel)),
  "TCMB Doviz Karsiligi TL Swap Piyasasi" = rep(0,length(sel)),
  "TCMB Doviz Karsiligi TL Swap Piyasasi - Stok"= rep(0,length(sel)), 
  "BIST Swap Piyasasi" = rep(0,length(sel)),
  "BIST Swap Piyasasi - Stok" = rep(0,length(sel)),
  "TCMB TL Karsiligi Altin Swap Piyasasi " = rep(0,length(sel)),
  "TCMB TL Karsiligi Altin Swap Piyasasi - Stok" = rep(0,length(sel)),
  "TCMB Doviz Karsiligi TL Swap Geleneksel Ihaleleri" = rep(0,length(sel)),
  "TCMB Doviz Karsiligi TL Swap Geleneksel Ihaleleri - Stok" = rep(0,length(sel)),
  "TCMB Doviz Karsiligi TL Swap Miktar Ihaleleri" = rep(0,length(sel)),
  "TCMB Doviz Karsiligi TL Swap Miktar Ihaleleri - Stok" = rep(0,length(sel)),
  "TCMB Altin Karsiligi TL Swap Geleneksel Ihaleleri" = rep(0,length(sel)),
  "TCMB Altin Karsiligi TL Swap Geleneksel Ihaleleri - Stok"= rep(0,length(sel)),
  "TCMB Doviz Karsiligi Altin Swap Piyasasi (Net Alim)"= rep(0,length(sel)),
  "TCMB Doviz Karsiligi Altin Swap Piyasasi - Stok (Net Alim)"= rep(0,length(sel)),
  "TOPLAM - STOK" = rep(0,length(sel))
)

w <- c()

for (ii in 1:length(sel)) {
  
  w <- swap[sel[ii]:selp[ii], ]$word
  
  for (k in 1:length(w)) {
    
    mat[ii,k] <- as.character(w[k])
    
  }
}

for (ii in 1:length(sel)) {
  
  mat[ii,1]<- as.Date(swap[sel[ii], ]$time) 
  
}

colnames(mat)[1]<- "time"

colnames(mat)[16]<- "toplam"

mat[, altin := mat[,7] + mat[,13]]

mat[, doviz := mat[,16] - mat[,17]]

#### Yabanci MB swaplarini---------

a <- data.table(read_pdf("https://www.tcmb.gov.tr/wps/wcm/connect/cc755e33-b5c0-4632-bfe4-2434d35da011/RT20210326TR.pdf?MOD=AJPERES&CACHEID=ROOTWORKSPACE-cc755e33-b5c0-4632-bfe4-2434d35da011-nzliV18"))

yswap <- a %>%
  unnest_tokens(word, text)

yswap <- yswap[page_id == 1]

yswap$rown <- c(1:nrow(yswap))

yswap[, word := gsub(".", "", word, fixed = TRUE)]

val <- as.numeric(yswap$word[yswap[ word == "kapsar"]$rown+5])

# tum kelimeleri numeric yapar

yswap$word <- as.numeric(yswap$word)

# Na lari omit et

yswap <- na.omit(yswap)

# 8 uzunluklu veri arar ?unku TR deki hi? bir veri trilyon degerinde olamaz

demak <- nchar(yswap$word) == 8

tt <- c()

for (ii in 1:length(demak)){
  
  if (demak[ii]){ tt <- yswap$word[ii]}
  
}


t1 <- substring(tt, 1, 2)

t2 <- substring(tt, 3, 4)

t3 <- substring(tt, 5, 8)

tswap <- paste0(t3, "-", t2, "-", t1)

tswap <- as.Date(tswap)

load(paste0(getwd(), "/Yabanci_MB.RData"))

y_mb_new <- data.table(time = tswap, mb = val)

if (sum(tswap == y_mb$time) < 1){
  
  y_mb <- rbind(y_mb, y_mb_new)
  
}

save(y_mb, file = paste0(getwd(), "/Yabanci_MB.RData"))

rm(a)

rm(swap)

rm(yswap)

rm(ii)

rm(k)

rm(sel)

rm(selp)

rm(w)

########################### Data Table #########################################

all_dates_to_2025 <- seq(as.Date("2021-01-01"), as.Date("2025-01-01"), by = "days")

all_dates_to_2025 <- as.data.table(all_dates_to_2025)

setnames(all_dates_to_2025, "time")

BRUTR[, brut := altin + doviz + SDR]

a_brut <- merge(all_dates_to_2025, BRUTR, by = "time", all.x = T, all.y = T)

a_brut <- na.locf(a_brut, fromLast = T)

a_BIY <- merge(all_dates_to_2025, BIY, by = "time", all.x = T, all.y = T)

a_BIY <- na.locf(a_BIY, fromLast = T)

mat[time == "2021-01-04", ]$time <- as.Date("2021-01-01")

tdt <- merge(mat, y_mb, by = "time")

tdt[, bdy := toplam + mb]

tdt <- tdt[, c(1,20)]

tdt <- merge(tdt, a_BIY, by = "time")

tdt[, biy := bsm + fb + sdr + dm]

setnames(tdt, c("time", "bdy", "bsb", "yib", "yibn", "yibt", "yiba", "bydby", "zk",
         "zkd", "zka", "dm", "mbydby", "sdry", "USD", "biy"))

tdt[, bidy := zkd + yibn + yibt + mbydby + dm]

tdt[, biay := zka + yiba]

tdt <- merge(a_brut, tdt, by = "time")

setnames(tdt, c("time", "bra", "brd", "brsdr", "mk", "a", "tnvm", 
                "brut", "bdy", "bsb", "yib", "yibn", "yibt", "yiba", "bydby",
                "zk", "zkd", "zka", "dm", "mbydby", "sdry", "USD", "biy", 
                "bidy", "biay"))

tdt[, a := NULL]

tdt <- merge(tdt, mat, by = "time")

tdt <- merge(tdt, y_mb, by = "time")

tdt <- tdt[, c(1:24, 39:42)]

names(tdt)[25:28] <- c("yibs", "yibas", "yibds", "ydmbs")

tdt[, bddy := yibds + ydmbs]

tdt[, bday := yibas]

tdt[, netr := brut - biy]

tdt[, shnetr := netr - bdy]

tdt[, shnar := bra - yiba - zka - yibas]

tdt[, ndr := brd - yibn - yibt - zkd - dm - mbydby]

tdt[, nar := bra - yiba - zka]

tdt[, shndr := brd - yibn - yibt - zkd - dm - mbydby - ydmbs - yibds]

tdt[, ty := biy + bdy]

tdt[, bdybro := bdy/brut]

################# tdt verileri #################################################

# time -   Zaman

# bra -    Brut Rezerv Altin

# brd -    Brut Rezerv Doviz

# brsdr -  Brut Rezerv SDR

# mk -     Menkul Kiymetler

# tnvm -   Toplam Nakit ve Mevduat

# brut -   Brut Rezerv

# bdy -    Bilanco Disi Yukumlulukler

# bsb -    Bankacilik Sektoru Bilancosu

# yib -    Yurtici Bankalar

# yibn -   Yurtici Bankalar Nakit

# yibt -   Yurtici Bankalar Teminat

# yiba -   Yurtici Bankalar Altin

# bydby -  Bankalarin Yurtdisi Bankalara Yukumlulugu

# zk -     Zorunlu Karsiliklar

# zkd -    Zorunlu Karsiliklar Doviz

# zka -    Zorunlu Karsiliklar Altin

# dm -     Diger Mevduatlar

# mbydby - MB'nin Yurtdisi Bankalara Yukumlulugu

# sdry -   SDR Yukumlulugu

# USD -    Doviz Kuru

# biy -    Bilanco İci Yukumlulukler

# bidy -   Bilanco İci Doviz Yukumlulugu

# biay -   Bilanco İci Altin Yukumlulugu

# yibs -   Yurtici Bankalar Swap

# yibas -  Yurtici Bankalar Altin Swapi

# yibds -  Yurtici Bankalar Doviz Swapi

# ydmbs -  Yurtdisi Merkez Bankalari Swap 

# bddy -   Bilanco Disi Doviz Yukumlulugu

# bday -   Bilanco Disi Altin Yukumlulugu

# netr -   Net Rezerv

# shnetr - Swap Haric Net Rezerv

# shnar -  Swap Haric Net Altin Rezervi

# ndr -    Net Doviz Rezervi

# nar -    Net Altin Rezervi

# shndr -  Swap Haric Net Doviz Rezervi

# ty -     Toplam Yukumlulukler

# bdybro - Bilanco Disi Yukumluluklerin Brut Rezerve Orani
