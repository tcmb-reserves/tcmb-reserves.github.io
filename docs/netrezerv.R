myCBRTKey <- "uKsYRjVuRs"

library(CBRT)

library(textreadr)

library(data.table)

library(tidytext)

library(dplyr)

library(zoo)

library(rmarkdown)

library(DT)

library(writexl)

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

pdftable_to_dt <- function(x) {
  a <-  data.table(read_pdf(x))
  
  a <- a %>%
    unnest_tokens(word, text)
  
  a$time <- as.Date(a$word, format = "%d.%m.%Y")
  
  a$rown <- c(1:nrow(a)) 
  
  c <- a[is.na(time) == F]
  
  c <- as.vector(c$rown)
  
  c[2]-c[1]-1
  
  d <- c + c[2]-c[1]-1
  
  d
  
  a[, word := gsub(",", "", word, fixed = TRUE)]
  
  as.numeric(a$word)
  
  a_table <- data.table(matrix(0, nrow = length(c), ncol = c[2]-c[1]-1))
  
  dat <- data.table("time" = as.Date(rep("2020-01-01", length(c))))
  
  a_table <- cbind(dat, a_table)
  
  q <- c()
  
  for (ii in 1:length(c)) {
    
    q <- a[c[ii]:d[ii], ]$word
    
    for (k in 1:length(q)) {
      
      a_table[ii,k] <- as.character(q[k])
      
    }
  }
  
  for (ii in 1:length(c)) {
    
    a_table[ii,1]<- as.Date(a[c[ii], ]$time) 
    
  }
  
  .GlobalEnv$a_table <- a_table
  
}

pdftable_to_dt("https://www.tcmb.gov.tr/wps/wcm/connect/a6ffdb2f-47d9-4ae9-8c39-5075867aaec3/TCMB+Tarafli+Swap+Islemleri.pdf?MOD=AJPERES&CACHEID=ROOTWORKSPACE-a6ffdb2f-47d9-4ae9-8c39-5075867aaec3-nzm2YQt%22")

mat <- a_table

setnames(mat, c("Valor Tarihi", "TCMB Doviz Karsiligi TL Swap Piyasasi", "TCMB Doviz Karsiligi TL Swap Piyasasi - Stok",
         "BIST Swap Piyasasi", "BIST Swap Piyasasi - Stok", "TCMB TL Karsiligi Altin Swap Piyasasi ",
         "TCMB TL Karsiligi Altin Swap Piyasasi - Stok", "TCMB Doviz Karsiligi TL Swap Geleneksel Ihaleleri",
         "TCMB Doviz Karsiligi TL Swap Geleneksel Ihaleleri - Stok", "TCMB Doviz Karsiligi TL Swap Miktar Ihaleleri",
         "TCMB Doviz Karsiligi TL Swap Miktar Ihaleleri - Stok", "TCMB Altin Karsiligi TL Swap Geleneksel Ihaleleri",
         "TCMB Altin Karsiligi TL Swap Geleneksel Ihaleleri - Stok", "TCMB Doviz Karsiligi Altin Swap Piyasasi (Net Alim)",
         "TCMB Doviz Karsiligi Altin Swap Piyasasi - Stok (Net Alim)", "TOPLAM - STOK"))

colnames(mat)[1]<- "time"

colnames(mat)[16]<- "toplam"

mat[, altin := mat[,7] + mat[,13]]

mat[, doviz := mat[,16] - mat[,17]]

pdftable_to_dt("https://www.tcmb.gov.tr/wps/wcm/connect/a5df108d-d109-4dc0-9ea6-76a78b196b4d/V%C4%B0OP+%C4%B0%C5%9Flemleri.pdf?MOD=AJPERES")

viop_table <- a_table

setnames(viop_table, c("time", "BIST VIOP Nezdinde Turk Lirası Uzlasmali Vadeli Doviz Satım Islemleri",
                       "Toplam Doviz Satım Pozisyonu"))

viop_1 <- data.table(
  "time" = as.Date("2021-01-01"),
  "BIST VIOP Nezdinde Turk Lirası Uzlasmali Vadeli Doviz Satım Islemleri" = as.numeric(0),
  "Toplam Doviz Satım Pozisyonu" = as.numeric(1397)
)

viop_table <- rbind(viop_1, viop_table)

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

demak <- nchar(yswap$word) >= 7

tt <- c()

for (ii in 1:length(demak)){
  
  if (demak[ii]){ tt <- yswap$word[ii]}
  
}

tswap <- c()

t1 <- c()

t2 <- c()

t3 <- c()

if (nchar(tt) == 7) {

t1 = substring(tt, 1, 1)

t2 = substring(tt, 2, 3)

t3 = substring(tt, 4, 7)

tswap = paste0(t3, "-", t2, "-", t1)
} else if (nchar(tt) == 8){
  t1 = substring(tt, 1, 2)
  
  t2 = substring(tt, 3, 4)
  
  t3 = substring(tt, 5, 8)
  
  tswap = paste0(t3, "-", t2, "-", t1)
}

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

tdt <- merge(tdt, viop_table, by = "time")

names(tdt)[17] <- "viop"

tdt[, biy := bsm + fb + sdr + dm + viop]

tdt <- tdt[, -16]

setnames(tdt, c("time", "bdy", "bsb", "yib", "yibn", "yibt", "yiba", "bydby", "zk",
                "zkd", "zka", "dm", "mbydby", "sdry", "USD", "viop", "biy"))

tdt[, bidy := zkd + yibn + yibt + mbydby + dm + viop + sdry]

tdt[, biay := zka + yiba]

tdt <- merge(a_brut, tdt, by = "time")

setnames(tdt, c("time", "bra", "brd", "brsdr", "mk", "a", "tnvm", 
                "brut", "bdy", "bsb", "yib", "yibn", "yibt", "yiba", "bydby",
                "zk", "zkd", "zka", "dm", "mbydby", "sdry", "USD", "viop","biy", 
                "bidy", "biay"))

tdt[, a := NULL]

tdt <- merge(tdt, mat, by = "time")

tdt <- merge(tdt, y_mb, by = "time")

tdt <- tdt[, c(1:25, 40:43)]

names(tdt)[26:29] <- c("yibs", "yibas", "yibds", "ydmbs")

tdt[, bddy := yibds + ydmbs]

tdt[, bday := yibas]

tdt[, netr := brut - biy]

tdt[, shnetr := netr - bdy]

tdt[, shnar := bra - yiba - zka - yibas]

tdt[, ndr := brd - yibn - yibt - zkd - dm - mbydby- viop]

tdt[, nar := bra - yiba - zka]

tdt[, shndr := brd - yibn - yibt - zkd - dm - mbydby - ydmbs - yibds - viop]

tdt[, ty := biy + bdy]

tdt[, bdybro := bdy/brut]

tablo = tdt

colnames(tablo) <- c("Zaman", 
                     "Brut Rezerv Altin",
                     "Brut Rezerv Doviz",
                     "Brut Rezerv SDR",
                     "Menkul Kiymetler",
                     "Toplam Nakit ve Mevduat",
                     "Brut Rezerv",
                     "Bilanco Disi Yukumlulukler",
                     "Bankacilik Sektoru Bilancosu",
                     "Yurtici Bankalar",
                     "Yurtici Bankalar Nakit",
                     "Yurtici Bankalar Teminat",
                     "Yurtici Bankalar Altin",
                     "Bankalarin Yurtdisi Bankalara Yukumlulugu",
                     "Zorunlu Karsiliklar",
                     "Zorunlu Karsiliklar Doviz",
                     "Zorunlu Karsiliklar Altin",
                     "Diger Mevduatlar",
                     "MB'nin Yurtdisi Bankalara Yukumlulugu",
                     "SDR Yukumlulugu",
                     "Doviz Kuru",
                     "VIOP",
                     "Bilanco İci Yukumlulukler",
                     "Bilanco İci Doviz Yukumlulugu",
                     "Bilanco İci Altin Yukumlulugu",
                     "Yurtici Bankalar Swap",
                     "Yurtici Bankalar Altin Swapi",
                     "Yurtici Bankalar Doviz Swapi",
                     "Yurtdisi Merkez Bankalari Swap",
                     "Bilanco Disi Doviz Yukumlulugu",
                     "Bilanco Disi Altin Yukumlulugu",
                     "Net Rezerv",
                     "Swap Haric Net Rezerv",
                     "Swap Haric Net Altin Rezervi",
                     "Net Doviz Rezervi",
                     "Net Altin Rezervi",
                     "Swap Haric Net Doviz Rezervi",
                     "Toplam Yukumlulukler",
                     "Bilanco Disi Yukumluluklerin Brut Rezerve Orani"
)


write_xlsx(tablo, paste0(getwd(), '/Tablo.xlsx'))

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

# viop -   VIOP
 
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
