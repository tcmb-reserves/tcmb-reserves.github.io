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
                         "TP.BL005" # Menkul Kiymetler (mk)
), startDate = "01-01-2021", CBRTKey = myCBRTKey, freq = 3)

BRUTR <- na.omit(BRUTR)

USDD <- getDataSeries("TP.DK.USD.A.YTL", startDate = "01-01-2021", CBRTKey = myCBRTKey, freq = 1)

USD1 <- data.table(time = as.Date("2021-01-01"), TP.DK.USD.A.YTL = as.numeric(7.4194))

USDD <- rbind(USD1, USDD)

BRUTR <- merge(BRUTR, USDD, by = "time")

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
                       "TP.BL099" # SDR
                       ), startDate = "01-01-2021", CBRTKey = myCBRTKey, freq = 3)

BIY <- na.omit(BIY)

BIY <- merge(BIY, USDD, by = "time")

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

### Yurtiçi Swap --------------

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

toplam <- as.numeric(yswap$word[yswap[ word == "kapsar"]$rown+ xxx])

diger <- as.numeric(yswap$word[yswap[ word == "kapsar"]$rown+ yyy])

val <- toplam + diger

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

load(paste0(getwd(), "/Total_Swap.RData"))

total_swap_new <- data.table(time = tswap, tsw = val)

if (sum(tswap == total_swap$time) < 1){
  
  total_swap <- rbind(total_swap, total_swap_new)
  
}

save(total_swap, file = paste0(getwd(), "/Total_Swap.RData"))

mat <- merge(mat, total_swap, by = "time")

mat[, ydmbs := tsw - toplam]


########################### Data Table #########################################

all_dates_to_2025 <- seq(as.Date("2021-01-01"), as.Date("2025-01-01"), by = "days")

all_dates_to_2025 <- as.data.table(all_dates_to_2025)

setnames(all_dates_to_2025, "time")

BRUTR[, brut := altin + doviz + SDR]

ypmevd <- getDataSeries("TP.YPMEVD.M131", startDate = "01-01-2021", CBRTKey = myCBRTKey, freq = 3)



ypmevd <- merge(all_dates_to_2025, ypmevd, by = "time", all.x = T, all.y = T)

ypmevd <- na.locf(ypmevd, fromLast = T)

a_brut <- merge(all_dates_to_2025, BRUTR, by = "time", all.x = T, all.y = T)

a_brut <- na.locf(a_brut, fromLast = T)

a_BIY <- merge(all_dates_to_2025, BIY, by = "time", all.x = T, all.y = T)

a_BIY <- na.locf(a_BIY, fromLast = T)

mat[time == "2021-01-04", ]$time <- as.Date("2021-01-01")

tdt <- mat

tdt <- tdt[, c(1,16:20)]

setnames(tdt, c("time", "yibs", "yibas", "yibds", "tsw", "ydmbs"))

tdt <- merge(tdt, a_BIY, by = "time")

tdt <- merge(tdt, viop_table, by = "time")

names(tdt)[21] <- "viop"

tdt[, bdy := tsw + viop]

tdt[, biy := bsm + fb + sdr + dm]

tdt <- tdt[, -20]

names(tdt)[9:11] <- c("yibn", "yibt", "yiba")

names(tdt)[17:18] <- c("mbydby", "sdry")

tdt[, bidy := zkd + yibn + yibt + mbydby + dm + sdry]

tdt[, biay := zka + yiba]

tdt <- merge(a_brut, tdt, by = "time")

names(tdt)[2:8] <- c("bra", "brd", "brsdr", "mk", "a", "tnvm", "brut")

tdt[, a := NULL]

names(tdt)[13] <- "bsb"

tdt[, bddy := yibds + ydmbs + viop]

tdt[, bday := yibas]

tdt[, tswa := yibas]

tdt[, tswd := tsw - yibas]

tdt[, netr := brut - biy]

tdt[, shnetr := netr - tsw]

tdt[, shnar := bra - biay - tswa]

tdt[, ndr := brd + sdry - bidy]

tdt[, nar := bra - biay]

tdt[, shndr := ndr - tswd]

tdt[, ty := biy + bdy]

tdt[, swbro := tsw/brut]

names(tdt)[25] <- "USD"

names(tdt)[18] <- "bydby"

tdt[, ndp := ndr - bddy]

tdt <- merge(tdt, ypmevd, by = "time")

names(tdt)[44] <- "ypmevd"

tdt[, swbmevd := yibs/ypmevd]

setcolorder(tdt, c("time", "brut", "brd", "bra", "brsdr", "mk","tnvm", "biy", "bidy", "biay", "dm",
                   "sdry", "mbydby", "bydby", "bsb", "zk", "zkd", "zka", "yib", "yibn", "yibt", "yiba", "bdy",
                   "bddy", "bday", "viop","yibs", "yibds", "yibas", "ydmbs", "tsw", "tswd", "tswa","netr", "ndr", "nar",
                   "shnetr", "shndr", "shnar", "ndp", "ty", "ypmevd","swbro", "swbmevd", "USD"))

data_table = tdt

colnames(data_table) <- c("Date",
                          "Gross Reserves",
                          "Gross  Foreign Exchange Reserves",
                          "Gross Gold Reserves",
                          "Gross SDR Reserves",
                          "Securities",
                          "Total Cash and Deposit",
                          "Balance Sheet Liabilities",
                          "Balance Sheet FX Liabilities",
                          "Balance Sheet Gold Liabilities",
                          "Other Deposits",
                          "SDR Liabilities",
                          "CBRT's Liabilities to Foreign Banks",
                          "Domestic Banks' Liabilities to Foreign Banks",
                          "Banking Sector Balance Sheet",
                          "Required Reserves",
                          "Required Reserves - FX",
                          "Required Reserves - Gold",
                          "Domestic Banks",
                          "Domestic Banks - Cash",
                          "Domestic Banks - Collateral",
                          "Domestic Banks - Gold",
                          "Off-Balance Sheet Liabilities",
                          "Off-Balance Sheet FX Liabilities",
                          "Off-Balance Sheet Gold Liabilities",
                          "Futures and Options Exchange",
                          "Domestic Banks - Swap",
                          "Domestic Banks - Swap - FX",
                          "Domestic Banks - Swap - Gold",
                          "Foreign Central Banks - Swap",
                          "Total SWAP",
                          "Total SWAP - FX",
                          "Total SWAP - Gold",
                          "Net Reserves",
                          "Net FX Reserves",
                          "Net Gold Reserves",
                          "Net Reserves (excluding Swap)",
                          "Net FX Reserves (excluding Swap)",
                          "Net Gold Reserves (excluding Swap)",
                          "Net FX Position",
                          "Total Liabilities",
                          "Domestic Banks - Total Deposits",
                          "Ratio of SWAP to Gross Reserves",
                          "Ratio of Domestic Banks Swap to Domestic Banks FX Deposits",
                          "USD"
)


write_xlsx(data_table, paste0(getwd(), '/data_table.xlsx'))

################# tdt verileri #################################################

# time -   Date

# bra -    Gross Gold Reserves

# brd -    Gross  Foreign Exchange Reserves

# brsdr -  Gross SDR Reserves

# mk -     Securities

# tnvm -   Total Cash and Deposit

# brut -   Total Gross Reserves

# bdy -    Off-Balance Sheet Liabilities

# bsb -    Banking Sector Balance Sheet

# yib -    Domestic Banks

# yibn -   Domestic Banks - Cash

# yibt -   Domestic Banks - Collateral

# yiba -   Domestic Banks - Gold

# bydby -  Domestic Banks' Liabilities to Foreign Banks

# zk -     Required Reserves

# zkd -    Required Reserves - FX

# zka -    Required Reserves - Gold

# dm -     Other Deposits

# mbydby - CBRT's Liabilities to Foreign Banks

# sdry -   SDR Liabilities

# USD -    Exchange Rate

# viop -   Options

# biy -    Balance Sheet Liabilities

# bidy -   Balance Sheet FX Liabilities

# biay -   Balance Sheet Gold Liabilities

# yibs -   Domestic Banks - Swap

# yibas -  Domestic Banks - Swap - Gold

# yibds -  Domestic Banks - Swap - FX

# ydmbs -  Foreign Central Banks - Swap

# bddy -   Off-Balance Sheet FX Liabilities

# bday -   Off-Balance Sheet Gold Liabilities

# netr -   Net Reserves

# shnetr - Net Reserves (excluding Swap)

# shnar -  Net Gold Reserves (excluding Swap)

# ndr -    Net FX Reserves

# nar -    Net Gold Reserves

# shndr -  Net FX Reserves (excluding Swap)

# ty -     Total Liabilities

# bdybro - Ratio of Off-Balance Sheet Liabilities to Gross Reserves
