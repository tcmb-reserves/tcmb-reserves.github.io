# Libraries Required
if (!require(textreadr)) install.packages('textreadr')
if (!require(data.table)) install.packages('data.table')
if (!require(tidytext)) install.packages('tidytext')
if (!require(dplyr)) install.packages('dplyr')
if (!require(writexl)) install.packages('writexl')

# Libraries
library(textreadr)
library(data.table)
library(tidytext)
library(dplyr)
library(writexl)

a <- data.table(read_pdf("https://www.tcmb.gov.tr/wps/wcm/connect/a6ffdb2f-47d9-4ae9-8c39-5075867aaec3/TCMB+Tarafli+Swap+Islemleri.pdf?MOD=AJPERES&CACHEID=ROOTWORKSPACE-a6ffdb2f-47d9-4ae9-8c39-5075867aaec3-nzm2YQt%22"))

swap <- a %>%
  unnest_tokens(word, text)

swap$time <- as.Date(swap$word, format =  "%d.%m.%Y")
swap$rown <- c(1:nrow(swap)) 

sel<- swap[is.na(time) == F]

sel <- as.vector(sel$rown)
selp <- sel+15
selp

swap[, word := gsub(",", "", word, fixed = TRUE)]
as.numeric(swap$word)




mat <- data.table(
  "Valor Tarihi" = rep(as.Date("2020-10-10"),length(sel)),
  "TCMB Doviz Karsiligi TL Swap Piyasasi" = rep(0,,length(sel)),
  "TCMB Doviz Karsiligi TL Swap Piyasasi - Stok"= rep(0,,length(sel)), 
  "BIST Swap Piyasasi" = rep(0,,length(sel)),
  "BIST Swap Piyasasi - Stok" = rep(0,,length(sel)),
  "TCMB TL Karsiligi Altin Swap Piyasasi " = rep(0,,length(sel)),
  "TCMB TL Karsiligi Altin Swap Piyasasi - Stok" = rep(0,,length(sel)),
  "TCMB Doviz Karsiligi TL Swap Geleneksel Ihaleleri" = rep(0,,length(sel)),
  "TCMB Doviz Karsiligi TL Swap Geleneksel Ihaleleri - Stok" = rep(0,,length(sel)),
  "TCMB Doviz Karsiligi TL Swap Miktar Ihaleleri" = rep(0,,length(sel)),
  "TCMB Doviz Karsiligi TL Swap Miktar Ihaleleri - Stok" = rep(0,,length(sel)),
  "TCMB Altin Karsiligi TL Swap Geleneksel Ihaleleri" = rep(0,,length(sel)),
  "TCMB Altin Karsiligi TL Swap Geleneksel Ihaleleri - Stok"= rep(0,,length(sel)),
  "TCMB Doviz Karsiligi Altin Swap Piyasasi (Net Alim)"= rep(0,,length(sel)),
  "TCMB Doviz Karsiligi Altin Swap Piyasasi - Stok (Net Alim)"= rep(0,,length(sel)),
  "TOPLAM - STOK" = rep(0,,length(sel))
)




w<- c()

for (ii in 1:length(sel)) {
  w<- swap[sel[ii]:selp[ii],]$word
  for (k in 1:length(w)) {
    mat[ii,k] <- as.character(w[k])
    
  }
  
}

for (ii in 1:length(sel)) {
  mat[ii,1]<- as.Date(swap[sel[ii],]$time) 
}



date<-as.Date(Sys.time())

write.csv(mat,paste0(getwd(),'/swap.csv'))

write_xlsx(mat,paste0(getwd(),'/swap.xlsx'))


