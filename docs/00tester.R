myCBRTKey <- "uKsYRjVuRs"


library(CBRT)

library(textreadr)

library(data.table)

library(tidytext)

library(dplyr)

ypmevd <- getDataSeries("TP.YPMEVD.M131", startDate = "01-01-2021", CBRTKey = myCBRTKey, freq = 3)

a <- data.table(read_pdf("https://www.tcmb.gov.tr/wps/wcm/connect/cc755e33-b5c0-4632-bfe4-2434d35da011/RT20210326TR.pdf?MOD=AJPERES&CACHEID=ROOTWORKSPACE-cc755e33-b5c0-4632-bfe4-2434d35da011-nzliV18"))

yswap <- a %>%
  unnest_tokens(word, text)

yswap <- yswap[page_id == 1]

yswap$rown <- c(1:nrow(yswap))

yswap[, word := gsub(".", "", word, fixed = TRUE)]

as.numeric(yswap$word[yswap[ word == "kapsar"]$rown+4])
as.numeric(yswap$word[yswap[ word == "kapsar"]$rown+5])
as.numeric(yswap$word[yswap[ word == "kapsar"]$rown+6])
as.numeric(yswap$word[yswap[ word == "kapsar"]$rown+18])
as.numeric(yswap$word[yswap[ word == "kapsar"]$rown+19])
as.numeric(yswap$word[yswap[ word == "kapsar"]$rown+20])
