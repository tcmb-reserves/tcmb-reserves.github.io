

source(paste0(getwd(),"/netrezerv.R"))

zaman <- as.Date("2021-04-30")
zaman1 <- as.Date("2021-04-23")
zaman2 <- as.Date("2021-04-22")
ilkk <- as.Date("2021-01-01")
ilkk2<- as.Date("2021-01-04")

det_tab<-data.frame(
"Brut Rezerv Doviz" , 
"Toplam Nakit ve Mevduat" , 
"Menkul Kiymetler" , 
"Brut Rezerv Altin" , 
"Brut Rezerv SDR" , 
"Brut Rezervler Toplam�" , 
"Bankacilik Sektoru Bilancosu" ,
"Zorunlu Karsiliklar" ,
"Zorunlu Karsiliklar Doviz" ,
"Zorunlu Karsiliklar Altin" ,
"Yurtici Bankalar" ,
"Yurtici Banka Nakit" ,
"Yurtici Banka Teminat" ,
"Yurtici Bankalar Altin" ,
"Yurtdisi Bankalar" ,
"SDR yukululugu" ,
"Diger Mevduatlar" ,
"Bilan�o i�i Doviz Yukumlulugu" ,
"Bilan�o i�i Altin Yukumlulugu" ,
"Bilan�o i�i Yukumlulukler" ,
"Yurtici Bankalar Swap" ,
"Yurtici Bankalar Doviz Swapi" ,
"Yurtici Bankalar Altin Swapi" ,
"Yurtici Merkez Bankalar� Swap" ,
"Bilan�o Disi Doviz Yukumlulugu" ,
"Bilan�o Disi Altin Yukumlulugu" ,
"Bilan�o Disi Yukumlulukler" ,
"Net Doviz Rezervi" ,
"Net Altin Rezervi" ,
"Net Rezervler" ,
"Swap Hari� Net Doviz Rezervi" , 
"Swap Hari� Net Altin Rezervi" , 
"Swap Hari� Net Rezervler"  


