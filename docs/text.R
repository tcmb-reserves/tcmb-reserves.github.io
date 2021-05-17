`r paste0("TCMB'nin ", 
          zaman,
          " tarihli net rezervleri gectigimiz haftaya gore ",
          paste0(abs(round((tdt[time == zaman]$netr) - (tdt[tdt[time == zaman, which = T] - 1]$netr), 2)))," Milyon Dolar",
          ifelse(sign((tdt[time == zaman]$netr) - (tdt[tdt[time == zaman, which = T] - 1]$netr)) == 1, paste0(" artarken"), paste0(" azalirken")),
          " yilbasina gore ise ",
          paste0(abs(round((tdt[time == zaman]$netr) - (tdt[time == "2021-01-01"]$netr), 2)))," Milyon Dolar",
          ifelse(sign((tdt[time == zaman]$netr) - (tdt[time == "2021-01-01"]$netr)) == 1, paste0(" artmistir."), paste0(" azalmistir.")),
          " Swap haric net rezervlerde bir hafta icinde ",
          paste0(abs(round((tdt[time == zaman]$shnetr) - (tdt[tdt[time == zaman, which = T] - 1]$shnetr), 2))), " Milyon Dolar'lik bir",
          ifelse(sign((tdt[time == zaman]$shnetr) - (tdt[tdt[time == zaman, which = T] - 1]$shnetr)) == 1, paste0(" artis oldugu"), paste0(" kayip yasandigi")),
          " gorunurken",
          " yilbasi ile kiyaslandiginda ",
          paste0(abs(round((tdt[time == zaman]$shnetr) - (tdt[time == "2021-01-01"]$shnetr), 2))), " Milyon Dolar",
          ifelse(sign((tdt[time == zaman]$shnetr) - (tdt[time == "2021-01-01"]$shnetr)) == 1, paste0(" arttigi"), paste0(" azaldigi")),
          " gorunuyor."
) `

`r paste0("Net altin varliklari bir hafta sonunda ",
          paste0(abs(round((tdt[time == zaman]$nar) - (tdt[tdt[time == zaman, which = T] - 1]$nar), 2))), " Milyon Dolar",
          ifelse(sign((tdt[time == zaman]$nar) - (tdt[tdt[time == zaman, which = T] - 1]$nar)) == 1, paste0(" artmis"), paste0(" azalmis")),
          " ve yilbasindan bu yana ",
          paste0(abs(round((tdt[time == zaman]$nar) - (tdt[time == "2021-01-01"]$nar), 2))), " Milyon Dolar",
          ifelse(sign((tdt[time == zaman]$nar) - (tdt[time == "2021-01-01"]$nar)) == 1, paste0(" artarak "), paste0(" azalarak ")),
          paste0(round((tdt[time == zaman]$nar), 2)), " Milyon Dolar olmustur."
          
) `
`r paste0("Net doviz varliklari da bu bir haftada ",
          paste0(abs(round((tdt[time == zaman]$ndr) - (tdt[tdt[time == zaman, which = T] - 1]$ndr), 2))), " Milyon Dolar",
          ifelse(sign((tdt[time == zaman]$ndr) - (tdt[tdt[time == zaman, which = T] - 1]$ndr)) == 1, paste0(" artmis"), paste0(" azalmis")),
          " ve yilbasindan itibaren ise ",
          paste0(abs(round((tdt[time == zaman]$ndr) - (tdt[time == "2021-01-01"]$ndr), 2))), " Milyon Dolar",
          ifelse(sign((tdt[time == zaman]$ndr) - (tdt[time == "2021-01-01"]$ndr)) == 1, paste0(" artarak "), paste0(" azalarak ")),
          paste0(round((tdt[time == zaman]$ndr), 2)), " Milyon Dolar olmustur."
          
) `

`r paste0("Swap haric net altin varliklari ise gecen haftaya gore ",
          paste0(abs(round((tdt[time ==zaman]$shnar) - (tdt[tdt[time == zaman, which = T] - 1]$shnar), 2))), " Milyon Dolar'lik",
          ifelse(sign((tdt[time ==zaman]$shnar) - (tdt[tdt[time == zaman, which = T] - 1]$shnar)) == 1, paste0(" artisla"), paste0(" kayipla")), " birlikte ", 
          paste0(round(tdt[time ==zaman]$shnar, 2)), " Milyon Dolar'a ulasmis, ",
          "Swap haric net doviz varliklari da ",
          paste0(abs(round((tdt[time == zaman]$shndr) - (tdt[tdt[time == zaman, which = T] - 1]$shndr), 2)))," Milyon Dolar'lik",
          ifelse(sign((tdt[time == zaman]$shndr) - (tdt[tdt[time == zaman, which = T] - 1]$shndr)) == 1, paste0(" artis"), paste0(" azalis"))," ile ",
          paste0(round((tdt[time == zaman]$shndr), 2)), " olmustur."
) `

`r paste0("Bilanco ici ve bilanco disi toplam yukumlulukler ise gecen haftaya gore ",
          paste0(abs(round((tdt[time == zaman]$ty) - (tdt[tdt[time == zaman, which = T] - 1]$ty), 2))), " Milyon Dolar",
          ifelse(sign((tdt[time == zaman]$ty) - (tdt[tdt[time == zaman, which = T] - 1]$ty)) == 1, paste0(" artmis"), paste0(" azalmis"))," ve yilbasina gore de ",
          paste0(abs(round((tdt[time == zaman]$ty) - (tdt[time == "2021-01-01"]$ty), 2))), " Milyon Dolar ",
          ifelse(sign((tdt[time == zaman]$ty) - (tdt[time == "2021-01-01"]$ty)) == 1, paste0(" artarak "), paste0(" azalarak ")),
          paste0(round((tdt[time == zaman]$ty)/1000, 2)), " Milyar Dolar olmustur."
          
) `

`r paste0( "Bilanco disi yukumluluklerin brut rezervlere orani ise %", round(tdt[time == zaman]$bdybro, 2), " olmustur.")`
