library(plotly)

library(data.table)

library(ggplot2)

library(Rmisc)

library(dplyr)

Sys.setlocale(category = "LC_ALL", locale = "Turkish")

################################# Tablolar #############################

yutk <- data.frame(
  
  "1" = c("Brut Rezervler", 
          paste0(round((tdt[time == zaman]$brut)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == zaman1]$brut)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == ilkk]$brut)/1000, 2), " Milyar $")),
  
  "2" = c("Bilanco ici Yukumlulukler",
          paste0(round((tdt[time == zaman]$biy)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == zaman1]$biy)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == ilkk]$biy)/1000, 2), " Milyar $")),
  
  "3" = c("Bilanco Disi Yukumlulukler",
          paste0(round((tdt[time == zaman]$bdy)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == zaman1]$bdy)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == ilkk]$bdy)/1000, 2), " Milyar $")),
  
  "4" = c("SWAP",
          paste0(round((tdt[time == zaman]$tsw)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == zaman1]$tsw)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == ilkk]$tsw)/1000, 2), " Milyar $")),
  
  "=1-2" = c("Net Rezervler",
             paste0(round((tdt[time == zaman]$netr)/1000, 2), " Milyar $"),
             paste0(round((tdt[time == zaman1]$netr)/1000, 2), " Milyar $"),
             paste0(round((tdt[time == ilkk]$netr)/1000, 2), " Milyar $")),
  
  "=1-2-4" = c("Swap Haric Net Rezervler", 
               paste0(round((tdt[time == zaman]$shnetr)/1000, 2), " Milyar $"),
               paste0(round((tdt[time == zaman1]$shnetr)/1000, 2), " Milyar $"),
               paste0(round((tdt[time == ilkk]$shnetr)/1000, 2), " Milyar $"))
)

yutk <- t(yutk)

yutk <-as.data.frame(yutk)

row.names(yutk) <- c("   1","   2","   3","   4","   =1-2","   =1-2-4")

colnames(yutk) <- c("Hesaplar", as.character(zaman), as.character(zaman1), as.character(ilkk))

yutk

det_tab <- data.frame(
  
  "1" = c("Brut Rezerv Doviz", 
          paste0(round((tdt[time == zaman]$brd)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == zaman1]$brd)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == ilkk]$brd)/1000, 2), " Milyar $")),
  
  "1.1" = c("Toplam Nakit ve Mevduat", 
            paste0(round((tdt[time == zaman]$tnvm)/1000, 2), " Milyar $"),
            paste0(round((tdt[time == zaman1]$tnvm)/1000, 2), " Milyar $"),
            paste0(round((tdt[time == ilkk]$tnvm)/1000, 2), " Milyar $")),
  
  "1.2" = c("Menkul Kiymetler", 
            paste0(round((tdt[time == zaman]$mk)/1000, 2), " Milyar $"),
            paste0(round((tdt[time == zaman1]$mk)/1000, 2), " Milyar $"),
            paste0(round((tdt[time == ilkk]$mk)/1000, 2), " Milyar $")),
  
  "2" = c("Brut Rezerv Altin", 
          paste0(round((tdt[time == zaman]$bra)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == zaman1]$bra)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == ilkk]$bra)/1000, 2), " Milyar $")),
  
  "3" = c("Brut Rezerv SDR", 
          paste0(round((tdt[time == zaman]$brsdr)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == zaman1]$brsdr)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == ilkk]$brsdr)/1000, 2), " Milyar $")),
  
  "4=1+2+3" = c("Brut Rezervler Toplami", 
                paste0(round((tdt[time == zaman]$brut)/1000, 2), " Milyar $"),
                paste0(round((tdt[time == zaman1]$brut)/1000, 2), " Milyar $"),
                paste0(round((tdt[time == ilkk]$brut)/1000, 2), " Milyar $")),
  
  "5" = c("Bankacilik Sektoru Bilancosu",
          paste0(round((tdt[time == zaman]$bsb)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == zaman1]$bsb)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == ilkk]$bsb)/1000, 2), " Milyar $")),
  
  "5.1" = c("Zorunlu Karsiliklar",
            paste0(round((tdt[time == zaman]$zk)/1000, 2), " Milyar $"),
            paste0(round((tdt[time == zaman1]$zk)/1000, 2), " Milyar $"),
            paste0(round((tdt[time == ilkk]$zk)/1000, 2), " Milyar $")),
  
  "5.11" = c("Zorunlu Karsiliklar Doviz",
             paste0(round((tdt[time == zaman]$zkd)/1000, 2), " Milyar $"),
             paste0(round((tdt[time == zaman1]$zkd)/1000, 2), " Milyar $"),
             paste0(round((tdt[time == ilkk]$zkd)/1000, 2), " Milyar $")),
  
  "5.12" = c("Zorunlu Karsiliklar Altin",
             paste0(round((tdt[time == zaman]$zka)/1000, 2), " Milyar $"),
             paste0(round((tdt[time == zaman1]$zka)/1000, 2), " Milyar $"),
             paste0(round((tdt[time == ilkk]$zka)/1000, 2), " Milyar $")),
  
  "5.2" = c("Yurtici Bankalar",
            paste0(round((tdt[time == zaman]$yib)/1000, 2), " Milyar $"),
            paste0(round((tdt[time == zaman1]$yib)/1000, 2),  " Milyar $"),
            paste0(round((tdt[time == ilkk]$yib)/1000, 2),  " Milyar $")),
  
  "5.21" = c("Yurtici Banka Nakit",
             paste0(round((tdt[time == zaman]$yibn)/1000, 2), " Milyar $"),
             paste0(round((tdt[time == zaman1]$yibn)/1000, 2), " Milyar $"),
             paste0(round((tdt[time == ilkk]$yibn)/1000, 2), " Milyar $")),
  
  "5.22" = c("Yurtici Banka Teminat",
             paste0(round((tdt[time == zaman]$yibt)/1000, 2), " Milyar $"),
             paste0(round((tdt[time == zaman1]$yibt)/1000, 2), " Milyar $"),
             paste0(round((tdt[time == ilkk]$yibt)/1000, 2), " Milyar $")),
  
  "5.23" = c("Yurtici Bankalar Altin",
             paste0(round((tdt[time == zaman]$yiba)/1000, 2), " Milyar $"),
             paste0(round((tdt[time == zaman1]$yiba)/1000, 2), " Milyar $"),
             paste0(round((tdt[time == ilkk]$yiba)/1000, 2), " Milyar $")),
  
  "6" = c("Yurtdisi Bankalar",
          paste0(round((tdt[time == zaman]$mbydby)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == zaman1]$mbydby)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == ilkk]$mbydby)/1000, 2), " Milyar $")),
  
  "7" = c("SDR yukumlulugu",
          paste0(round((tdt[time == zaman]$sdry)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == zaman1]$sdry)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == ilkk]$sdry)/1000, 2), " Milyar $")),
  
  "8" = c("Diger Mevduatlar",
          paste0(round((tdt[time == zaman]$dm)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == zaman1]$dm)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == ilkk]$dm)/1000, 2),  " Milyar $")),
  
  "9" = c("VIOP",
          paste0(round((tdt[time == zaman]$viop)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == zaman1]$viop)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == ilkk]$viop)/1000, 2),  " Milyar $")),
  
  "10=5.11+5.21+5.22+6+7+8" = c("Bilanco ici Doviz Yukumlulugu",
                             paste0(round((tdt[time == zaman]$bidy)/1000, 2), " Milyar $"),
                             paste0(round((tdt[time == zaman1]$bidy)/1000, 2), " Milyar $"),
                             paste0(round((tdt[time == ilkk]$bidy)/1000, 2), " Milyar $")),
  
  "11=5.12+5.23" = c("Bilanco ici Altin Yukumlulugu",
                     paste0(round((tdt[time == zaman]$biay)/1000, 2), " Milyar $"),
                     paste0(round((tdt[time == zaman1]$biay)/1000, 2), " Milyar $"),
                     paste0(round((tdt[time == ilkk]$biay)/1000, 2), " Milyar $")),
  
  "12=10+11" = c("Bilanco ici Yukumlulukler",
                paste0(round((tdt[time == zaman]$biy)/1000, 2), " Milyar $"),
                paste0(round((tdt[time == zaman1]$biy)/1000, 2), " Milyar $"),
                paste0(round((tdt[time == ilkk]$biy)/1000, 2), " Milyar $")),
  
  "13" = c("Yurtici Bankalar Swap",
           paste0(round((tdt[time == zaman]$yibs)/1000, 2), " Milyar $"),
           paste0(round((tdt[time == zaman1]$yibs)/1000, 2), " Milyar $"),
           paste0(round((tdt[time == ilkk]$yibs)/1000, 2), " Milyar $")),
  
  "13.1" = c("Yurtici Bankalar Doviz Swapi",
             paste0(round((tdt[time == zaman]$yibds)/1000, 2), " Milyar $"),
             paste0(round((tdt[time == zaman1]$yibds)/1000, 2), " Milyar $"),
             paste0(round((tdt[time == ilkk]$yibds)/1000, 2), " Milyar $")),
  
  "13.2" = c("Yurtici Bankalar Altin Swapi",
             paste0(round((tdt[time == zaman]$yibas)/1000, 2), " Milyar $"),
             paste0(round((tdt[time == zaman1]$yibas)/1000, 2), " Milyar $"),
             paste0(round((tdt[time == ilkk]$yibas)/1000, 2), " Milyar $")),
  
  "14" = c("Yurtdisi Merkez Bankalari Swap",
           paste0(round((tdt[time == zaman]$ydmbs)/1000, 2), " Milyar $"),
           paste0(round((tdt[time == zaman1]$ydmbs)/1000, 2), " Milyar $"),
           paste0(round((tdt[time == ilkk]$ydmbs)/1000, 2), " Milyar $")),
  
  "15=14+13.1+9" = c("Bilanco Disi Doviz Yukumlulugu",
                   paste0(round((tdt[time == zaman]$bddy)/1000, 2), " Milyar $"),
                   paste0(round((tdt[time == zaman1]$bddy)/1000, 2), " Milyar $"),
                   paste0(round((tdt[time == ilkk]$bddy)/1000, 2), " Milyar $")),
  
  "16=13.2" = c("Bilanco Disi Altin Yukumlulugu",
           paste0(round((tdt[time == zaman]$bday)/1000, 2), " Milyar $"),
           paste0(round((tdt[time == zaman1]$bday)/1000, 2), " Milyar $"),
           paste0(round((tdt[time == ilkk]$bday)/1000, 2), " Milyar $")),
  
  "17=15+16" = c("Bilanco Disi Yukumlulukler",
                 paste0(round((tdt[time == zaman]$bdy)/1000, 2), " Milyar $"),
                 paste0(round((tdt[time == zaman1]$bdy)/1000, 2), " Milyar $"),
                 paste0(round((tdt[time == ilkk]$bdy)/1000, 2), " Milyar $")),
  
  "18" = c("Toplam SWAP",
                paste0(round((tdt[time == zaman]$tsw)/1000, 2), " Milyar $"),
                paste0(round((tdt[time == zaman1]$tsw)/1000, 2), " Milyar $"),
                paste0(round((tdt[time == ilkk]$tsw)/1000, 2), " Milyar $")),
  
  "19" = c("Toplam Swap - Doviz",
                paste0(round((tdt[time == zaman]$tswd)/1000, 2), " Milyar $"),
                paste0(round((tdt[time == zaman1]$tswd)/1000, 2), " Milyar $"),
                paste0(round((tdt[time == ilkk]$tswd)/1000, 2), " Milyar $")),
  
  "20" = c("Toplam Swap - Altin",
                paste0(round((tdt[time == zaman]$tswa)/1000, 2), " Milyar $"),
                paste0(round((tdt[time == zaman1]$tswa)/1000, 2), " Milyar $"),
                paste0(round((tdt[time == ilkk]$tswa)/1000, 2), " Milyar $")),
  
  "21=22-15" = c("Net Doviz Pozisyonu",
                paste0(round((tdt[time == zaman]$ndp)/1000, 2), " Milyar $"),
                paste0(round((tdt[time == zaman1]$ndp)/1000, 2), " Milyar $"),
                paste0(round((tdt[time == ilkk]$ndp)/1000, 2), " Milyar $")),
  
  "22=1+3-10" = c("Net Doviz Rezervi",
               paste0(round((tdt[time == zaman]$ndr)/1000, 2), " Milyar $"),
               paste0(round((tdt[time == zaman1]$ndr)/1000, 2), " Milyar $"),
               paste0(round((tdt[time == ilkk]$ndr)/1000, 2), " Milyar $")),
  
  "23=2-11" = c("Net Altin Rezervi",
                paste0(round((tdt[time == zaman]$nar)/1000, 2), " Milyar $"),
                paste0(round((tdt[time == zaman1]$nar)/1000, 2), " Milyar $"),
                paste0(round((tdt[time == ilkk]$nar)/1000, 2), " Milyar $")),
  
  "24=4-12" = c("Net Rezervler",
                 paste0(round((tdt[time == zaman]$netr)/1000, 2), " Milyar $"),
                 paste0(round((tdt[time == zaman1]$netr)/1000, 2), " Milyar $"),
                 paste0(round((tdt[time == ilkk]$netr)/1000, 2), " Milyar $")),
  
  "25=22-19" = c("Swap Haric Net Doviz Rezervi", 
                 paste0(round((tdt[time == zaman]$shndr)/1000, 2), " Milyar $"),
                 paste0(round((tdt[time == zaman1]$shndr)/1000, 2), " Milyar $"),
                 paste0(round((tdt[time == ilkk]$shndr)/1000, 2), " Milyar $")),
  
  "26=23-20" = c("Swap Haric Net Altin Rezervi", 
                   paste0(round((tdt[time == zaman]$shnar)/1000, 2), " Milyar $"),
                   paste0(round((tdt[time == zaman1]$shnar)/1000, 2), " Milyar $"),
                   paste0(round((tdt[time == ilkk]$shnar)/1000, 2), " Milyar $")),
  
  "27=24-18" = c("Swap Haric Net Rezervler", 
                 paste0(round((tdt[time == zaman]$shnetr)/1000, 2), " Milyar $"),
                 paste0(round((tdt[time == zaman1]$shnetr)/1000, 2), " Milyar $"),
                 paste0(round((tdt[time == ilkk]$shnetr)/1000, 2), " Milyar $"))
)


det_tab <- t(det_tab)

det_tab

det_tab <- as.data.frame(det_tab)

row.names(det_tab)<-c("1",
                      "1.1",
                      "1.2",
                      "2",
                      "3",
                      "4=1+2+3",
                      "5",
                      "5.1",
                      "5.11",
                      "5.12",
                      "5.2",
                      "5.21",
                      "5.22",
                      "5.23",
                      "6",
                      "7",
                      "8",
                      "9",
                      "10=5.11+5.21+5.22+6+7+8",
                      "11=5.12+5.23",
                      "12=10+11",
                      "13",
                      "13.1",
                      "13.2",
                      "14",
                      "15=14+13.1+9",
                      "16=13.2",
                      "17=15+16",
                      "18",
                      "19",
                      "20",
                      "21=22-15",
                      "22=1-10",
                      "23=2-11",
                      "24=4-12",
                      "25=22-19",
                      "26=23-20",
                      "27=24-18")

colnames(det_tab) <- c("Hesaplar", as.character(zaman), as.character(zaman1), as.character(ilkk))

det_tab


lab= c(
  "Merkez Bankasi Rezervleri ve Yukumlulukleri", #1 
  "Bilanco Ici Yukumlulukler", #2
  "Brut Rezervler", #3
  "Bilanco Disi Yukumlulukler", #4
  "Bankacilik Sektoru Mevduati", #5
  "Diger Mevduatlar", #6
  "VIOP", #6.5
  "Yurtdisi Bankalar", #7
  "SDR Tahsisati", #8
  "Yurtici Bankalar", #9
  "Yurtdisi Bankalar", #10
  "Zorunlu Karsiliklar", #11
  "Yurtici Banka Nakit", #12
  "Yurtici Banka Teminat", #13
  "Yurtici Banka Altin", #14
  "ZK Doviz", #15
  "ZK Altin", #16
  "Rezerv Altin", #17
  "Rezerv Doviz", #18
  "SDR", #19
  "Menkul Kiymetler", #20
  "Toplam Nakit ve Mevduatlar", #21
  "Yabanci MB", #22
  "Yurtici Bankalar Swap", #23
  "Yabanci Swap Doviz", #24
  "Swap Altin", #25
  "Swap Doviz" #26
  
)




par = c(
  "",
  "Merkez Bankasi Rezervleri ve Yukumlulukleri",
  "Merkez Bankasi Rezervleri ve Yukumlulukleri",
  "Merkez Bankasi Rezervleri ve Yukumlulukleri",
  "Bilanco Ici Yukumlulukler",
  "Bilanco Ici Yukumlulukler",
  "Bilanco Ici Yukumlulukler",
  "Bilanco Ici Yukumlulukler",
  "Bilanco Ici Yukumlulukler",
  "Bankacilik Sektoru Mevduati",
  "Bankacilik Sektoru Mevduati",
  "Bankacilik Sektoru Mevduati",
  "Yurtici Bankalar",
  "Yurtici Bankalar",
  "Yurtici Bankalar",
  "Zorunlu Karsiliklar",
  "Zorunlu Karsiliklar",
  "Brut Rezervler",
  "Brut Rezervler",
  "Brut Rezervler",
  "Rezerv Doviz",
  "Rezerv Doviz",
  "Bilanco Disi Yukumlulukler",
  "Bilanco Disi Yukumlulukler",
  "Yabanci MB",
  "Yurtici Bankalar Swap",
  "Yurtici Bankalar Swap"
)

valu = c(
  "",
  round(tdt[time == zaman]$biy, 2),
  round(tdt[time == zaman]$brut, 2),
  round(tdt[time == zaman]$bdy, 2),
  round(tdt[time == zaman]$bsb, 2),
  round(tdt[time == zaman]$dm, 2),
  round(tdt[time == zaman]$viop, 2),
  round(tdt[time == zaman]$mbydby, 2),
  round(tdt[time == zaman]$sdry, 2),
  round(tdt[time == zaman]$yib, 2),
  round(tdt[time == zaman]$bydby, 2),
  round(tdt[time == zaman]$zk, 2),
  round(tdt[time == zaman]$yibn, 2),
  round(tdt[time == zaman]$yibt, 2),
  round(tdt[time == zaman]$yiba, 2),
  round(tdt[time == zaman]$zkd, 2),
  round(tdt[time == zaman]$zka, 2),
  round(tdt[time == zaman]$bra, 2),
  round(tdt[time == zaman]$brd, 2),
  round(tdt[time == zaman]$brsdr, 2),
  round(tdt[time == zaman]$mk, 2),
  round(tdt[time == zaman]$tnvm, 2),
  round(tdt[time == zaman]$ydmbs, 2),
  round(tdt[time == zaman]$yibs, 2),
  round(tdt[time == zaman]$ydmbs, 2),
  round(tdt[time == zaman]$yibas, 2),
  round(tdt[time == zaman]$yibds, 2)
)


fig<-plot_ly(
  type="treemap",
  labels=lab,
  parents=par,
  values=valu,
  textinfo="label+value",
  marker=list(colors=c("#1a1a1a", #1
                       "#4292c6", #2
                       "#41ab5d", #3
                       "#ef3b2c", #4
                       "#6baed6", #5
                       "#6baed6", #6
                       "#6baed6", #6.5
                       "#6baed6", #7
                       "#6baed6", #8
                       "#9ecae1", #9
                       "#9ecae1", #10
                       "#9ecae1", #11
                       "#c6dbef", #12
                       "#c6dbef", #13
                       "#c6dbef", #14
                       "#c6dbef", #15
                       "#c6dbef", #16
                       "#74c476", #17
                       "#74c476", #18
                       "#74c476", #19
                       "#a1d99b", #20
                       "#a1d99b", #21
                       "#fb6a4a", #22
                       "#fb6a4a", #23
                       "#fc9272", #24
                       "#fc9272", #25
                       "#fc9272"))) #26

fig

nets <- data.frame(code = c("Doviz", "Altin", "Net Rezerv", "Altin", "Doviz", "Net Uluslararasi Rezerv"),
                   value = c(tdt[time == zaman]$ndr, tdt[time == zaman]$nar, tdt[time == zaman]$netr, 
                             tdt[time == zaman]$shnar, tdt[time == zaman]$shndr, tdt[time == zaman]$shnetr))

nets <- as.data.table(nets)

snr <- plot_ly(nets) %>%
  add_trace(x = nets[4:5,]$code, y = round(nets[4:5,]$value/1000, 2), type = 'bar', width = .3,
            marker = list(color = c("#4682b4", "#b22222")),
            hovertemplate = "%{value} Milyar Dolar <br> %{label}<extra></extra>")%>% 
  layout(title = "Swap Haric Net Rezerv Pozisyonu (Milyar Dolar)", 
         shapes = list(type = 'line', x0 = -1, x1 = 2.5,
                     y0 = round(sum(nets[4,]$value, nets[5,]$value)/1000, 2), 
                     y1 = round(sum(nets[4,]$value, nets[5,]$value)/1000, 2), 
                     line = list(dash = 'dot', width = 3))) %>% 
  layout(annotations = list(yref = 'paper', xref = 0, y = 0.28, x = 0, showarrow = FALSE,
                           text = paste0("Swap Haric Net Rezerv: ", round(nets[6,2]/1000, 2)), size = 10),
         xaxis=list(fixedrange=T),yaxis=list(fixedrange=T))

snr

nr <- plot_ly(nets) %>%
  add_trace(x = nets[1:2,]$code, y = round(nets[1:2,]$value/1000, 2), type = 'bar', width = .3,
            marker = list(color = c("#4682b4", "#b22222")),
            hovertemplate = "%{value} Milyar Dolar <br> %{label}<extra></extra>")%>% 
  layout(title = " Net Rezerv Pozisyonu (Milyar Dolar)", 
         shapes=list(type = 'line', x0 = -1, x1 = 2.5,
                     y0 = round(sum(nets[1,]$value, nets[2,]$value)/1000,2), 
                     y1 = round(sum(nets[1,]$value, nets[2,]$value)/1000,2), 
                     line = list(dash = 'dot', width = 3))) %>% 
  layout(annotations= list(yref = 'paper', xref = 0, y = 0.64, x = 1, showarrow = FALSE,
                           text = paste0("Net Rezerv: ", round(nets[3,2]/1000,2)), size = 10),
         xaxis=list(fixedrange=T),yaxis=list(fixedrange=T))

nr




#################### swap bolu brutu hesapladiktan sonra "zama"daki data yerine koyucaz, grafik cikacak

tdt <- tdt[time < as.Date(zaman+1)]


zama <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = tdt[2:nrow(tdt)]$swbro, 
                type = 'scatter', mode = 'lines',
                hovertemplate = "%{x} <br> %{y} <extra></extra>") %>%
  layout(title = "SWAP - Brut Rezerv Orani",
         yaxis = list(tickformat = "%",fixedrange=T),
         xaxis = list(type = 'date',tickformat = "%d %B <br>%Y",fixedrange=T))

zama 

viog <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = tdt[2:nrow(tdt)]$viop/1000, 
                type = 'scatter', mode = 'lines',
                hovertemplate = "%{x} <br> %{y} Milyar Dolar <extra></extra>") %>%
  layout(title = "VIOP",
         xaxis = list(type = 'date',tickformat = "%d %B <br>%Y",fixedrange=T),
         yaxis = list(fixedrange=T))

viog

ndpg <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = tdt[2:nrow(tdt)]$ndp/1000, 
                type = 'scatter', mode = 'lines',
                hovertemplate = "%{x} <br> %{y} Milyar Dolar <extra></extra>") %>%
  layout(title = "Net Doviz Pozisyonu",
         xaxis = list(type = 'date',tickformat = "%d %B <br>%Y",fixedrange=T),
         yaxis = list(fixedrange=T))

ndpg

ypme <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = tdt[2:nrow(tdt)]$swbmevd, 
                type = 'scatter', mode = 'lines',
                hovertemplate = "%{x} <br> %{y} <extra></extra>") %>%
  layout(title = "Yurtici Bankalar SWAP - Bankalar YP Mevduati Orani",
         yaxis = list(tickformat = "%",fixedrange=T),
         xaxis = list(type = 'date',tickformat = "%d %B <br>%Y",fixedrange=T))

ypme

tgr <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$brut/1000, 2), type = 'scatter', mode = 'lines', 
               name = 'Brut Rezervler', hovertemplate = "Brut Rezervler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>") %>%
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$biy/1000, 2), mode = 'lines', name = 'Bilanco Ici Yukumlulukler',
            hovertemplate = "Bilanco Ici Yukumlulukler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>")%>% 
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$bdy/1000, 2), mode = 'lines', name = 'Bilanco Disi Yukumlulukler',
            hovertemplate = "Bilanco Disi Yukumlulukler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>")%>%
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$netr/1000, 2), mode = 'lines', name = 'Net Rezervler',
            hovertemplate = "Net Rezervler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>")%>% 
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$shnetr/1000, 2), mode = 'lines', name = 'Swap Hariç Net Rezervler',
            hovertemplate = "Swap Hariç Net Rezervler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>")%>% 
  layout(title = " Net Rezerv Pozisyonu (Milyar Dolar)") %>% 
  layout(yaxis = list(tickformat = "000", fixedrange=T),xaxis=list(fixedrange=T))
tgr

a <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$brut/1000, 2), type = 'scatter', mode = 'lines', 
             name = 'Brut Rezervler', hovertemplate = "Brut Rezervler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>") %>%
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$biy/1000, 2), mode = 'lines', name = 'Bilanco Ici Yukumlulukler',
            hovertemplate = "Bilanco Ici Yukumlulukler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>")%>% 
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$bdy/1000, 2), mode = 'lines', name = 'Bilanco Disi Yukumlulukler',
            hovertemplate = "Bilanco Disi Yukumlulukler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>")%>% 
  layout(title = " Net Rezerv Pozisyonu (Milyar Dolar)") %>% 
  layout(yaxis = list(tickformat = "000", fixedrange =T),xaxis=list(fixedrange=T))
a

b <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$netr/1000, 2), type = 'scatter', mode = 'lines', 
             name = 'Net Rezervler', hovertemplate = "Net Rezervler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>") %>%
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$shnetr/1000, 2), mode = 'lines', name = 'Swap Hariç Net Rezervler',
            hovertemplate = "Swap Hariç Net Rezervler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>")%>% 
  layout(title = " Net Rezerv Pozisyonu (Milyar Dolar)") %>% 
  layout(yaxis = list(tickformat = "000",fixedrange=T),xaxis=list(fixedrange=T))
b

c <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$brut/1000, 2), type = 'scatter', mode = 'lines', 
             name = 'Brut Rezervler', hovertemplate = "Brut Rezervler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>") %>%
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$biy/1000, 2), mode = 'lines', name = 'Bilanco Ici Yukumlulukler',
            hovertemplate = "Bilanco Ici Yukumlulukler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>")%>% 
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$bdy/1000, 2), mode = 'lines', name = 'Bilanco Disi Yukumlulukler',
            hovertemplate = "Bilanco Disi Yukumlulukler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>")%>%
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$netr/1000, 2), mode = 'lines', name = 'Net Rezervler',
            hovertemplate = "Net Rezervler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>")%>% 
  layout(title = " Net Rezerv Pozisyonu (Milyar Dolar)") %>% 
  layout(yaxis = list(tickformat = "000",fixedrange=T),xaxis=list(fixedrange=T))
c

d <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$netr/1000, 2), type = 'scatter', mode = 'marker + lines', 
             name = 'Net Rezervler', hovertemplate = "Net Rezervler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>") %>%
  add_trace(x = tdt[2:nrow(tdt)]$time, y = -1*round(tdt[2:nrow(tdt)]$biy/1000, 2), type = 'bar', name = 'Bilanco Ici Yukumlulukler',
            hovertemplate = "Bilanco Ici Yukumlulukler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>")%>% 
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$brut/1000, 2), type = 'bar', name = 'Brut Rezervler',
            hovertemplate = "Brut Rezervler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>")%>% 
  layout(title = " Net Rezerv Pozisyonu (Milyar Dolar)") %>% 
  layout(yaxis = list(tickformat = "000",fixedrange=T),barmode = 'stack-relative',xaxis=list(fixedrange=T))
d

e <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$shnetr/1000, 2), type = 'scatter', mode = 'marker + lines', 
             name = 'Swap Haric Net Rezervler', hovertemplate = "Swap Haric Net Rezervler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>") %>%
  add_trace(x = tdt[2:nrow(tdt)]$time, y = -1*round(tdt[2:nrow(tdt)]$biy/1000, 2), type = 'bar', name = 'Bilanco Ici Yukumlulukler',
            hovertemplate = "Bilanco Ici Yukumlulukler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>")%>% 
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$brut/1000, 2), type = 'bar', name = 'Brut Rezervler',
            hovertemplate = "Brut Rezervler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>")%>% 
  add_trace(x = tdt[2:nrow(tdt)]$time, y = -1*round(tdt[2:nrow(tdt)]$bdy/1000, 2), type = 'bar', name = 'Bilanco Disi Yukumlulukler',
            hovertemplate = "Bilanco Disi Yukumlulukler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>")%>% 
  layout(title = " Net Rezerv Pozisyonu (Milyar Dolar)") %>% 
  layout(yaxis = list(tickformat = "000",fixedrange=T),barmode = 'stack-relative',
         xaxis=list(fixedrange=T))
e

f <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$shnetr/1000, 2), type = 'scatter', mode = 'marker + lines', 
             name = 'Swap Haric Net Rezervler', hovertemplate = "Swap Haric Net Rezervler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>") %>%
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$shnar/1000, 2), type = 'bar', name = 'Altin',
            hovertemplate = "Altin <br> %{y} Milyar Dolar <br> %{x}<extra></extra>")%>% 
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$shndr/1000, 2), type = 'bar', name = 'Doviz',
            hovertemplate = "Doviz <br> %{y} Milyar Dolar <br> %{x}<extra></extra>")%>% 
  layout(title = " Net Rezerv Pozisyonu (Milyar Dolar)") %>% 
  layout(yaxis = list(tickformat = "000",fixedrange=T),barmode = 'stack-relative',
         xaxis=list(fixedrange=T))
f

g <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$netr/1000, 2), type = 'scatter', mode = 'marker + lines', 
             name = 'Net Rezervler', hovertemplate = "Net Rezervler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>") %>%
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$nar/1000, 2), type = 'bar', name = 'Altin',
            hovertemplate = "Altin <br> %{y} Milyar Dolar <br> %{x}<extra></extra>")%>% 
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$ndr/1000, 2), type = 'bar', name = 'Doviz',
            hovertemplate = "Doviz <br> %{y} Milyar Dolar <br> %{x}<extra></extra>")%>% 
  layout(title = " Net Rezerv Pozisyonu (Milyar Dolar)") %>% 
  layout(yaxis = list(tickformat = "000",fixedrange=T),barmode = 'stack-relative',
         xaxis=list(fixedrange=T))
g

shnetr_graph <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$shnetr/1000, 2), 
                        type = 'scatter', mode = 'marker + lines', name = 'Swap Haric Net Rezervler', 
                        hovertemplate = "Swap Haric Net Rezervler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>") %>%
                 layout(title = list(text = paste0('Swap Haric Net Rezervler',
                                                   '<br>',
                                                   '<sup>',
                                                   '(Milyar Dolar)',
                                                   '</sup>')),
                        xaxis=list(fixedrange=T),yaxis=list(fixedrange=T))

shnetr_graph

netr_graph <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$netr/1000, 2), 
                        type = 'scatter', mode = 'marker + lines', name = 'Net Rezervler', 
                        hovertemplate = "Net Rezervler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>") %>%
  layout(title = list(text = paste0('Net Rezervler',
                                    '<br>',
                                    '<sup>',
                                    '(Milyar Dolar)',
                                    '</sup>')),
         xaxis=list(fixedrange=T),yaxis=list(fixedrange=T))

netr_graph

brut_graph <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$brut/1000, 2), 
                        type = 'scatter', mode = 'marker + lines', name = 'Brut Rezervler', 
                        hovertemplate = "Brut Rezervler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>") %>%
  layout(title = list(text = paste0('Brut Rezervler',
                                    '<br>',
                                    '<sup>',
                                    '(Milyar Dolar)',
                                    '</sup>')),
         xaxis=list(fixedrange=T),yaxis=list(fixedrange=T))

brut_graph

biy_graph <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$biy/1000, 2), 
                        type = 'scatter', mode = 'marker + lines', name = 'Bilanco Ici Yukumlulukler', 
                        hovertemplate = "Bilanco Ici Yukumlulukler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>") %>%
  layout(title = list(text = paste0('Bilanco Ici Yukumlulukler',
                                    '<br>',
                                    '<sup>',
                                    '(Milyar Dolar)',
                                    '</sup>')),
         xaxis=list(fixedrange=T),yaxis=list(fixedrange=T))

biy_graph

bdy_graph <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$bdy/1000, 2), 
                     type = 'scatter', mode = 'marker + lines', name = 'Bilanco Disi Yukumlulukler', 
                     hovertemplate = "Bilanco Disi Yukumlulukler <br> %{y} Milyar Dolar <br> %{x}<extra></extra>") %>%
  layout(title = list(text = paste0('Bilanco Disi Yukumlulukler',
                                    '<br>',
                                    '<sup>',
                                    '(Milyar Dolar)',
                                    '</sup>')),
         xaxis=list(fixedrange=T),yaxis=list(fixedrange=T))

bdy_graph

nar_graph <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$nar/1000, 2), 
                     type = 'scatter', mode = 'marker + lines', name = 'Net Altin Rezervi', 
                     hovertemplate = "Net Altin Rezervi <br> %{y} Milyar Dolar <br> %{x}<extra></extra>") %>%
  layout(title = list(text = paste0('Net Altin Rezervi',
                                    '<br>',
                                    '<sup>',
                                    '(Milyar Dolar)',
                                    '</sup>')),
         xaxis=list(fixedrange=T),yaxis=list(fixedrange=T))

nar_graph

ndr_graph <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$ndr/1000, 2), 
                     type = 'scatter', mode = 'marker + lines', name = 'Net Doviz Rezervi', 
                     hovertemplate = "Net Doviz Rezervi <br> %{y} Milyar Dolar <br> %{x}<extra></extra>") %>%
  layout(title = list(text = paste0('Net Doviz Rezervi',
                                    '<br>',
                                    '<sup>',
                                    '(Milyar Dolar)',
                                    '</sup>')),
         xaxis=list(fixedrange=T),yaxis=list(fixedrange=T))

ndr_graph

shndr_graph <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$shndr/1000, 2), 
                     type = 'scatter', mode = 'marker + lines', name = 'Swap Haric Net Doviz Rezervi', 
                     hovertemplate = "Swap Haric Net Doviz Rezervi <br> %{y} Milyar Dolar <br> %{x}<extra></extra>") %>%
  layout(title = list(text = paste0('Swap Haric Net Doviz Rezervi',
                                    '<br>',
                                    '<sup>',
                                    '(Milyar Dolar)',
                                    '</sup>')),
         xaxis=list(fixedrange=T),yaxis=list(fixedrange=T))

shndr_graph

shnar_graph <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$shnar/1000, 2), 
                       type = 'scatter', mode = 'marker + lines', name = 'Swap Haric Net Altin Rezervi', 
                       hovertemplate = "Swap Haric Net Altin Rezervi <br> %{y} Milyar Dolar <br> %{x}<extra></extra>") %>%
  layout(title = list(text = paste0('Swap Haric Net Altin Rezervi',
                                    '<br>',
                                    '<sup>',
                                    '(Milyar Dolar)',
                                    '</sup>')),
         xaxis=list(fixedrange=T),yaxis=list(fixedrange=T))

shnar_graph

sub1 <- subplot(netr_graph, shnetr_graph, nrows = 2, shareX = T, titleY  = T)%>%
  layout(hovermode = "x unified") %>%
  layout(title = list(text = paste0('Net ve Swap Haric Net Rezervler',
                                    '<br>',
                                    '<sup>',
                                    '(Milyar Dolar)',
                                    '</sup>')),legend = list(orientation = 'h'),
         xaxis=list(fixedrange=T),yaxis=list(fixedrange=T)) 

sub1

sub2 <- subplot(brut_graph, biy_graph, bdy_graph, nrows = 3, shareX = T, titleY  = T)%>%
  layout(hovermode = "x unified") %>%
  layout(title = list(text = paste0('Brut Rezervler ve Yukumlulukler',
                                    '<br>',
                                    '<sup>',
                                    '(Milyar Dolar)',
                                    '</sup>')),legend = list(orientation = 'h'),
         xaxis=list(fixedrange=T),yaxis=list(fixedrange=T)) 

sub2

sub3 <- subplot(nar_graph, netr_graph, ndr_graph, nrows = 3, shareX = T, titleY  = T)%>%
  layout(hovermode = "x unified") %>%
  layout(title = list(text = paste0('Net Rezervler',
                                    '<br>',
                                    '<sup>',
                                    '(Milyar Dolar)',
                                    '</sup>')),legend = list(orientation = 'h'),
         xaxis=list(fixedrange=T),yaxis=list(fixedrange=T)) 

sub3

sub4 <- subplot(shnar_graph, shnetr_graph, shndr_graph, nrows = 3, shareX = T, titleY  = T)%>%
  layout(hovermode = "x unified") %>%
  layout(title = list(text = paste0('Swap Haric Net Rezervler',
                                    '<br>',
                                    '<sup>',
                                    '(Milyar Dolar)',
                                    '</sup>')),legend = list(orientation = 'h'),
         xaxis=list(fixedrange=T),yaxis=list(fixedrange=T)) 

sub4

