library(plotly)

library(data.table)

library(ggplot2)

library(Rmisc)

library(dplyr)

Sys.setlocale(category = "LC_ALL", locale = "Turkish")

################################# Tablolar #############################

yutk <- data.frame(
  
  "1" = c("Gross Reserves", 
          paste0(round((tdt[time == zaman]$brut)/1000, 2), " Billion $"),
          paste0(round((tdt[time == zaman1]$brut)/1000, 2), " Billion $"),
          paste0(round((tdt[time == ilkk]$brut)/1000, 2), " Billion $")),
  
  "2" = c("Balance Sheet Liabilities",
          paste0(round((tdt[time == zaman]$biy)/1000, 2), " Billion $"),
          paste0(round((tdt[time == zaman1]$biy)/1000, 2), " Billion $"),
          paste0(round((tdt[time == ilkk]$biy)/1000, 2), " Billion $")),
  
  "3" = c("Off-Balance Sheet Liabilities",
          paste0(round((tdt[time == zaman]$bdy)/1000, 2), " Billion $"),
          paste0(round((tdt[time == zaman1]$bdy)/1000, 2), " Billion $"),
          paste0(round((tdt[time == ilkk]$bdy)/1000, 2), " Billion $")),
  
  "4" = c("SWAP",
          paste0(round((tdt[time == zaman]$tsw)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == zaman1]$tsw)/1000, 2), " Milyar $"),
          paste0(round((tdt[time == ilkk]$tsw)/1000, 2), " Milyar $")),
  
  "=1-2" = c("Net Reserves",
             paste0(round((tdt[time == zaman]$netr)/1000, 2), " Billion $"),
             paste0(round((tdt[time == zaman1]$netr)/1000, 2), " Billion $"),
             paste0(round((tdt[time == ilkk]$netr)/1000, 2), " Billion $")),
  
  "=1-2-4" = c("Net Reserves (excluding Swap)", 
               paste0(round((tdt[time == zaman]$shnetr)/1000, 2), " Billion $"),
               paste0(round((tdt[time == zaman1]$shnetr)/1000, 2), " Billion $"),
               paste0(round((tdt[time == ilkk]$shnetr)/1000, 2), " Billion $"))
)

yutk <- t(yutk)

yutk <-as.data.frame(yutk)

row.names(yutk) <- c("   1","   2","   3","   4","   =1-2","   =1-2-4")

colnames(yutk) <- c("Accounts", as.character(zaman), as.character(zaman1), as.character(ilkk))

yutk

det_tab <- data.frame(
  
  "1" = c("Gross FX Reserves", 
          paste0(round((tdt[time == zaman]$brd)/1000, 2), " Billion $"),
          paste0(round((tdt[time == zaman1]$brd)/1000, 2), " Billion $"),
          paste0(round((tdt[time == ilkk]$brd)/1000, 2), " Billion $")),
  
  "1.1" = c("Total Cash and Deposit", 
            paste0(round((tdt[time == zaman]$tnvm)/1000, 2), " Billion $"),
            paste0(round((tdt[time == zaman1]$tnvm)/1000, 2), " Billion $"),
            paste0(round((tdt[time == ilkk]$tnvm)/1000, 2), " Billion $")),
  
  "1.2" = c("Securities", 
            paste0(round((tdt[time == zaman]$mk)/1000, 2), " Billion $"),
            paste0(round((tdt[time == zaman1]$mk)/1000, 2), " Billion $"),
            paste0(round((tdt[time == ilkk]$mk)/1000, 2), " Billion $")),
  
  "2" = c("Gross Gold Reserves", 
          paste0(round((tdt[time == zaman]$bra)/1000, 2), " Billion $"),
          paste0(round((tdt[time == zaman1]$bra)/1000, 2), " Billion $"),
          paste0(round((tdt[time == ilkk]$bra)/1000, 2), " Billion $")),
  
  "3" = c("Gross SDR Reserves", 
          paste0(round((tdt[time == zaman]$brsdr)/1000, 2), " Billion $"),
          paste0(round((tdt[time == zaman1]$brsdr)/1000, 2), " Billion $"),
          paste0(round((tdt[time == ilkk]$brsdr)/1000, 2), " Billion $")),
  
  "4=1+2+3" = c("Total Gross Reserves", 
                paste0(round((tdt[time == zaman]$brut)/1000, 2), " Billion $"),
                paste0(round((tdt[time == zaman1]$brut)/1000, 2), " Billion $"),
                paste0(round((tdt[time == ilkk]$brut)/1000, 2), " Billion $")),
  
  "5" = c("Banking Sector Balance Sheet",
          paste0(round((tdt[time == zaman]$bsb)/1000, 2), " Billion $"),
          paste0(round((tdt[time == zaman1]$bsb)/1000, 2), " Billion $"),
          paste0(round((tdt[time == ilkk]$bsb)/1000, 2), " Billion $")),
  
  "5.1" = c("Required Reserves",
            paste0(round((tdt[time == zaman]$zk)/1000, 2), " Billion $"),
            paste0(round((tdt[time == zaman1]$zk)/1000, 2), " Billion $"),
            paste0(round((tdt[time == ilkk]$zk)/1000, 2), " Billion $")),
  
  "5.11" = c("Required Reserves - FX",
             paste0(round((tdt[time == zaman]$zkd)/1000, 2), " Billion $"),
             paste0(round((tdt[time == zaman1]$zkd)/1000, 2), " Billion $"),
             paste0(round((tdt[time == ilkk]$zkd)/1000, 2), " Billion $")),
  
  "5.12" = c("Required Reserves - Gold",
             paste0(round((tdt[time == zaman]$zka)/1000, 2), " Billion $"),
             paste0(round((tdt[time == zaman1]$zka)/1000, 2), " Billion $"),
             paste0(round((tdt[time == ilkk]$zka)/1000, 2), " Billion $")),
  
  "5.2" = c("Domestic Banks",
            paste0(round((tdt[time == zaman]$yib)/1000, 2), " Billion $"),
            paste0(round((tdt[time == zaman1]$yib)/1000, 2),  " Billion $"),
            paste0(round((tdt[time == ilkk]$yib)/1000, 2),  " Billion $")),
  
  "5.21" = c("Domestic Banks - Cash",
             paste0(round((tdt[time == zaman]$yibn)/1000, 2), " Billion $"),
             paste0(round((tdt[time == zaman1]$yibn)/1000, 2), " Billion $"),
             paste0(round((tdt[time == ilkk]$yibn)/1000, 2), " Billion $")),
  
  "5.22" = c("Domestic Banks - Collateral",
             paste0(round((tdt[time == zaman]$yibt)/1000, 2), " Billion $"),
             paste0(round((tdt[time == zaman1]$yibt)/1000, 2), " Billion $"),
             paste0(round((tdt[time == ilkk]$yibt)/1000, 2), " Billion $")),
  
  "5.23" = c("Domestic Banks - Gold",
             paste0(round((tdt[time == zaman]$yiba)/1000, 2), " Billion $"),
             paste0(round((tdt[time == zaman1]$yiba)/1000, 2), " Billion $"),
             paste0(round((tdt[time == ilkk]$yiba)/1000, 2), " Billion $")),
  
  "6" = c("CBRT's Liabilities to Foreign Banks",
          paste0(round((tdt[time == zaman]$mbydby)/1000, 2), " Billion $"),
          paste0(round((tdt[time == zaman1]$mbydby)/1000, 2), " Billion $"),
          paste0(round((tdt[time == ilkk]$mbydby)/1000, 2), " Billion $")),
  
  "7" = c("SDR Liabilities",
          paste0(round((tdt[time == zaman]$sdry)/1000, 2), " Billion $"),
          paste0(round((tdt[time == zaman1]$sdry)/1000, 2), " Billion $"),
          paste0(round((tdt[time == ilkk]$sdry)/1000, 2), " Billion $")),
  
  "8" = c("Other Deposits",
          paste0(round((tdt[time == zaman]$dm)/1000, 2), " Billion $"),
          paste0(round((tdt[time == zaman1]$dm)/1000, 2), " Billion $"),
          paste0(round((tdt[time == ilkk]$dm)/1000, 2),  " Billion $")),
  
  "9" = c("Options",
          paste0(round((tdt[time == zaman]$viop)/1000, 2), " Billion $"),
          paste0(round((tdt[time == zaman1]$viop)/1000, 2), " Billion $"),
          paste0(round((tdt[time == ilkk]$viop)/1000, 2),  " Billion $")),
  
  "10=5.11+5.21+5.22+6+7+8" = c("Balance Sheet FX Liabilities",
                             paste0(round((tdt[time == zaman]$bidy)/1000, 2), " Billion $"),
                             paste0(round((tdt[time == zaman1]$bidy)/1000, 2), " Billion $"),
                             paste0(round((tdt[time == ilkk]$bidy)/1000, 2), " Billion $")),
  
  "11=5.12+5.23" = c("Balance Sheet Gold Liabilities",
                     paste0(round((tdt[time == zaman]$biay)/1000, 2), " Billion $"),
                     paste0(round((tdt[time == zaman1]$biay)/1000, 2), " Billion $"),
                     paste0(round((tdt[time == ilkk]$biay)/1000, 2), " Billion $")),
  
  "12=10+11" = c("Balance Sheet Liabilities",
                paste0(round((tdt[time == zaman]$biy)/1000, 2), " Billion $"),
                paste0(round((tdt[time == zaman1]$biy)/1000, 2), " Billion $"),
                paste0(round((tdt[time == ilkk]$biy)/1000, 2), " Billion $")),
  
  "13" = c("Domestic Banks - Swap",
           paste0(round((tdt[time == zaman]$yibs)/1000, 2), " Billion $"),
           paste0(round((tdt[time == zaman1]$yibs)/1000, 2), " Billion $"),
           paste0(round((tdt[time == ilkk]$yibs)/1000, 2), " Billion $")),
  
  "13.1" = c("Domestic Banks - Swap - FX",
             paste0(round((tdt[time == zaman]$yibds)/1000, 2), " Billion $"),
             paste0(round((tdt[time == zaman1]$yibds)/1000, 2), " Billion $"),
             paste0(round((tdt[time == ilkk]$yibds)/1000, 2), " Billion $")),
  
  "13.2" = c("Domestic Banks - Swap - Gold",
             paste0(round((tdt[time == zaman]$yibas)/1000, 2), " Billion $"),
             paste0(round((tdt[time == zaman1]$yibas)/1000, 2), " Billion $"),
             paste0(round((tdt[time == ilkk]$yibas)/1000, 2), " Billion $")),
  
  "14" = c("Foreign Central Banks - Swap",
           paste0(round((tdt[time == zaman]$ydmbs)/1000, 2), " Billion $"),
           paste0(round((tdt[time == zaman1]$ydmbs)/1000, 2), " Billion $"),
           paste0(round((tdt[time == ilkk]$ydmbs)/1000, 2), " Billion $")),
  
  "15=14+13.1+9" = c("Off Balance Sheet FX Liabilities",
                   paste0(round((tdt[time == zaman]$bddy)/1000, 2), " Billion $"),
                   paste0(round((tdt[time == zaman1]$bddy)/1000, 2), " Billion $"),
                   paste0(round((tdt[time == ilkk]$bddy)/1000, 2), " Billion $")),
  
  "16=13.2" = c("Off Balance Sheet Gold Liabilities",
           paste0(round((tdt[time == zaman]$bday)/1000, 2), " Billion $"),
           paste0(round((tdt[time == zaman1]$bday)/1000, 2), " Billion $"),
           paste0(round((tdt[time == ilkk]$bday)/1000, 2), " Billion $")),
  
  "17=15+16" = c("Off Balance Sheet Liabilities",
                 paste0(round((tdt[time == zaman]$bdy)/1000, 2), " Billion $"),
                 paste0(round((tdt[time == zaman1]$bdy)/1000, 2), " Billion $"),
                 paste0(round((tdt[time == ilkk]$bdy)/1000, 2), " Billion $")),
  
  "18" = c("Total SWAP",
           paste0(round((tdt[time == zaman]$tsw)/1000, 2), " Milyar $"),
           paste0(round((tdt[time == zaman1]$tsw)/1000, 2), " Milyar $"),
           paste0(round((tdt[time == ilkk]$tsw)/1000, 2), " Milyar $")),
  
  "19" = c("Total Swap - FX",
           paste0(round((tdt[time == zaman]$tswd)/1000, 2), " Milyar $"),
           paste0(round((tdt[time == zaman1]$tswd)/1000, 2), " Milyar $"),
           paste0(round((tdt[time == ilkk]$tswd)/1000, 2), " Milyar $")),
  
  "20" = c("Total Swap - Gold",
           paste0(round((tdt[time == zaman]$tswa)/1000, 2), " Milyar $"),
           paste0(round((tdt[time == zaman1]$tswa)/1000, 2), " Milyar $"),
           paste0(round((tdt[time == ilkk]$tswa)/1000, 2), " Milyar $")),
  
  "21=22-15" = c("Net FX Position",
                 paste0(round((tdt[time == zaman]$ndp)/1000, 2), " Milyar $"),
                 paste0(round((tdt[time == zaman1]$ndp)/1000, 2), " Milyar $"),
                 paste0(round((tdt[time == ilkk]$ndp)/1000, 2), " Milyar $")),
  
  "22=1+3-10" = c("Net FX Reserves",
               paste0(round((tdt[time == zaman]$ndr)/1000, 2), " Billion $"),
               paste0(round((tdt[time == zaman1]$ndr)/1000, 2), " Billion $"),
               paste0(round((tdt[time == ilkk]$ndr)/1000, 2), " Billion $")),
  
  "23=2-11" = c("Net Gold Reserves",
                paste0(round((tdt[time == zaman]$nar)/1000, 2), " Billion $"),
                paste0(round((tdt[time == zaman1]$nar)/1000, 2), " Billion $"),
                paste0(round((tdt[time == ilkk]$nar)/1000, 2), " Billion $")),
  
  "24=4-12" = c("Net Reserves",
                 paste0(round((tdt[time == zaman]$netr)/1000, 2), " Billion $"),
                 paste0(round((tdt[time == zaman1]$netr)/1000, 2), " Billion $"),
                 paste0(round((tdt[time == ilkk]$netr)/1000, 2), " Billion $")),
  
  "25=22-19" = c("Net FX Reserves (excluding Swap)", 
                 paste0(round((tdt[time == zaman]$shndr)/1000, 2), " Billion $"),
                 paste0(round((tdt[time == zaman1]$shndr)/1000, 2), " Billion $"),
                 paste0(round((tdt[time == ilkk]$shndr)/1000, 2), " Billion $")),
  
  "26=23-20" = c("Net Gold Reserves (excluding Swap)", 
                   paste0(round((tdt[time == zaman]$shnar)/1000, 2), " Billion $"),
                   paste0(round((tdt[time == zaman1]$shnar)/1000, 2), " Billion $"),
                   paste0(round((tdt[time == ilkk]$shnar)/1000, 2), " Billion $")),
  
  "27=24-18" = c("Net Reserves (excluding Swap)", 
                 paste0(round((tdt[time == zaman]$shnetr)/1000, 2), " Billion $"),
                 paste0(round((tdt[time == zaman1]$shnetr)/1000, 2), " Billion $"),
                 paste0(round((tdt[time == ilkk]$shnetr)/1000, 2), " Billion $"))
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

colnames(det_tab) <- c("Accounts", as.character(zaman), as.character(zaman1), as.character(ilkk))

det_tab

lab= c(
  "CBRT Reserves and Liabilities", #1
  "Balance Sheet Liabilities", #2
  "Gross Reserves", #3
  "Off-Balance Sheet Liabilities", #4
  "Banking Sector Deposits", #5
  "Other Deposits", #6
  "Options", #6.5
  "CBRT's Liabilities to Foreign Banks", #7
  "SDR Allocation", #8
  "Domestic Banks", #9
  "Domestic Banks' Liabilities to Foreign Banks", #10
  "Required Reserves", #11
  "Domestic Banks - Cash", #12
  "Domestic Banks - Collateral", #13
  "Domestic Banks - Gold", #14
  "RR - FX", #15
  "RR - Gold", #16
  "Gold Reserves", #17
  "FX Reserves", #18
  "SDR", #19
  "Securities", #20
  "Total Cash and Deposits", #21
  "Foreign Central Banks", #22
  "Domestic Banks Swap", #23
  "Foreign Swap FX", #24
  "Swap - Gold", #25
  "Swap - FX" #26
  
)




par = c(
  "",
  "CBRT Reserves and Liabilities",
  "CBRT Reserves and Liabilities",
  "CBRT Reserves and Liabilities",
  "Balance Sheet Liabilities",
  "Balance Sheet Liabilities",
  "Balance Sheet Liabilities",
  "Balance Sheet Liabilities",
  "Balance Sheet Liabilities",
  "Banking Sector Deposits",
  "Banking Sector Deposits",
  "Banking Sector Deposits",
  "Domestic Banks",
  "Domestic Banks",
  "Domestic Banks",
  "Required Reserves",
  "Required Reserves",
  "Gross Reserves",
  "Gross Reserves",
  "Gross Reserves",
  "FX Reserves",
  "FX Reserves",
  "Off-Balance Sheet Liabilities",
  "Off-Balance Sheet Liabilities",
  "Foreign Central Banks",
  "Domestic Banks Swap",
  "Domestic Banks Swap"
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

nets <- data.frame(code = c("FX","Gold","Net Reserves","Gold","FX","Net International Reserve"),
                   value = c(tdt[time == zaman]$ndr, tdt[time == zaman]$nar, tdt[time == zaman]$netr, 
                             tdt[time == zaman]$shnar, tdt[time == zaman]$shndr, tdt[time == zaman]$shnetr))

nets <- as.data.table(nets)


snr <- plot_ly(nets) %>%
  add_trace(x = nets[4:5,]$code, y = round(nets[4:5,]$value/1000, 2), type = 'bar', width = .3,
            marker = list(color = c("#4682b4", "#b22222")),
            hovertemplate = "%{value} Billion Dollar <br> %{label}<extra></extra>")%>% 
  layout(title = "Net Reserves Position Excluding Swap (Billion Dollar)", 
         shapes=list(type = 'line', x0 = -1, x1 = 2.5,
                     y0 = round(sum(nets[4,]$value, nets[5,]$value)/1000,2), 
                     y1 = round(sum(nets[4,]$value, nets[5,]$value)/1000,2), 
                     line = list(dash = 'dot', width = 3))) %>% 
  layout(annotations= list(yref = 'paper', xref = 0, y = 0.28, x = 1, showarrow = FALSE,
                           text = paste0("Net Reserves (excluding Swap): ",round(nets[6,2]/1000,2)), size = 10),
         xaxis=list(fixedrange=T),yaxis=list(fixedrange=T))
snr

nr <- plot_ly(nets) %>%
  add_trace(x = nets[1:2,]$code, y = round(nets[1:2,]$value/1000, 2), type = 'bar', width = .3,
            marker = list(color = c("#4682b4", "#b22222")),
            hovertemplate = "%{value} Billion Dollar <br> %{label}<extra></extra>")%>% 
  layout(title = "Net Reserves Position (Billion Dollar)", 
         shapes=list(type = 'line', x0 = -1, x1 = 2.5,
                     y0 = round(sum(nets[1,]$value, nets[2,]$value)/1000,2), 
                     y1 = round(sum(nets[1,]$value, nets[2,]$value)/1000,2), 
                     line = list(dash = 'dot', width = 3))) %>% 
  layout(annotations= list(yref = 'paper', xref = 0, y = 0.64, x = 0, showarrow = FALSE,
                           text = paste0("Net Reserves: ",round(nets[3,2]/1000,2)), size = 10),
         xaxis=list(fixedrange=T),yaxis=list(fixedrange=T))
nr




#################### swap bolu brutu hesapladiktan sonra "zama"daki data yerine koyucaz, grafik cikacak

tdt <- tdt[time < as.Date(zaman+1)]


zama <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = tdt[2:nrow(tdt)]$swbro, 
                type = 'scatter', mode = 'lines',
                hovertemplate = "%{x} <br> %{y} <extra></extra>") %>%
  layout(title = "SWAP - Gross Reserves Ratio",
         yaxis = list(tickformat = "%",fixedrange=T),
         xaxis = list(type = 'date',tickformat = "%d %B <br>%Y",fixedrange=T))

zama 

viog <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = tdt[2:nrow(tdt)]$viop/1000, 
                type = 'scatter', mode = 'lines',
                hovertemplate = "%{x} <br> %{y} Billion Dollar <extra></extra>") %>%
  layout(title = "FX Futures and Options",
         xaxis = list(type = 'date',tickformat = "%d %B <br>%Y",fixedrange=T),yaxis=list(fixedrange=T))

viog

ndpg <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = tdt[2:nrow(tdt)]$ndp/1000, 
                type = 'scatter', mode = 'lines',
                hovertemplate = "%{x} <br> %{y} Billion Dolar <extra></extra>") %>%
  layout(title = "Net FX Position",
         xaxis = list(type = 'date',tickformat = "%d %B <br>%Y",fixedrange=T),yaxis=list(fixedrange=T))

ndpg

ypme <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = tdt[2:nrow(tdt)]$swbmevd, 
                type = 'scatter', mode = 'lines',
                hovertemplate = "%{x} <br> %{y} <extra></extra>") %>%
  layout(title = "Ratio of Domestic Banks - Swap to Domestic Banks FX Deposits",
         yaxis = list(tickformat = "%",fixedrange=T),
         xaxis = list(type = 'date',tickformat = "%d %B <br>%Y",fixedrange=T))

ypme

tgr <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$brut/1000, 2), type = 'scatter', mode = 'lines', 
               name = 'Gross Reserves', hovertemplate = "Gross Reserves <br> %{y} Billion Dollar <br> %{x}<extra></extra>") %>%
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$biy/1000, 2), mode = 'lines', name = 'Balance Sheet Liabilities',
            hovertemplate = "Balance Sheet Liabilities <br> %{y} Billion Dollar <br> %{x}<extra></extra>")%>% 
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$bdy/1000, 2), mode = 'lines', name = 'Off-Balance Sheet Liabilities',
            hovertemplate = "Off-Balance Sheet Liabilities <br> %{y} Billion Dollar <br> %{x}<extra></extra>")%>%
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$netr/1000, 2), mode = 'lines', name = 'Net Reserves',
            hovertemplate = "Net Reserves <br> %{y} Billion Dollar <br> %{x}<extra></extra>")%>% 
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$shnetr/1000, 2), mode = 'lines', name = 'Net Reserves (excluding Swap)',
            hovertemplate = "Net Reserves (excluding Swap) <br> %{y} Billion Dollar <br> %{x}<extra></extra>")%>% 
  layout(title = " Net Reserves Position (Billion Dollar)") %>% 
  layout(yaxis = list(tickformat = "000",fixedrange=T),xaxis=list(fixedrange=T))
tgr

a <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$brut/1000, 2), type = 'scatter', mode = 'lines', 
             name = 'Gross Reserves', hovertemplate = "Gross Reserves <br> %{y} Billion Dollar <br> %{x}<extra></extra>") %>%
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$biy/1000, 2), mode = 'lines', name = 'Balance Sheet Liabilities',
            hovertemplate = "Balance Sheet Liabilities <br> %{y} Billion Dollar <br> %{x}<extra></extra>")%>% 
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$bdy/1000, 2), mode = 'lines', name = 'Off-Balance Sheet Liabilities',
            hovertemplate = "Off-Balance Sheet Liabilities <br> %{y} Billion Dollar <br> %{x}<extra></extra>")%>% 
  layout(title = " Net Reserves Position (Billion Dollar)") %>% 
  layout(yaxis = list(tickformat = "000",fixedrange=T),xaxis=list(fixedrange=T))
a

b <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$netr/1000, 2), type = 'scatter', mode = 'lines', 
             name = 'Net Reserves', hovertemplate = "Net Reserves <br> %{y} Billion Dollar <br> %{x}<extra></extra>") %>%
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$shnetr/1000, 2), mode = 'lines', name = 'Net Reserves (excluding Swap)',
            hovertemplate = "Net Reserves (excluding Swap) <br> %{y} Billion Dollar <br> %{x}<extra></extra>")%>% 
  layout(title = " Net Reserves Position (Billion Dollar)") %>% 
  layout(yaxis = list(tickformat = "000",fixedrange=T),xaxis=list(fixedrange=T))
b

c <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$brut/1000, 2), type = 'scatter', mode = 'lines', 
             name = 'Gross Reserves', hovertemplate = "Gross Reserves <br> %{y} Billion Dollar <br> %{x}<extra></extra>") %>%
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$biy/1000, 2), mode = 'lines', name = 'Balance Sheet Liabilities',
            hovertemplate = "Balance Sheet Liabilities <br> %{y} Billion Dollar <br> %{x}<extra></extra>")%>% 
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$bdy/1000, 2), mode = 'lines', name = 'Off-Balance Sheet Liabilities',
            hovertemplate = "Off-Balance Sheet Liabilities <br> %{y} Billion Dollar <br> %{x}<extra></extra>")%>%
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$netr/1000, 2), mode = 'lines', name = 'Net Reserves',
            hovertemplate = "Net Reserves <br> %{y} Billion Dollar <br> %{x}<extra></extra>")%>% 
  layout(title = " Net Reserves Position (Billion Dollar)") %>% 
  layout(yaxis = list(tickformat = "000",fixedrange=T),xaxis=list(fixedrange=T))
c

d <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$netr/1000, 2), type = 'scatter', mode = 'marker + lines', 
             name = 'Net Reserves', hovertemplate = "Net Reserves <br> %{y} Billion Dollar <br> %{x}<extra></extra>") %>%
  add_trace(x = tdt[2:nrow(tdt)]$time, y = -1*round(tdt[2:nrow(tdt)]$biy/1000, 2), type = 'bar', name = 'Balance Sheet Liabilities',
            hovertemplate = "Balance Sheet Liabilities <br> %{y} Billion Dollar <br> %{x}<extra></extra>")%>% 
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$brut/1000, 2), type = 'bar', name = 'Gross Reserves',
            hovertemplate = "Gross Reserves <br> %{y} Billion Dollar <br> %{x}<extra></extra>")%>% 
  layout(title = " Net Reserves Position (Billion Dollar)") %>% 
  layout(yaxis = list(tickformat = "000",fixedrange=T),barmode = 'stack-relative',xaxis=list(fixedrange=T))
d

e <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$shnetr/1000, 2), type = 'scatter', mode = 'marker + lines', 
             name = 'Net Reserves (excluding Swap)', hovertemplate = "Net Reserves (excluding Swap) <br> %{y} Billion Dollar <br> %{x}<extra></extra>") %>%
  add_trace(x = tdt[2:nrow(tdt)]$time, y = -1*round(tdt[2:nrow(tdt)]$biy/1000, 2), type = 'bar', name = 'Balance Sheet Liabilities',
            hovertemplate = "Balance Sheet Liabilities <br> %{y} Billion Dollar <br> %{x}<extra></extra>")%>% 
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$brut/1000, 2), type = 'bar', name = 'Gross Reserves',
            hovertemplate = "Gross Reserves <br> %{y} Billion Dollar <br> %{x}<extra></extra>")%>% 
  add_trace(x = tdt[2:nrow(tdt)]$time, y = -1*round(tdt[2:nrow(tdt)]$bdy/1000, 2), type = 'bar', name = 'Off-Balance Sheet Liabilities',
            hovertemplate = "Off-Balance Sheet Liabilities <br> %{y} Billion Dollar <br> %{x}<extra></extra>")%>% 
  layout(title = " Net Reserves Position (Billion Dollar)") %>% 
  layout(yaxis = list(tickformat = "000",fixedrange=T),barmode = 'stack-relative',xaxis=list(fixedrange=T))
e

f <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$shnetr/1000, 2), type = 'scatter', mode = 'marker + lines', 
             name = 'Net Reserves (excluding Swap)', hovertemplate = "Net Reserves (excluding Swap) <br> %{y} Billion Dollar <br> %{x}<extra></extra>") %>%
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$shnar/1000, 2), type = 'bar', name = 'Gold',
            hovertemplate = "Gold <br> %{y} Billion Dollar <br> %{x}<extra></extra>")%>% 
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$shndr/1000, 2), type = 'bar', name = 'FX',
            hovertemplate = "FX <br> %{y} Billion Dollar <br> %{x}<extra></extra>")%>% 
  layout(title = " Net Reserves Position (Billion Dollar)") %>% 
  layout(yaxis = list(tickformat = "000", fixedrange=T),barmode = 'stack-relative',xaxis=list(fixedrange=T))
f

g <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$netr/1000, 2), type = 'scatter', mode = 'marker + lines', 
             name = 'Net Reserves', hovertemplate = "Net Reserves <br> %{y} Billion Dollar <br> %{x}<extra></extra>") %>%
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$nar/1000, 2), type = 'bar', name = 'Gold',
            hovertemplate = "Gold <br> %{y} Billion Dollar <br> %{x}<extra></extra>")%>% 
  add_trace(x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$ndr/1000, 2), type = 'bar', name = 'FX',
            hovertemplate = "FX <br> %{y} Billion Dollar <br> %{x}<extra></extra>")%>% 
  layout(title = " Net Reserves Position (Billion Dollar)") %>% 
  layout(yaxis = list(tickformat = "000",fixedrange=T),barmode = 'stack-relative',yaxis=list(fixedrange=T))
g

shnetr_graph <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$shnetr/1000, 2), 
                        type = 'scatter', mode = 'marker + lines', name = 'Net Reserves (excluding Swap)', 
                        hovertemplate = "Net Reserves (excluding Swap) <br> %{y} Billion Dollar <br> %{x}<extra></extra>") %>%
  layout(title = list(text = paste0('Net Reserves (excluding Swap)',
                                    '<br>',
                                    '<sup>',
                                    '(Billion Dollar)',
                                    '</sup>')),yaxis=list(fixedrange=T),xaxis=list(fixedrange=T))

shnetr_graph

netr_graph <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$netr/1000, 2), 
                      type = 'scatter', mode = 'marker + lines', name = 'Net Reserves', 
                      hovertemplate = "Net Reserves <br> %{y} Billion Dollar <br> %{x}<extra></extra>") %>%
  layout(title = list(text = paste0('Net Reserves',
                                    '<br>',
                                    '<sup>',
                                    '(Billion Dollar)',
                                    '</sup>')),yaxis=list(fixedrange=T),xaxis=list(fixedrange=T))

netr_graph

brut_graph <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$brut/1000, 2), 
                      type = 'scatter', mode = 'marker + lines', name = 'Gross Reserves', 
                      hovertemplate = "Gross Reserves <br> %{y} Billion Dollar <br> %{x}<extra></extra>") %>%
  layout(title = list(text = paste0('Gross Reserves',
                                    '<br>',
                                    '<sup>',
                                    '(Billion Dollar)',
                                    '</sup>')),yaxis=list(fixedrange=T),xaxis=list(fixedrange=T))

brut_graph

biy_graph <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$biy/1000, 2), 
                     type = 'scatter', mode = 'marker + lines', name = 'Balance Sheet Liabilities', 
                     hovertemplate = "Balance Sheet Liabilities <br> %{y} Billion Dollar <br> %{x}<extra></extra>") %>%
  layout(title = list(text = paste0('Balance Sheet Liabilities',
                                    '<br>',
                                    '<sup>',
                                    '(Billion Dollar)',
                                    '</sup>')),yaxis=list(fixedrange=T),xaxis=list(fixedrange=T))

biy_graph

bdy_graph <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$bdy/1000, 2), 
                     type = 'scatter', mode = 'marker + lines', name = 'Off-Balance Sheet Liabilities', 
                     hovertemplate = "Off-Balance Sheet Liabilities <br> %{y} Billion Dollar <br> %{x}<extra></extra>") %>%
  layout(title = list(text = paste0('Off-Balance Sheet Liabilities',
                                    '<br>',
                                    '<sup>',
                                    '(Billion Dollar)',
                                    '</sup>')),yaxis=list(fixedrange=T),xaxis=list(fixedrange=T))

bdy_graph

nar_graph <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$nar/1000, 2), 
                     type = 'scatter', mode = 'marker + lines', name = 'Net Gold Reserves', 
                     hovertemplate = "Net Gold Reserves <br> %{y} Billion Dollar <br> %{x}<extra></extra>") %>%
  layout(title = list(text = paste0('Net Gold Reserves',
                                    '<br>',
                                    '<sup>',
                                    '(Billion Dollar)',
                                    '</sup>')),yaxis=list(fixedrange=T),xaxis=list(fixedrange=T))

nar_graph

ndr_graph <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$ndr/1000, 2), 
                     type = 'scatter', mode = 'marker + lines', name = 'Net FX Reserves', 
                     hovertemplate = "Net FX Reserves <br> %{y} Billion Dollar <br> %{x}<extra></extra>") %>%
  layout(title = list(text = paste0('Net FX Reserves',
                                    '<br>',
                                    '<sup>',
                                    '(Billion Dollar)',
                                    '</sup>')),yaxis=list(fixedrange=T),xaxis=list(fixedrange=T))

ndr_graph

shndr_graph <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$shndr/1000, 2), 
                       type = 'scatter', mode = 'marker + lines', name = 'Net FX Reserves (excluding Swap)', 
                       hovertemplate = "Net FX Reserves (excluding Swap) <br> %{y} Billion Dollar <br> %{x}<extra></extra>") %>%
  layout(title = list(text = paste0('Net FX Reserves (excluding Swap)',
                                    '<br>',
                                    '<sup>',
                                    '(Billion Dollar)',
                                    '</sup>')),yaxis=list(fixedrange=T),xaxis=list(fixedrange=T))

shndr_graph

shnar_graph <- plot_ly(tdt, x = tdt[2:nrow(tdt)]$time, y = round(tdt[2:nrow(tdt)]$shnar/1000, 2), 
                       type = 'scatter', mode = 'marker + lines', name = 'Net Gold Reserves (excluding Swap)', 
                       hovertemplate = "Net Gold Reserves (excluding Swap) <br> %{y} Billion Dollar <br> %{x}<extra></extra>") %>%
  layout(title = list(text = paste0('Net Gold Reserves (excluding Swap)',
                                    '<br>',
                                    '<sup>',
                                    '(Billion Dollar)',
                                    '</sup>')),yaxis=list(fixedrange=T),xaxis=list(fixedrange=T))

shnar_graph

sub1 <- subplot(netr_graph, shnetr_graph, nrows = 2, shareX = T, titleY  = T)%>%
  layout(hovermode = "x unified") %>%
  layout(title = list(text = paste0('Net Reserves and Net Reserves (excluding Swap)',
                                    '<br>',
                                    '<sup>',
                                    '(Billion Dollar)',
                                    '</sup>')),legend = list(orientation = 'h'),yaxis=list(fixedrange=T),xaxis=list(fixedrange=T)) 

sub1

sub2 <- subplot(brut_graph, biy_graph, bdy_graph, nrows = 3, shareX = T, titleY  = T)%>%
  layout(hovermode = "x unified") %>%
  layout(title = list(text = paste0('Gross Reserves and Liabilities',
                                    '<br>',
                                    '<sup>',
                                    '(Billion Dollar)',
                                    '</sup>')),legend = list(orientation = 'h'),yaxis=list(fixedrange=T),xaxis=list(fixedrange=T)) 

sub2

sub3 <- subplot(nar_graph, netr_graph, ndr_graph, nrows = 3, shareX = T, titleY  = T)%>%
  layout(hovermode = "x unified") %>%
  layout(title = list(text = paste0('Net Reserves',
                                    '<br>',
                                    '<sup>',
                                    '(Billion Dollar)',
                                    '</sup>')),legend = list(orientation = 'h'),yaxis=list(fixedrange=T),xaxis=list(fixedrange=T)) 

sub3

sub4 <- subplot(shnar_graph, shnetr_graph, shndr_graph, nrows = 3, shareX = T, titleY  = T)%>%
  layout(hovermode = "x unified") %>%
  layout(title = list(text = paste0('Net Reserves (excluding Swap)',
                                    '<br>',
                                    '<sup>',
                                    '(Billion Dollar)',
                                    '</sup>')),legend = list(orientation = 'h'),yaxis=list(fixedrange=T),xaxis=list(fixedrange=T)) 

sub4
