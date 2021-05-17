library(plotly)
library(data.table)
library(ggplot2)
library(Rmisc)
library(dplyr)

Sys.setlocale(category = "LC_ALL", locale = "Turkish")

source(paste0(getwd(),"/netrezerv.R"))








colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')


BIY_graph <- BIY %>% plot_ly(labels = c("SDR","Yabanci Bankalar","Diger Mevduatlar","Bankacilik Sektoru Mevduatlari"), 
                             values = c(BIY[time == zaman]$sdr,
                                        BIY[time == zaman]$fb, 
                                        BIY[time == zaman]$dm,
                                        BIY[time == zaman]$bsm),
                             type = 'pie',
                             textinfo = 'label+percent',
                             insidetextfont = list(color = '#FFFFFF'),
                             hoverinfo = 'text',
                             text = ~paste('$', c(round(BIY[time == zaman]$sdr/1000,2),
                                                  round(BIY[time == zaman]$fb/1000,2),
                                                  round(BIY[time == zaman]$dm/1000,2),
                                                  round(BIY[time == zaman]$bsm/1000,2)), ' billions'),
                             marker = list(colors = colors,
                                           line = list(color = '#FFFFFF', width = 1)))%>% 
  layout(title =  paste0('Bilanco İci Yukumlulukler',
                         '<br>',
                         '<sup>',
                         paste0(zaman, " Tarihi Milyar $ cinsinden"),
                         '</sup>',
                         '<br>'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         legend = list(orientation = 'h'))
BIY_graph


BSM_graph <- BIY %>% plot_ly(labels = c("Zorunlu Karsiliklar","Yurtici Bankalar", "Yurtdisi Bankalar"), 
                             values = c(BIY[time == zaman]$zk,
                                        BIY[time == zaman]$yib,
                                        BIY[time == zaman]$ydb),
                             type = 'pie',
                             textinfo = 'label+percent',
                             insidetextfont = list(color = '#FFFFFF'),
                             hoverinfo = 'text',
                             text = ~paste('$', c(round(BIY[time == zaman]$zk/1000,2),
                                                  round(BIY[time == zaman]$yib/1000,2),
                                                  round(BIY[time == zaman]$ydb/1000,2)),' billions'),
                             marker = list(colors = colors,
                                           line = list(color = '#FFFFFF', width = 1)))%>% 
  layout(title =  paste0('Bankacilik Sektoru Mevduatlari',
                         '<br>',
                         '<sup>',
                         paste0(zaman, " Tarihi Milyar $ cinsinden"),
                         '</sup>',
                         '<br>'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         legend = list(orientation = 'h'))
BSM_graph

YIB_graph <- BIY %>% plot_ly(labels = c("Nakit","Teminat", "Altin"), 
                             values = c(BIY[time == zaman]$nakit,
                                        BIY[time == zaman]$teminat,
                                        BIY[time == zaman]$altin),
                             type = 'pie',
                             textinfo = 'label+percent',
                             insidetextfont = list(color = '#FFFFFF'),
                             hoverinfo = 'text',
                             text = ~paste('$', c(round(BIY[time == zaman]$nakit/1000,2),
                                                  round(BIY[time == zaman]$teminat/1000,2),
                                                  round(BIY[time == zaman]$altin/1000,3)),' billions'),
                             marker = list(colors = colors,
                                           line = list(color = '#FFFFFF', width = 1)))%>% 
  layout(title =  paste0('Yurtici Bankalar',
                         '<br>',
                         '<sup>',
                         paste0(zaman, " Tarihi Milyar $ cinsinden"),
                         '</sup>',
                         '<br>'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         legend = list(orientation = 'h'))
YIB_graph

ZK_graph <- BIY %>% plot_ly(labels = c("Doviz", "Altin"), 
                            values = c(BIY[time == zaman]$zkd,
                                       BIY[time == zaman]$zka),
                            type = 'pie',
                            textinfo = 'label+percent',
                            insidetextfont = list(color = '#FFFFFF'),
                            hoverinfo = 'text',
                            text = ~paste('$', c(round(BIY[time == zaman]$zkd/1000,2),
                                                 round(BIY[time == zaman]$zka/1000,2)),' billions'),
                            marker = list(colors = colors,
                                          line = list(color = '#FFFFFF', width = 1)))%>% 
  layout(title =  paste0('Zorunlu Karsiliklar',
                         '<br>',
                         '<sup>',
                         paste0(zaman, " Tarihi Milyar $ cinsinden"),
                         '</sup>',
                         '<br>'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         legend = list(orientation = 'h'))
ZK_graph

BRUTR <- as.data.table(BRUTR)

BR_graph <- BRUTR %>% plot_ly(labels = c("Altin", "Doviz", "SDR"), 
                              values = c(BRUTR[time == zaman]$altin,
                                         BRUTR[time == zaman]$doviz,
                                         BRUTR[time == zaman]$SDR),
                              type = 'pie',
                              textinfo = 'label+percent',
                              insidetextfont = list(color = '#FFFFFF'),
                              hoverinfo = 'text',
                              text = ~paste('$', c(round(BRUTR[time == zaman]$altin/1000,2),
                                                   round(BRUTR[time == zaman]$doviz/1000,2),
                                                   round(BRUTR[time == zaman]$SDR/1000,2)),' billions'),
                              marker = list(colors = colors,
                                            line = list(color = '#FFFFFF', width = 1)))%>% 
  layout(title =  paste0('Brut Rezervler',
                         '<br>',
                         '<sup>',
                         paste0(zaman, " Tarihi Milyar $ cinsinden"),
                         '</sup>',
                         '<br>'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         legend = list(orientation = 'h'))
BR_graph

BR_doviz_graph <- BRUTR %>% plot_ly(labels = c("Menkul Kiymetler", "Toplam Nakit Mevduatlar"), 
                                    values = c(BRUTR[time == zaman]$mk,
                                               BRUTR[time == zaman]$tot),
                                    type = 'pie',
                                    textinfo = 'label+percent',
                                    insidetextfont = list(color = '#FFFFFF'),
                                    hoverinfo = 'text',
                                    text = ~paste('$', c(round(BRUTR[time == zaman]$mk/1000,2),
                                                         round(BRUTR[time == zaman]$tot/1000,2)),' billions'),
                                    marker = list(colors = colors,
                                                  line = list(color = '#FFFFFF', width = 1)))%>% 
  layout(title =  paste0('Doviz',
                         '<br>',
                         '<sup>',
                         paste0(zaman, " Tarihi Milyar $ cinsinden"),
                         '</sup>',
                         '<br>'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         legend = list(orientation = 'h'))
BR_doviz_graph

BDY_graph <- mat %>% plot_ly(labels = c("Yabanci Merkez Bankalari", "Yurtici Bankalar"), 
                             values = c(y_mb[time == zaman]$mb,
                                        mat[time == zaman]$toplam),
                             type = 'pie',
                             textinfo = 'label+percent',
                             insidetextfont = list(color = '#FFFFFF'),
                             hoverinfo = 'text',
                             text = ~paste('$', c(round(y_mb[time == zaman]$mb/1000,2),
                                                  round(mat[time == zaman]$toplam/1000,2)),' billions'),
                             marker = list(colors = colors,
                                           line = list(color = '#FFFFFF', width = 1)))%>% 
  layout(title =  paste0('Bilanco Disi Yukumlulukler',
                         '<br>',
                         '<sup>',
                         paste0(zaman, " Tarihi Milyar $ cinsinden"),
                         '</sup>',
                         '<br>'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         legend = list(orientation = 'h'))

BDY_graph

BDY_YIB_graph <- mat %>% plot_ly(labels = c("Altin", "Doviz"), 
                                 values = c(mat[time == zaman]$altin,
                                            mat[time == zaman]$doviz),
                                 type = 'pie',
                                 textinfo = 'label+percent',
                                 insidetextfont = list(color = '#FFFFFF'),
                                 hoverinfo = 'text',
                                 text = ~paste('$', c(round(mat[time == zaman]$altin/1000,2),
                                                      round(mat[time == zaman]$doviz/1000,2)),' billions'),
                                 marker = list(colors = colors,
                                               line = list(color = '#FFFFFF', width = 1)))%>% 
  layout(title =  paste0('Yurtici Bankalar',
                         '<br>',
                         '<sup>',
                         paste0(zaman, " Tarihi Milyar $ cinsinden"),
                         '</sup>',
                         '<br>'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         legend = list(orientation = 'h'))

BDY_YIB_graph

fig_3 <- plot_ly()

fig_3 <- fig_3 %>% add_pie(data = BIY, labels = c("SDR","Yabanci Bankalar","Diger Mevduatlar","Bankacilik Sektoru Mevduatlari"),
                           marker = list(colors = c("#d95f0e","#fec44f", "#fff7bc", "#969696")),
                           values = c(round(BIY[time == zaman]$sdr/1000,2),
                                      round(BIY[time == zaman]$fb/1000,2), 
                                      round(BIY[time == zaman]$dm/1000,2),
                                      round(BIY[time == zaman]$bsm/1000,2)), 
                           name = "Bilanco İci Yukumlulukler", domain = list(row = 0, column = 0),
                           hovertemplate = "%{value} Milyon Dolar <br> %{label}")
fig_3 <- fig_3 %>% add_pie(data = BIY, labels = c("Zorunlu Karsiliklar","Yurtici Bankalar", "Yurtdisi Bankalar"),
                           marker = list(colors = c("#225ea8","#238443", "#525252")),
                           values = c(round(BIY[time == zaman]$zk/1000,2),
                                      round(BIY[time == zaman]$yib/1000,2),
                                      round(BIY[time == zaman]$ydb/1000,2)),
                           name = "Bankacilik Sektoru Mevduatlari", domain = list(row = 0, column = 1),
                           hovertemplate = "%{value} Milyon Dolar <br> %{label}")
fig_3 <- fig_3 %>% add_pie(data = BIY, labels = c("Nakit","Teminat", "Altin"),
                           marker = list(colors = c("#41ab5d","#78c679", "#addd8e")),
                           values = c(round(BIY[time == zaman]$nakit/1000,2),
                                      round(BIY[time == zaman]$teminat/1000,2),
                                      round(BIY[time == zaman]$altin/1000,2)),
                           name = "Yurtici Bankalar", domain = list(row = 1, column = 0),
                           hovertemplate = "%{value} Milyon Dolar <br> %{label}")
fig_3 <- fig_3 %>% add_pie(data = BIY, labels = c("Doviz", "Altin"),
                           marker = list(colors = c("#3690c0", "#41b6c4")),
                           values = c(round(BIY[time == zaman]$zkd/1000,2),
                                      round(BIY[time == zaman]$zka/1000,2)),
                           name = "Zorunlu Karsiliklar", domain = list(row = 1, column = 1),
                           hovertemplate = "%{value} Milyon Dolar <br> %{label}")
fig_3 <- fig_3 %>% layout(title = "Bilanco İci Yukumlulukler", showlegend = T,
                          grid=list(rows=2, columns=2),
                          xaxis = list(showgrid = T, zeroline = T, showticklabels = T),
                          yaxis = list(showgrid = T, zeroline = T, showticklabels = T))

fig_3


fig_1 <- plot_ly()

fig_1 <- fig_1 %>% add_pie(data = BIY, labels = c("Altin", "Doviz", "SDR"),
                           marker = list(colors = c("#762a83", "#bd0026","#41ab5d")),
                           values = c(round(BRUTR[time == zaman]$altin/1000,2),
                                      round(BRUTR[time == zaman]$doviz/1000,2), 
                                      round(BRUTR[time == zaman]$SDR/1000,2)), 
                           name = "Brut Rezervler", domain = list(row = 0, column = 0),
                           hovertemplate = "%{value} Milyon Dolar <br> %{label}")
fig_1 <- fig_1 %>% add_pie(data = BIY, labels = c("Menkul Kiymetler", "Toplam Nakit Mevduatlar"),
                           marker = list(colors = c("#fd8d3c", "#fc4e2a")),
                           values = c(round(BRUTR[time == zaman]$mk/1000,2),
                                      round(BRUTR[time == zaman]$tot/1000,2)),
                           name = "Doviz", domain = list(row = 0, column = 1),
                           hovertemplate = "%{value} Milyon Dolar <br> %{label}")
fig_1 <- fig_1 %>% layout(title = "Brut Rezervler", showlegend = T,
                          grid=list(rows=1, columns=2),
                          xaxis = list(showgrid = T, zeroline = T, showticklabels = T),
                          yaxis = list(showgrid = T, zeroline = T, showticklabels = T))

fig_1

fig_2 <- plot_ly()

fig_2 <- fig_2 %>% add_pie(data = BIY, labels = c("Yabanci Merkez Bankalari", "Yurtici Bankalar"),
                           marker = list(colors = c("#6a51a3","#ef3b2c")),
                           values = c(round(y_mb[time == zaman]$mb/1000,2),
                                      round(mat[time == zaman]$toplam/1000,2)), 
                           name = "Bilanco Disi Yukumlulukler (Swap Yukumlulugu)", domain = list(row = 0, column = 0),
                           hovertemplate = "%{value} Milyon Dolar <br> %{label}")
fig_2 <- fig_2 %>% add_pie(data = BIY, labels = c("Altin", "Doviz"),
                           marker = list(colors = c("#fb6a4a","#fc9272")),
                           values = c(round(mat[time == zaman]$altin/1000,2),
                                      round(mat[time == zaman]$doviz/1000,2)),
                           name = "Yurtici Bankalar", domain = list(row = 0, column = 1),
                           hovertemplate = "%{value} Milyon Dolar <br> %{label}")
fig_2 <- fig_2 %>% layout(title = "Bilanco Disi Yukumlulukler (Swap Yukumlulugu)", showlegend = T,
                          grid=list(rows=1, columns=2),
                          xaxis = list(showgrid = T, zeroline = T, showticklabels = T),
                          yaxis = list(showgrid = T, zeroline = T, showticklabels = T))

fig_2