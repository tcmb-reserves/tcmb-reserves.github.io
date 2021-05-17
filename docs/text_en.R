x= "2021-04-30"
paste0(" CBRT's net reserves ",
       ifelse(sign(netrezerv(x=x)-netrezerv(x=x2))==1, paste0(" increased "), paste0(" decreased ")),
       " by ",
       paste0(abs(round(netrezerv(x=x)-netrezerv(x=x2),2)))," Million Dollars ",
       " compared to last week, and ",
       ifelse(sign(netrezerv(x=x)-netrezerv(x="2021-01-01"))==1, paste0(" expanded "), paste0(" declined ")),
       " by ",
       paste0(abs(round(netrezerv(x=x)-netrezerv(x="2021-01-01"),2))),
       " Million Dollars ",
       " compared to the new year. ",
       " Net reserves except swap ",
       ifelse(sign(swapharic(x=x,y=y,z=z)-swapharic(x= "2021-04-23",
                                                    y= "2021-04-22",
                                                    z= "2021-04-22"))==1, paste0(" rise "), paste0(" fell ")),
       " by ",
       paste0(abs(round(swapharic(x=x,y=y,z=z)-swapharic(x= "2021-04-23",
                                                         y= "2021-04-22",
                                                         z= "2021-04-22"),2))), 
       " Million Dollars ",
       " compared to previous week, and ",
       ifelse(sign(swapharic(x=x,y=y,z=z)-swapharic(1))==1, paste0(" raised "), paste0(" dropped ")),
       " by ",
       paste0(abs(round(swapharic(x=x,y=y,z=z)-swapharic(1),2))), 
       " Million Dollars ",
       " compared to the new year. "
)

paste0(" Net gold reserves ",
       ifelse(sign(fnra(x=x)-fnra(x=x2))==1, paste0("rise"), paste0("decline")),
       " by ",
       paste0(abs(round(fnra(x=x)-fnra(x=x2),2))), 
       " Million Dollars ",
       " since last week and ",
       ifelse(sign(fnra(x=x)-fnra(x="2021-01-01"))==1, paste0(" gain "), paste0(" loss ")),
       paste0(abs(round(fnra(x=x)-fnra(x="2021-01-01"),2))), 
       " Million Dollars ",
       " since the new year and reached to ",
       paste0(round(fnra(x=x),2)), " Million Dollars. "
       
)

paste0(" Net foreign exchange reserves ",
       ifelse(sign(fnrd(x=x)-fnrd(x=x2))==1, paste0(" increased "), paste0(" decreased ")),
       " by ",
       paste0(abs(round(fnrd(x=x)-fnrd(x=x2),2))), 
       " Million Dollars ",
       " compared to previous week and ",
       ifelse(sign(fnrd(x=x)-fnrd(x="2021-01-01"))==1, paste0(" rise "), paste0(" fell ")),
       " by ",
       paste0(abs(round(fnrd(x=x)-fnrd(x="2021-01-01"),2))), 
       " Million Dollars ",
       " and reached to ",
       paste0(round(fnrd(x=x),2)), " Million Dollars. "
       
)

paste0(" Since last week, net gold reserves except swap ",
       ifelse(sign(fsnra(x=x,z=z)-fsnra(x= "2021-04-23",
                                        z= "2021-04-22"))==1, paste0(" raised "), paste0(" dropped ")),
       " by ",
       paste0(abs(round(fsnra(x=x,z=z)-fsnra(x= "2021-04-23",
                                             z= "2021-04-22"),2))), 
       " Million Dollars ",
       " and reached to ",
       paste0(round(fsnra(x=x,z=z),2)), " Million Dollars. ",
       " In addition, net foreign exchange reserves except swap ",
       ifelse(sign(fsnrd(x=x,y=y,z=z)-fsnrd(x= "2021-04-23",
                                            y= "2021-04-22",
                                            z= "2021-04-22"))==1, paste0(" expanded "), paste0(" declined ")),
       " by ",
       paste0(abs(round(fsnrd(x=x,y=y,z=z)-fsnrd(x= "2021-04-23",
                                                 y= "2021-04-22",
                                                 z= "2021-04-22"),2))),
       " Million Dollars ",
       " and reached to ",
       paste0(round(fsnrd(x=x,y=y,z=z),2)), " Million Dollars. "
)

paste0(" Total liabilities (including balance sheet and off-balance sheet) ",
       ifelse(sign(lia[time == as.Date(x)]$lia-lia[time ==as.Date("2021-04-23")]$lia)==1, paste0(" increased "), paste0(" decreased ")),
       " by ",
       paste0(abs(round(lia[time == as.Date(x)]$lia-lia[time ==as.Date("2021-04-23")]$lia,2))), 
       " Million Dollars ",
       " since the last week. ",
       " Also, it ",
       ifelse(sign(lia[time == as.Date(x)]$lia-lia[time ==as.Date("2021-01-01")]$lia)==1, paste0(" rise "), paste0(" fell ")),
       " by ",
       paste0(abs(round(lia[time == as.Date(x)]$lia-lia[time ==as.Date("2021-01-01")]$lia,2))), 
       " Million Dollars ",
       " and reached to ",
       paste0(round(lia[time == as.Date(x)]$lia/1000,2)), " Billion Dollars, ",
       " compared to the new year. "
       
)


paste0(" The ratio of off-balance sheet liabilities to gross reserves is % ", round(100*sbr[time == zaman]$rate,2), " . ")

