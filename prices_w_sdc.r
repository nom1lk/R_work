f45 <- function(consumption,year,quarter,billing,dwelling) { 
    
    ifelse(
        consumption <= (billing * 440)/1000, (consumption * prices[prices$Year == year & prices$Quarter == quarter,][,c(3)]) + 
            (ifelse(dwelling == "Detached", (0.75 * consumption  * prices[prices$Year == year & prices$Quarter == quarter,][,c(6)]), (0.85 * consumption  * prices[prices$Year == year & prices$Quarter == quarter,][,c(6)]))),
             ifelse(
                 (consumption > ((billing * 440)/1000)) & (consumption <= ((billing * 880)/1000)),
                 (((billing * 440)/1000) * prices[prices$Year == year & prices$Quarter == quarter,][,c(3)]) + 
                     (((consumption - (billing * 440)/1000)) * prices[prices$Year == year & prices$Quarter == quarter,][,c(4)]) +
                     ifelse(dwelling == "Detached", (0.75 * consumption  * prices[prices$Year == year & prices$Quarter == quarter,][,c(6)]), (0.85 * consumption  * prices[prices$Year == year & prices$Quarter == quarter,][,c(6)])) 
                 ,
                 (((billing * 440)/1000) * prices[prices$Year == year & prices$Quarter == quarter,][,c(3)]) +
                     (((billing * 440)/1000) * prices[prices$Year == year & prices$Quarter == quarter,][,c(4)]) +
                     (((consumption - (billing * 880)/1000)) * prices[prices$Year == year & prices$Quarter == quarter,][,c(5)]) +
                     ifelse(dwelling == "Detached", (0.75 * consumption  * prices[prices$Year == year & prices$Quarter == quarter,][,c(6)]), (0.85 * consumption  * prices[prices$Year == year & prices$Quarter == quarter,][,c(6)])) 
             )
            )
}

system.time(cons$realpaid_w_sdc <- mapply(f45, consumption = cons$Cons, year = cons$Year, quarter = cons$Quarter, billing = cons$Billing_period, dwelling = cons$Dwel_type))

