format_region <- function (data) {
  
  data[data == 0] <- NA #1,569,510 obs

  if (!is.null(data$TA_Name)) {
    
  data$region <- recode(data$TA_Name, 
                           "Auckland - City" = "Auckland",
                           "Auckland - Franklin" = "Auckland",       
                           "Auckland - Manukau" = "Auckland", 
                           "Auckland - North Shore" = "Auckland", 
                           "Auckland - Papakura" = "Auckland",           
                           "Auckland - Rodney" = "Auckland",
                           "Auckland - Waitakere" = "Auckland",
                           "Central Hawkes Bay District" = "Central Hawke's Bay District",
                           "Hutt City" = "Lower Hutt City",
                           "MacKenzie District" = "Mackenzie District",
                           "Matamata Piako District" = "Matamata-Piako District",
                           "Queenstown Lakes District" = "Queenstown-Lakes District",
                           "Thames Coromandel District" = "Thames-Coromandel District",
                           "Whanganui District" = "Wanganui District"
                        )
  }
  
  if (!is.null(data$region)) {
    
  data$br_region <- recode(data$region, 
                              "Wellington City" = "Wellington",
                              "Masterton District" = "Wellington",
                              "Carterton District" = "Wellington",
                              "South Wairarapa District" = "Wellington",
                              "Upper Hutt City" = "Wellington",
                              "Lower Hutt City" = "Wellington",
                              "Porirua City" = "Wellington",  
                              "Kapiti Coast District" = "Wellington",
                              
                              "Kaikoura District" = "Canterbury",
                              "Hurunui District" = "Canterbury",
                              "Waimakariri District" = "Canterbury",
                              "Christchurch City" = "Canterbury",    
                              "Selwyn District" = "Canterbury",
                              "Ashburton District" = "Canterbury",
                              "Mackenzie District" = "Canterbury",
                              "Timaru District" = "Canterbury",
                              "Waimate District" = "Canterbury",
                              
                              "Buller District" = "Rest of South Island",
                              "Central Otago District" = "Rest of South Island",
                              "Clutha District" = "Rest of South Island",  
                              "Dunedin City" = "Rest of South Island",  
                              "Gore District" = "Rest of South Island",
                              "Grey District" = "Rest of South Island", 
                              "Invercargill City" = "Rest of South Island",
                              "Nelson City" = "Rest of South Island", 
                              "Queenstown-Lakes District" = "Rest of South Island",
                              "Southland District" = "Rest of South Island",
                              "Tasman District" = "Rest of South Island", 
                              "Waitaki District" = "Rest of South Island",
                              "Marlborough District" = "Rest of South Island",             
                           
                              
                              "Central Hawke's Bay District" = "Rest of North Island",              
                              "Far North District" = "Rest of North Island",               
                              "Gisborne District" = "Rest of North Island",                
                              "Hamilton City" = "Rest of North Island",                   
                              "Hastings District" = "Rest of North Island",                
                              "Hauraki District" = "Rest of North Island",                 
                              "Horowhenua District" = "Rest of North Island",             
                              "Kaipara District" = "Rest of North Island",                 
                              "Kawerau District" = "Rest of North Island",                 
                              "Manawatu District" = "Rest of North Island",               
                              "Matamata-Piako District" = "Rest of North Island",          
                              "Napier City" = "Rest of North Island",                     
                              "New Plymouth District" = "Rest of North Island",            
                              "Opotiki District" = "Rest of North Island",                 
                              "Other NI" = "Rest of North Island",                       
                              "Otorohanga District" = "Rest of North Island",              
                              "Palmerston North City" = "Rest of North Island",            
                              "Rangitikei District" = "Rest of North Island",             
                              "Rotorua District" = "Rest of North Island",                 
                              "Ruapehu District" = "Rest of North Island",                 
                              "South Taranaki District" = "Rest of North Island",         
                              "South Waikato District" = "Rest of North Island",           
                              "Stratford District" = "Rest of North Island",               
                              "Tararua District" = "Rest of North Island",                
                              "Taupo District" = "Rest of North Island",                   
                              "Tauranga City" = "Rest of North Island",                    
                              "Thames-Coromandel District" = "Rest of North Island",      
                              "Waikato District" = "Rest of North Island",                 
                              "Waipa District" = "Rest of North Island",                   
                              "Wairoa District" = "Rest of North Island",                 
                              "Waitomo District" = "Rest of North Island",                 
                              "Wanganui District" = "Rest of North Island",                   
                              "Western Bay of Plenty District" = "Rest of North Island",   
                              "Westland District" = "Rest of North Island",                
                              "Whakatane District" = "Rest of North Island",              
                              "Whangarei District" = "Rest of North Island"        
  )
  }
  
  if (!is.null(data$Region_Advert)) {
    
  data$br_region <- recode(data$Region_Advert, 
                           "Wellington" = "Wellington",
                           "Wairarapa" = "Wellington",
                           
                           "Canterbury" = "Canterbury",
                           
                           "Central Otago / Lakes District" = "Rest of South Island",
                           "Nelson & Bays" = "Rest of South Island",
                           "Nelson / Tasman" = "Rest of South Island",  
                           "Nelson" = "Rest of South Island",  
                           "Otago" = "Rest of South Island",  
                           "Southland" = "Rest of South Island",
                           "West Coast" = "Rest of South Island", 
                           "Marlborough" = "Rest of South Island",
                           "Tasman" = "Rest of South Island",
                           
                           "Bay Of Plenty" = "Rest of North Island",      
                           "Bay of Plenty" = "Rest of North Island",              
                           "Central North Island" = "Rest of North Island",               
                           "Gisborne" = "Rest of North Island",                
                           "Coromandel" = "Rest of North Island",                   
                           "Hawke's Bay" = "Rest of North Island",                
                           "Hawkes Bay" = "Rest of North Island",                 
                           "Manawatu / Wanganui" = "Rest of North Island",             
                           "Manawatu / Whanganui" = "Rest of North Island",
                           "Manawatu-Whanganui" = "Rest of North Island",                 
                           "Manawatu-Wanganui" = "Rest of North Island",                 
                           "Northland" = "Rest of North Island",                 
                           "Taranaki" = "Rest of North Island",               
                           "Waikato" = "Rest of North Island",             
       
                           "Auckland" = "Auckland"                        
  )
  }
  
  if (!is.null(data$Region_Advert)) {
    
    data$Region_Name <- recode(data$Region_Advert, 
                             "Wellington" = "Wellington",
                             "Wairarapa" = "Wellington",
                             
                             "Canterbury" = "Canterbury",
                             
                             "Central Otago / Lakes District" = "Rest of South Island",
                             "Nelson & Bays" = "Rest of South Island",
                             "Nelson / Tasman" = "Rest of South Island",  
                             "Nelson" = "Rest of South Island",  
                             "Otago" = "Rest of South Island",  
                             "Southland" = "Rest of South Island",
                             "West Coast" = "Rest of South Island", 
                             "Marlborough" = "Rest of South Island",
                             "Tasman" = "Rest of South Island",
                             
                             "Bay Of Plenty" = "Rest of North Island",      
                             "Bay of Plenty" = "Rest of North Island",              
                             "Central North Island" = "Rest of North Island",               
                             "Gisborne" = "Rest of North Island",                
                             "Coromandel" = "Rest of North Island",                   
                             "Hawke's Bay" = "Rest of North Island",                
                             "Hawkes Bay" = "Rest of North Island",                 
                             "Manawatu / Wanganui" = "Rest of North Island",             
                             "Manawatu / Whanganui" = "Rest of North Island",
                             "Manawatu-Whanganui" = "Rest of North Island",                 
                             "Manawatu-Wanganui" = "Rest of North Island",                 
                             "Northland" = "Rest of North Island",                 
                             "Taranaki" = "Rest of North Island",               
                             "Waikato" = "Rest of North Island",             
                             
                             "Auckland" = "Auckland"                        
    )
  }
  
  
  if (!is.null(data$Region_Name)) {
    
    data$br_region <- recode(data$Region_Name, 
                             "Wellington" = "Wellington",
                             "Wairarapa" = "Wellington",

                             "Canterbury" = "Canterbury",
                             
                             "Central Otago / Lakes District" = "Rest of South Island",
                             "Nelson & Bays" = "Rest of South Island",
                             "Nelson / Tasman" = "Rest of South Island",  
                             "Otago" = "Rest of South Island",  
                             "Southland" = "Rest of South Island",
                             "West Coast" = "Rest of South Island", 
                             "Marlborough" = "Rest of South Island",
                             "Nelson" = "Rest of South Island",  
                             "Tasman" = "Rest of South Island",
                             
                             "Bay Of Plenty" = "Rest of North Island",    
                             "Bay of Plenty" = "Rest of North Island",              
                             "Central North Island" = "Rest of North Island",               
                             "Gisborne" = "Rest of North Island",                
                             "Coromandel" = "Rest of North Island",                   
                             "Hawke's Bay" = "Rest of North Island",                
                             "Hawkes Bay" = "Rest of North Island",                 
                             "Manawatu / Wanganui" = "Rest of North Island",             
                             "Manawatu / Whanganui" = "Rest of North Island", 
                             "Manawatu-Whanganui" = "Rest of North Island",                 
                             "Manawatu-Wanganui" = "Rest of North Island",  
                             "Northland" = "Rest of North Island",                 
                             "Taranaki" = "Rest of North Island",               
                             "Waikato" = "Rest of North Island",             
                             
                             "Auckland" = "Auckland",
                             
                             "NZ" = "NZ"
    )
  }
  
  return(data)
}

date_to_month <- function(dt){
  # Take a date and return the last day of that month as a date
  ceiling_date(dt, unit = "months") - days(1)
  # Take a date and return the first day of that month 
  #floor_date(dt, unit = "months") 
  
}

date_to_quarter <- function(dt){
  # Take a date and return the last day of the quarter that the date is in
  ceiling_date(dt, unit = "quarter") - days(1)
  
}
