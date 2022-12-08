


library(tidyverse)
library(here)
library(readr)
library(RODBC)

sbird_db_spp <- c("AMAV"                  "AMBI"                  "AMGP"                 
[13] "AMKE"                  "AWPE"                  "BAEA"                  "BASA"                  "BBPL"                  "BCNH"                 
[19] "BLOY"                  "BLTU"                  "BNST"                  "COHA"                  "COSN"                  "DOSP"                 
[25] "DUNL"                  "FEHA"                  "GBHE"                  "GOEA"                  "GREG"                  "GRHE"                 
[31] "GRYE"                  "KILL"                  "LBCU"                  "LBDO"                  "LEGP"                  "LESA"                 
[37] "LEYE"                  "LWSA"                  "MAGO"                  "MERL"                  "NOHA"                  "OSPR"                 
[43] "PAGP", "PEEP", "PEFA", "PESA", "PHAL", "PRFA", "REKN", "REPH", "RLHA", "RNPH", "RSHA", "RTHA", "RUFF", "RUTU", "SAND", "SBDO", "SEOW", "SEPL", "SESA", "SNEG", "SNPL", "SOSA", "SPSA", "SSHA", "SSCOHA", "STSA", "SURF", "WESA", "WILL", "WIPH", "WATA", "WHIM", "WTKI", "YELL")



db <- here("data_files/Shorebirds.mdb")

 con2 <- odbcConnectAccess2007(db)

sbird_table <- sqlFetch(con2, "SHOREBIRD") 
 

dat <- data.frame(x=names(sbird_table)) %>% 
  summarise(znames = paste(x,collapse=", ")) 


sbird_table %>% 
  pivot_longer(cols = c(AMAV, AMBI, AMGP, AMKE, AWPE, BAEA, BASA, BBPL, BCNH, BLOY, BLTU, BNST, COHA, COSN, DOSP, DUNL, FEHA, GBHE, GOEA, GREG, GRHE, GRYE, KILL, LBCU, LBDO, LEGP, LESA, LEYE, LWSA, MAGO, MERL, NOHA, OSPR, PAGP, PEEP, PEFA, PESA, PHAL, PRFA, REKN, REPH, RLHA, RNPH, RSHA, RTHA, RUFF, RUTU, SAND, SBDO, SEOW, SEPL, SESA, SNEG, SNPL, SOSA, SPSA, SSHA, SSCOHA, STSA, SURF, WESA, WILL, WIPH, WATA, WHIM, WTKI, YELL), names_to = "species", values_to = "count")  %>% 
  filter(MONTH == 8, 
         DAY == 18,
         YEAR == 2022,
         count > 0,
         `COUNT AREA` == "WCD") %>% view()
  
  



