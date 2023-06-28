library(RODBC)


  dat.pth <- "C:/GitRep/Pacific_Catch_Tool/Data/"


  channellog <- odbcDriverConnect("DSN=Visual FoxPro Database;UID=;PWD=;
                                   SourceDB=\\\\corp.spc.int\\shared\\FAME\\NC_NOU\\OFP\\db1\\tuna_dbs\\Log_dbs\\DBF\\logsheet.dbc;
                                   SourceType=DBC;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=Yes;Deleted=Yes;")


#  sqlTables(channellog)
#  sqlColumns(channellog, "l_best")


  
  ez_dat <- sqlQuery(channellog, query = "SELECT yy, IIF(INLIST(gr_id,'L','S','P','T'), gr_id, 'Z') AS gear, flag_id AS flag, ez_id AS eez,
                                                 sum(alb_c) AS alb_mt, sum(bet_c) AS bet_mt, sum(skj_c) AS skj_mt, sum(yft_c) AS yft_mt
                                          FROM a_yb_ez WHERE yy >= 1990
                                          GROUP BY yy, gear, flag, eez ", max = 0, stringsAsFactors = FALSE)
  
  write.table(ez_dat, file = paste0(dat.pth, "ACE_BY_FLAG.TXT"), sep = ",", row.names = FALSE)
  
  
  
  map_dat <- sqlQuery(channellog, query = "SELECT yy, gr_id AS gear, flag_id AS flag,
                            INT(IIF(RIGHT(lon_short,1)='W',360-(VAL(LEFT(lon_short,3))+(VAL(SUBSTR(lon_short,4,2))/60)),VAL(LEFT(lon_short,3))+(VAL(SUBSTR(lon_short,4,2))/60)))+2.5 AS lond,
                            IIF(RIGHT(lat_short,1)='S',-1,1)*(VAL(LEFT(lat_short,2)))+2.5 AS latd,
                         sum(alb_c) AS alb_mt, sum(bet_c) AS bet_mt, sum(skj_c) AS skj_mt, sum(yft_c) AS yft_mt
                         FROM a_model 
                         GROUP BY yy, gear, flag, lond, latd ", max = 0, stringsAsFactors = FALSE)

  write.table(map_dat, file = paste0(dat.pth, "AGGREGATE_RAISED_5X5_S-L-P-T-Z.TXT"), sep = ",", row.names = FALSE)
  
  
  
  
  
  
  
  
  
  