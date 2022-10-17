reporters in the PRESS reporter list:

- ReporterName: used to merge with survey
- donorname: fully compatible with CRS

  which(!df_reporters_press$donorname_unified  %in% df_reporters$ReporterName )
  which(!df_reporters_press$donorname_unified  %in% df_reporters$crs_name_en )
  which(!df_reporters_press$donorname_unified  %in% df_reporters$ch_name)
  which(!df_reporters_press$donorname_unified  %in% df_reporters$name_modification)
  
  which(!df_reporters_press$donorname  %in% df_reporters$ReporterName )
  which(!df_reporters_press$donorname  %in% df_reporters$crs_name_en ) # fully compatible!
  which(!df_reporters_press$donorname  %in% df_reporters$ch_name)
  which(!df_reporters_press$donorname  %in% df_reporters$name_modification)
  
  which(!df_reporters_press$ReporterName  %in% df_reporters$ReporterName )
  which(!df_reporters_press$ReporterName   %in% df_reporters$crs_name_en )
  which(!df_reporters_press$ReporterName   %in% df_reporters$ch_name)
  which(!df_reporters_press$ReporterName   %in% df_reporters$name_modification)

- ReporterId: seems to be the CH id (TBC)

however, not every Reporter Name have a donor name
We only focus on the ones that are used in merging

The ones without reporter name but with a donor name, happened to be not used at all in merging. 
They do not exist in survey data at all!


You can indeed improve the press donor list by correcting the unece etc. But it's not currently urgent. 

After merging survey list with the survey provider list, it does not have a column that is fully compatible with the ch list




