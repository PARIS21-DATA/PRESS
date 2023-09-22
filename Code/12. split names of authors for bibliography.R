"Thomas de Hoop, Andi Coombes, Averi Chakrabarti, Marlous de Milliano, Anna Warren, Chinmaya Holla, Ozen Guven, Beth Anne Card, Philomena Panagoulias, Melissa Rodgers, Shalu Jain, Adria Molotsky, Eve Namisango" %>% 
  str_split(pattern = ", ") %>% 
  unlist %>% 
  str_split(patter = " ") %>% 
  sapply(function(x) paste0(paste0(x[2:length(x)], collapse = " "), ", ", x[1] )) %>% 
  writeLines("data/tmp/undp synthesis report.txt")