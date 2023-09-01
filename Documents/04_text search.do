* 1) identify data- and statistics-related projects
* 2) identify statistical domains

clear 
set more off

use "${workdata}w_country_char_2022", clear


****************************************************************
*** 1.1 Keyword search of projecttitles
****************************************************************
gen other_stats = 0 
	
display "statisti"
replace other_stats = 1 if strpos(projecttitle, "statisti") & purposecode!=16062 // covers french "statistique"
replace other_stats = 1 if strpos(projecttitle, "estadisti") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "estadísti") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "estatísti") & purposecode!=16062 // portuguese

display "national account"
replace other_stats = 1 if strpos(projecttitle, "national account") & purposecode!=16062 // check whether this makes sense...
replace other_stats = 1 if strpos(projecttitle, "comptes nationaux") & purposecode!=16062 // check whether this makes sense...
replace other_stats = 1 if strpos(projecttitle, "cuentas nacionales") & purposecode!=16062 // check whether this makes sense...
replace other_stats = 1 if strpos(projecttitle, "contas nacionais") & purposecode!=16062 // portuguese

display "price index"
replace other_stats = 1 if strpos(projecttitle, "price index") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "indice de precios") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "índice de precios") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "indice des prix") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "índice de preço") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "indice de preco") & purposecode!=16062

display "production index"
replace other_stats = 1 if strpos(projecttitle, "production index") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "índice de producción") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "índice de produccion") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "indice de production") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "índice de produção") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "indice de producao") & purposecode!=16062

display "survey"
replace other_stats = 1 if strpos(projecttitle, "survey") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "enquête") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "enquete") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "enquesta") & purposecode!=16062 // spanish
replace other_stats = 1 if strpos(projecttitle, "inquérito") & purposecode!=16062 // portuguese
replace other_stats = 1 if strpos(projecttitle, "inquerito") & purposecode!=16062 // portuguese

display "census"
replace other_stats = 1 if strpos(projecttitle, "census") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "recensement") & purposecode!=16062	// french
replace other_stats = 1 if strpos(projecttitle, "censo") & purposecode!=16062	// spanish and portuguese

display "information system"
replace other_stats = 1 if strpos(projecttitle, "information system") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "systeme d'information") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "système d'information") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "sistema de informacion") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "sistema de información") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "sistema de informação") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "sistema de informacao") & purposecode!=16062

* birth registration
display "birth registr"
replace other_stats = 1 if strpos(projecttitle, "birth registr") & purposecode!=16062 	
replace other_stats = 1 if strpos(projecttitle, "enregistrement des naiss") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "inscripción del naci") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "inscripcion del naci") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "registo dos nasci") & purposecode!=16062 

* death registration
display "death registr"
replace other_stats = 1 if strpos(projecttitle, "death registr") & purposecode!=16062 	
replace other_stats = 1 if strpos(projecttitle, "enregistrement des déc") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "enregistrement des dec") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "inscripción del defunc") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "inscripcion del defunc") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "registo do óbito") & purposecode!=16062 	
replace other_stats = 1 if strpos(projecttitle, "registo do obito") & purposecode!=16062 	

* civil registration
display "civil registration"
replace other_stats = 1 if strpos(projecttitle, "civil registr") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "crvs") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "registre civil") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "registro civil") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "registro civil") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "registo civil") & purposecode!=16062 // 

display "land registration"
replace other_stats = 1 if strpos(projecttitle, "land registr") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "enregistrement fonc") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "enregistrement des terrai") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "inscripción de tierra") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "inscripcion de tierra") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "registro de tera") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "registo de terren") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "registo de propriedad") & purposecode!=16062 

display "cadaster"
replace other_stats = 1 if strpos(projecttitle, "cadaster") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "cadastre") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "catastro") & purposecode!=16062 & !strpos(projecttitle, "catastrop")
replace other_stats = 1 if strpos(projecttitle, "cadastro") & purposecode!=16062 

* business registries
display "business registr"
replace other_stats = 1 if strpos(projecttitle, "business registr") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "registre des entrepr") & purposecode!=16062 // french
replace other_stats = 1 if strpos(projecttitle, "registre du commerce") & purposecode!=16062 // french	
replace other_stats = 1 if strpos(projecttitle, "registro mercantil") & purposecode!=16062 // spanish	
replace other_stats = 1 if strpos(projecttitle, "registo das empresas") & purposecode!=16062 // portuguese	
replace other_stats = 1 if strpos(projecttitle, "registos comerciais") & purposecode!=16062 // portuguese	

* database
display "database"
replace other_stats = 1 if strpos(projecttitle, "database") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "base de données") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "base de donnees") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "base de datos") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "banco de dados") & purposecode!=16062 

* big data
display "big data"
replace other_stats = 1 if strpos(projecttitle, "big data") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "mégadonnées") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "megadonnees") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "datos masivos") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "megadados") & purposecode!=16062 


* IMF D4D
display "data for decisions"
replace other_stats = 1 if strpos(projecttitle, "data for decisions") & purposecode!=16062 // IMF D4D
replace other_stats = 1 if strpos(projecttitle, "données pour les décisions") & purposecode!=16062 // IMF D4D
replace other_stats = 1 if strpos(projecttitle, "donnees pour les decisions") & purposecode!=16062 // IMF D4D
replace other_stats = 1 if strpos(projecttitle, "datos para decisiones") & purposecode!=16062 // IMF D4D
replace other_stats = 1 if strpos(projecttitle, "dados para decisões") & purposecode!=16062 // IMF D4D
replace other_stats = 1 if strpos(projecttitle, "dados para decisoes") & purposecode!=16062 // IMF D4D


* Data innovation 
display "data innovation"
replace other_stats = 1 if strpos(projecttitle, "data innovation") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "données innovantes") & purposecode!=16062 //
replace other_stats = 1 if strpos(projecttitle, "donnees innovantes") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "datos innovadores") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "dados inovadores") & purposecode!=16062 // 

* Data science 
display "data science"
replace other_stats = 1 if strpos(projecttitle, "data science") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "science des données") & purposecode!=16062 //
replace other_stats = 1 if strpos(projecttitle, "science des donnees") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "cienca de datos") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "ciência de dados") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "ciencia de dados") & purposecode!=16062 // 

display "data for development"
replace other_stats = 1 if strpos(projecttitle, "data for development") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "données pour le développement") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "donnees pour le developpement") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "datos para el desarrollo") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "dados para desenvolvimento") & purposecode!=16062 // 

* Data journalism
display "data journalism"
replace other_stats = 1 if strpos(projecttitle, "data journalism") & purposecode!=16062 // rti international
replace other_stats = 1 if strpos(projecttitle, "journalisme de donnees") & purposecode!=16062 // rti international	
replace other_stats = 1 if strpos(projecttitle, "journalisme de données") & purposecode!=16062 // rti international	
replace other_stats = 1 if strpos(projecttitle, "periodismo de datos") & purposecode!=16062 // rti international	
replace other_stats = 1 if strpos(projecttitle, "jornalismo de dados") & purposecode!=16062 // rti international	

* Data centers
display "data center"
replace other_stats = 1 if strpos(projecttitle, "data center") & purposecode!=16062 // korea!
replace other_stats = 1 if strpos(projecttitle, "data centre") & purposecode!=16062 // korea!
replace other_stats = 1 if strpos(projecttitle, "centre de donnees") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "centre de données") & purposecode!=16062 //
replace other_stats = 1 if strpos(projecttitle, "centro de datos") & purposecode!=16062 // rti international	
replace other_stats = 1 if strpos(projecttitle, "centro de dados") & purposecode!=16062 // rti international	

*
display "data for education"
replace other_stats = 1 if strpos(projecttitle, "data for education") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "données pour l'éducation") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "donnees pour l'education") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "datos para la educación") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "datos para la educacion") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "dados para educação") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "dados para educacao") & purposecode!=16062 // 

display "education data"
replace other_stats = 1 if strpos(projecttitle, "education data") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "données sur l'éducation") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "donnees sur l'education") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "datos educativos") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "dados educacionais") & purposecode!=16062 // 

display "data for health"
replace other_stats = 1 if strpos(projecttitle, "data for health") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "données pour la santé") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "donnees pour la sante") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "datos para la salud") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "dados para saúde") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "dados para saude") & purposecode!=16062 // 

display "global pulse"
replace other_stats = 1 if strpos(projecttitle, "global pulse") & purposecode!=16062 // 

display "health data"
replace other_stats = 1 if strpos(projecttitle, "health data") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "données de santé") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "donnees de sante") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "datos de salud") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "dados de saúde") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "dados de saude") & purposecode!=16062 // 

display "refugee data"
replace other_stats = 1 if strpos(projecttitle, "refugee data") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "données sur les réfugiés") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "donnees sur les refugies") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "datos de refugiados") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "dados de refugiados") & purposecode!=16062 // 

display "global data"
replace other_stats = 1 if strpos(projecttitle, "global data") & purposecode!=16062 // 

* immigration data?
replace other_stats = 1 if strpos(projecttitle, "migration data") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "données de migration") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "donnees de migration") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "datos de migración") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "datos de migracion") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "dados de migração") & purposecode!=16062 	
replace other_stats = 1 if strpos(projecttitle, "dados de migracao") & purposecode!=16062 

* spatial data? donnees spatial
display "spatial data"
replace other_stats = 1 if strpos(projecttitle, "spatial data") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "données spatiale") & purposecode!=16062 
replace other_stats = 1 if strpos(projecttitle, "donnees spatiale") & purposecode!=16062 

display "data collection"
replace other_stats = 1 if strpos(projecttitle, "data collection") & purposecode!=16062 //
replace other_stats = 1 if strpos(projecttitle, "collecte de données") & purposecode!=16062 //
replace other_stats = 1 if strpos(projecttitle, "collecte de donnees") & purposecode!=16062 //
replace other_stats = 1 if strpos(projecttitle, "collecte des données") & purposecode!=16062 //
replace other_stats = 1 if strpos(projecttitle, "collecte des donnees") & purposecode!=16062 //
replace other_stats = 1 if strpos(projecttitle, "rassemblement des données") & purposecode!=16062 //
replace other_stats = 1 if strpos(projecttitle, "rassemblement des donnees") & purposecode!=16062 //
replace other_stats = 1 if strpos(projecttitle, "recopilación de datos") & purposecode!=16062 //
replace other_stats = 1 if strpos(projecttitle, "recopilacion de datos") & purposecode!=16062 //
replace other_stats = 1 if strpos(projecttitle, "colección de datos") & purposecode!=16062 //
replace other_stats = 1 if strpos(projecttitle, "coleccion de datos") & purposecode!=16062 //
replace other_stats = 1 if strpos(projecttitle, "compilación de datos") & purposecode!=16062 //
replace other_stats = 1 if strpos(projecttitle, "compilacion de datos") & purposecode!=16062 //
replace other_stats = 1 if strpos(projecttitle, "regocida de datos") & purposecode!=16062 //

replace other_stats = 1 if strpos(projecttitle, "action through data") & purposecode!=16062 // https://dukeghic.org/regional-action-through-data-rad/
*replace other_stats = 1 if strpos(projecttitle, "data driven") & purposecode!=16062 // 

display "data project"
replace other_stats = 1 if strpos(projecttitle, "data project") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "projet de données") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "projet de donnees") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "proyecto de datos") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "projeto de dados") & purposecode!=16062 // 

display "open government data"
replace other_stats = 1 if strpos(projecttitle, "open government data") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "données publiques ouvertes") & purposecode!=16062 // 
replace other_stats = 1 if strpos(projecttitle, "donnees publiques ouvertes") & purposecode!=16062 // 

* other
replace other_stats = 1 if strpos(projecttitle, "open data") & purposecode!=16062	
replace other_stats = 1 if strpos(projecttitle, "données ouvertes") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "donnees ouvertes") & purposecode!=16062	
replace other_stats = 1 if strpos(projecttitle, "datos abiertos") & purposecode!=16062	
replace other_stats = 1 if strpos(projecttitle, "dados abertos") & purposecode!=16062	

replace other_stats = 1 if strpos(projecttitle, "openstreetmap") & purposecode!=16062 // rti international

* OPHI
replace other_stats = 1 if (strpos(projecttitle, "ophi ") | strpos(projecttitle, " ophi")) & purposecode!=16062 // OPHI (note blanks!!!)

display "satellite data"
replace other_stats = 1 if strpos(projecttitle, "satellite data") & purposecode!=16062	
replace other_stats = 1 if strpos(projecttitle, "données satelli") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "donnees satelli") & purposecode!=16062	
replace other_stats = 1 if strpos(projecttitle, "datos satelital") & purposecode!=16062	
replace other_stats = 1 if strpos(projecttitle, "dados de satélit") & purposecode!=16062		
replace other_stats = 1 if strpos(projecttitle, "dados de satelit") & purposecode!=16062		

display "IMF financial sector stability fund"
replace other_stats = 1 if (strpos(projecttitle, "financial sector stability fund") | strpos(projecttitle, "fssf")) & channelcode==43000

replace other_stats = 1 if strpos(projecttitle, "satellite data") & purposecode!=16062	
replace other_stats = 1 if strpos(projecttitle, "données satelli") & purposecode!=16062
replace other_stats = 1 if strpos(projecttitle, "donnees satelli") & purposecode!=16062	
replace other_stats = 1 if strpos(projecttitle, "datos satelital") & purposecode!=16062	
replace other_stats = 1 if strpos(projecttitle, "dados de satélit") & purposecode!=16062		
replace other_stats = 1 if strpos(projecttitle, "dados de satelit") & purposecode!=16062	

display "50X2030"
replace other_stats = 1 if  strpos(projecttitle,"2030") & strpos(projecttitle,"50") & purposecode!=16062	

display "peacebuilding data"
replace other_stats = 1 if strpos(projecttitle, "peacebuilding data")	& purposecode!=16062

display "global data"
count if strpos(projecttitle, "global data") & purposecode!=16062	 & other_stats==0
tab donorname if strpos(projecttitle, "global data") & purposecode!=16062 & other_stats==0

display "data4development"
count if strpos(projecttitle, "data4development") & purposecode!=16062	 & other_stats==0
tab donorname if strpos(projecttitle, "data4development") & purposecode!=16062 & other_stats==0
replace other_stats = 1 if strpos(projecttitle, "data4development") & purposecode!=16062
	

****************************************************************
*** 2. Manual curation
*** 2.1 Manual additions 
****************************************************************

*** Netherlands and BMGF support the IMF's financial access survey (fas)
tab donorname source  if strpos(longdescription, "financial access survey")
replace other_stats = 1 if strpos(longdescription, "financial access survey") & purposecode!=16062 

***NSOs as implementes
replace other_stats = 1 if strpos(channelreportedname,"statist") & purposecode!=16062
replace other_stats = 1 if strpos(longdescription,"institute of statistics") & purposecode!=16062
replace other_stats = 1 if strpos(longdescription,"mathermatical and statistical modelling") & purposecode!=16062
replace other_stats = 1 if strpos(longdescription,"statistics canada") & purposecode!=16062
replace other_stats = 1 if strpos(longdescription,"fiscal and monetary statistics") & purposecode!=16062
replace other_stats = 1 if strpos(longdescription,"statistical production of ine") & purposecode!=16062
replace other_stats = 1 if strpos(channelreportedname,"danmarks statistik") & purposecode!=16062 // denmark

replace other_stats = 1 if strpos(channelreportedname,"istat") & purposecode!=16062 & donorname=="Italy"

*** MCC
* Tanzania 
replace other_stats = 1 if agencyname=="Millennium Challenge Corporation" & strpos(longdescription,"data-driven communities activity")

* Cote d'Ivoire
replace other_stats = 1 if agencyname=="Millennium Challenge Corporation" & strpos(longdescription,"data analytics")
replace other_stats = 1 if agencyname=="Millennium Challenge Corporation" & strpos(longdescription,"data for youth")

*** Germany
replace other_stats = 1 if strpos(projecttitle, "remote sensing data") & purposecode!=16062 & donorname=="Germany" // 
replace other_stats = 1 if strpos(longdescription, "remote sensing data") & purposecode!=16062 & donorname=="Germany" // 
replace other_stats = 1 if strpos(longdescription, "information system supplying hydrological and meteorological data") & purposecode!=16062 & donorname=="Germany" // 
replace other_stats = 1 if strpos(longdescription, "capacity development in gis and data base management") & purposecode!=16062 & donorname=="Germany" // 
replace other_stats = 1 if strpos(longdescription, "higher resolution data") & purposecode!=16062 & donorname=="Germany" // 
replace other_stats = 1 if strpos(longdescription, "data collection and analysis") & purposecode!=16062 & donorname=="Germany" //  
replace other_stats = 1 if strpos(longdescription, "data journalism") & purposecode!=16062 & donorname=="Germany" // 
replace other_stats = 1 if strpos(longdescription, "processing and evaluation of data") & purposecode!=16062 & donorname=="Germany" // 
replace other_stats = 1 if strpos(longdescription, "global forest survey") & purposecode!=16062 & donorname=="Germany" // 	
replace other_stats = 1 if strpos(projecttitle, "consumer data protection") & purposecode!=16062 & donorname=="Germany" // 	
replace other_stats = 1 if strpos(longdescription, "web-based data analysis platform") & purposecode!=16062 & donorname=="Germany" // 	
/* 2020 Profiles - Additional projects as asked by Germany */

	
replace other_stats = 1 if strpos(projecttitle, "strengthening good governance kenia") &  purposecode == 15113 & donorname=="Germany" // 		
replace other_stats = 1 if strpos(projecttitle, "social protection innovation and learning") & purposecode == 16010 &  donorname=="Germany" // 	
replace other_stats = 1 if strpos(projecttitle, "citizens engagement and innovative data use for africa's development")  & purposecode == 15150 & donorname=="Germany" // 		
replace other_stats = 1 if strpos(projecttitle, "partners for review") & purposecode ==15110 & donorname=="Germany" // 		
replace other_stats = 1 if strpos(projecttitle, "knowledge for nutrition") & purposecode ==12240 &  donorname=="Germany" // 		
replace other_stats = 1 if strpos(projecttitle, "soil information for sustainable land use in cameroon") & purposecode ==31130 & donorname=="Germany" // 		
replace other_stats = 1 if strpos(projecttitle, "support to the moldovan government for the implementation of the 2030 agenda") & purposecode ==43010 & donorname=="Germany" // 		
replace other_stats = 1 if strpos(projecttitle, "implementation of 2030 agenda in bolivia") & purposecode ==14010 &  donorname=="Germany" // 		
replace other_stats = 1 if strpos(projecttitle, "sdg initative namibia") & purposecode ==15114 & donorname=="Germany" // 		
replace other_stats = 1 if strpos(projecttitle, "support to the identification of poor households (idpoor) programme") &  purposecode ==16010 & donorname=="Germany" // 		
replace other_stats = 1 if strpos(projecttitle, "studies and experts fund kyrgyzstan") & purposecode ==43010 & donorname=="Germany" // 		
replace other_stats = 1 if strpos(projecttitle, "programme macroeconomic reform - green growth") &  purposecode ==15110 & crsid == "2020002030a" & donorname=="Germany" // 		
	
	
	
/* Additional projects identified by Germany from PARIS21 methodology - to be considered for a smart merge in 2023 

replace other_stats = 1 if inlist(crsid,"2010003915","2010009939","2010009940") & donorname=="Germany"

replace other_stats = 1 if inlist(crsid,"2010011436","2010009779") & donorname=="Germany"


replace other_stats = 1 if inlist(crsid,"2010009784","2010009786","2010009882","2010010622","2010011457") & donorname=="Germany"

replace other_stats = 1 if inlist(crsid,"2011008444","2011011550","2011012301") & donorname=="Germany"


replace other_stats = 1 if inlist(crsid,"2012004669","2013002091","2013004973","2013007894","2013009143") & donorname=="Germany"

replace other_stats = 1 if inlist(crsid,"2014011726","2015127466","2016012607","2016005772","2017008275") & donorname=="Germany"

replace other_stats = 1 if inlist(crsid,"2017002362","2017004638","2017011443","2015127466","2018003466") & donorname=="Germany"

replace other_stats = 1 if inlist(crsid,"2018007639","2018001985") & donorname=="Germany"


replace other_stats = 1 if inlist(crsid,"2015127466","2015127466","2019003811","2015127466") & donorname=="Germany"

*/

// projects to be deleted as per exchange with Germany 	

drop if strpos(projecttitle, "for uncensored flow of information on public issues by community based broadcast media in latin america")  & donorname == "Germany"  
	
*** France
replace other_stats = 1 if strpos(projecttitle, "financement opal") & purposecode!=16062 & donorname=="France" // OPAL
replace other_stats = 1 if strpos(projecttitle, "installation de deux stations hydrométriques") & purposecode!=16062 & donorname=="France"

*** Switzerland (as per email exchange)
replace other_stats = 1 if inlist(crsid,"2016003165","2016003166","2016003339","2016003340","2016003341") & donorname=="Switzerland"


*********************
*** negative list ***
*********************
* belgium
replace other_stats = 0 if crsid=="20180C2918" // something with drones detecting explosive remnants???

*CaDB
replace other_stats = 0 if strpos(projecttitle, "contamination survey") & purposecode!=16062

* czech republic
replace other_stats = 0 if strpos(projecttitle, "hydrogeological survey") & purposecode!=16062

* denmark 
replace other_stats = 0 if strpos(projecttitle, "fiber optic backbone link")

* France
replace other_stats = 0 if strpos(projecttitle, "fasep 1096-creation de data centers souverains")
replace other_stats = 0 if strpos(projecttitle, "soutien de la politique de l'éducation nationale")
replace other_stats = 0 if strpos(projecttitle, "soutien aux politiques d'éducation")
replace other_stats = 0 if strpos(projecttitle, "soutienpolitiqueeduc")
replace other_stats = 0 if strpos(projecttitle, "enquete qualitative")
replace other_stats = 0 if strpos(projecttitle, "sediment survey")

* germany  

replace other_stats = 0 if strpos(projecttitle, "database to document human rights violations") & purposecode!=16062
replace other_stats = 0 if strpos(projecttitle, "wreck management information system") & purposecode!=16062
replace other_stats = 0 if strpos(projecttitle, "wreck management information system") & purposecode!=16062
replace other_stats = 0 if strpos(projecttitle, "patent information system") & purposecode!=16062
replace other_stats = 0 if strpos(longdescription, "support the implementation of environmental legislation in particular as regards compliance or restoration of forest policy for private ownership of land") & purposecode!=16062


* italy
replace other_stats = 0 if strpos(projecttitle, "penitentiary information system") & purposecode!=16062
replace other_stats = 0 if strpos(projecttitle, "geophisical survey of the valley of the kings") & purposecode!=16062
replace other_stats = 0 if strpos(projecttitle, "metallic artefacts") & purposecode!=16062
replace other_stats = 0 if strpos(projecttitle, "philosophy / data sciences") & purposecode!=16062
replace other_stats = 0 if strpos(projecttitle, "increasing the number of international students attending phd courses in: data science") & purposecode!=16062
replace other_stats = 0 if strpos(projecttitle, "database for election observation missions") & purposecode!=16062
drop if crsid == "2017000137" & purposecode == 16062 & donorname == "Italy" // asked to remove in 2022 


* luxembourg 
replace other_stats = 0 if strpos(projecttitle, "minusma: commission d'enquête internationale") & purposecode!=16062

* netherlands
replace other_stats = 0 if strpos(projecttitle, "survey on nl & id history") & purposecode!=16062
replace other_stats = 0 if strpos(projecttitle, "cot_port_cotonou_quai_survey") & purposecode!=16062
replace other_stats = 0 if strpos(projecttitle, "court decision database") & purposecode!=16062
replace other_stats = 0 if strpos(projecttitle, "cot_port_cotonou_quai_survey") & purposecode!=16062
replace purposecode = 0 if strpos(projecttitle, "ilo rmg program") // purpose code is 16062!!!

* poland
replace other_stats = 0 if channelreportedname == "national bank of poland" // keep it in

* sweden
replace other_stats = 0 if strpos(projecttitle, "surveying political candidates") // sweden

* switzerland
replace other_stats = 0 if strpos(projecttitle, "small arms survey") & purposecode!=16062 
replace other_stats = 0 if strpos(projecttitle, "mine survey") & purposecode!=16062

* uk
replace other_stats = 0 if strpos(projecttitle, "national accountability") & purposecode!=16062 // also eu
replace other_stats = 0 if strpos(projecttitle, "international account") & purposecode!=16062 // also eu

* us
replace other_stats = 0 if strpos(projecttitle, "geological survey") & purposecode!=16062 // also eu
replace other_stats = 0 if strpos(projecttitle, "remnants survey") & purposecode!=16062
replace other_stats = 0 if strpos(projecttitle, "explosive remnants") & purposecode!=16062
replace other_stats = 0 if strpos(projecttitle, "cluster munition and other conventional weapons survey") & purposecode!=16062	
replace other_stats = 0 if strpos(projecttitle, "unexploded ordnance") & purposecode!=16062	
replace other_stats = 0 if strpos(projecttitle, "explosive ordnance") & purposecode!=16062	
replace other_stats = 0 if strpos(projecttitle, "clearance") & purposecode!=16062	
replace other_stats = 0 if strpos(projecttitle, "demining") & purposecode!=16062
replace other_stats = 0 if strpos(projecttitle, "mine action") & purposecode!=16062	
replace other_stats = 0 if strpos(projecttitle, "demining") & purposecode!=16062

* denmark
replace other_stats = 0 if strpos(projecttitle, "hydrogeological survey") & purposecode!=16062

* japan
replace other_stats = 0 if strpos(projecttitle, "risques des mines") & purposecode!=16062
replace other_stats = 0 if strpos(projecttitle, "disaster management information system") & purposecode!=16062

* korea
replace other_stats = 0 if strpos(projecttitle, "mine hazard") & purposecode!=16062
replace other_stats = 0 if strpos(projecttitle, "law information system") & purposecode!=16062
replace other_stats = 0 if strpos(projecttitle, "emergency management information system") & purposecode!=16062
replace other_stats = 0 if strpos(projecttitle, "public safety management information system") & purposecode!=16062
replace other_stats = 0 if strpos(projecttitle, "public saftey management information system") & purposecode!=16062
replace other_stats = 0 if strpos(projecttitle, "immigration information system") & purposecode!=16062

* eu
replace other_stats = 0 if strpos(projecttitle, "survey of hydrogeological") & purposecode!=16062
replace other_stats = 0 if strpos(projecttitle, "catastrophe") & purposecode!=16062

* spain
replace other_stats = 0 if strpos(projecttitle, "catastrophic") & purposecode!=16062

* switzerland
replace other_stats = 0 if strpos(projecttitle, "demining") & purposecode!=16062
replace other_stats = 0 if strpos(projecttitle, "mine victims") & purposecode!=16062
*replace other_stats = 0 if strpos(projecttitle, "mine victims") & purposecode!=16062

/* as per Switzerland''s request to remove these projects */ 
replace other_stats = 0 if crsid=="2020003921" &  strpos(projecttitle, "financement enquête et plaidoyer sur l'exploitation de la forêt en amazonie") &  donorname == "Switzerland"
replace other_stats = 0 if crsid=="2018005231" &  strpos(projecttitle, "medici senza frontiere - sostegno per interventi in caso di catastrofe") & donorname == "Switzerland"
replace other_stats = 0 if crsid=="2020006913" &  strpos(projecttitle, "mission d'enquête sur les résultats des projets au kwilu") & donorname == "Switzerland"
replace other_stats = 0 if crsid=="2020008519" &  strpos(projecttitle, "survey comprehensive sexuality education - small action credit") &  donorname == "Switzerland"
replace other_stats = 0 if crsid=="2020006926" &  strpos(projecttitle, "enquête conjointe et plaidoyer auprès des nations unies sur l'impact négatif des mégaprojets au guatemala") &  donorname == "Switzerland"
replace other_stats = 0 if crsid=="2020008679" &  strpos(projecttitle, "fao/pam: enquête sécurité alimentaire - small action credit") & purposecode == 31110 &  donorname == "Switzerland"
replace other_stats = 0 if crsid=="2020008679" &  strpos(projecttitle, "fao/pam: enquête sécurité alimentaire - small action credit") & purposecode == 43071 &  donorname == "Switzerland"


* united arab emiates (refugee costs, camp information system in Jordan)
replace other_stats = 0 if strpos(projecttitle, "information system related to the comp") & purposecode!=16062  

* individual projects
replace other_stats = 0 if crsid=="2011000041"
replace other_stats = 0 if crsid=="2016000008"
replace other_stats = 0 if crsid=="2010000119"
replace other_stats = 0 if crsid=="2011000448"
replace other_stats = 0 if crsid=="2012000002"
replace other_stats = 0 if crsid=="2013021796"
replace other_stats = 0 if crsid=="2012000094"
replace other_stats = 0 if crsid=="2012000093"
replace other_stats = 0 if crsid=="2011000180"
replace other_stats = 0 if crsid=="2013000005"

*** exclusion based on purpose codes 
replace other_stats = 0 if purposecode==15250  // Removal of land mines and explosive remnants of war 

***************
*** combine ***
***************
capture drop source
gen source = 0
**# Bookmark #1
replace source = 1 if purposecode==16062
replace source = 2 if inlist(purposecode, 12196, 13096, 15196)
replace source = 3 if other_stats==1
replace source = 4 if select_support==1
label define source 0 "Other ODA" 1 "Purpose code: statistical capacity building" 2 "Other purpose codes" 3 "Text mining" 4 "Select other sources"
label value source source

************************************************
* tag DAC members and profile countries
************************************************
capture drop dac 
capture drop dac10
gen dac = 0 
foreach member of global dac {
	replace dac =1 if donorname=="`member'"
	}
	
gen dac10 = 0 
foreach member of global dac10 {
	replace dac10 =1 if donorname=="`member'"
	}
	
*****************
*** gender marker
*** this seems to be rather informative
*****************
replace gender = 9 if missing(gender)
label define gender 0 "Not an objective" 1 "Significant objective" 2 "Principal objective" 9 "Not screened"
label value gender gender
*table donorname gender  if inrange(source,1,2) & inrange(year,2016,2018), contents(sum usd_disbursement_defl)

******************************************************
*** reproductive, maternal, newborn and child health
******************************************************
replace rmnch = 9 if missing(rmnch)
label define rmnch 0 "Not an objective" 1 "At least 1/4" 2 "At least 1/2" 3 "Most, but not all" 4 "Primary objective" 9 "Not screened"
label value rmnch rmnch
*table donorname rmnch  if inrange(source,1,2) & inrange(year,2016,2018), contents(sum usd_disbursement_defl) // all categories only since 2013?

************************************************
*** channel multilaterals - not used currently
************************************************
gen channel_multi = "Other"
replace channel_multi = "World Bank Group" if channelname == "World Bank Group" | strpos(channelname,"International Bank for Reconstruction") | channelname=="International Development Association"
replace channel_multi = "IDB" if  strpos(channelname,"Inter-American Development Bank") 
replace channel_multi = "AfDB" if  strpos(channelname,"African Development Bank") 
replace channel_multi = "IMF" if channelname == "International Monetary Fund (IMF)"
replace channel_multi = "UNICEF" if channelname == "United Nations Children's Fund"
replace channel_multi = "UNDP" if channelname == "United Nations Development Programme"
replace channel_multi = "UN WOMEN" if strpos(channelname,"United Nations Entity for Gender Equa")
replace channel_multi = "WFP" if channelname == "World Food Programme"
replace channel_multi = "WHO" if strpos(channelname,"World Health Organisation")
replace channel_multi = "UNFPA" if strpos(channelname,"United Nations Population Fund")
replace channel_multi = "FAO" if strpos(channelname,"Food and Agricultural Organisation")
replace channel_multi = "OECD" if strpos(channelname,"Organisation for Economic Co-operation")
replace channel_multi = "ILO" if  strpos(channelname,"International Labour Organisation") 

************************************
*** statitics support for what?
*** define domains
************************************
keep if inrange(year,2010,2020)

capture drop sector
gen sector = .

label define sector 1 "General statistical capacity" 2 "Population" 3 "Education" 4 "Health" 5 "Economy" 6 "Agriculture" 7 "Gender " 8 "Environment" 9 "Other"
label value sector sector
************************************************************************************************************************************************************
*** first layer: define class of statistic by sector, assigning all activities under purpose code 16062 initially to general statistical capacity building
************************************************************************************************************************************************************

* general, economics, education, health,  
replace sector = 1 if purposecode==16062 // general
replace sector = 2 if inrange(purposecode,13000,13999) // population
replace sector = 3 if inrange(purposecode,11000,11999) // education
replace sector = 4 if inrange(purposecode,12000,12999) // health

replace sector = 5 if inlist(purposecode,15123, 15124, 15196, 15111, 15117, 15118, 15119) // administration of foreign aid, persnnel services, government and civil society statistics and data, pfm ,budget planning, national audit, debt and aid managmeent
replace sector = 5 if inlist(purposecode,15114, 15116, 15155, 15156, 15125) // drm, public procurement
replace sector = 5 if inrange(purposecode,15142,15142) // economic statistics: macroeconomic policy
replace sector = 5 if inrange(purposecode,23000,25999) // economic statistics: energy, financial and business statistics 
replace sector = 5 if inrange(purposecode,32000,32999) // economic statistics: industry, mining, construction
replace sector = 5 if inrange(purposecode,33000,33999) // economic statistics: trade policies and regulations
replace sector = 5 if inrange(purposecode,21000,21999) // economic statistics: transport
replace sector = 6 if inrange(purposecode,31000,31999) // agriculture statistics: 
replace sector = 6 if inrange(purposecode,43071,43073) // food security, safety and quality
replace sector = 7 if inrange(purposecode,15170,15180) // gender statistics: 
replace sector = 8 if inrange(purposecode,14050,14050) // environment statistics: waste management/ disposal
replace sector = 8 if inrange(purposecode,14015,14015) // environment statistics: water resource conservation
replace sector = 8 if inrange(purposecode,41000,41999) // environment statistics: 
replace sector = 8 if inrange(purposecode,43040,43049) // environment statistics: rural development, including rural land policy management
replace sector = 9 if missing(sector)
replace sector = 9 if strpos(projecttitle, "aiddata center") // support to the AidData Center is classified as support to SMEs and thus economic statistics; re-classified as "Other"
replace sector = 9 if strpos(projecttitle, "construction of data centers for national id system project") // Korea - aims to strengthen government administration, security, etc....not econ stats 
gen sector1 = sector // for tracking how this procedure affects allocation

************************************************************************************************************************************************************
*** second layer: based on text mining of project titles
************************************************************************************************************************************************************
replace sector = 1  if strpos(projecttitle,"central bureau of statistics") & strpos(projecttitle,"core funding") // siwtzerland PCBS core funding
replace sector = 2  if crsid=="2016001694" // siwtzerland PCBS census
replace sector = 2 if strpos(projecttitle,"population census")
replace sector = 2 if strpos(projecttitle,"housing census")
replace sector = 2 if strpos(projecttitle,"crvs")
replace sector = 2 if strpos(projecttitle,"birth registr") 
replace sector = 2 if strpos(projecttitle,"civil registr") 
replace sector = 2 if strpos(projecttitle,"migration data") 
replace sector = 2 if strpos(projecttitle,"migration statistics") 
replace sector = 2 if strpos(projecttitle,"world bank/unhcr joint data center")
replace sector = 2 if strpos(projecttitle,"support to the identification of poor households (idpoor) programme") // new Germany
replace sector = 3 if strpos(projecttitle,"education management information system")
replace sector = 3 if strpos(projecttitle,"emis") 
replace sector = 3 if strpos(projecttitle,"global programme digital transformation") & purposecode ==22040 &  donorname=="Germany" // new germany 
replace sector = 4 if strpos(projecttitle,"health management information system")
replace sector = 4 if strpos(projecttitle,"health information system")
replace sector = 4 if strpos(projecttitle,"hmis")
replace sector = 4 if strpos(projecttitle,"demographic and health survey")
replace sector = 4 if strpos(projecttitle, "social protection innovation and learning") & purposecode == 16010 &  donorname=="Germany" // new Germany
replace sector = 4 if strpos(projecttitle,"dhs")
replace sector = 4 if strpos(projecttitle,"washington group") // disability data classified under health (CSA)
replace sector = 4 if strpos(projecttitle,"disability data") // disability data classified under health (CSA)
replace sector = 4 if strpos(projecttitle,"death registr") // in keeping with the CSA (https://ec.europa.eu/eurostat/ramon/other_documents/csa/csa_rev_1_october_2009.pdf); death registration should be classified here

replace sector = 5 if strpos(projecttitle,"economic statistic") 
replace sector = 5 if strpos(projecttitle,"fiscal statistic") 
replace sector = 5 if strpos(projecttitle,"finance statistic") 
replace sector = 5 if strpos(projecttitle,"business registr") 
replace sector = 5 if strpos(projecttitle,"national accounts") 
replace sector = 5 if strpos(projecttitle,"tax revenue information system") 
replace sector = 5 if strpos(projecttitle,"land registr") 
replace sector = 5 if strpos(projecttitle,"land valuation") 
replace sector = 5 if strpos(projecttitle,"business ownership survey") 
replace sector = 5 if strpos(projecttitle,"business survey")
replace sector = 5 if strpos(projecttitle,"enterprise survey")  // 
replace sector = 5 if strpos(projecttitle,"programme macroeconomic reform - green growth") &  purposecode ==15110 & crsid == "2020002030a" & donorname=="Germany" // new germany
replace sector = 5 if strpos(projecttitle,"enterprise statist") 
replace sector = 5 if strpos(projecttitle,"business census")
replace sector = 5 if strpos(projecttitle,"enterprise census")  
replace sector = 5 if strpos(projecttitle,"price statistic")  
replace sector = 5 if strpos(projecttitle,"consumer price")
replace sector = 5 if strpos(projecttitle,"producer statistic")   
replace sector = 5 if strpos(projecttitle,"trade statistic")    
replace sector = 5 if strpos(projecttitle,"international comparison program") 
replace sector = 6 if strpos(projecttitle,"agricultural census")
replace sector = 6 if strpos(projecttitle,"agricultural survey")
replace sector = 6 if strpos(projecttitle,"farm survey")
replace sector = 6 if  strpos(projecttitle,"2030") & strpos(projecttitle,"50") // 50-by-2030 initiative

replace sector = 7 if strpos(projecttitle,"gender statist")
replace sector = 7 if strpos(projecttitle,"statistics for gender equality")
replace sector = 8 if strpos(projecttitle,"environment statist")
replace sector = 8 if strpos(projecttitle,"environmental statist")
replace sector = 8 if strpos(projecttitle,"geographic information system") // 
replace sector = 8 if strpos(projecttitle,"climate information system") // 
replace sector = 8 if strpos(projecttitle,"ocean observation") // 
replace sector = 8 if strpos(projecttitle,"biodiversity") // 
replace sector = 8 if strpos(projecttitle,"forest survey") // 
replace sector = 8 if strpos(projecttitle,"plant genetic resources") // 
replace sector = 8 if strpos(projecttitle,"land use") //
replace sector = 8 if strpos(projecttitle,"land cover") 
replace sector = 8 if strpos(projecttitle,"glacier monitoring for energy and water security") // new germany
replace sector = 8 if strpos(projecttitle,"implementation of 2030 agenda in bolivia") &  purposecode == 14010 & donorname=="Germany" // new germany, water policy data 
replace sector = 8 if strpos(projecttitle,"land information system") //
replace sector = 8 if strpos(projecttitle,"meteorological satellite data") //

gen sector2 = sector // keeping track on how layers affect allocation

************************************************************************************************************************************************************
*** third layer: based on main purpose of multilateral through which aid is delivered
************************************************************************************************************************************************************
replace sector = 2 if channelcode==47066 & (sector==9 | sector==1) // IOM
replace sector = 2 if channelcode==41121 & (sector==9 | sector==1) // UNHCR
replace sector = 2 if channelcode==41119 & (sector==9 | sector==1) // UNFPA
replace sector = 3 if channelcode==41304 & (sector==9 | sector==1) // UNESCO
replace sector = 3 if channelcode==47501 & (sector==9 | sector==1) // GPE
replace sector = 4 if channelcode==41110 & (sector==9 | sector==1) // UNAIDS
replace sector = 4 if channelcode==41307 & (sector==9 | sector==1) // WHO - assessed contributions
replace sector = 4 if channelcode==41143 & (sector==9 | sector==1) // WHO - core voluntary contributions
replace sector = 4 if channelcode==47122 & (sector==9 | sector==1) // GAVI
replace sector = 4 if channelcode==47045 & (sector==9 | sector==1) // Global Fund
replace sector = 4 if channelcode==47083 & (sector==9 | sector==1) // Pan-American Health Organization
replace sector = 5 if inrange(channelcode,43000,43999) & (sector==9 | sector==1) // IMF
replace sector = 5 if channelcode==41123 & (sector==9 | sector==1) // UNIDO
replace sector = 6 if channelcode==41301 & (sector==9 | sector==1) // FAO (Germany's forest survey more to do with environment...)
replace sector = 6 if channelcode==41108 & (sector==9 | sector==1) // IFAD
replace sector = 6 if channelcode==47063 & (sector==9 | sector==1) // International Livestock Research Institute
replace sector = 7 if channelcode==41146 & (sector==9 | sector==1) // UNWOMEN
replace sector = 8 if channelcode==47129 & (sector==9 | sector==1) // GEF
replace sector = 8 if channelcode==47130 & (sector==9 | sector==1) // GEF
replace sector = 8 if channelcode==47044 & (sector==9 | sector==1) // GEF
replace sector = 8 if channelcode==41317 & (sector==9 | sector==1) // Green Climate Fund
replace sector = 8 if channelcode==47067 & (sector==9 | sector==1) // IPCC
replace sector = 8 if channelcode==41116 & (sector==9 | sector==1) // UNEP
replace sector = 8 if channelcode==41316 & (sector==9 | sector==1) // UNFCCC

gen sector3 = sector // keeping track on how layers affect allocation

**********************
*** manual curation
**********************
replace sector = 2 if strpos(longdescription, "support for the population and housing census") // 
replace sector = 2 if strpos(projecttitle, "unfpa, census 2012")

**********************
*** new purpose codes
**********************
replace sector = 4 if purposecode==12196 // health statistics and data
replace sector = 2 if purposecode==13096 // population statistics and data


**************************************
*** other manipulation ***************
**************************************

* norway : SSB as channel donor government, not research institute
replace parentchannelcode = 11000 if parentchannelcode == 51000 & strpos(channelreportedname,"ssb -") & donorname=="Norway"
* norway : PCSB as channel recipient government, not research institute
replace parentchannelcode = 12000 if  parentchannelcode == 51000 & (strpos(projecttitle,"pcbs") | strpos(channelreportedname,"pcbs") ) & donorname=="Norway" // support to PCBS is public sector, donor


drop if purposecode == 91010 // Administrative costs (non-sector allocable)
drop if purposecode == 99820 // Promotion of development awareness (non-sector allocable)
drop if inrange(purposecode,60000,69999) // action relating to debt 

* keep only ODA
tab flowname source
drop if flowname=="Equity Investment"


save "${workdata}for_analysis_2022$test", replace 
save "V:\D4D\BACKUP\crs analysis\workdata\crs_filtered_2022", replace 

tab year source
exit 



