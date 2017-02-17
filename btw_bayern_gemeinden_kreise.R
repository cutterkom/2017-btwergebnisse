# scraper für die resultate der bundestagswahlen 2013 in bayern auf gemeinde- und kreisebene
#http://www.wahlen.bayern.de/bundestagswahlen/


library(dplyr)
library(rvest)
library(purrr)

clean_election_results_table_bavaria_2013 <- function(dataframe) {
  
  variable_names_first <- c("code", "gemeinde", "wahlberechtigte", "waehler", "art_der_stimme", "ungueltige_stimmen", "ungueltige_stimmen_pct", "gueltige_stimmen", "csu_absolut", "csu", "spd_absolut", "spd", "gruene_absolut", "gruene", "linke_absolut", "linke", "sonstige_absolut", "sonstige")
  variable_names_second <- c("code", "gemeinde", "nichtwaehler", "wahlbeteiligung_pct", "art_der_stimme",  "ungueltige_stimmen", "ungueltige_stimmen_pct", "gueltige_stimmen", "csu_absolut", "csu", "spd_absolut", "spd", "gruene_absolut", "gruene", "linke_absolut", "linke", "sonstige_absolut", "sonstige")
  
  
  # create df with every second row (starting at 1)
  # two df's: 1) results, 2) general information
  d_first <- dataframe %>% group_by(X1) %>% filter(row_number()==1) %>% ungroup()
  names(d_first) <- variable_names_first
  
  d_first_general_information <- d_first %>% select(code, wahlberechtigte, waehler)
  d_first <- d_first %>% select(-wahlberechtigte, -waehler)
  
  # create df with every second row (starting at 2)
  # two df's: 1) results, 2) general information
  d_second <- dataframe %>% group_by(X1) %>% filter(row_number()==2)%>% ungroup()
  names(d_second) <- variable_names_second
  d_second_general_information <- d_second %>% select(code, nichtwaehler, wahlbeteiligung_pct)
  d_second <- d_second %>% select(-nichtwaehler, -wahlbeteiligung_pct)
  
  # bind general information together
  d_general_information <- left_join(d_first_general_information, d_second_general_information, by = "code")
  
  # bind results together
  d_first_second <- rbind(d_first, d_second)
  
  # all data for one letter
  df <- left_join(d_general_information, d_first_second, by ="code")
  
  # clean
  make_numeric <- c("wahlbeteiligung_pct", "ungueltige_stimmen_pct", "csu", "spd", "gruene", "linke", "sonstige")
  df <- df %>% map_at(make_numeric, gsub, pattern = ",", replacement = ".")
  df <- df %>% map_at(make_numeric, as.numeric)
  df <- as.data.frame(df)
  
}


################
# Gemeinden
################
d_gemeinden <- "http://www.wahlen.bayern.de/bw2013/bw2013_gue.php?g=a&t=1&r=1&suchbegriff=az" %>% read_html() %>% html_table(fill=TRUE)
d_gemeinden <- d_gemeinden[3] %>% as.data.frame() %>% tail(-3)
d_gemeinden <- clean_election_results_table_bavaria_2013(d_gemeinden)
d_gemeinden$jahr <- 2013
d_gemeinden$region <- "gemeinde"
write.table(d_gemeinden, "data/btw_bayern_gemeinden_2013.csv", row.names = F, quote = F, sep=";")

################
# Kreise
################
d_kreise <- "http://www.wahlen.bayern.de/bw2013/bw2013_gue.php?g=r&t=1&r=2" %>% read_html() %>% html_table(fill=TRUE)
d_kreise <- d_kreise[2] %>% as.data.frame() %>% tail(-3)
d_kreise <- clean_election_results_table_bavaria_2013(d_kreise)
d_kreise$jahr <- 2013
d_kreise$region <- "kreis"
write.table(d_gemeinden, "data/btw_bayern_kreise_2013.csv", row.names = F, quote = F, sep=";")
