### 1st Assignment - Data and Descriptive Statistics
# Vitor Ferreira Lins
# Encoding: ISO8859-1

library(sidrar)
library(dplyr)
library(tidyr)

# Collecting dataset from SIDRA (IBGE)
dataset <- get_sidra(
  api =paste0("/t/6783/n6/2600500,2602803,2607505,2610806,2615805,2616001,2602209,2604908,",
  "2605400,2605806,2608107,2608909,2609105,2609709,2610509,2612109,2612703,2613800,2614501,",
  "2615003,2615409,2616209,2600609,2601706,2601904,2602605,2603108,2603801,2604106,2606408,",
  "2608008,2610905,2611200,2611705,2612406,2612505,2613008,2613107,2614709,2600302,2600807,",
  "2601003,2601300,2602100,2602308,2602407,2603207,2603306,2603504,2603702,2604700,2605004,",
  "2606002,2606507,2606705,2608305,2608404,2608602,2608701,2608800,2610103,2610202,2610301,",
  "2612000,2612307,2613206,2613305,2615102,2604155,2608255,2616183/v/2057,2073,2074,2326,9741,",
  "9742,9751,9771/p/all/c12625/41140/c220/110085/d/v2074%200,v9742%203,v9751%202,v9771%202")
) %>% select(c("Município", "Município (Código)", "Variável", "Valor")) %>% 
  setNames(c("city", "city_cod", "var", "value")) %>% 
  mutate(city_cod =as.double(city_cod)) %>%
  pivot_wider(id_cols =c(city, city_cod), names_from =var, values_from =value)

# Variables dictionary
dict <- names(dataset)[-c(1,2)]
names(dataset)[-c(1,2)] <- c(
  "n_heads", "n_milk_cows", "l_prod_milk", "n_settlements",
  "n_rep_heads", "a_pasture", "l_prod_milk/cow", "n_heads/a")
names(dict) <- names(dataset)[-c(1,2)]

# Cosmetic data correction
dataset$city <- gsub(" \\(PE\\)", "", dataset$city)

# Writing data down
if (!dir.exists("data")) {dir.create("data")}

write.csv(dataset, "data/dataset.csv")
saveRDS(dataset, "data/dataset.rds")
write.csv(dict, "data/dictionary.csv")
saveRDS(dict, "data/dictionary.rds")

