### DO NOT RUN, it might take several minutes
### Contains every procedure followed.
library(tidyverse)
library(Benchmarking)
library(geobr)

# function to add spacial variables to the data later
geomcoord <- function(x, gc =1) {
   output <- c()
   for (indx in 1:length( x[["geom"]] )) {
      output[indx] <- mean({
         x[["geom"]][[indx]][[1]][[1]] %>%
            as.data.frame() %>%
            .[,gc]
      })
   }
   output
}

# dataset
var_names <- c(
   "l_prod_milk", "n_settlements", "a_pasture", "n_milk_cows_percent")
data <- readRDS("data/dataset.rds") %>% .[.$city!="Sairé", ] %>%
   mutate(n_milk_cows_percent ={(.$n_milk_cows / .$n_heads)*100}) %>%
   select(-c(
      "n_milk_cows", "n_rep_heads", "n_heads", "l_prod_milk/cow", "n_heads/a"
   ))

# matrices for modelling, input and output
mtx_X <- as.matrix(data[, var_names[-1] ])
rownames(mtx_X) <- data$city
mtx_Y <- as.matrix(data[, var_names[1] ])
rownames(mtx_Y) <- data$city

# adding spacial data to the dataset
data_wMap <- read_municipality(code_muni ="PE", year =2017) %>% 
   set_names(c("city_cod", "city", "state_cod", "abbrev_state", "geom")) %>%
   inner_join(., data, by ="city_cod") %>% mutate(city =city.y) %>%
   select(-c(city.y, city.x))

data <- cbind(
   longi =geomcoord(data_wMap),
   latit =geomcoord(data_wMap, gc =2),
   data
)

# matrix for modelling, spacial data
mtx_Z <- data$longi %>% cbind(longi =., latit =data$latit) %>%
   as.matrix()
rownames(mtx_Z) <- data$city

rm(geomcoord, var_names)

### Triyng possibilities to fit best the model
# 1. cleaning dataset
# 1.1. removing correlated variables

### TESTING HYPOTESES
results <- list()

results$conv_in <- FEAR::test.convexity(t(mtx_X), t(mtx_Y), 1)
results$conv_out <- FEAR::test.convexity(t(mtx_X), t(mtx_Y), 2)
results$conv_hyp <- FEAR::test.convexity(t(mtx_X), t(mtx_Y), 3)

results$rts_in <- FEAR::test.rts(t(mtx_X), t(mtx_Y), 1)
results$rts_out <- FEAR::test.rts(t(mtx_X), t(mtx_Y), 2)
results$rts_hyp <- FEAR::test.rts(t(mtx_X), t(mtx_Y), 3)

### ESTIMATING POSSIBLE MODELS
#detach("package:FEAR", unload =T)
library(Benchmarking)

# MODEL 1: only the number of settlements is non dicretionary
model <- list()
direc <- c(0, 1, 1)
model$fdh1 <- dea.direct(
   mtx_X, mtx_Y, c(direc,1), ORIENTATION ="in-out", RTS ="fdh")
# MODEL 2: same as MODEL 1 but, output is also non discretionary
model$fdh2 <- dea.direct(
   mtx_X, mtx_Y, c(direc,0), ORIENTATION ="in-out", RTS ="fdh")
# MODEL 3: Same as Model 1, but assuming convexity and variable returns to scale
model$vrs3 <- dea.direct(
   mtx_X, mtx_Y, c(direc,1), ORIENTATION ="in-out", RTS ="vrs", SLACK =T)
# MODEL 4: Same as Model 2, but assuming convexity and variable returns to scale
model$vrs4 <- dea.direct(
   mtx_X, mtx_Y, c(direc,0), ORIENTATION ="in-out", RTS ="vrs", SLACK =T)
# MODEL 4 WAS SELECTED
rm(direc)

### Mean efficiencies for inputs, model-wise:
#ordered by biggest milk producers
model$effs <- data.frame(
   fdh1 ={eff(model$fdh1) %>% rowMeans()},
   fdh2 ={eff(model$fdh2) %>% rowMeans()},
   vrs3 ={eff(model$vrs3) %>% rowMeans()},
   vrs4 ={eff(model$vrs4) %>% rowMeans()}
) %>% .[match(data$city[order(data$l_prod_milk, decreasing =T)], rownames(.)), ]

### Plots
# isoquants
plot_isoqaunts <- function(){
   par(mfrow =c(1,2))
   dea.plot.isoquant(
      x2 =mtx_X[,1], x1 =mtx_X[,2], RTS ="fdh",
      ylab ="n_settlements", xlab ="a_pasture"
   )
   dea.plot.isoquant(
      x2 =mtx_X[,1], x1 =mtx_X[,3], RTS ="fdh", 
      ylab ="", xlab ="n_milk_cows_percent")
}
# models
plot_models <- function(){
   par(mfrow =c(1,3))
   dea.plot(mtx_X, mtx_Y, ORIENTATION ="in-out", RTS ="fdh", main ="FDH Models")
   dea.plot(mtx_X, mtx_Y, ORIENTATION ="in-out", RTS ="vrs", main ="VRS Models")
   dea.plot(mtx_X, mtx_Y, ORIENTATION ="in-out", RTS ="irs", main ="IRS Models")
}
# efficiency (ordered by amount of milk produced)
plot_effs <- function() {
   par(mfrow =c(2,2))
   plot(
      1:70, model$effs$fdh1, type ="h",
      main ="Model 1", xlab ="", ylab ="Eff. score"
   )
   plot(
      1:70, model$effs$fdh2, type ="h",
      main ="Model 2", xlab ="", ylab =""
   )
   plot(
      1:70, model$effs$vrs3, type ="h",
      main ="Model 3", xlab ="Milk producer (decreasing)", ylab ="Eff. score"
   )
   plot(
      1:70, model$effs$vrs4, type ="h",
      main ="Model 4", xlab ="Milk producer (decreasing)", ylab =""
   )
}

# Bootstrap results
results$bstrap_default <- FEAR::boot.sw98(t(mtx_X), t(mtx_Y))

### Separability test
results$sep_dual <- FEAR::test.sep.cont(t(mtx_X), t(mtx_Y), t(mtx_Z))
results$sep_lon <- FEAR::test.sep.cont(t(mtx_X), t(mtx_Y), t( as.matrix(mtx_Z[,1]) ))
results$sep_lat <- FEAR::test.sep.cont(t(mtx_X), t(mtx_Y), t( as.matrix(mtx_Z[,2]) ))

### SAVING DATA
save.image("data/environment.RData")

