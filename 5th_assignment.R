### 5th Assignment - Directional Distance Function
# Vitor Ferreira Lins
# Encoding: ISO8859-1

library(dplyr)
library(Benchmarking)

readRDS("data/dataset.rds") %>% .[.$city != "Sairé", ] -> data

mtx_X <- as.matrix(data[, !{
   names(data) %in% c("city", "l_prod_milk", "l_prod_milk/cow", "n_heads/a")} ])
rownames(mtx_X) <- data$city
mtx_Y <- as.matrix(data[, {names(data) %in% c("l_prod_milk")} ])
rownames(mtx_Y) <- data$city

### Efficiency models (em)
model <- list()

# MODEL 1: input-oriented, where the number of settlements is non dicretionary
direc <- c(1, 1, 0, 1, 1)
model$fdh1 <- dea.direct(mtx_X, mtx_Y, c(direc,1), ORIENTATION ="in-out", RTS ="fdh", SLACK =T)
# MODEL 2: same as MODEL 1, but, input-output oriented, and output not discretionary
model$fdh2 <- dea.direct(mtx_X, mtx_Y, c(direc,0), ORIENTATION ="in-out", RTS ="fdh", SLACK =T)
# MODEL 3: Same as Model 1, but assuming convexity and variable returns to scale
model$vrs3 <- dea.direct(mtx_X, mtx_Y, c(direc,1), ORIENTATION ="in-out", RTS ="irs", SLACK =T)
# MODEL 4: Same as Model 2, but assuming convexity and variable returns to scale
model$vrs4 <- dea.direct(mtx_X, mtx_Y, c(direc,0), ORIENTATION ="in-out", RTS ="irs", SLACK =T)
rm(direc)

### Mean efficiencies for inputs, model-wise:
#ordered by biggest milk producers
model$effs <- data.frame(
   fdh1 ={eff(model$fdh1) %>% rowMeans()},
   fdh2 ={eff(model$fdh2) %>% rowMeans()},
   vrs3 ={eff(model$vrs3) %>% rowMeans()},
   vrs4 ={eff(model$vrs4) %>% rowMeans()}
) %>% .[match(data$city[order(data$l_prod_milk, decreasing =T)], rownames(.)), ]

model$in_slacks <- data.frame(
   vrs3 ={model$vrs3$sx %>% rowMeans()},
   vrs4 ={model$vrs4$sx %>% rowMeans()}
) 
row.names(model$in_slacks) <- data$city
model$in_slacks <- model$in_slacks[
   match(data$city[order(data$l_prod_milk, decreasing =T)], 
   rownames(model$in_slacks)), 
]

### Plots
# isoquants
plot_isoqaunts <- function(){
   par(mfrow =c(2,2))
   dea.plot.isoquant(
      x2 =mtx_X[,1], x1 =mtx_X[,2], RTS ="fdh", ylab ="n_heads", xlab ="n_milk_cows")
   dea.plot.isoquant(
      x2 =mtx_X[,1], x1 =mtx_X[,3], RTS ="fdh", ylab ="", xlab ="n_settlements")
   dea.plot.isoquant(
      x2 =mtx_X[,1], x1 =mtx_X[,4], RTS ="fdh", ylab ="n_heads", xlab ="n_rep_heads")
   dea.plot.isoquant(
      x2 =mtx_X[,1], x1 =mtx_X[,5], RTS ="fdh", ylab ="", xlab ="a_pasture")
}

# models
plot_models <- function(){
   par(mfrow =c(3,1))
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
      main ="Model 4", xlab ="Milk producer (decreasing)", ylab ="")
}

