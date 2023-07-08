######################################################
# Dataset treatment, modelling, tests, and bootstrap
# NOT recommended to run, might take several minutes
######################################################

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

### Plots to be used later
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

######################################################
# Tables and Visualisations used in the article
# Will produce several outputs
######################################################

library(tmap)
library(tidyverse)
library(Benchmarking)
library(knitr)
library(readxl)
opts_chunk$set(echo = T)

# loading database, models and functions
load('data/environment.RData')
# loading personal ggplot2 styles
source('style/my_ggtheme.R')

# Table 1
temp <- read_xlsx("data/2nd_Assignment_table-short.xlsx")
kable(temp[,-length(temp)], longtable =T)
rm(temp)

# Table 2
var_names <- names(data[, -c(1:4)])
fqrt <- function(x) { quantile(x, prob =.25) }
tqrt <- function(x) { quantile(x, prob =.75) }
temp <- tibble(
   Variables =var_names,
   Mean =sapply(data[,var_names], mean),
   SD =sapply(data[,var_names], sd),
   `1st Quartile` =sapply(data[,var_names], fqrt),
   Median =sapply(data[,var_names], median),
   `3rd Quartile` =sapply(data[,var_names], tqrt)
)
kable(temp, digits =2, caption ="Source: IBGE, own elaboration")
rm(fqrt, tqrt, temp, var_names)

# Table 3
l_milk_total <- sum(data$l_prod_milk)
temp <- data[
   order(data$l_prod_milk, decreasing =T), c("city", "l_prod_milk")
] %>%
   mutate(percent ={(.$l_prod_milk/l_milk_total) *100}) %>%
   mutate(percent =paste0(round(percent,2), "%")) %>%
   setNames(c("City", "Milk Produced (liters)", "Total milk production")) %>%
   mutate(`Milk Produced (liters)` =as.character(`Milk Produced (liters)`))

rbind(
   temp[1:6,],
   tibble(
      City ="...", `Milk Produced (liters)` ="...", 
      `Total milk production` ="..."
   ), 
   temp[64:70,]
) %>% kable()

rm(l_milk_total, temp)

# Table 4
var_names <- names(data[, -c(1:4)])
cor(data[,var_names]) %>% kable()
rm(var_names)

# Table 5
temp <- peers(model$vrs4) %>% as.vector() %>% na.omit()

tibble(
   City =data$city[unique(temp)],
   Peered =summary(as.factor(temp)),
   `Milk Produced (liters)` =data$l_prod_milk[unique(temp)]
) %>% .[order(.$Peered, decreasing =T), ] %>% kable()
rm(temp)

# Table 6
dhat <- 1/results$bstrap_default$dhat.bc
slc <- order(data$l_prod_milk, decreasing =T)

tibble(
   Test =c(
      "Shapiro-Wilk",
      "Mann-Whitney (Major and Lesser 50%)",
      "Mann-Whitney (Major and Lesser 20%)"
   ),
   `P-Value` = c(
      shapiro.test(dhat)$p.value,
      wilcox.test(dhat[slc[1:35]], dhat[slc[36:70]], exact =F)$p.value,
      wilcox.test(dhat[slc[1:14]], dhat[slc[57:70]], exact =F)$p.value
   ),
   `Null hypothesis` =c(
      "Normality", "True location shift is not zero",
      "True location shift is not zero"
   )
) %>% kable()

rm(dhat, slc)

# Custom ggplot theme
custom_theme <- function() {
   theme_bw() +
      theme(
         text =element_text(size =14, family ="serif"),
         legend.position ="bottom",
         axis.text.x = element_text(angle = 45, vjust =1, hjust =1))
}

# Figure 1
var_names <- c( "city", names(data[, -c(1:4)]) )
temp <- data[ , var_names] %>%
   mutate(
      a_pasture =a_pasture/100,
      l_prod_milk =l_prod_milk/100,
      n_settlements =n_settlements/10
   )

pivot_longer(temp, cols =-city) %>% 
   ggplot(., aes(y =value, x =name, fill =name)) + geom_boxplot() +
   custom_theme() + scale_fill_brewer(palette ="Blues") +
   theme(legend.position ="none") + coord_flip() +
   labs(x ="", y ="", fill ="Variable", caption =
           '* "a_pasture", "l_prod_milk" and "n_settlements" in 100 \n Source: IBGE, own elaboration') +
   
   rm(var_names)

# Figure 2
tm_shape(data_wMap) +
   tm_polygons("l_prod_milk", palette ="Blues", style = "log10_pretty") +
   tm_text('city', remove.overlap =T, size =.83, col ="black")

# Figure 3
plot_effs()

# Figure 4
slc <- order(data$l_prod_milk, decreasing =T)[c(1:6, 65:70)]

tibble(
   inf =results$bstrap_default$conf.int[,1],
   sup =results$bstrap_default$conf.int[,2]
) %>% mutate(
   dhat =results$bstrap_default$dhat.bc,
   bmp ={data$l_prod_milk >200},
   city =data$city
) -> temp

ggplot(temp[slc,], aes(x =city, color =bmp)) + 
   geom_point(aes(y ={1/dhat}), size =4) +
   geom_errorbar(ymin ={1/temp$inf[slc]}, ymax ={1/temp$sup[slc]}, size =1.2) +
   labs(x ='', y ='Estimated efficiency scores', color = 'Big milk producer') +
   expand_limits(y =c(0.25,1)) + custom_theme() + coord_flip()
rm(slc)

# Figure 5
dhat <- results$bstrap_default$dhat.bc
slc <- order(data$l_prod_milk, decreasing =T)
temp <- tibble(dhat =1/dhat[slc],bprd ={data$l_prod_milk[slc] > 1406})

ggplot(temp, aes(x =dhat, fill =bprd)) + custom_theme() +
   geom_density(alpha =0.3) +
   geom_vline(
      xintercept =c(mean(temp$dhat[1:35]), mean(temp$dhat[36:70])),
      color =c("royalblue", "orchid"), size =.8, alpha =.75) +
   labs(x ="", y ="", fill ="")+
   scale_fill_manual(
      values =c("royalblue", "orchid"),
      labels =c("Lesser 50%", "Major 50%"))

ggplot(temp[c(1:14, 57:70), ], aes(x =dhat, fill =bprd)) + custom_theme() +
   geom_density(alpha =0.3) +
   geom_vline(
      xintercept =c(mean(temp$dhat[1:14]), mean(temp$dhat[57:70])),
      color =c("royalblue", "orchid"), size =.8, alpha =.75) +
   labs(x ="", y ="", fill ="")+
   scale_fill_manual(
      values =c("royalblue", "orchid"),
      labels =c("Lesser 20%", "Major 20%"))

rm(dhat, slc, temp)