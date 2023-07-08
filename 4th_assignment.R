### 4th Assignment - Testing Assumptions and Seminal Models
# Vitor Ferreira Lins
# Encoding: ISO8859-1

#install.packages("C:/Users/vflin/Downloads/FEAR.zip", repos =NULL, type ="win.binary")
library(FEAR)
library(dplyr)

### Getting and tidying data
readRDS("data/dataset.rds") %>% .[.$city != "Sairé", ] -> data

mtx_X <- as.matrix(data[, !{
  names(data) %in% c("city", "l_prod_milk", "l_prod_milk/cow", "n_heads/a")
} ])
rownames(mtx_X) <- data$city
mtx_Y <- as.matrix(data[, {names(data) %in% c("l_prod_milk")} ])
rownames(mtx_Y) <- data$city

### Testing assumptions
# h_1: Variable Return to Scale; h_0: Constant
test.rts(t(mtx_X), t(mtx_Y), ORIENTATION =1, METRIC =1, NSPLIT =10, NREP =1000)
#8.563954 mins running with i7-4770 processor
#tau: 1.470837 0.580403 // pval: 0.001 0.007, avg =0.004

# h_1: Non-Covexity of the production; h_0: convexity
test.convexity(
  t(mtx_X), t(mtx_Y), ORIENTATION =1, 
  METRIC =1, NSPLIT =10, NREP =1000
)
#2.286952 mins running with i7-4770 processor
#tau: 1.3273498 0.4461835 // pval: 0.006 0.093, avg =0.0495

### Seminal models
detach("package:FEAR", unload =T)
library(Benchmarking)

par(mfrow =c(2,2))
dea_vrs <- dea(mtx_X, mtx_Y, RTS ="vrs", SLACK =T, DUAL =T)
dea.plot(mtx_X, mtx_Y, RTS ="vrs", SLACK =T, DUAL =T)

dea_crs <- dea(mtx_X, mtx_Y, RTS ="crs", SLACK =T, DUAL =T)
dea.plot(mtx_X, mtx_Y, RTS ="crs", SLACK =T, DUAL =T)

dea_fdh <- dea(mtx_X, mtx_Y, RTS ="fdh", SLACK =T, DUAL =T)
dea.plot(mtx_X, mtx_Y, RTS ="fdh", SLACK =T, DUAL =T)

results <- list(
  vrs =tibble(
    city =data$city,
    eff =eff(dea_vrs),
    improv_potential ={1 - eff(dea_vrs)},
    data.frame(peers(dea_vrs)),
    data.frame(lambda(dea_vrs)),
    {data.frame(dea_vrs$ux) %>%
      setNames(paste("u", colnames(mtx_X), sep ="_"))},
    v_l_prod_milk =dea_vrs$vy,
    {data.frame(dea_vrs$sx) %>%
      setNames(paste("s", colnames(mtx_X), sep ="_"))},
    s_l_prod_milk =dea_vrs$sy
  ),
  crs =tibble(
    city =data$city,
    eff =eff(dea_crs),
    improv_potential ={1 - eff(dea_crs)},
    data.frame(peers(dea_crs)),
    data.frame(lambda(dea_crs)),
    {data.frame(dea_crs$ux) %>%
      setNames(paste("u", colnames(mtx_X), sep ="_"))},
    v_l_prod_milk =dea_crs$vy,
    {data.frame(dea_crs$sx) %>%
      setNames(paste("s", colnames(mtx_X), sep ="_"))},
    s_l_prod_milk =dea_crs$sy
  ),
  fdh =tibble(
    city =data$city,
    eff =eff(dea_fdh),
    improv_potential ={1 - eff(dea_fdh)},
    data.frame(peers(dea_fdh)),
    data.frame(lambda(dea_fdh)),
    {data.frame(dea_fdh$ux) %>%
      setNames(paste("u", colnames(mtx_X), sep ="_"))},
    v_l_prod_milk =dea_fdh$vy,
    {data.frame(dea_fdh$sx) %>%
      setNames(paste("s", colnames(mtx_X), sep ="_"))},
    s_l_prod_milk =dea_fdh$sy
  )
)

if (!dir.exists("data")) {dir.create("data")}
write.csv(results$vrs, file ="data/results_vrs.csv")
write.csv(results$crs, file ="data/results_crs.csv")
write.csv(results$fdh, file ="data/results_fdh.csv")

# The set of models that best fits the necessity of the data are the vrs and fdh, 
# although the decreasing returns model (with vrs) are adequate, the non convexity
# hypotesis being accepted left me with FDH model as my choice
