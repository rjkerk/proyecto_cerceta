library(terra)
library(dplyr)
library(tidyverse)
library(readr)
library(raster)
library(zyp)
library(ggplot2)
library(data.table)
#Cargar las capas#
parcelas <- vect("outputs/01_parcelas_filtradas.shp")
parcelas.df <- as.data.frame(parcelas)
parcelas.df <- mutate(parcelas.df, IDrow = row_number())
MNDWIlist <- list.files(path = "data/GEE/MNDWI_anuales", pattern = ".tif$", all.files = FALSE, full.names = TRUE)
MNDWIrast <- lapply(MNDWIlist, rast)
class <- function(MNDWIrast) {
  classify(MNDWIrast, rbind(c(-1, 0.07, 1),  c(0.07, 0.335, 2), c(0.335, 0.659, 3), c(0.659, 1, 4)))
}
MNDWIreclass <- lapply(MNDWIrast, class)
MNDWIextract <- function(MNDWIreclass) {
  extract(MNDWIreclass, parcelas, fun="table", na.rm=TRUE, exact=FALSE)
}
MNDWIparcelas <- lapply(MNDWIreclass, MNDWIextract)
MNDWIpframe <- lapply(MNDWIparcelas, as.data.frame)
colnames <- c("IDrow", "seco", "vegetal", "verde", "azul")
MNDWIfinal <- MNDWIpframe |> 
  lapply(setNames, colnames) |> 
  lapply(name.merge) |> 
  lapply(function(x) x[names(x) %in% c("seco", "vegetal", "verde", "azul", "parcela")])
name.merge <- function(MNDWIpframe) {
  merge(MNDWIpframe, parcelas.df, by = "IDrow")
}

#Mean Area de Estudio
MNDWIstack <- sds(MNDWIrast)
MNDWImean <- app(MNDWIstack, mean)
MNDWImeanparcelas <- extract(MNDWImean, parcelas, fun = "table", na.rm="TRUE")
stack.class <- classify(MNDWImean, rbind(c(-1, 0.07, 1),  c(0.07, 0.335, 2), c(0.335, 0.659, 3), c(0.659, 1, 4)))
plot(stack.class)
MNDWIstack.df <- as.data.frame(stack.class)

#Mean Parcelas
MNDWIextract2 <- function(MNDWIrast) {
  extract(MNDWIrast, parcelas, fun="mean", na.rm=TRUE, exact=FALSE)
}
#MNDWIparcelas2 <- lapply(MNDWIrast, MNDWIextract2)
#MNDWIdframe <- lapply(MNDWIparcelas2, as.data.frame)

MNDWIdf <- lapply(MNDWIrast, as.data.frame)
MNDWIbind <- rbindlist(MNDWIdf)
f <- MNDWIextract2(MNDWIbind)

e <- lapply(MNDWIrast, MNDWIextract2)
colnames2 <- c("IDrow", "MNDWImean")
MNDWImeanfinal <- e |> 
  lapply(setNames, colnames2) |> 
  lapply(name.merge) |> 
  lapply(as.data.frame)
names(MNDWImeanfinal) <- c("2005", 
                "2006",
                "2007", 
                "2008",
                "2009",
                "2010",
                "2012",
                "2013",
                "2014",
                "2015",
                "2016",
                "2017",
                "2018",
                "2019",
                "2020",
                "2021",
                "2022",
                "2023",
                "2024",
                "2025")
lapply(MNDWImeanfinal, function(x) write.table(data.frame(x), 'MNDWImeanparcelas.csv', append= T, sep=',' ))

MNDWImediaporparcela <- as.data.frame(read.csv("MNDWImeanparcelas.csv"))
MNDWImediaporparcela <- MNDWImediaporparcela[-c(69:1379),]

MNDWIparcela.list <- setNames(split(MNDWImediaporparcela, seq(nrow(MNDWImediaporparcela))), rownames(MNDWImediaporparcela$Parcela))
MNDWIparcelnumeric <- lapply(MNDWIparcela.list, as.numeric)
MKparcelas <- lapply(MNDWIparcelnumeric, MannKendall)

#Seco
dfseco <- data.frame(id = MNDWIfinal[[1]][[5]], 
                     y2005 = MNDWIfinal[[1]][[1]], 
                     y2006 = MNDWIfinal[[2]][[1]],
                     y2007 = MNDWIfinal[[3]][[1]],
                     y2008 = MNDWIfinal[[4]][[1]],
                     y2009 = MNDWIfinal[[5]][[1]],
                     y2010 = MNDWIfinal[[6]][[1]],
                     y2012 = MNDWIfinal[[7]][[1]],
                     y2013 = MNDWIfinal[[8]][[1]],
                     y2014 = MNDWIfinal[[9]][[1]],
                     y2015 = MNDWIfinal[[10]][[1]],
                     y2016 = MNDWIfinal[[11]][[1]],
                     y2017 = MNDWIfinal[[12]][[1]],
                     y2018 = MNDWIfinal[[13]][[1]],
                     y2019 = MNDWIfinal[[14]][[1]],
                     y2020 = MNDWIfinal[[15]][[1]],
                     y2021 = MNDWIfinal[[16]][[1]],
                     y2022 = MNDWIfinal[[17]][[1]],
                     y2023 = MNDWIfinal[[18]][[1]],
                     y2024 = MNDWIfinal[[19]][[1]],
                     y2025 = MNDWIfinal[[20]][[1]])
                     
dfseco <- dfseco |> 
  rowwise() |> 
  mutate(max = max(c_across(y2005:y2025))) |> 
  ungroup()

seco.total <- dfseco |> 
  bind_rows(
    summarise(dfseco, 
              id = "Total",
              across(where(is.numeric), sum)
    )
  )

dfseco.ratio <- seco.total |> 
  mutate(r2005 = y2005/max,
         r2006 = y2006/max,
         r2007 = y2007/max,
         r2008 = y2008/max,
         r2009 = y2009/max,
         r2010 = y2010/max,
         r2012 = y2012/max,
         r2013 = y2013/max,
         r2014 = y2014/max,
         r2015 = y2015/max,
         r2016 = y2016/max,
         r2017 = y2017/max,
         r2018 = y2018/max,
         r2019 = y2019/max,
         r2020 = y2020/max,
         r2021 = y2021/max,
         r2022 = y2022/max,
         r2023 = y2023/max,
         r2024 = y2024/max,
         r2025 = y2025/max)

dfseco.ratio <- select(-(y2005:y2025))

seco.total.vect <- as.numeric(seco.total[69,2:21])
seco.total.hect <- seco.total.vect * 900 * 0.0001
years <- c(2005:2010, 2012:2025)

seco.MannKendall <- MannKendall(seco.total.hect)
print(seco.MannKendall)

seco.tau_result <- cor.test(seco.total.hect, years, method = "kendall")
print(seco.tau_result)
seco.trend_line <- predict(loess(seco.total.hect ~ years))

ggplot() +
  geom_point(aes(x = years, y = seco.total.hect), color = "blue") +
  geom_smooth(aes(x = years, y = seco.trend_line), method = lm, color = "red") +
  labs(x = "Year", y = "Área Seca (ha)", title = "Tendencia de Tierra Seca en Área de Estudio") +
  theme_minimal()

#Vegetal
dfvegetal <- data.frame(id = MNDWIfinal[[1]][[5]], 
                     y2005 = MNDWIfinal[[1]][[2]], 
                     y2006 = MNDWIfinal[[2]][[2]],
                     y2007 = MNDWIfinal[[3]][[2]],
                     y2008 = MNDWIfinal[[4]][[2]],
                     y2009 = MNDWIfinal[[5]][[2]],
                     y2010 = MNDWIfinal[[6]][[2]],
                     y2012 = MNDWIfinal[[7]][[2]],
                     y2013 = MNDWIfinal[[8]][[2]],
                     y2014 = MNDWIfinal[[9]][[2]],
                     y2015 = MNDWIfinal[[10]][[2]],
                     y2016 = MNDWIfinal[[11]][[2]],
                     y2017 = MNDWIfinal[[12]][[2]],
                     y2018 = MNDWIfinal[[13]][[2]],
                     y2019 = MNDWIfinal[[14]][[2]],
                     y2020 = MNDWIfinal[[15]][[2]],
                     y2021 = MNDWIfinal[[16]][[2]],
                     y2022 = MNDWIfinal[[17]][[2]],
                     y2023 = MNDWIfinal[[18]][[2]],
                     y2024 = MNDWIfinal[[19]][[2]],
                     y2025 = MNDWIfinal[[20]][[2]])
dfvegetal <- dfvegetal |> 
  rowwise() |> 
  mutate(max = max(c_across(y2005:y2025))) |> 
  ungroup()

vegetal.total <- dfvegetal |> 
  bind_rows(
    summarise(dfvegetal, 
              id = "Total",
              across(where(is.numeric), sum)
    )
  )

vegetal.total.vect <- as.numeric(vegetal.total[69,2:21])
vegetal.total.hect <- vegetal.total.vect * 900 * 0.0001
years <- c(2005:2010, 2012:2025)

vegetal.MannKendall <- MannKendall(vegetal.total.hect)
print(vegetal.MannKendall)

vegetal.tau_result <- cor.test(vegetal.total.hect, years, method = "kendall")
print(vegetal.tau_result)
vegetal.trend_line <- predict(loess(vegetal.total.hect ~ years))

ggplot() +
  geom_point(aes(x = years, y = vegetal.total.hect), color = "blue") +
  geom_smooth(aes(x = years, y = vegetal.trend_line), method = lm, color = "red") +
  labs(x = "Year", y = "Area Vegetal (ha)", title = "Tendencia de Tierra Vegetal en Área de Estudio") +
  theme_minimal()

#Verde
dfverde <- data.frame(id = MNDWIfinal[[1]][[5]], 
                     y2005 = MNDWIfinal[[1]][[3]], 
                     y2006 = MNDWIfinal[[2]][[3]],
                     y2007 = MNDWIfinal[[3]][[3]],
                     y2008 = MNDWIfinal[[4]][[3]],
                     y2009 = MNDWIfinal[[5]][[3]],
                     y2010 = MNDWIfinal[[6]][[3]],
                     y2012 = MNDWIfinal[[7]][[3]],
                     y2013 = MNDWIfinal[[8]][[3]],
                     y2014 = MNDWIfinal[[9]][[3]],
                     y2015 = MNDWIfinal[[10]][[3]],
                     y2016 = MNDWIfinal[[11]][[3]],
                     y2017 = MNDWIfinal[[12]][[3]],
                     y2018 = MNDWIfinal[[13]][[3]],
                     y2019 = MNDWIfinal[[14]][[3]],
                     y2020 = MNDWIfinal[[15]][[3]],
                     y2021 = MNDWIfinal[[16]][[3]],
                     y2022 = MNDWIfinal[[17]][[3]],
                     y2023 = MNDWIfinal[[18]][[3]],
                     y2024 = MNDWIfinal[[19]][[3]],
                     y2025 = MNDWIfinal[[20]][[3]])

dfverde <- dfverde |> 
  rowwise() |> 
  mutate(max = max(c_across(y2005:y2025))) |> 
  ungroup()

verde.total <- dfverde |> 
  bind_rows(
    summarise(dfverde, 
              id = "Total",
              across(where(is.numeric), sum)
    )
  )

verde.total.vect <- as.numeric(verde.total[69,2:21])
verde.total.hect <- verde.total.vect * 900 * 0.0001
years <- c(2005:2010, 2012:2025)

verde.MannKendall <- MannKendall(verde.total.hect)
print(verde.MannKendall)

verde.tau_result <- cor.test(verde.total.hect, years, method = "kendall")
print(verde.tau_result)
verde.trend_line <- predict(loess(verde.total.hect ~ years))

ggplot() +
  geom_point(aes(x = years, y = verde.total.hect), color = "blue") +
  geom_smooth(aes(x = years, y = verde.trend_line), method = lm, color = "red") +
  labs(x = "Year", y = "Agua Verde (ha)", title = "Tendencia de Agua Verde en Área de Estudio") +
  theme_minimal()

#Azul
dfazul <- data.frame(id = MNDWIfinal[[1]][[5]], 
                     y2005 = MNDWIfinal[[1]][[4]], 
                     y2006 = MNDWIfinal[[2]][[4]],
                     y2007 = MNDWIfinal[[3]][[4]],
                     y2008 = MNDWIfinal[[4]][[4]],
                     y2009 = MNDWIfinal[[5]][[4]],
                     y2010 = MNDWIfinal[[6]][[4]],
                     y2012 = MNDWIfinal[[7]][[4]],
                     y2013 = MNDWIfinal[[8]][[4]],
                     y2014 = MNDWIfinal[[9]][[4]],
                     y2015 = MNDWIfinal[[10]][[4]],
                     y2016 = MNDWIfinal[[11]][[4]],
                     y2017 = MNDWIfinal[[12]][[4]],
                     y2018 = MNDWIfinal[[13]][[4]],
                     y2019 = MNDWIfinal[[14]][[4]],
                     y2020 = MNDWIfinal[[15]][[4]],
                     y2021 = MNDWIfinal[[16]][[4]],
                     y2022 = MNDWIfinal[[17]][[4]],
                     y2023 = MNDWIfinal[[18]][[4]],
                     y2024 = MNDWIfinal[[19]][[4]],
                     y2025 = MNDWIfinal[[20]][[4]])
dfazul <- dfazul |> 
  rowwise() |> 
  mutate(max = max(c_across(y2005:y2025))) |> 
  ungroup()

azul.total <- dfazul |> 
  rowwise() |> 
  bind_rows(
    summarise(dfazul, 
              id = "Total",
              across(where(is.numeric), sum)
    )
  )

azul.total.vect <- as.numeric(azul.total[69,2:21])
azul.total.hect <- azul.total.vect * 900 * 0.0001
years <- c(2005:2010, 2012:2025)

azul.MannKendall <- MannKendall(azul.total.hect)
print(azul.MannKendall)

azul.tau_result <- cor.test(azul.total.hect, years, method = "kendall")
print(azul.tau_result)
azul.trend_line <- predict(loess(azul.total.hect ~ years))

ggplot() +
  geom_point(aes(x = years, y = azul.total.hect), color = "blue") +
  geom_smooth(aes(x = years, y = azul.trend_line), method = lm, color = "red") +
  labs(x = "Year", y = "Agua Azul (ha)", title = "Tendencia de Agua Azul en Área de Estudio") +
  theme_minimal()

write_csv(dfseco, "data\\MNDWI_parcelas\\seco.csv")
write_csv(dfvegetal, "data\\MNDWI_parcelas\\vegetal.csv")
write_csv(dfverde, "data\\MNDWI_parcelas\\verde.csv")
write_csv(dfazul, "data\\MNDWI_parcelas\\azul.csv")

