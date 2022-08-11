
library(rpart)



model <- glm(direction~precip_tau+temp_tau+pop_tau+snow_tau, family="binomial", data=slope_data_analysis_LR)
options(scipen=999)
summary(model)
pscl::pR2(model)["McFadden"]
caret::varImp(model)
car::vif(model)




b <- slope_data_analysis %>%
     dplyr::do(model = randomForest::randomForest(formula = lsa_tau ~
                                                    precip_tau +
                                                    temp_tau +
                                                    pop_tau +
                                                    snow_tau,
                                                 data = ., na.action=na.roughfix)) %>%
    dplyr::collect() %>%
    dplyr::ungroup(.)

  e <- as.data.frame(b$model[[1]]$importance)
  e$predictor <- row.names(e)
  e$NSE = hydroGOF::NSE(b$model[[1]]$predicted, b$model[[1]]$y)

  e <- e %>% arrange(-IncNodePurity)

  global_rf <- e %>%
    mutate(predictor_new = case_when(
      predictor == "humidity_slope" ~ "Δ Humidity",
      predictor == "population_slope" ~ "Δ Population",
      predictor == "precip_slope" ~ "Δ Precipitation",
      predictor == "snow_slope" ~ "Δ Snowfall",
      predictor == "temp_slope" ~ "Δ Temperature",
      predictor == "elevation" ~ "Elevation (m)",
      predictor == "slope_100" ~ "Near-shore slope",
      predictor == "wshd_area" ~ "Watershed Area",
      predictor == "shore_dev" ~ "Shoreline Complexity",
      TRUE ~ NA_character_)) %>%
    mutate(IncNodePurity = round(IncNodePurity, digits = 0))


global_rf_figure <- ggplot(global_rf, aes(y = IncNodePurity, x = NA, group = predictor_new))+
                    geom_bar(aes(fill = predictor_new),stat = "identity")+
                    scale_fill_viridis(option = "C", na.value = "white",
                    direction = -1, discrete = T)+
                    geom_text(aes(label=IncNodePurity),color="black",size=5,
                              position=position_stack(vjust=0.5))
                    labs(title = "Global RF Node Purity")

fit <- rpart(area_slope ~
               cloud_slope +
               humidity_slope +
               population_slope +
               temp_slope +
               snow_slope +
               precip_slope +
               wshd_area +
               slope_100 +
               elevation +
               shore_dev,
               method = "anova", data = slope_data_analysis)


fit.pruned = prune(fit, cp = 0.0001)

plot(fit.pruned)
text(fit.pruned, cex = 0.9, xpd = TRUE)




png(file = "./output/figures/decTreeGFG.png", width = 600,
    height = 600)

# Plot
plot(fit, uniform = TRUE,
     main = "Lake area change
                 Tree using Regression")
text(fit, use.n = TRUE, cex = .7)

# Saving the file
dev.off()
