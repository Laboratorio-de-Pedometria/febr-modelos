# Install required packages
if (!require("partykit")) {
  install.packages("partykit", dependencies = TRUE)
}

# Download data from FEBR
febr_data <- data.table::fread("~/ownCloud/febr-repo/publico/febr-superconjunto.txt", dec = ",")
colnames(febr_data)

# Calculate sampling depth
febr_data[["profund"]] <- rowMeans(febr_data[, c("profund_sup", "profund_inf")])

# Filter NAs in 'dsi'
dsi_isna <- !is.na(febr_data[["dsi"]])
dsi_data <- febr_data[dsi_isna, ]
dim(dsi_data)
hist(dsi_data[["dsi"]], xlab = expression("Densidade, g cm"^-3), ylab = "Frequência", main = "")

# Análise exploratória
pedometrics::plotCor(
  round(cor(dsi_data[, c("argila", "areia", "carbono", "silte", "profund", "terrafina")],
    use = "complete", method = "spearman"), 2)
)

# Specify categorical variables
colnames(dsi_data)
dsi_data[["estado_id"]] <- as.factor(dsi_data[["estado_id"]])
barplot(sort(table(dsi_data[["estado_id"]]), decreasing = TRUE),
  xlab = "Unidade da federação", ylab = "Frequência")

# Estimate model
# dsi_formula <- dsi ~ argila + areia + terrafina + carbono + ctc + ph | estado_id + profund
# dsi_formula <- dsi ~ argila + areia + carbono + I(argila * carbono) + I(argila * silte) | profund + terrafina
dsi_formula <- dsi ~ argila + silte + carbono + log1p(carbono) + I(argila * carbono) | profund + terrafina + areia
t0 <- proc.time()
dsi_model <- partykit::glmtree(formula = dsi_formula, data = dsi_data,
  family = gaussian(link = "identity"), verbose = TRUE, restart = TRUE, alpha = 0.10,
  cores = 3, prune = "AIC")
proc.time() - t0
print(dsi_model)
plot(dsi_model)
summary(dsi_model)
round(sqrt(mean(residuals(dsi_model)^2)), 2)
round(1 - (sum(residuals(dsi_model)^2) / sum((dsi_model$data[["dsi"]] - mean(dsi_model$data[["dsi"]]))^2)), 2)

dev.off()
png("ptf-density/res/fig/mob-fit-left-branch.png", width = 480 * 2, height = 480 * 2, res = 72)
plot(dsi_model[[2]])
dev.off()

dev.off()
png("ptf-density/res/fig/mob-fit-right-branch.png", width = 480 * 2, height = 480 * 2, res = 72)
plot(dsi_model[[23]])
dev.off()

# Export regression model
save(dsi_model, file = "ptf-density/res/model/dsi-model.Rda")

dsi_prediction <- predict(dsi_model, newdata = febr_data[!dsi_isna, ])
idx <- dsi_prediction > min(dsi_data[["dsi"]], na.rm = TRUE) & 
  dsi_prediction < max(dsi_data[["dsi"]], na.rm = TRUE)
hist(dsi_prediction[idx])
length(dsi_prediction[idx])
