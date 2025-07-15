par(mfrow=c(3,2))
advanced_economies_dataset <- imf_clean[imf_clean$country %in% advanced_economies,]
advanced_economies_plot <- plot(advanced_economies_dataset$inflation_avg_cpi_pct, advanced_economies_dataset$unemployment_rate_pct,col = rgb(0.1, 0.1, 0.1, 0.2), main = "inflation vs unemployment advanced economies", xlab = "inflation", ylab = "unemployment", )
abline(lm(unemployment_rate_pct ~ inflation_avg_cpi_pct, data = advanced_economies_dataset), lty = 1, lwd = 2, col = "red")
abline(mean(advanced_economies_dataset$unemployment_rate_pct),0, col = "blue", alpha = 0.8)
abline(v=mean(advanced_economies_dataset$inflation_avg_cpi_pct))

emerging_europe_economies_dataset <- imf_clean[imf_clean$country %in% emerging_europe_economies,]
emerging_europe_economies_plot <- plot(emerging_europe_economies_dataset$inflation_avg_cpi_pct, emerging_europe_economies_dataset$unemployment_rate_pct,col = rgb(0.1, 0.1, 0.1, 0.2), main = "inflation vs unemployment emerging europe economies", xlab = "inflation", ylab = "unemployment")
abline(lm(unemployment_rate_pct ~ inflation_avg_cpi_pct, data = emerging_europe_economies_dataset), lty = 1, lwd = 2, col = "red")
abline(mean(emerging_europe_economies_dataset$unemployment_rate_pct),0, col = "blue", alpha = 0.8)
abline(v= mean(emerging_europe_economies_dataset$inflation_avg_cpi_pct))

latam_caribbean_economies_dataset <- imf_clean[imf_clean$country %in% latam_caribbean_economies,]
latam_caribbean_economies_plot <- plot(latam_caribbean_economies_dataset$inflation_avg_cpi_pct, latam_caribbean_economies_dataset$unemployment_rate_pct,col = rgb(0.1, 0.1, 0.1, 0.2), main = "inflation vs unemployment latam caribbean economies", xlab = "inflation", ylab = "unemployment")
abline(lm(unemployment_rate_pct ~ inflation_avg_cpi_pct, data = latam_caribbean_economies_dataset), lty = 1, lwd = 2, col = "red")
abline(mean(latam_caribbean_economies_dataset$unemployment_rate_pct),0, col = "blue", alpha = 0.8)
abline(v=mean(latam_caribbean_economies_dataset$inflation_avg_cpi_pct))

mena_central_asia_economies_dataset <- imf_clean[imf_clean$country %in% mena_central_asia_economies,]
mena_central_asia_economies_plot <- plot(mena_central_asia_economies_dataset$inflation_avg_cpi_pct, mena_central_asia_economies_dataset$unemployment_rate_pct,col = rgb(0.1, 0.1, 0.1, 0.2), main = "inflation vs unemployment mena central asia economies", xlab = "inflation", ylab = "unemployment")
abline(lm(unemployment_rate_pct ~ inflation_avg_cpi_pct, data = mena_central_asia_economies_dataset), lty = 1, lwd = 2, col = "red")
abline(mean(mena_central_asia_economies_dataset$unemployment_rate_pct),0, col = "blue", alpha = 0.8)
abline(v=mean(mena_central_asia_economies_dataset$inflation_avg_cpi_pct))

sub_saharan_africa_economies_dataset <- imf_clean[imf_clean$country %in% sub_saharan_africa_economies,]
sub_saharan_africa_economies_plot <- plot(sub_saharan_africa_economies_dataset$inflation_avg_cpi_pct, sub_saharan_africa_economies_dataset$unemployment_rate_pct,col = rgb(0.1, 0.1, 0.1, 0.2), main = "inflation vs unemployment sub saharan africa economies", xlab = "inflation", ylab = "unemployment")
abline(lm(unemployment_rate_pct ~ inflation_avg_cpi_pct, data = sub_saharan_africa_economies_dataset), lty = 1, lwd = 2, col = "red")
abline(mean(sub_saharan_africa_economies_dataset$unemployment_rate_pct),0, col = "blue", alpha = 0.8)
abline(v=mean(sub_saharan_africa_economies_dataset$inflation_avg_cpi_pct))

emerging_developing_asia_economies_dataset <- imf_clean[imf_clean$country %in% emerging_developing_asia_economies,]
emerging_developing_asia_economies_plot <- plot(emerging_developing_asia_economies_dataset$inflation_avg_cpi_pct, emerging_developing_asia_economies_dataset$unemployment_rate_pct,col = rgb(0.1, 0.1, 0.1, 0.2), main = "inflation vs unemployment emerging developing asia economies", xlab = "inflation", ylab = "unemployment")
abline(lm(unemployment_rate_pct ~ inflation_avg_cpi_pct, data = emerging_developing_asia_economies_dataset), lty = 1, lwd = 2, col = "red")
abline(mean(emerging_developing_asia_economies_dataset$unemployment_rate_pct),0, col = "blue", alpha = 0.8)
abline(v=mean(emerging_developing_asia_economies_dataset$inflation_avg_cpi_pct))
