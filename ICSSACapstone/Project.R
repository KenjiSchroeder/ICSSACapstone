# UnemployedInflation
imf_data <- read.csv("IMF_WEO_imputed.csv")
# importing data
imf_clean <- subset(imf_data, imf_data$inflation_avg_cpi_pct >-0.2 & imf_data$inflation_avg_cpi_pct < 1)
# cleaning data


# putting all the countries we have data on into catagories for later comparison
advanced_economies <- c("Australia", "Austria", "Belgium", "Canada", "Cyprus", "Czechia",
"Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
"Hong Kong SAR, China", "Iceland", "Ireland", "Israel", "Italy",
"Japan", "Korea, Rep.", "Latvia", "Lithuania", "Luxembourg",
"Macao SAR, China", "Malta", "Netherlands", "New Zealand",
"Norway", "Portugal", "Puerto Rico", "San Marino", "Singapore",
"Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland",
"Taiwan, China", "United Kingdom", "United States")

emerging_europe_economies <- c("Albania", "Bosnia and Herzegovina", "Bulgaria", "Croatia",
"Hungary", "Kosovo", "Moldova", "Montenegro", "North Macedonia",
"Poland", "Romania", "Russian Federation", "Serbia", "Turkiye",
"Ukraine")

latam_caribbean_economies <- c("Antigua and Barbuda", "Argentina", "Aruba", "Bahamas, The",
"Barbados", "Belize", "Bolivia", "Brazil", "Chile", "Colombia",
"Costa Rica", "Dominica", "Dominican Republic", "Ecuador",
"El Salvador", "Grenada", "Guatemala", "Guyana", "Haiti",
"Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama",
"Paraguay", "Peru", "St. Kitts and Nevis", "St. Lucia",
"St. Vincent and the Grenadines", "Suriname", "Trinidad and Tobago",
"Uruguay", "Venezuela, RB")

mena_central_asia_economies <- c("Afghanistan", "Algeria", "Armenia", "Azerbaijan", "Bahrain",
"Djibouti", "Egypt, Arab Rep.", "Georgia", "Iran, Islamic Rep.",
"Iraq", "Jordan", "Kazakhstan", "Kuwait", "Kyrgyz Republic",
"Lebanon", "Libya", "Mauritania", "Morocco", "Oman", "Pakistan",
"Qatar", "Saudi Arabia", "Sudan", "Syrian Arab Republic",
"Tajikistan", "Tunisia", "Turkmenistan", "United Arab Emirates",
"Uzbekistan", "Yemen, Rep.")

sub_saharan_africa_economies <- c("Angola", "Benin", "Botswana", "Burkina Faso", "Burundi",
"Cabo Verde", "Cameroon", "Central African Republic",
"Chad", "Comoros", "Congo, Dem. Rep.", "Congo, Rep.",
"Cote d'Ivoire", "Equatorial Guinea", "Eswatini", "Ethiopia",
"Gabon", "Gambia, The", "Ghana", "Guinea", "Guinea-Bissau",
"Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali",
"Mauritius", "Mozambique", "Namibia", "Niger", "Nigeria",
"Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles",
"Sierra Leone", "Somalia", "South Africa", "South Sudan",
"Tanzania", "Togo", "Uganda", "Zambia", "Zimbabwe")

emerging_developing_asia_economies <- c("Bangladesh", "Bhutan", "Brunei Darussalam", "Cambodia",
"China", "Fiji", "India", "Indonesia", "Kiribati", "Lao PDR",
"Malaysia", "Maldives", "Marshall Islands",
"Micronesia, Fed. Sts.", "Mongolia", "Myanmar", "Nauru",
"Nepal", "Palau", "Papua New Guinea", "Philippines",
"Samoa", "Solomon Islands", "Sri Lanka", "Thailand",
"Timor-Leste", "Tonga", "Tuvalu", "Vanuatu", "Viet Nam")

imf_clean$region <- NA
# assigns regions based off the country lists
imf_clean$region[imf_clean$country %in% advanced_economies] <- "Advanced Economies"
imf_clean$region[imf_clean$country %in% emerging_europe_economies] <- "Emerging Europe"
imf_clean$region[imf_clean$country %in% latam_caribbean_economies] <- "Latin America & Caribbean"
imf_clean$region[imf_clean$country %in% mena_central_asia_economies] <- "MENA & Central Asia"
imf_clean$region[imf_clean$country %in% sub_saharan_africa_economies] <- "Sub-Saharan Africa"
imf_clean$region[imf_clean$country %in% emerging_developing_asia_economies] <- "Emerging Asia"

# removes rows that weren't classified:
imf_clean <- subset(imf_clean, !is.na(region))

dim(imf_clean)
str(imf_clean)
head(imf_clean)

par(mfrow = c(2, 4))

inflation_hist = hist(imf_clean$inflation_avg_cpi_pct,breaks=10,col = "red", main = "inflation hist")
unemployment_hist = hist(imf_clean$unemployment_rate_pct,breaks = 10, col = "grey",main = "unemployment hist")
gpd_hist = hist(imf_clean$gdp_growth_pct, breaks = 10, col = "green", main = "gpd hist")
govdebt_hist = hist(imf_clean$govt_gross_debt_pct_gdp, breaks = 10, col = "darkblue", main = "govenment debt hist")

inflation_bplt = boxplot(imf_clean$inflation_avg_cpi_pct,col = "red", main = "inflation boxplot")
unemployment_bplt = boxplot(imf_clean$unemployment_rate_pct,col = "grey", main = "unemployment boxplot")
gpd_bplt = boxplot(imf_clean$gdp_growth_pct,col = "green", main = "gpd boxplot")
govdebt_bplt = boxplot(imf_clean$govt_gross_debt_pct_gdp,col = "darkblue", main = "government debt boxplot")

imf_clean$ihs_inflation <- asinh(imf_clean$inflation_avg_cpi_pct)
imf_clean$ihs_unemployment <- asinh(imf_clean$unemployment_rate_pct)
imf_clean$ihs_debt <- asinh(imf_clean$govt_gross_debt_pct_gdp)
imf_clean$ihs_gdp_growth <- asinh(imf_clean$gdp_growth_pct)

par(mfrow = c(2,2))
inflation_hist_IHS = hist(imf_clean$ihs_inflation,breaks=10,col = "red", main = "inflation hist ihs")
unemployment_hist_IHS = hist(imf_clean$ihs_unemployment,breaks = 10, col = "grey",main = "unemployment hist ihs")
gpd_hist_IHS = hist(imf_clean$ihs_gdp_growth, breaks = 10, col = "green", main = "gpd hist ihs")
govdebt_hist_IHS = hist(imf_clean$ihs_debt, breaks = 10, col = "darkblue", main = "govenment debt hist ihs")

scatter_plot_inf_nojob <- plot(imf_clean$inflation_avg_cpi_pct, imf_clean$unemployment_rate_pct,col = rgb(0.1, 0.1, 0.1, 0.2), main = "inflation vs unemployment")


#scatterplots for all the regions comparing inflation to unemployment 
par(mfrow=c(2,3))
advanced_economies_dataset <- imf_clean[imf_clean$country %in% advanced_economies,]
advanced_economies_plot <- plot(advanced_economies_dataset$inflation_avg_cpi_pct, advanced_economies_dataset$unemployment_rate_pct,col = rgb(0.1, 0.1, 0.1, 0.2), main = "inflation vs unemployment advanced economies")

emerging_europe_economies_dataset <- imf_clean[imf_clean$country %in% emerging_europe_economies,]
emerging_europe_economies_plot <- plot(emerging_europe_economies_dataset$inflation_avg_cpi_pct, emerging_europe_economies_dataset$unemployment_rate_pct,col = rgb(0.1, 0.1, 0.1, 0.2), main = "inflation vs unemployment emerging europe economies")

latam_caribbean_economies_dataset <- imf_clean[imf_clean$country %in% latam_caribbean_economies,]
latam_caribbean_economies_plot <- plot(latam_caribbean_economies_dataset$inflation_avg_cpi_pct, latam_caribbean_economies_dataset$unemployment_rate_pct,col = rgb(0.1, 0.1, 0.1, 0.2), main = "inflation vs unemployment latam caribbean economies")

mena_central_asia_economies_dataset <- imf_clean[imf_clean$country %in% mena_central_asia_economies,]
mena_central_asia_economies_plot <- plot(mena_central_asia_economies_dataset$inflation_avg_cpi_pct, mena_central_asia_economies_dataset$unemployment_rate_pct,col = rgb(0.1, 0.1, 0.1, 0.2), main = "inflation vs unemployment mena central asia economies")

sub_saharan_africa_economies_dataset <- imf_clean[imf_clean$country %in% sub_saharan_africa_economies,]
sub_saharan_africa_economies_plot <- plot(sub_saharan_africa_economies_dataset$inflation_avg_cpi_pct, sub_saharan_africa_economies_dataset$unemployment_rate_pct,col = rgb(0.1, 0.1, 0.1, 0.2), main = "inflation vs unemployment sub saharan africa economies")

emerging_developing_asia_economies_dataset <- imf_clean[imf_clean$country %in% emerging_developing_asia_economies,]
emerging_developing_asia_economies_plot <- plot(emerging_developing_asia_economies_dataset$inflation_avg_cpi_pct, emerging_developing_asia_economies_dataset$unemployment_rate_pct,col = rgb(0.1, 0.1, 0.1, 0.2), main = "inflation vs unemployment emerging developing asia economies")

#coreletorrelation test
cor_test <- cor.test(imf_clean$inflation_avg_cpi_pct,imf_clean$unemployment_rate_pct)

#Anova test (no idea the function to put in here)
#aov_test <- summary(aov(unemployment_rate_pct ~ c(advanced_economies,emerging_europe_economies,latam_caribbean_economies,mena_central_asia_economies,sub_saharan_africa_economies,emerging_developing_asia_economies), imf_clean))