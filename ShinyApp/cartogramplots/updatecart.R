#Updates jpeg files of cartogram plots
#creates plots for years 2000 to 2017

for(x in 2000:2017){
  jpeg(paste('ShinyApp/cartogramplots/cart', x, '.jpg', sep = ''))
  print(ffcartogram(x))
  dev.off()
}
