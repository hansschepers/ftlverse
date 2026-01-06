grad <- NULL
grad[1] <- rgb(170/255, 225/255, 200/255)   #"#AAE1C8"
grad[2] <- rgb(255/255, 255/255, 180/255)   #"#FFFFB4"
grad[3] <- rgb(255/255, 185/255, 170/255)   #"#FFB9AA"
ramp <- colorRamp(grad)   # c("#d0EEb0", "#FFEE30", "#EEb0b0")  green yellow, red
RYG <- rgb( ramp(seq(0, 1, length = 17)), maxColorValue = 255)
