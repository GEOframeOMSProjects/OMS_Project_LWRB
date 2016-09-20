	Regression<-function(path,model,novalue){
	# path: the path of the file cointaning the site specific measurements (RH: relative Humidity, T= Air Temperature, P = Precipitation and Elev= The Elevetation of the site)
	# model: the model must be chosen between the 10 simplified models tested in the paper
	# novalue= the no data value in the measurements
	# Xr, Yr, Zr: the model's parameters obtained with the regression proposed
	# a0..a15: Coefficients of the regression
	library(DAAG)
	mydata=read.table(path,sep=",",header=TRUE)
	mydata[mydata==novalue]=1
			
	# Angstrom (1918) 
	if(model==1){
		a0=3.148e+00
		a1=-3.495e-02 
		a2=-1.424e-01
		a3=-2.749e-03
		a4= 2.182e-03 
		a5=1.868e-03  
		a6=4.298e-05
		a7=2.498e-04
		a8=-3.170e-05  
		a9=-2.431e-04 
		a10=-7.105e-06
		a11=-3.255e-06 
		a12=4.935e-06 
		a13= 1.062e-07 
		a14=6.763e-07
		a15=-1.233e-08  
		
		Xr=a0+a1*mydata$RH+a2*mydata$T+a3*mydata$P+a4*mydata$Elev+a5*mydata$RH*mydata$T+a6*mydata$RH*mydata$P+a7*mydata$T*mydata$P+a8*mydata$RH*mydata$Elev+a9*mydata$T*mydata$Elev+a10*mydata$P*mydata$Elev+a11*mydata$RH*mydata$T*mydata$P+a12*mydata$RH*mydata$T*mydata$Elev+a13*mydata$RH*mydata$P*mydata$Elev+a14*mydata$T*mydata$P*mydata$Elev+a15*mydata$RH*mydata$T*mydata$P*mydata$Elev
		
		fitY <- lm(Y ~ RH * T * P * Elev, data=mydata)
		a0=6.758e-01
		a1= -7.518e-03 
		a2=-3.633e-02 
		a3=1.147e-03 
		a4=2.629e-03  
		a5=6.175e-04
		a6=-1.708e-05 
		a7=-1.706e-05
		a8= -3.666e-05
		a9=-1.511e-04
		a10=-6.112e-06
		a11=4.161e-07
		a12=2.365e-06 
		a13=9.341e-08 
		a14=3.622e-07
		a15=-6.173e-09 
		
		Yr=a0+a1*mydata$RH+a2*mydata$T+a3*mydata$P+a4*mydata$Elev+a5*mydata$RH*mydata$T+a6*mydata$RH*mydata$P+a7*mydata$T*mydata$P+a8*mydata$RH*mydata$Elev+a9*mydata$T*mydata$Elev+a10*mydata$P*mydata$Elev+a11*mydata$RH*mydata$T*mydata$P+a12*mydata$RH*mydata$T*mydata$Elev+a13*mydata$RH*mydata$P*mydata$Elev+a14*mydata$T*mydata$P*mydata$Elev+a15*mydata$RH*mydata$T*mydata$P*mydata$Elev
		
		
		a0= 6.556e+00 
		a1=-1.011e-01 
		a2=-3.760e-01
		a3=-7.618e-03
		a4=-7.428e-04 
		a5=4.869e-03
		a6=1.140e-04  
		a7= 4.843e-04
		a8=2.274e-05 
		a9=-1.671e-04
		a10= -6.610e-06 
		a11=-6.398e-06 
		a12=3.657e-06
		a13=7.788e-08 
		a14=8.552e-07 
		a15=-1.392e-08
		
		Zr=a0+a1*mydata$RH+a2*mydata$T+a3*mydata$P+a4*mydata$Elev+a5*mydata$RH*mydata$T+a6*mydata$RH*mydata$P+a7*mydata$T*mydata$P+a8*mydata$RH*mydata$Elev+a9*mydata$T*mydata$Elev+a10*mydata$P*mydata$Elev+a11*mydata$RH*mydata$T*mydata$P+a12*mydata$RH*mydata$T*mydata$Elev+a13*mydata$RH*mydata$P*mydata$Elev+a14*mydata$T*mydata$P*mydata$Elev+a15*mydata$RH*mydata$T*mydata$P*mydata$Elev
		print("Xr")
		print(Xr)
		print("Yr")
		print(Yr)
		print("Zr")
		print(Zr)
	}
	
	
   #Brunt’s (1932)
	if(model==2){
		a0=-4.487e-01 
		a1=1.841e-02 
		a2=7.536e-02 
		a3= 9.663e-04 
		a4= 4.041e-04
		a5=-1.192e-03 
		a6=-1.668e-05 
		a7=-1.517e-04
		a8=-5.309e-06
		a9=-1.404e-05
		a10=-9.272e-08 
		a11=2.151e-06
		a12=-2.150e-07
		a13=-1.463e-08 
		a14=1.701e-08
		a15=1.942e-09 
		
		Xr=a0+a1*mydata$RH+a2*mydata$T+a3*mydata$P+a4*mydata$Elev+a5*mydata$RH*mydata$T+a6*mydata$RH*mydata$P+a7*mydata$T*mydata$P+a8*mydata$RH*mydata$Elev+a9*mydata$T*mydata$Elev+a10*mydata$P*mydata$Elev+a11*mydata$RH*mydata$T*mydata$P+a12*mydata$RH*mydata$T*mydata$Elev+a13*mydata$RH*mydata$P*mydata$Elev+a14*mydata$T*mydata$P*mydata$Elev+a15*mydata$RH*mydata$T*mydata$P*mydata$Elev
		
		a0=-5.435e-01 
		a1=1.871e-03 
		a2= 4.862e-03 
		a3=4.830e-04 
		a4=-9.423e-04 
		a5= 5.490e-04 
		a6=2.487e-06 
		a7= 7.891e-05
		a8= 2.124e-05 
		a9= 1.102e-04
		a10= 2.705e-06
		a11=-1.709e-06 
		a12=-2.089e-06
		a13=-3.464e-08  
		a14=-2.972e-07  
		a15=3.292e-09 
		
		Yr=a0+a1*mydata$RH+a2*mydata$T+a3*mydata$P+a4*mydata$Elev+a5*mydata$RH*mydata$T+a6*mydata$RH*mydata$P+a7*mydata$T*mydata$P+a8*mydata$RH*mydata$Elev+a9*mydata$T*mydata$Elev+a10*mydata$P*mydata$Elev+a11*mydata$RH*mydata$T*mydata$P+a12*mydata$RH*mydata$T*mydata$Elev+a13*mydata$RH*mydata$P*mydata$Elev+a14*mydata$T*mydata$P*mydata$Elev+a15*mydata$RH*mydata$T*mydata$P*mydata$Elev
		print("Xr")
		print(Xr)
		print("Yr")
		print(Yr)
	}
	
	#Swinbank (1963)
	if(model==3){
		a0=-1.581e+01 
		a1=3.216e-01
		a2=1.158e+00  
		a3=2.569e-02  
		a4=3.066e-02 
		a5=-1.866e-02
		a6=-3.897e-04 
		a7=-1.599e-03 
		a8=-5.290e-04 
		a9=-2.182e-03
		a10=-5.422e-05
		a11=2.508e-05 
		a12=3.946e-05 
		a13=8.560e-07
		a14=4.218e-06
		a15=-6.826e-08  
		
		Xr=a0+a1*mydata$RH+a2*mydata$T+a3*mydata$P+a4*mydata$Elev+a5*mydata$RH*mydata$T+a6*mydata$RH*mydata$P+a7*mydata$T*mydata$P+a8*mydata$RH*mydata$Elev+a9*mydata$T*mydata$Elev+a10*mydata$P*mydata$Elev+a11*mydata$RH*mydata$T*mydata$P+a12*mydata$RH*mydata$T*mydata$Elev+a13*mydata$RH*mydata$P*mydata$Elev+a14*mydata$T*mydata$P*mydata$Elev+a15*mydata$RH*mydata$T*mydata$P*mydata$Elev
		print("Xr")
		print(Xr)
	}
	
	#Idso and Jackson [1969]
	if(model==4){
		a0=4.723e-01
		a1=1.815e-03 
		a2=-2.270e-03  
		a3= 1.256e-03 
		a4= 7.995e-04 
		a5=-2.488e-04 
		a6=-2.451e-05   
		a7= -8.155e-05
		a8=-1.415e-05 
		a9=-1.483e-05
		a10=-1.744e-06
		a11=1.539e-06
		a12=6.447e-08 
		a13= 3.052e-08  
		a14= 2.797e-08 
		a15=-1.311e-10  
		
		Xr=a0+a1*mydata$RH+a2*mydata$T+a3*mydata$P+a4*mydata$Elev+a5*mydata$RH*mydata$T+a6*mydata$RH*mydata$P+a7*mydata$T*mydata$P+a8*mydata$RH*mydata$Elev+a9*mydata$T*mydata$Elev+a10*mydata$P*mydata$Elev+a11*mydata$RH*mydata$T*mydata$P+a12*mydata$RH*mydata$T*mydata$Elev+a13*mydata$RH*mydata$P*mydata$Elev+a14*mydata$T*mydata$P*mydata$Elev+a15*mydata$RH*mydata$T*mydata$P*mydata$Elev
		
		a0=1.590e+02 
		a1=-2.639e+00
		a2= -9.184e+00
		a3=-2.552e-01
		a4=-3.095e-01
		a5= 1.506e-01 
		a6=3.824e-03 
		a7= 1.562e-02
		a8=5.253e-03
		a9= 2.147e-02 
		a10=5.807e-04 
		a11=-2.378e-04 
		a12= -3.797e-04 
		a13=-9.096e-06  
		a14= -4.311e-05  
		a15= 6.916e-07 
		
		Yr=a0+a1*mydata$RH+a2*mydata$T+a3*mydata$P+a4*mydata$Elev+a5*mydata$RH*mydata$T+a6*mydata$RH*mydata$P+a7*mydata$T*mydata$P+a8*mydata$RH*mydata$Elev+a9*mydata$T*mydata$Elev+a10*mydata$P*mydata$Elev+a11*mydata$RH*mydata$T*mydata$P+a12*mydata$RH*mydata$T*mydata$Elev+a13*mydata$RH*mydata$P*mydata$Elev+a14*mydata$T*mydata$P*mydata$Elev+a15*mydata$RH*mydata$T*mydata$P*mydata$Elev
		print("Xr")
		print(Xr)
		print("Yr")
		print(Yr)
	}
	
	#Brutsaert [1975]
	if(model==5){
		a0=3.396e+00 
		a1=-1.883e-02 
		a2=-1.284e-01  
		a3= -2.852e-03
		a4= -2.082e-03
		a5=1.752e-03 
		a6=2.891e-05    
		a7= 2.570e-04
		a8=1.103e-05  
		a9=2.295e-04 
		a10=7.613e-06
		a11=-3.114e-06
		a12= -2.693e-06  
		a13= -7.312e-08  
		a14= -7.329e-07
		a15=8.455e-09   
		
		Xr=a0+a1*mydata$RH+a2*mydata$T+a3*mydata$P+a4*mydata$Elev+a5*mydata$RH*mydata$T+a6*mydata$RH*mydata$P+a7*mydata$T*mydata$P+a8*mydata$RH*mydata$Elev+a9*mydata$T*mydata$Elev+a10*mydata$P*mydata$Elev+a11*mydata$RH*mydata$T*mydata$P+a12*mydata$RH*mydata$T*mydata$Elev+a13*mydata$RH*mydata$P*mydata$Elev+a14*mydata$T*mydata$P*mydata$Elev+a15*mydata$RH*mydata$T*mydata$P*mydata$Elev
		
		a0=7.838e+01  
		a1=-7.259e-01 
		a2=-2.428e+00 
		a3=-1.055e-01
		a4=1.238e-01 
		a5=9.914e-03
		a6= 1.185e-03
		a7= 3.647e-03
		a8=-1.790e-03
		a9= -9.677e-03
		a10=-2.865e-04  
		a11=-2.724e-05  
		a12=1.506e-04
		a13=3.713e-06  
		a14= 2.156e-05  
		a15= -2.881e-07  
		
		Yr=a0+a1*mydata$RH+a2*mydata$T+a3*mydata$P+a4*mydata$Elev+a5*mydata$RH*mydata$T+a6*mydata$RH*mydata$P+a7*mydata$T*mydata$P+a8*mydata$RH*mydata$Elev+a9*mydata$T*mydata$Elev+a10*mydata$P*mydata$Elev+a11*mydata$RH*mydata$T*mydata$P+a12*mydata$RH*mydata$T*mydata$Elev+a13*mydata$RH*mydata$P*mydata$Elev+a14*mydata$T*mydata$P*mydata$Elev+a15*mydata$RH*mydata$T*mydata$P*mydata$Elev
		print("Xr")
		print(Xr)
		print("Yr")
		print(Yr)
	}
	
	#Idso [1981]
	if(model==6){
		a0=1.305e+00  
		a1=-9.770e-03 
		a2=-7.087e-02 
		a3= -2.398e-03 
		a4= -2.462e-03
		a5=1.139e-03  
		a6=3.452e-05    
		a7=2.136e-04
		a8=2.849e-05  
		a9=1.625e-04 
		a10=5.740e-06
		a11=-3.047e-06
		a12= -1.761e-06
		a13= -6.611e-08   
		a14= -3.865e-07
		a15=3.846e-09  
		
		Xr=a0+a1*mydata$RH+a2*mydata$T+a3*mydata$P+a4*mydata$Elev+a5*mydata$RH*mydata$T+a6*mydata$RH*mydata$P+a7*mydata$T*mydata$P+a8*mydata$RH*mydata$Elev+a9*mydata$T*mydata$Elev+a10*mydata$P*mydata$Elev+a11*mydata$RH*mydata$T*mydata$P+a12*mydata$RH*mydata$T*mydata$Elev+a13*mydata$RH*mydata$P*mydata$Elev+a14*mydata$T*mydata$P*mydata$Elev+a15*mydata$RH*mydata$T*mydata$P*mydata$Elev
		
		a0=-4.461e+01 
		a1=5.704e-01 
		a2=4.356e+00 
		a3=1.088e-01 
		a4=8.830e-02  
		a5=-5.332e-02
		a6= -1.332e-03 
		a7= -9.213e-03
		a8=-9.265e-04 
		a9=-5.909e-03
		a10=-1.628e-04   
		a11=1.153e-04 
		a12=6.032e-05 
		a13= 1.640e-06  
		a14= 1.157e-05   
		a15= -1.067e-07  
		
		Yr=a0+a1*mydata$RH+a2*mydata$T+a3*mydata$P+a4*mydata$Elev+a5*mydata$RH*mydata$T+a6*mydata$RH*mydata$P+a7*mydata$T*mydata$P+a8*mydata$RH*mydata$Elev+a9*mydata$T*mydata$Elev+a10*mydata$P*mydata$Elev+a11*mydata$RH*mydata$T*mydata$P+a12*mydata$RH*mydata$T*mydata$Elev+a13*mydata$RH*mydata$P*mydata$Elev+a14*mydata$T*mydata$P*mydata$Elev+a15*mydata$RH*mydata$T*mydata$P*mydata$Elev
		print("Xr")
		print(Xr)
		print("Yr")
		print(Yr)
	}
	
	#Monteith and Unsworth [1990]
	if(model==7){
		a0=1.390e+03  
		a1=-2.576e+01 
		a2=-8.291e+01 
		a3=-1.981e+00 
		a4=-2.945e+00 
		a5=1.543e+00   
		a6=3.515e-02     
		a7=1.146e-01 
		a8= 5.131e-02   
		a9=1.848e-01
		a10= 5.066e-03
		a11=-2.143e-03 
		a12=  -3.256e-03
		a13=-8.120e-05   
		a14=-3.311e-04 
		a15=5.165e-06   
		
		Xr=a0+a1*mydata$RH+a2*mydata$T+a3*mydata$P+a4*mydata$Elev+a5*mydata$RH*mydata$T+a6*mydata$RH*mydata$P+a7*mydata$T*mydata$P+a8*mydata$RH*mydata$Elev+a9*mydata$T*mydata$Elev+a10*mydata$P*mydata$Elev+a11*mydata$RH*mydata$T*mydata$P+a12*mydata$RH*mydata$T*mydata$Elev+a13*mydata$RH*mydata$P*mydata$Elev+a14*mydata$T*mydata$P*mydata$Elev+a15*mydata$RH*mydata$T*mydata$P*mydata$Elev
		
		a0=-3.657e+00 
		a1= 7.708e-02 
		a2=2.512e-01
		a3=5.113e-03 
		a4=7.961e-03 
		a5=-4.428e-03 
		a6=-8.838e-05  
		a7=-2.921e-04
		a8=-1.423e-04 
		a9=-5.312e-04 
		a10=-1.333e-05   
		a11=5.368e-06
		a12= 9.782e-06 
		a13= 2.180e-07 
		a14= 9.518e-07    
		a15=-1.557e-08  
		
		Yr=a0+a1*mydata$RH+a2*mydata$T+a3*mydata$P+a4*mydata$Elev+a5*mydata$RH*mydata$T+a6*mydata$RH*mydata$P+a7*mydata$T*mydata$P+a8*mydata$RH*mydata$Elev+a9*mydata$T*mydata$Elev+a10*mydata$P*mydata$Elev+a11*mydata$RH*mydata$T*mydata$P+a12*mydata$RH*mydata$T*mydata$Elev+a13*mydata$RH*mydata$P*mydata$Elev+a14*mydata$T*mydata$P*mydata$Elev+a15*mydata$RH*mydata$T*mydata$P*mydata$Elev
		print("Xr")
		print(Xr)
		print("Yr")
		print(Yr)
	}
	
	
   #Konzelmann et al [1994]
	if(model==8){
		a0=1.75e+01 
		a1=-2.22e-01 
		a2=-9.27e-01
		a3=-3.10e-02 
		a4=-8.15e-03
		a5=1.08e-02 
		a6=4.12e-04     
		a7=1.86e-03  
		a8=8.63e-05 
		a9=1.07e-04  
		a10=1.18e-05  
		a11=-2.31e-05  
		a12=3.83e-06
		a13=-1.18e-07      
		a14=4.50e-08
		a15=-1.03e-08     
		
		Xr=a0+a1*mydata$RH+a2*mydata$T+a3*mydata$P+a4*mydata$Elev+a5*mydata$RH*mydata$T+a6*mydata$RH*mydata$P+a7*mydata$T*mydata$P+a8*mydata$RH*mydata$Elev+a9*mydata$T*mydata$Elev+a10*mydata$P*mydata$Elev+a11*mydata$RH*mydata$T*mydata$P+a12*mydata$RH*mydata$T*mydata$Elev+a13*mydata$RH*mydata$P*mydata$Elev+a14*mydata$T*mydata$P*mydata$Elev+a15*mydata$RH*mydata$T*mydata$P*mydata$Elev
		
		a0=-2.16e+01 
		a1=2.77e-01 
		a2=1.06e+00  
		a3=3.83e-02   
		a4=1.22e-02 
		a5=-1.02e-02  
		a6=-4.95e-04  
		a7=-2.06e-03 
		a8=-1.38e-04
		a9=-1.34e-04
		a10=-1.49e-05   
		a11=2.35e-05 
		a12=-5.26e-06 
		a13=1.62e-07
		a14=-2.67e-07   
		a15=1.50e-08 
		
		Yr=a0+a1*mydata$RH+a2*mydata$T+a3*mydata$P+a4*mydata$Elev+a5*mydata$RH*mydata$T+a6*mydata$RH*mydata$P+a7*mydata$T*mydata$P+a8*mydata$RH*mydata$Elev+a9*mydata$T*mydata$Elev+a10*mydata$P*mydata$Elev+a11*mydata$RH*mydata$T*mydata$P+a12*mydata$RH*mydata$T*mydata$Elev+a13*mydata$RH*mydata$P*mydata$Elev+a14*mydata$T*mydata$P*mydata$Elev+a15*mydata$RH*mydata$T*mydata$P*mydata$Elev
		print("Xr")
		print(Xr)
		print("Yr")
		print(Yr)
	}
	
	# Prata [1996] 

	if(model==9){
		a0=3.327e+01 
		a1=-4.042e-01 
		a2=-1.055e+00 
		a3=2.743e-04 
		a4= 1.192e-01  
		a5=3.678e-03 
		a6=-1.925e-05 
		a7=-6.504e-04
		a8=-1.765e-03 
		a9=-1.011e-02 
		a10=-3.159e-04 
		a11=1.841e-05  
		a12=1.712e-04
		a13= 4.476e-06 
		a14= 2.536e-05
		a15=-3.837e-07  
		
		Xr=a0+a1*mydata$RH+a2*mydata$T+a3*mydata$P+a4*mydata$Elev+a5*mydata$RH*mydata$T+a6*mydata$RH*mydata$P+a7*mydata$T*mydata$P+a8*mydata$RH*mydata$Elev+a9*mydata$T*mydata$Elev+a10*mydata$P*mydata$Elev+a11*mydata$RH*mydata$T*mydata$P+a12*mydata$RH*mydata$T*mydata$Elev+a13*mydata$RH*mydata$P*mydata$Elev+a14*mydata$T*mydata$P*mydata$Elev+a15*mydata$RH*mydata$T*mydata$P*mydata$Elev
		
	
		a0=1.622e+01 
		a1= -2.685e-01
		a2=6.013e-02  
		a3=1.785e-02 
		a4=1.472e-01  
		a5=-3.700e-04
		a6=-5.442e-05 
		a7=-2.082e-03 
		a8=-1.880e-03 
		a9=-1.204e-02
		a10=-3.886e-04
		a11= 2.465e-05
		a12= 1.834e-04 
		a13=5.014e-06 
		a14=3.054e-05 
		a15=-4.305e-07 
		
		Yr=a0+a1*mydata$RH+a2*mydata$T+a3*mydata$P+a4*mydata$Elev+a5*mydata$RH*mydata$T+a6*mydata$RH*mydata$P+a7*mydata$T*mydata$P+a8*mydata$RH*mydata$Elev+a9*mydata$T*mydata$Elev+a10*mydata$P*mydata$Elev+a11*mydata$RH*mydata$T*mydata$P+a12*mydata$RH*mydata$T*mydata$Elev+a13*mydata$RH*mydata$P*mydata$Elev+a14*mydata$T*mydata$P*mydata$Elev+a15*mydata$RH*mydata$T*mydata$P*mydata$Elev
		
		
		a0= -3.693e+00
		a1= 7.643e-02
		a2=2.662e-01
		a3= 5.635e-03 
		a4=1.717e-02
		a5= -4.928e-03 
		a6=-9.164e-05 
		a7=-2.629e-04
		a8=-2.621e-04 
		a9= -9.776e-04 
		a10= -2.942e-05
		a11=4.916e-06 
		a12= 1.587e-05 
		a13=4.226e-07 
		a14=1.551e-06  
		a15=-2.393e-08 
		
		Zr=a0+a1*mydata$RH+a2*mydata$T+a3*mydata$P+a4*mydata$Elev+a5*mydata$RH*mydata$T+a6*mydata$RH*mydata$P+a7*mydata$T*mydata$P+a8*mydata$RH*mydata$Elev+a9*mydata$T*mydata$Elev+a10*mydata$P*mydata$Elev+a11*mydata$RH*mydata$T*mydata$P+a12*mydata$RH*mydata$T*mydata$Elev+a13*mydata$RH*mydata$P*mydata$Elev+a14*mydata$T*mydata$P*mydata$Elev+a15*mydata$RH*mydata$T*mydata$P*mydata$Elev
		print("Xr")
		print(Xr)
		print("Yr")
		print(Yr)
		print("Zr")
		print(Zr)
	}	
	
	
 #Dilley and O'brien [1998]
	if(model==10){
		a0=-2.954e+02 
		a1=3.570e+00 
		a2=2.421e+01
		a3=-4.465e-02  
		a4=2.974e-02 
		a5=-3.585e-01
		a6= 3.552e-03 
		a7=-1.166e-02 
		a8=1.905e-03
		a9=-2.691e-02 
		a10=-2.332e-04
		a11=8.395e-05 
		a12=4.548e-04
		a13=-2.277e-06 
		a14=7.625e-05
		a15=-1.061e-06
		
		Xr=a0+a1*mydata$RH+a2*mydata$T+a3*mydata$P+a4*mydata$Elev+a5*mydata$RH*mydata$T+a6*mydata$RH*mydata$P+a7*mydata$T*mydata$P+a8*mydata$RH*mydata$Elev+a9*mydata$T*mydata$Elev+a10*mydata$P*mydata$Elev+a11*mydata$RH*mydata$T*mydata$P+a12*mydata$RH*mydata$T*mydata$Elev+a13*mydata$RH*mydata$P*mydata$Elev+a14*mydata$T*mydata$P*mydata$Elev+a15*mydata$RH*mydata$T*mydata$P*mydata$Elev
		
		
		a0=1.580e+03 
		a1= -1.497e+01
		a2=-8.536e+01  
		a3=-1.731e+00 
		a4=1.276e-01  
		a5=7.876e-01 
		a6= 1.677e-02
		a7=1.372e-01
		a8=-1.416e-02
		a9=-3.255e-02  
		a10=-5.663e-04 
		a11= -1.359e-03
		a12= 1.538e-03 
		a13=2.780e-05 
		a14=6.494e-05 
		a15=-2.630e-06   
		
		Yr=a0+a1*mydata$RH+a2*mydata$T+a3*mydata$P+a4*mydata$Elev+a5*mydata$RH*mydata$T+a6*mydata$RH*mydata$P+a7*mydata$T*mydata$P+a8*mydata$RH*mydata$Elev+a9*mydata$T*mydata$Elev+a10*mydata$P*mydata$Elev+a11*mydata$RH*mydata$T*mydata$P+a12*mydata$RH*mydata$T*mydata$Elev+a13*mydata$RH*mydata$P*mydata$Elev+a14*mydata$T*mydata$P*mydata$Elev+a15*mydata$RH*mydata$T*mydata$P*mydata$Elev
		
		
		a0=-2.109e+03
		a1=2.284e+01
		a2=1.247e+02 
		a3= 2.708e+00  
		a4=4.325e-01
		a5=-1.083e+00 
		a6=-2.760e-02 
		a7=-1.947e-01 
		a8=1.044e-02 
		a9= 1.505e-02 
		a10= -1.182e-04
		a11=1.910e-03
		a12= -1.722e-03
		a13= -2.366e-05 
		a14=-4.540e-05 
		a15=2.907e-06 
		
		Zr=a0+a1*mydata$RH+a2*mydata$T+a3*mydata$P+a4*mydata$Elev+a5*mydata$RH*mydata$T+a6*mydata$RH*mydata$P+a7*mydata$T*mydata$P+a8*mydata$RH*mydata$Elev+a9*mydata$T*mydata$Elev+a10*mydata$P*mydata$Elev+a11*mydata$RH*mydata$T*mydata$P+a12*mydata$RH*mydata$T*mydata$Elev+a13*mydata$RH*mydata$P*mydata$Elev+a14*mydata$T*mydata$P*mydata$Elev+a15*mydata$RH*mydata$T*mydata$P*mydata$Elev
		print("Xr")
		print(Xr)
		print("Yr")
		print(Yr)
		print("Zr")
		print(Zr)
	}
	
}