# particles (using MADT-UV) LOCAL

library(fields)     # for interp.surface.grid
library(ncdf)       # for ncdf
library(gsubfn)     # for strapply
library(shapefiles) # for shapefiles 

# temporal parameter(s)
# first turtle departs November
# last turtle departs January
# years 2008 2009

# OK
# sdate  <- as.Date("2008-11-01")   # start date for first particle release
# e1date <- as.Date("2009-01-31")   # final day of last particle release
# e2date <- as.Date("2010-01-31")   # final day of advection (1 year from last day of particle release)

# OK 
 sdate  <- as.Date("2009-11-01")   # start date for first particle release
 e1date <- as.Date("2010-01-31")   # final day of last particle release
 e2date <- as.Date("2011-01-31")   # final day of advection (1 year from last day of particle release)

r1 <- seq(sdate,e1date,1)       

# ptt deployment location (Gabon)
y <- -3.96  #(lat -3.96)
x <- 11.15  #(lon 11.15) 

# construct list of available uv current data

# uv <- list.files("c:\\Folders\\Projects\\Loggerheads\\global_merged_madt_uv\\",full.names=TRUE)
uv <- list.files("E:\\aviso\\data\\madt\\dt\\uv",full.names=TRUE)
t <- rep(NA,length(uv))
b <- strapply(uv, "\\d+",simplify = TRUE)
for (k in 1:dim(b)[2]){
	t[k] <- as.Date(b[1,k],"%Y%m%d")
}

# for each day load MADT-UV and extract relevant U/V components
for (j in 1:length(r1)){
#for (j in 1){
   
    clat <- y
    clon <- x
    r2 <- r1[j]:e2date
    
    lt <- rep(NA,length(r2))
    ln <- rep(NA,length(r2))
    ts <- rep(NA,length(r2))
    dt <- rep(NA,length(r2))
      
    t0 <- r2[1]-1
    k <- 1

    while (k <= length(r2)){
        
	  ix <- which.min(abs(t-r2[k]))
	  fn <- uv[ix]
	  d <- open.ncdf(fn)

	  lonall <- get.var.ncdf(d,"NbLongitudes") # call all lon data
	  latall <- get.var.ncdf(d,"NbLatitudes")  # call all lat data
	  px <- clon
	  py <- clat
	  pr <- 3

	  pxmin <- floor(px) - pr
	  pymax <- ceiling(py) + pr
	  pxmax <- ceiling(px) + pr
	  pymin <- floor(py) - pr

	  start  <- c((lat <- which.min(abs(pymin - latall))),(lon <- which.min(abs(pxmin - lonall))))
	  finish <- c((lat <- which.min(abs(pymax - latall))),(lon <- which.min(abs(pxmax - lonall))))
	  count  <- c(abs(start[1]-finish[1]),abs(start[2]-finish[2]))

	  lon <- get.var.ncdf(d,"NbLongitudes",start=start[2],count=count[2])
	  lat <- get.var.ncdf(d,"NbLatitudes",start=start[1],count=count[1]) 

	  u <- get.var.ncdf(d,"Grid_0001",start=start,count=count)
	  v <- get.var.ncdf(d,"Grid_0002",start=start,count=count) 

	  close.ncdf(d)
	  grid.list <- list(x=px, y=py)

	  u2 <- list(x=lon, y=lat, z=u)
	  u.intrp <- interp.surface.grid(u2, grid.list)
	  u_val <- u.intrp$z                                       # cm/s

	  v2 <- list(x=lon, y=lat, z=v)
	  v.intrp <- interp.surface.grid(v2, grid.list)
        v_val <- v.intrp$z                                       # cm/s

        ts[k] <- (t[ix] - t0) * 86400   
        dt[k] <- t[ix]
        t0    <- t[ix]
        
	  u_val <- u_val * ts[k]      
	  v_val <- v_val * ts[k]  

	  u_val <- (u_val/100000)     # km/day
 	  v_val <- (v_val/100000)     # km/day

	  rand1 <- runif(1, 0, 100)     # introduce a random term that increase/decreases the u and v values by up to 100%
	  rand2 <- runif(1, 0, 100)
	  u_val <- u_val + (u_val/100*rand1)      
        v_val <- v_val + (v_val/100*rand2) 

	  u_val <- u_val/111.1949267  # divide by a constant: /111.1949267 (km to degrees)
        v_val <- v_val/111.1949267  # divide by a constant: /111.1949267 (km to degrees)
 
        lt[k] <- clat + v_val 	# v is north-south (latitude)
        ln[k] <- clon + u_val 	# u is east-west (longitue)
        
        clat <- lt[k]
        clon <- ln[k]
        k <- k + 1;
    }
    
    #append release locations 
    lt <- c(y,lt)
    ln <- c(x,ln)
    dt <- c(r1[j],dt)

# correct longitude 0-360 to (0-180 East +ve) (0-180 West -ve)
	ln2 <- NULL
	for(i in seq(along=ln)) {
	if (ln[i]>180) { ln2 <- c(ln2,ln[i]-360)  } else { ln2 <- c(ln2,ln[i])  }
	}
	ln3 <- ln2

# construct output file name
	id <- r1[j]

# write points shapefile
	shpT <- data.frame(X=ln3, Y=lt) 
	Id <- seq(1,dim(shpT)[1],1)
	shpT2 <- data.frame(Id, X=ln3, Y=lt)
	attT <- data.frame(Id=seq(1,dim(shpT)[1],1), date=dt, lat=lt, lon=ln3)
	pointsShapefile <- convert.to.shapefile(shpT2, attT, "Id", 1)
	f = paste("C:\\Folders\\GIS\\GIS_data\\#Olive_ridley\\particle_drifter\\points\\2009\\",id,"_point",sep="")
	write.shapefile(pointsShapefile, f, arcgis=T) 

# write polyline shapefile
	Id2 <-  rep(1,dim(shpT)[1],1)
	shpT3 <- data.frame(Id2, X=ln3, Y=lt)
	attT2 <- data.frame(Id2=1,Name="Item1")
	polylineShapefile <- convert.to.shapefile(shpT3, attT2, "Id2", 3)
	f = paste("C:\\Folders\\GIS\\GIS_data\\#Olive_ridley\\particle_drifter\\polylines\\2009\\",id,"_line",sep="")	
	write.shapefile(polylineShapefile, f, arcgis=T)

}
