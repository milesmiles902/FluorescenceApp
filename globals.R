library(hyperSpec)


setClass("Spectra", contains = "hyperSpec")

#Load Data from comma delimited file
load <- function(file = file.choose(), x = "time") {
  dat <- read.table(textConnection(gsub(",", "\t", readLines(file))))
  
  if(x == "time"){
    xaxis <- as.numeric(dat[-1, 1])
    yaxis <- as.vector(t(dat[1, -1]))
  } else if(x == "wave") {
    xaxis <- as.numeric(dat[1, -1])
    yaxis <- as.vector(t(dat[-1, 1]))
  } else{
    return (print("Error in the court!"))
  }
  
  dm <- as.matrix(dat[-1, -1])
  
  if(is.unsorted(xaxis, na.rm = TRUE, strictly = TRUE)){
    xaxis <- rev(xaxis)
    dm <- dm[,ncol(dm):1]
  }
  
  if(is.unsorted(yaxis, na.rm = TRUE, strictly = TRUE)){
    yaxis <- rev(yaxis)
    dm <- dm[nrow(dm):1,]
  }
  
  spec.data <- new ("Spectra", 
                    data = data.frame(
                      x = xaxis
                    ),
                    spc = dm,
                    wavelength = yaxis,
                    label = list(
                      x = "Time (ps)",
                      spc = "Intensity",
                      wavelength = "Wavelength (nm)"
                    )                
  )
  return(spec.data)
}

#Time units to index.
time2i = function (spectrum, x = stop ("You need a time point!")){
  chk.hy (spectrum)
  validObject (spectrum)
  
  ## special in formula
  max = max (spectrum$x)
  min = min (spectrum$x)
  
  envir = attr (spectrum, ".Environment")
  
  `~` = function (e1, e2){
    if (missing (e2))              # happens with formula ( ~ end)
      stop ("Time must be a both-sided formula")
    
    if ((Re (e1) < min (spectrum@x) && Re (e2) < min (spectrum@x)) ||
        (Re (e1) > max (spectrum@x) && Re (e2) > max (spectrum@x))){
      NULL                       # Time points completely outside the wl. range of x
    } else {
      e1 = .getindex (x, Re (e1)) + Im (e1)
      e2 = .getindex (x, Re (e2)) + Im (e2)
      
      if (e1 <= 0 || e2 <= 0|| e1 > length (spectrum@x) || e2 > length (spectrum@x))
        warning ("time2i: formula yields indices outside the object.")
      
      seq (e1, e2)
    }
  }
  
  .conv.range = function (range){
    if (is.numeric (range)){
      .getindex (spectrum, range, extrapolate = FALSE)
    } else
      eval (range)
  }
  
  if (is.list (x)) {
    unlist (lapply (x, .conv.range))
  } else {
    .conv.range (x)
  }
}

#Helper function for time2i
.getindex = function (spectrum, x, extrapolate = TRUE){
  if (! extrapolate) {
    x[x < min (spectrum$x)] = NA
    x[x > max (spectrum$x)] = NA
  }
  tmp = x [! is.na (x)]
  if (length (tmp) > 0) {
    tmp = sapply (tmp,
                  function (x, y) which.min (abs (x  - y)),
                  spectrum$x)
    x[! is.na (x)] = tmp
  }
  x
}

#Brush function to find indices for both waves and time
brush <- function(df, min, max, wave = TRUE){
  if(wave){
    l = wl2i(df, min):wl2i(df, max)
    return (df[l = l, wl.index = TRUE])
  } else{
    t = time2i(df, min):time2i(df, max)
    return (df[t])
  }
}

