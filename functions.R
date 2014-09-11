
n <- NA
k <- 0.8
step <- 4
epsilon <- 0.1
method <- "monoH.FC"
MASplineVector <- function(x, n , k, step, epsilon, method = "monoH.FC", 
                           n0 = n, n1 = n, k0 = k, k1 = k) {                                
  # x vektoriaus "pratempimas" pirmyn ir atgal taikant MA ("vidus" turi buti be "skyliu")
  # Args:
  #   x: vektorius, kurio pradzioje ar/ir pabaigoje esantys NA keiciami 
  #      naujomis reiksmemis taikant MA (paskutine reikme + pokyciu vidurkis * koeficientas)
  #   n: paskutiniu pokyciu, kuriems imamas vidurkis, skaicius (n1 - pirmyn, n0 - atgal)
  #      jei nenurodyta, turimu stebejimu skaicius N dalinamas pusiau ir apvalinamas
  #   k: koeficientas, is kurio dauginams pokyciu vidurkis, 
  #      pries pridedant ji prie paskutines zinomos reiksmes (k1 - pirmyn, k0 - atgal)
  #   step: kelintame zingsnyje jau turi buti palaipsniui pasiektas MA koeficientas, NA tas pats kas 0
  #   epsilon: NA - tiesiog spline&MA, ne NA - per koki epsilon padidintu originaliu reziu negali perzengti MA rezultatas
  #   method : metodas sline'ams paduodamas funkcijai splinefun
  # Returns:
  #   vectorius x, su pratemptomis taikant MA reiksmemis pradzioje ir pabaigoje vektoriaus
  #   jei x nera daugiau zinomu reiksmiu nei n, grazina nepakeista x
  
  # require(TTR)
  
  ### spline 
  # atmetami galuose NA
  xtest=-1
  xf=x
  
  while(any(xtest<0,na.rm=T)){
    x=xf
    #   print(epsilon)
    x.length <- length(x)
    x.notna <- which(!is.na(x))
    x.first <- min(x.notna)
    x.last <- max(x.notna)
    x <- x[c(x.first : x.last)]  
    time <- c(x.first : x.last)
    # uzpildomos trukstamos reiksmes splainu pagalba   
    s <- splinefun(time, x, method = method)
    x <- s(time)
    x <- c(rep(NA, x.first-1), x, rep(NA, x.length-x.last))
    
    
    ### set k0, n0, k1, n1
    sk.pok <- max(which(!is.na(x)))-min(which(!is.na(x)))
    n.def <- sk.pok   # ceiling(sk.pok/2) 
    k.def <- 1
    # jei koef NA, nustatomi pagal nutylejima
    n1 <- ifelse(is.na(n1), n.def, n1)
    n0 <- ifelse(is.na(n0), n.def, n0)
    k1 <- ifelse(is.na(k1), k.def, k1)
    k0 <- ifelse(is.na(k0), k.def, k0)
    # jei 'n' reiksmes virsija turimu pokyciu skaiciu, imamas max
    n1 <- ifelse(n1 > sk.pok, sk.pok, n1)
    n0 <- ifelse(n0 > sk.pok, sk.pok, n0)
    
    x.notna.spl <- which(!is.na(x))
    
    ### MA
    if (length(x.notna.spl) > n1) {
      j <- x.last # pozicija su paskutiniu zinomu
      
      # MA pratempimas pirmyn
      while (j < x.length) {
        x_aug <- (c(x, 0) - c(0, x))[c(1:length(x))] # augimai pameciui (pirmyn)
        y <- x_aug[!(is.na(x_aug))] 
        # koregavimas koeficiento del palaipsnio perejimo
        if (is.na(step) | step == 0 | step == 1){
          k11 <- k1
        } else {
          step.no <- j - x.last + 1
          if (step.no >= step) {
            k11 <- k1
          } else {
            k11 <- k1 + (1 - k1) * (step - step.no) / step
          }
        }
        # MA vienu zingsniu i prieki
        x[j+1] <- ifelse(n1==1, x[j]+y[1]*k11, x[j]+SMA(y, n1)[length(y)]*k11)  
        j <- j+1
      }
    }    
    
    if (length(x.notna.spl) > n0) { 
      j <- x.first # pozicija su pirmu zinomu
      
      # MA pratempimas atgal
      while (j > 1) {
        x_aug <- (c(x, 0) - c(0, x))[c(1:length(x))] 
        x_maz <- c(-x_aug[2:length(x_aug)], NA) # mazejimai pameciui (atgal)
        y <- x_maz[!(is.na(x_maz))] 
        # koregavimas koeficiento del palaipsnio perejimo
        if (is.na(step) | step == 0 | step == 1){
          k00 <- k0
        } else {
          step.no <- x.first - j + 1
          if (step.no >= step) {
            k00 <- k0
          } else {
            k00 <- k0 + (1 - k0) * (step - step.no) / step
          }
        }
        x[j-1] <- ifelse(n0==1, x[j]+y[1]*k00, x[j]+SMA(y, n0)[n0]*k00) # MA vienu zingsniu atgal
        j <- j-1
      }
    }
    
    
    ### restriction: new values in range 
    ### [min.orig - epsilon * origgap, max.orig + epsilon * origgap]
    if (!is.na(epsilon)){
      origlo.bound <- min(x[x.notna])
      origup.bound <- max(x[x.notna])
      origgap <- origup.bound - origlo.bound
      
      lo.bound <- origlo.bound - epsilon * origgap
      up.bound <- origup.bound + epsilon * origgap
      
      xna <- setdiff(c(1:length(x)), x.notna)
      xna.forward <- xna[xna > x.last]
      xna.backward <- xna[xna < x.first]
      # if out of bounds MA forward
      if (any(x[xna.forward] > up.bound)){
        x[xna.forward] <- NA
        x[length(x)] <- up.bound
      } else {
        if (any(x[xna.forward] < lo.bound)){
          x[xna.forward] <- NA
          x[length(x)] <- lo.bound
        }
      }
      # if out of bounds MA backward
      if (any(x[xna.backward] > up.bound)){
        x[xna.backward] <- NA
        x[1] <- up.bound
      } else {
        if (any(x[xna.backward] < lo.bound)){
          x[xna.backward] <- NA
          x[1] <- lo.bound
        }
      }
      
      # spline if needed
      if (any(is.na(x))){
        time <- c(1:length(x))
        # uzpildomos trukstamos reiksmes splainu pagalba   
        s <- splinefun(time, x, method = method)
        x <- s(time)
      }
    }
    
    epsilon=epsilon*0.7
    if(epsilon<0.01) break  
    
    xtest=x
  }
  
  return(x) # grazinamas papildytas x vektorius (arba pirminis, jei n per didelis) 
}