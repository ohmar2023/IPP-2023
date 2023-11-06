



distr <- function(v,n,db)
{
  r <- rep(0,length(v))
  
  repeat {

    k = ceiling(n/length(v[v!=10000]))
    #print(k)
    if(sum(v-k<0)==0)
    {
      r[v!=10000] <- k
      db <- cbind(db,"Unif.Mod" = r)
      return(db)
      break
    }
    else{
    r[v-k<=0] <- v[v-k<=0]
    n = n - abs(sum((v-k)[v-k<0]))
    v[!v-k>=0] <- 10000
    }
    
  }

}


# USANDO FUNCION PARA DISTRIBUCION ----------------------------------------

dom <- aux %>% select(dom_m)
dom <- unique(dom$dom_m) 
r <- NULL
for(i in c(1:length(dom)))
{
  #i=2
  db <- aux %>% filter(dom_m==dom[i])
  v <- (db$Nh)
  n <- db$n4[1]
  r1 <- distr(v,n,db)
  r <- rbind(r,r1)
}
r %>% adorn_totals() %>% View()
