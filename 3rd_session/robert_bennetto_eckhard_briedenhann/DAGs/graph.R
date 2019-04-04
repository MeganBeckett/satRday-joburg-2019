library(Rcpp)




# A -2-> B -3-> C


e1 <- list(From = "A", To= "B")
e2 <- list(From = "B", To= "C")
e3 <- list(From = "B", To= "D")

edgeList <- list(e1,e2,e3)

G <- list()


for( e in edgeList) {
  G[[e$From]] <- c(G[[e$From]],e$To) 
}


display <- function(G, start) {
  for (n in 1:length(G)){
    if(names(G[n]) == start){
      path <- start
      for(adj in G[n]){
       path <- paste(path," -> ", adj) 
      }
      cat(path,"\n")
    }
  }
}


display(G,"B")
