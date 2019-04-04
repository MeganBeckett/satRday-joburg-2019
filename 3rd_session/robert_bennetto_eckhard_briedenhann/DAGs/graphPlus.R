

# ref class for edge
state <- setRefClass("vertex", fields = list(id = "numeric", description = "character"), 
                     methods = list(
                       show = function(){
                          cat("State\nDescription:",description, "\nID:",id,"\n")
                       }
                     ))

transition <- setRefClass("edge", fields = list(from = "vertex", to = "vertex", weight = "numeric", description ="character"),
                          methods = list(
                            show = function(){
                              cat("Edge\nDescription:",description,"\nWeight:",weight, "\nFrom:\n")
                              from$show()
                              cat("To:\n")
                              to$show()
                            })
)

adjList <- setRefClass("adjList", fields = list(adjs = "list"),
                       methods = list(
                         addEdge = function(e){
                           if(!("edge" %in% class(e))){
                             stop("Cannot add non edge class to adjacency list")
                           }
                           vName <- paste0("v",e$from$id)
                           adjs[[vName]] <<- c(adjs[[vName]],e$to$id)
                           cat("Edge Added")
                         },
                         show = function(){
                           vNames <- names(adjs)
                           for(vN in vNames){
                            cat("Vertex:",vN,"\n")
                            cat("Adjs:",adjs[[vN]],"\n")
                           }
                         }
                       )
                       )

dag <- setRefClass("dag", fields = list( vertices = "list", edges = "list", adjList = "adjList", vertexCount = "numeric"),
                  methods = list(
                    newVertex = function(description){
                      v <- state(id = vertexCount, description = description )
                      vertexCount <<- vertexCount + 1
                      vertices <<- c(vertices,v)
                      return(v)
                    },
                    createTransition = function(from, to, weight, description){
                      
                      e <- transition(from = from, to = to, weight= weight, description = description)
                      edges <<- c(edges,e)
                      adjList$addEdge(e)
                      
                      return(e)
                    }
                  ))

g <- dag(vertexCount = 0)

awake <- g$newVertex(description = "Awake")
awake

dressedA <- g$newVertex(description = "DressedA")
dressedA

dressedB <- g$newVertex(description = "DressedB")
dressedB

getDressedA <- g$createTransition(from = awake,to = dressedA, weight = 1, description = "Getting pretty")
getDressedA

getDressedB <- g$createTransition(from = awake,to = dressedB, weight = 0.5, description = "Getting cool")
getDressedB

g$adjList



