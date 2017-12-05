BaseAnalysis <- R6Class("BaseAnalysis",
  #define variables
  public = list(
    #basic definitions
    participant = NULL,
    dataDirectory = NULL,
    tests = NULL,
    initialize = function(dir, id){
      private$dataDirectory(dir,id)
      self$SetParticipant(id)
      #TODO - check the data
      if(nargs() >= 2) {
       self$ReadData()
      }
    },
    SetParticipant = function(string){
      self$participant = string
    },
    ReadData = function(override = F){
      private$readData(override)
    }
  ),
  private = list(
    rootDataDirectory = NULL,
    setDataDirectory = function(directory = private$rootDataDirectory, id = self$participant){
      if (is.null(directory)) stop("no directory set")
      private$rootDataDirectory = directory
      self$dataDirectory = paste(directory,id,sep="/")
      return(self$dataDirectory)
    },
    readData = function(){} #usually overriden
  )
)