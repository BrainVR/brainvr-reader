UnityAnalysis <- R6Class("UnityAnalysis",
  inherit = BaseAnalysis,
 
  #define variables
  public = list(
    #basic definitions
    session = NULL,
    sessionDirectory = NULL,
    playerLog = NULL,
    initialize = function(dir, id, session = NULL, override = F){
      self$SetParticipant(id)
      private$setDataDirectory(dir)
      self$SetSession(session)
      #TODO - check the data
      if(nargs() >= 2) {
        self$ReadData(override)
      }
    },
    #define what is valid in the current context
    SetSession = function(session){
      self$session = if(is.null(session)) NULL else paste("Session", session, sep="")
      return(private$setSessionDirectory())
    },
    DrawTrialImage = function(trialID){
      plot = MakeTrialImage(self$playerLog, self$tests[[1]], trialID)
      plot
      return(plot)
    },
    TrialInfo = function(trialID){
      ls = list()
      test = self$tests[[1]]
      ls = trial_info(test, trialID, self$playerLog)
      return(ls)
    },
    # returns only results from a first test, even if there were multiple
    TestResults = function(force = F, i_test = 1){
      if (!is.null(private$resultsTable) & !force) return(private$resultsTable)
      ls = list()
      test = self$tests[[i_test]]
      private$resultsTable = test_results(test, self$playerLog)
      return(private$resultsTable)
    },
    ExportPlayerLog = function(path = getwd()){
      export_player_log(self$playerLog, self$participant, path = path)
    },
    ExportEvents = function(i_test = 1){
      df = collect_events(self$tests[[i_test]], self$playerLog)
      filePath = paste(getwd(), "/", self$participant, ".events", sep = "")
      write.table(df, filePath, sep = ";", quote = F, row.names = F)
    }
  ),
  private = list(
    #fields
    resultsTable = NULL,
    isValid = function(){
      return(TRUE)
    },
    setSessionDirectory = function(){
      if(is.null(self$dataDirectory)) private$setDataDirectory()
      self$sessionDirectory = paste(self$dataDirectory, self$session, sep = "/")
      return(self$sessionDirectory)
    },
    readData = function(override = F, save = T){
      #checks for path
      if (is.null(self$sessionDirectory)) stop("no session directory set")
      #open experiment_logs to see how many do we have
      experimentLog = open_experiment_logs(self$sessionDirectory)
      
      if(is.null(experimentLog)) stop("Experiment log not found")
      #if multiple logs or no logs, quit
      
      self$playerLog = open_player_log(self$sessionDirectory, override = override)
      if(is.null(self$playerLog)) stop("Player log not found")
      #preprocesses player log
      #checks if there is everything we need and if not, recomputes the stuff
      changed = private$preprocessPlayerLog()
      if (changed & save) save_preprocessed_player(self$sessionDirectory, self$playerLog)
        
      testLogs = open_test_logs(self$sessionDirectory)
      for (i in length(testLogs)){
        self$tests[[i]] = testLogs[[i]]
      }
     
      private$isValid()
      #checks if multiple sessions
      #asks for session
      #loads experiment log
      #loads player log
      #logs all quest logs
    },
    preprocessPlayerLog = function(){
      #check_stuff
      #check columns
      
      #this should work as data.tables are passed by reference
      changed = preprocess_player_log(self$playerLog)
      return(changed)
    }
  )
)
