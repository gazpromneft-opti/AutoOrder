ErrorHandle <- setRefClass('Errors', 
  fields = list(
    errors = 'list',
    isSaved = 'logical',
    time = 'list',
    timeConst = 'numeric'
  ),
    methods = list(
      initialize = function() {
        if(!require('magrittr')) 
          stop('Install `magrittr` library')
        if(!require('rlist'))
          stop('Install `rlist` library')
        timeConst <<- 25
        time <<- list(
          now = getTime(),
          last = NULL
        )
        isSaved <<- FALSE
        errors <<- list()
      },
      getTime = function() {
        as.POSIXct(Sys.time())
      },
      isNeedToSave = function() {
        if(is.null(time$last)) break
        if(as.numeric(getTime() - time$last) >= timeConst) store();
      },
      store = function() {},
      throw = function(message = 'Unknown error', extra = NULL) {
        errors <<- list.append(errors, list(
          time = getTime(),
          msg = message,
          additional = extra
        ))
        time$last <<- getTime()
        isNeedToSave()
      }
   ))