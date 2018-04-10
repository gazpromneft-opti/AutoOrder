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


DataBase <- setRefClass('DB', 
  fields = list(
    server = 'character',                   # 10.225.47.88
    port = 'numeric',                       # 1433
    user =  'character',                    # root
    password = 'character',                 # ********
    database = 'character',                 # optidb
    encrypt = 'character',                  # no
    trust_server_certificate = 'character', # yes,
    con = 'Microsoft SQL Server',
    tbl = 'character'
  ),
  contains = 'Errors',
  methods = list(
    initialize = function() {
      
      if(!require('DBI'))  stop('Install `odbc` library')
      if(!require('magrittr')) stop('Install `magrittr` library')
      
      server <<- '10.225.47.88'
      port <<- 1433
      user <<- 'root'
      password <<- rstudioapi::askForPassword("Database password")
      database <<- 'optidb'
      encrypt <<- 'no'
      trust_server_certificate <<- 'yes'
      
    },
    connect = function() {
      con <<- dbConnect(odbc(),
         Driver = "SQL Server",
         Server = server,
         Database = database,
         UID = user,
         PWD = password,
         Port = port
       ) 
    },
    tables = function(...) {
      tbl <<- dbListTables(con, ...)
    },
    listInfo = function(name) {
      dbListFields(con, name)
    },
    query = function(sql, number) {
      dbSendQuery(con, sql) %>%
        dbFetch(n = number)
    },
    full = function(name) {
      dbReadTable(con, name)
    },
    
    # Work with DB without SQL
    # Params:
    #  @target [String]: what need to GET
    # Fetch data from DB
    #@return [Data.Frame]
    
    q = function(target) {
      switch(target
        
      )
    }
  )
)

db <- DataBase$new()
db$connect()
db$tables()
db$tbl %>% as.data.frame() %>% View()
db$listInfo('messages')
db$query('SELECT text, message_id FROM messages', 10)

db$full('messages')
?dbReadTabledbReadTable
?dbReadTable