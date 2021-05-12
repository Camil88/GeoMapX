

# DB CONNECTION -----------------------------------------------------------

pool <- pool::dbPool(odbc::odbc(), dsn = "SQL_DSN")

onStop(function() {
  pool::poolClose(pool)
})





