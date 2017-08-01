context("Update file inventory")

## Code adopted from an answer by Joris Meys on StackOverflow
## (https://stackoverflow.com/questions/7012796/ping-a-website-in-r), see
## http://manpages.ubuntu.com/manpages/xenial/man8/ping.8.html for further
## options. Arguments 'interval' and 'deadline' are currently disabled when
## function is run on Windows.
ping = function(x, count = 1L, interval = 1, deadline = 5, ...) {

  skip_if_not(curl::has_internet())

  os = Sys.info()[["sysname"]]
  cnt = ifelse(os == "Windows", "-n", "-c")
  out = system2("ping", paste(cnt, count, if (os != "Windows") {
    paste("-i", interval, "-w", deadline)
  }, x), stdout = FALSE, stderr = FALSE, ...)

  if (os == "Windows") {
    return(out == 0) # 1 = 'Ping request could not find host'
  } else {
    return(out %in% 0:1)  # 1 = 'Host found, but not all packages could be sent'
                          # 2 = 'Ping request could not find host'
  }
}

test_that("serverPath() of ECOCAST is reachable", {

  skip_if_not(curl::has_internet())

  for (i in 0:1) {
    x = serverPath(version = i)
    x = paste0(x, "/00FILE-LIST.txt")
    expect_is(readLines(x), "character")

    y = gsub("ecocast", "ecocase", x)
    expect_error(suppressWarnings(readLines(y)), "cannot open the connection")
  }
})

test_that("serverPath() of NASANEX is reachable", {
  x = serverPath("nasanex")
  x = gsub("https://", "", x)
  expect_true(ping(x, count = 3L, interval = .5))

  y = gsub("s3", "s4", x)
  expect_false(ping(y, count = 3L, interval = .5))
})
