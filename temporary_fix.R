
old_path <- Sys.getenv("PATH")
new_path <- paste("C:\\rtools45\\mingw64\\bin", 
                  paste(strsplit(old_path, ";")[[1]][!grepl("x86_64-w64-mingw32.static.posix", strsplit(old_path, ";")[[1]])], collapse=";"), 
                  sep=";")
Sys.setenv(PATH = new_path)


# cmdstan_path <- cmdstanr::cmdstan_path()
# unlink(file.path(cmdstan_path, "bin"), recursive = TRUE)
# unlink(file.path(cmdstan_path, "make", "build"), recursive = TRUE)  # optional extra

