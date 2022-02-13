## Local CMD checks

### Test environments
- local x86_64-w64-mingw32 (64-bit), 4.1.2 (2021-11-01) ;

R CMD check results

Duration: 4m 23.6s

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded



## R-hub builder

### Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

R CMD check results

> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)

> On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

0 errors √ | 0 warnings √ | 1 notes x

### Explanations 
- The detritus check seems to be a false positive, no such directory or file 
exist in the temp directory.    


## win-builder

### Test environments
- using platform: x86_64-w64-mingw32 (64-bit), R 4.0.3 (2020-10-10)
- using platform: x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2021-11-06 r81149)

R CMD check results

Status: OK

