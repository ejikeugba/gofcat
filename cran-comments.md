## Local CMD checks

### Test environments
- local x86_64-w64-mingw32 (64-bit), 4.1.2 (2021-11-01) 

R CMD check results
0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded



## R-hub builder

### Test environments
- R-hub Windows Server 2022, R-devel, 64 bit      (0 errors √ | 0 warnings √ | 1 notes x)
- R-hub Ubuntu Linux 20.04.1 LTS, R-release, GCC  (0 errors √ | 0 warnings √ | 0 notes √)
- R-hub Fedora Linux, R-devel, clang, gfortran    (0 errors √ | 0 warnings √ | 0 notes √)

R CMD check results

> On Windows Server 2022
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

### About Note 
- The detritus check seems to be a false positive, no such directory or file exist in the temp directory. This happens to be common on rhub windows, especially with the recent Windows Server 2022.    



## win-builder

### Test environments
- using platform: x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2022-02-11 r81718 ucrt)

R CMD check results

Status: OK



## Further comments
- Errors dictated in the previous version (0.1.1), via continuous cran checks, have all been fixed in this latest version.
