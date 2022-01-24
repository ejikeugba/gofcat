### Test environments
* local x86_64-w64-mingw32 (64-bit), 4.1.2 (2021-11-01) ;

-- R CMD check results ------------------------------------------------- gofcat 0.1.0 ----
Duration: 3m 51.8s

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded



## R-hub builder

### Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)

  New submission
  
  Possibly misspelled words in DESCRIPTION:
    CRM (11:72)
    Gertheiss (22:61)
    Hosmer (14:5)
    Lemeshow (14:12)
    Lipsitz (15:21)
    Pulkstenis (15:37)
    Ugba (22:45)
    logloss (21:39)

> On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

0 errors √ | 0 warnings √ | 2 notes x

### Explanations 
- The suggested misspelled words are all correct
- The detritus check seems to be a false positive, no such directory or file 
exist in the temp directory.    


## win-builder

### Test environments
- using platform: x86_64-w64-mingw32 (64-bit), R 4.0.3 (2020-10-10)
- using platform: x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2021-11-06 r81149)

R CMD check results

Status: OK











## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Ejike R. Ugba <ejike.ugba@outlook.com>'
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    CRM (11:72)
    Gertheiss (22:61)
    Hosmer (14:5)
    Lemeshow (14:12)
    Lipsitz (15:21)
    Pulkstenis (15:37)
    Ugba (22:45)
    logloss (21:39)

> On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

0 errors √ | 0 warnings √ | 2 notes x

