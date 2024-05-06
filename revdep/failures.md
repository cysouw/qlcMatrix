# cliqueMS

<details>

* Version: 1.16.0
* GitHub: https://github.com/osenan/cliqueMS
* Source code: https://github.com/cran/cliqueMS
* Date/Publication: 2023-10-24
* Number of recursive dependencies: 169

Run `revdepcheck::revdep_details(, "cliqueMS")` for more info

</details>

## In both

*   checking whether package ‘cliqueMS’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/cysouw/Github/qlcMatrix/revdep/checks.noindex/cliqueMS/new/cliqueMS.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘cliqueMS’ ...
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 15.0.0 (clang-1500.3.9.4)’
using C++11
using SDK: ‘MacOSX14.4.sdk’
clang++ -arch arm64 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/Rcpp/include' -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/BH/include' -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/RcppArmadillo/include' -I/opt/R/arm64/include     -fPIC  -falign-functions=64 -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/Rcpp/include' -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/BH/include' -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/RcppArmadillo/include' -I/opt/R/arm64/include     -fPIC  -falign-functions=64 -Wall -g -O2  -c findAnnotationR.cpp -o findAnnotationR.o
In file included from findAnnotationR.cpp:1:
./annotationCliqueMSR.h:739:5: warning: variable 'completeroundscore' is uninitialized when used here [-Wuninitialized]
...
clang++ -arch arm64 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/Rcpp/include' -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/BH/include' -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/RcppArmadillo/include' -I/opt/R/arm64/include     -fPIC  -falign-functions=64 -Wall -g -O2  -c findCliquesR.cpp -o findCliquesR.o
clang++ -arch arm64 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/Rcpp/include' -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/BH/include' -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/RcppArmadillo/include' -I/opt/R/arm64/include     -fPIC  -falign-functions=64 -Wall -g -O2  -c findIsotopesR.cpp -o findIsotopesR.o
clang++ -arch arm64 -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -o cliqueMS.so RcppExports.o findAnnotationR.o findCliquesR.o findIsotopesR.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'gfortran' not found
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [cliqueMS.so] Error 1
ERROR: compilation failed for package ‘cliqueMS’
* removing ‘/Users/cysouw/Github/qlcMatrix/revdep/checks.noindex/cliqueMS/new/cliqueMS.Rcheck/cliqueMS’


```
### CRAN

```
* installing *source* package ‘cliqueMS’ ...
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 15.0.0 (clang-1500.3.9.4)’
using C++11
using SDK: ‘MacOSX14.4.sdk’
clang++ -arch arm64 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/Rcpp/include' -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/BH/include' -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/RcppArmadillo/include' -I/opt/R/arm64/include     -fPIC  -falign-functions=64 -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/Rcpp/include' -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/BH/include' -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/RcppArmadillo/include' -I/opt/R/arm64/include     -fPIC  -falign-functions=64 -Wall -g -O2  -c findAnnotationR.cpp -o findAnnotationR.o
In file included from findAnnotationR.cpp:1:
./annotationCliqueMSR.h:739:5: warning: variable 'completeroundscore' is uninitialized when used here [-Wuninitialized]
...
clang++ -arch arm64 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/Rcpp/include' -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/BH/include' -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/RcppArmadillo/include' -I/opt/R/arm64/include     -fPIC  -falign-functions=64 -Wall -g -O2  -c findCliquesR.cpp -o findCliquesR.o
clang++ -arch arm64 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/Rcpp/include' -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/BH/include' -I'/Users/cysouw/Github/qlcMatrix/revdep/library.noindex/cliqueMS/RcppArmadillo/include' -I/opt/R/arm64/include     -fPIC  -falign-functions=64 -Wall -g -O2  -c findIsotopesR.cpp -o findIsotopesR.o
clang++ -arch arm64 -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -o cliqueMS.so RcppExports.o findAnnotationR.o findCliquesR.o findIsotopesR.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'gfortran' not found
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [cliqueMS.so] Error 1
ERROR: compilation failed for package ‘cliqueMS’
* removing ‘/Users/cysouw/Github/qlcMatrix/revdep/checks.noindex/cliqueMS/old/cliqueMS.Rcheck/cliqueMS’


```
