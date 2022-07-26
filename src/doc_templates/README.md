



```
docker build -t doxyjen .
docker run -v /data/acs/ginan:/data/ginan -it doxyjen
```




Apply the patch located in this directory to enable correctly ordered call graphs to be output in the doxygen documentation,

git clone the doxygen repo to the bottom hash below.

apply the git patch

in the doxygen dir - make using the readme

the doxygen file in Ginan probably points at a different install dir, fix it in either side by chaning install dir or adding a symlink




The git log history is below.

driver@driver-G7-7700:/data/tmp/doxygen/build (master)$ git log
commit be6c9de75f77aae8569eab29be99629c8730fb6d (HEAD -> master)
Author: me <me@email.com>
Date:   Wed Apr 20 14:26:22 2022 +1000

    Patch to make call graphs output in order of calling

commit 77074075dc61f8c1be7fdba940bddbb7315980da
Merge: c88238cf 7b6405bd
Author: Dimitri van Heesch <doxygen@gmail.com>
Date:   Tue Apr 19 20:53:13 2022 +0200


git clone git@github.com:doxygen/doxygen.git
cd doxygen
git checkout 77074075dc61f8c1be7fdba940bddbb7315980da
mv patch ./
git apply 0001-Patch-to-make-call-graphs-output-in-order-of-calling.patch
