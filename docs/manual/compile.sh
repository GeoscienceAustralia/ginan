rm -rf build
mkdir -p build
rm GinanManual.pdf*
cd build
latexmk -cd ../main.tex -pdf
cp main.pdf ../GinanManual.pdf
