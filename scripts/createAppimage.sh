#!/bin/bash


mkdir linuxDeploy
cd linuxDeploy
wget https://github.com/linuxdeploy/linuxdeploy/releases/download/1-alpha-20230713-1/linuxdeploy-x86_64.AppImage

apt update -y
apt install -y file
chmod a+x linuxdeploy-x86_64.AppImage
./linuxdeploy-x86_64.AppImage --appimage-extract-and-run --appdir AppDir -d /src/pea.desktop -e ../../bin/pea -i /Docs/favicons/favicon-128.png --output appimage