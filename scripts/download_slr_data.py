#!/usr/bin/python3
# Requires bs4 ($ pip install bs4)
# Requires a ~/.netrc file containing "machine urs.earthdata.nasa.gov login <username> password <password>" with a login from https://cddis.nasa.gov/archive/slr/products

import requests
from bs4 import BeautifulSoup
import gzip
import shutil
import os
import re

write_loc = "../inputData/slr/downloaded"

data_url = "https://cddis.nasa.gov/archive/slr/data/npt_crd"
orbits_url = "https://cddis.nasa.gov/archive/slr/products/orbits"
satellite = "lageos1"

satellite = input("Input the name of the satellite (e.g. lageos1): ")

print("Downloading crd/npt files")
for j in range (2008, 2021):
    year = str(j)
    for i in range(1, 13):
        if i < 10:
            file_date = year + "0" + str(i)
        else:
            file_date = year + str(i);

        file_name = f"{satellite}_{file_date}.npt"
        url = f"{data_url}/{satellite}/{year}/{file_name}"
        r = requests.get(url)

        if not r.ok:
            continue

        os.makedirs(f"{write_loc}/data/{satellite}/{year}", exist_ok=True)
        with open((f"{write_loc}/data/{satellite}/{year}/{file_name}"), 'wb') as fp:
            print("Downloading: " + file_name)
            for chunk in r.iter_content(chunk_size=1000):
                fp.write(chunk)

print("Downloading orbit data")

orbits_url = f"{orbits_url}/{satellite}"

r = requests.get(orbits_url)

if not r.ok:
    print("Satellite sp3 files not available.")
    exit()

flag = True
base_soup = BeautifulSoup(r.content, "html.parser")
num_directories = base_soup.find_all("a", class_="archiveDirText")
for d in num_directories:
    dir_name = d.text
    dir_r = requests.get(f"{orbits_url}/{dir_name}")

    file_soup = BeautifulSoup(dir_r.content, "html.parser")
    files = file_soup.find_all("a", class_="archiveItemText")
    for f in files:
        f_name = f.text
        match = re.match(f"ilrsa.orb.{satellite}.{dir_name}.v(\d+).sp3.gz", f.text)
        
        if (bool(match)):
            print(f"Downloading: {f.text}")
            flag = False
            file_r = requests.get(f"{orbits_url}/{dir_name}/{f.text}")
            file_path = f"{write_loc}/products/orbits/{satellite}/{f.text}"
            os.makedirs(f"{write_loc}/products/orbits/{satellite}", exist_ok=True)
            with open(file_path, "wb") as fp:
                for chunk in file_r.iter_content(chunk_size=1000):
                    fp.write(chunk)
            with gzip.open(file_path) as f_in:
                    with open(file_path[:-3], "wb") as f_out:
                        shutil.copyfileobj(f_in, f_out)
            os.remove(file_path)
    if flag:
        for f in files:
            f_name = f.text
            match = re.match(f"([a-z]+).orb.{satellite}.{dir_name}.v(\d+).sp3.gz", f.text)

            if (bool(match)):
                print(f"Downloading: {f.text}")
                file_r = requests.get(f"{orbits_url}/{dir_name}/{f.text}")
                os.makedirs(f"{write_loc}/products/orbits/{satellite}", exist_ok=True)
                file_path = f"{write_loc}/products/orbits/{satellite}/{f.text}"
                with open(file_path, "wb") as fp:
                    for chunk in file_r.iter_content(chunk_size=1000):
                        fp.write(chunk)
                with gzip.open(file_path) as f_in:
                    with open(file_path[:-3], "wb") as f_out:
                        shutil.copyfileobj(f_in, f_out)
                os.remove(file_path)

                break



print("All downloads complete")