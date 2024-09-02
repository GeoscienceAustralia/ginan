import os
import requests
from bs4 import BeautifulSoup
from datetime import datetime, timedelta
import argparse
from tqdm import tqdm  # Import tqdm for progress bar

# Function to download a file with progress bar
def download_file(url, folder_path, replace=False):
    local_filename = os.path.join(folder_path, url.split('/')[-1])
    
    # Skip download if file exists and replace is not True
    if not replace and os.path.exists(local_filename):
        print(f'Skipping {local_filename}, file already exists.')
        return local_filename

    response = requests.get(url, stream=True)
    response.raise_for_status()
    
    # Get the total file size from the headers
    total_size = int(response.headers.get('content-length', 0))
    
    # Create a progress bar
    with open(local_filename, 'wb') as f, tqdm(
        desc=local_filename,
        total=total_size,
        unit='iB',
        unit_scale=True,
        unit_divisor=1024,
    ) as bar:
        for chunk in response.iter_content(chunk_size=8192):
            f.write(chunk)
            bar.update(len(chunk))
    
    return local_filename

# Function to find file links on a web page that match the specific pattern
def find_file_links(base_url, date_range):
    file_links = []

    # Generate URLs and filenames based on the date range and pattern
    urls_and_filenames = generate_urls_and_filenames(base_url, date_range)

    for url, filename in urls_and_filenames:
        response = requests.get(url)
        response.raise_for_status()
        soup = BeautifulSoup(response.content, 'html.parser')
        for link in soup.find_all('a', href=True):
            href = link['href']
            if href == filename:
                file_links.append(os.path.join(url, href))

    return file_links

# Function to generate URLs and filenames based on date range
def generate_urls_and_filenames(base_url, date_range):
    start_datetime, end_datetime = date_range
    filenames = []
    current_datetime = start_datetime

    while current_datetime <= end_datetime:
        year = current_datetime.strftime('%Y')
        date_str = current_datetime.strftime('%Y%m%d')
        hour_str = current_datetime.strftime('H%H')
        if hour_str in ['H00', 'H06', 'H12', 'H18']:
            filename = f'VMF3_{date_str}.{hour_str}'
            url = f'{base_url}/{year}/'
            filenames.append((url, filename))
        current_datetime += timedelta(hours=6)
        current_datetime = current_datetime.replace(hour=(current_datetime.hour // 6) * 6)

    return filenames

# Function to download the orography file based on grid resolution
def download_orography_file(grid_resolution, folder_path, replace=False):
    orography_base_url = 'https://vmf.geo.tuwien.ac.at/station_coord_files/'
    if grid_resolution == '1x1':
        filename = 'orography_ell_1x1'
    elif grid_resolution == '5x5':
        filename = 'orography_ell_5x5'
    else:
        print(f'Unsupported grid resolution: {grid_resolution}')
        return
    
    url = f'{orography_base_url}/{filename}'
    local_filename = os.path.join(folder_path, filename)
    
    print(f'Downloading orography file: {url}')
    download_file(url, folder_path, replace)
    print('Orography file requested downloaded!')

# Main function to download files from a web page
def download_files_from_webpage(base_url, folder_path, date_range, replace=False):
    if not os.path.exists(folder_path):
        os.makedirs(folder_path)
    
    file_links = find_file_links(base_url, date_range)
    for file_link in file_links:
        print(f'Downloading: {file_link}')
        download_file(file_link, folder_path, replace)
    print('All requested VMF3 files downloaded!')

# Function to parse command-line arguments
def parse_arguments():
    parser = argparse.ArgumentParser(description='Download files from a web page based on date range.')
    parser.add_argument('--start-datetime', type=str, required=True, help='Start datetime in the format YYYY-MM-DD_HH:MM:SS')
    parser.add_argument('--end-datetime', type=str, required=True, help='End datetime in the format YYYY-MM-DD_HH:MM:SS')
    parser.add_argument('--grid-resolution', type=str, choices=['1x1', '5x5'], default='1x1', help='Grid resolution: 1x1 or 5x5 (default: 1x1)')
    parser.add_argument('--download-dir', type=str, default='./downloads', help='Directory to download files (default: ./downloads)')
    parser.add_argument('--orography', action='store_true', help='Download the orography file based on grid resolution')
    parser.add_argument('--replace', action='store_true', help='Replace files if they already exist')
    return parser.parse_args()

# Usage
args = parse_arguments()
start_datetime = datetime.strptime(args.start_datetime, '%Y-%m-%d_%H:%M:%S')
end_datetime = datetime.strptime(args.end_datetime, '%Y-%m-%d_%H:%M:%S')
date_range = (start_datetime, end_datetime)
grid_resolution = args.grid_resolution
folder_path = args.download_dir
replace = args.replace

base_url = f'https://vmf.geo.tuwien.ac.at/trop_products/GRID/{grid_resolution}/VMF3/VMF3_OP'

download_files_from_webpage(base_url, folder_path, date_range, replace)

if args.orography:
    download_orography_file(grid_resolution, folder_path, replace)

