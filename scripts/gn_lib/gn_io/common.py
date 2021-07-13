'''Base functions for file reading'''
import gzip
import unlzw

def _lzw2bytes(path):
    '''Decompresses .Z file and outputs the content.
    Memory leak corrected in the github repo v0.1.2'''
    with open(path,'rb') as lzw_file:
        lzw_compressed = lzw_file.read()
    databytes = unlzw.unlzw(lzw_compressed)
    del lzw_compressed
    return databytes

def _gz2bytes(path):
    '''Decompresses .gz file and outputs bytes content'''
    with gzip.open(filename=path,mode='rb') as gz_file:
        databytes = gz_file.read()
    return databytes

def _txt2bytes(path):
    '''Simple file read function'''
    with open(path,'rb') as file:
        databytes = file.read()
    return databytes

def path2bytes(path):
    '''Main file reading function.'''
    if path.endswith('.Z'):
        databytes = _lzw2bytes(path)
    elif path.endswith('.gz'):
        databytes = _gz2bytes(path)
    else:
        databytes = _txt2bytes(path)
    return databytes
