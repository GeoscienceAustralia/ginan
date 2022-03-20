'''Base functions for file reading'''
import base64
import gzip
import hashlib
import logging
import os as _os
import tarfile

import unlzw

MB = 1024 * 1024

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
    if isinstance(path, bytes): # no reading is necessary - pass through.
        return path

    if path.endswith('.Z'):
        databytes = _lzw2bytes(path)
    elif path.endswith('.gz'):
        databytes = _gz2bytes(path)
    else:
        databytes = _txt2bytes(path)
    return databytes

def tar_reset(tarInfo):
    tarInfo.uid = tarInfo.gid = tarInfo.mtime = 0
    tarInfo.uname = tarInfo.gname = "root"
    tarInfo.pax_headers = {}
    return tarInfo

def tar_comp(srcpath,destpath,reset_info=False,compression = 'bz2'):
    '''tar and compress a directory'''
    with tarfile.open(destpath, f"w:{compression}") as tar:
        logging.info(msg='Compressing {} to {}'.format(srcpath,destpath))
        tar.add(srcpath, arcname=_os.path.basename(srcpath),filter=tar_reset if reset_info else None)

def tar_extr(srcpath,destpath):
    with tarfile.open(srcpath,"r:*") as tar:
        destpath = _os.path.dirname(srcpath)
        logging.info(msg='Extracting {} to {}'.format(srcpath,destpath))
        tar.extractall(path=destpath)

def compute_checksum(path2file):
    logging.info(f'computing checksum of "{path2file}"')
    with open(path2file,'rb') as file:
        filehash = hashlib.md5()
        while True:
            data = file.read(8 * MB)
            if len(data) == 0:
                break
            filehash.update(data)
    checksum = base64.b64encode(filehash.digest()).decode()
    logging.info(f'Got "{checksum}"')
    return checksum
