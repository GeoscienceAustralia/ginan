# setup script for gn_lib

from setuptools import setup, find_packages

setup(
    name="gn_lib",
    version="0.0.1",
    description="Ginan python scripts",
    author="ga.gov.au",
    packages=find_packages(include=['gn_lib', 'gn_lib.*', 'gn_lib.gn_io', 'gn_lib.gn_io.*']),
)
