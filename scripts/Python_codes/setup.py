# setup script for gn_lib

from setuptools import setup, find_packages

setup(
    name="gn_lib",
    version="0.0.1",
    description="Ginan python scripts",
    author="ga.gov.au",
    packages=find_packages(include=["gn_lib","gn_lib.*"]),
    scripts=["diffutil.py","log2snx.py","merge_sp3.py","snx2map.py","sp3_compare.py"],
    install_requires=["plotext==4.2", "scipy", "requests", "requests_oauthlib"],
)
