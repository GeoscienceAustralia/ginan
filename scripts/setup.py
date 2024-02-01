from setuptools import setup, find_packages

setup(
    name="gn",
    version="0.1",
    packages=find_packages(),
    package_data={"gn": ["templates/*.yaml"]},
    entry_points={
        "console_scripts": [
            "gn=gn.main:cli",
        ],
    },
)
