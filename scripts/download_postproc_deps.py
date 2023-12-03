""" Downloads the dependencies required to post-process
    a Static session RINEX v3 file with Ginan.
"""
from pathlib import Path

from download_model_deps import ModelDependencyDownloader
from download_rinex_deps import download_rinex_deps


def download_deps(rinex_path: Path, target_dir: Path):
    downloader = ModelDependencyDownloader(target_dir)
    downloader.download()

    download_rinex_deps(rinex_path, target_dir)


if __name__ == "__main__":
    target_dir = Path("deps")
    rinex_path = Path("TEST.23o")
    download_deps(rinex_path, target_dir)
