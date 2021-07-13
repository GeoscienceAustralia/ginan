#Change into the directory this script is in
cd "$(dirname "${BASH_SOURCE[0]}")"
packer validate packer/rhe7.json
packer build packer/rhe7.json
