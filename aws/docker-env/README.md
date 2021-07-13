This docker image contains all the pre-requisites for `ginan` but not `ginan` itself.
To rebuild it, run the following from `ginan` root:

```bash
export TAG=$(git describe --tags --always)
docker build -t gnssanalysis/ginan-env:latest -f ./aws/docker-env/Dockerfile .
docker tag gnssanalysis/ginan-env:latest gnssanalysis/ginan-env:$TAG
docker login -u gnssanalysis
docker push gnssanalysis/ginan-env:latest
docker push gnssanalysis/ginan-env:$TAG
```
