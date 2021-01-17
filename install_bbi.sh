#!/bin/bash
set -ex

while getopts v:p:s: flag
do
    case "${flag}" in
        v) version=${OPTARG};;
        p) path=${OPTARG};;
        s) skip=${OPTARG};;
    esac
done

# if skip flag has been passed ignore
if [ "$skip" ]; then
  echo 'skipping install of bbi'
  exit 0
fi


# check to see if version already there, if so exit
if [ -f "$path" ]; then
   # query bbi version so the expanded command would be like /ephemeral/bbi version = "v3.0.0"
  if [ "$(eval '$path version')" = "$version" ]; then
    echo "bbi version $version already installed, skipping..."
    exit 0
  fi
fi

wget -O bbi.tar.gz -q https://github.com/metrumresearchgroup/bbi/releases/download/$version/bbi_linux_amd64.tar.gz
tar -xzf bbi.tar.gz
rm bbi.tar.gz
chmod +x bbi_linux_amd64/bbi
mv bbi_linux_amd64/bbi $path
