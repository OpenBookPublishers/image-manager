#!/bin/bash

set -eu
cd $(dirname $0)

rsync -avP --delete ./build/ ../apps/imagemanager/priv/public/

