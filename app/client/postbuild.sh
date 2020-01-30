#!/bin/bash

set -eu
cd $(dirname $0)

rsync -avP --delete ./build/ ../imagemanager/apps/imagemanager/priv/public/

